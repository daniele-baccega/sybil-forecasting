# Sybil
#
# Author: Daniele Baccega
# Data: COVID19 R library
# Prophet: https://facebook.github.io/prophet/docs/quick_start.html

# Download files and load data
#
# Inputs:
#   - country_long:               name of the interested country (e.g. Italy, Austria)
#   - global_final_date:          final date for the data
#   - reproduce:                  reproduce the results of the paper
#   - variants                    true if we are considering variants, false otherwise
#   - variants_to_disregard:      variants not to be considered
#   - variants_aggregated:        aggregation of variants (must be a list)
#   - variants_aggregated_names:  names of the aggregated variants (must have the same length of variants_aggregated)
#
# Output:
#   - df_COVID19_ref_init:        dataframe with Covid-19 data
#   - df_variants_init:           dataframe with variants data
#   - updated_file:               false if the files were not update, true otherwise
download_files_and_load_data <- function(country_long, global_final_date, reproduce, variants, variants_to_disregard = list(), variants_aggregated = list(), variants_aggregated_names = list()){
  if(!is.list(variants_aggregated) || !is.list(variants_aggregated_names))
    stop("Variables variants_aggregated and variants_aggregated_names must be lists!")
  
  if(length(variants_aggregated) != length(variants_aggregated_names))
    stop("Variables variants_aggregated and variants_aggregated_names must have the same size!")
  
  if(!file.exists("datasets")){
    system("mkdir datasets")
  }
  
  covid19_data <- paste0("datasets/", country_long, "_", Sys.Date(), ".csv")
  covid19_variants_data <- paste0("datasets/variants_data_", Sys.Date(), ".csv")
  
  # Check if the files are updated
  updated_file <- file.exists(covid19_data) && file.exists(covid19_variants_data)
  
  if(reproduce){
    updated_file <- TRUE
    covid19_data <- paste0("datasets/", country_long, "_2023-11-22.csv")
    if(country_long %in% c("France", "Belgium"))
      covid19_data <- paste0("datasets/", country_long, "_2024-05-02.csv")
    covid19_variants_data <- "datasets/variants_data_2023-07-25.csv"
  }
  
  if(!updated_file){
    # Download the updated data
    covid19(country = country_long, level = 3, start = "2020-01-01", end = global_final_date, dir = '.')
    system(paste0("rm ", Sys.Date(), "/country/index.csv"))
    if(length(list.files(paste0(Sys.Date(), "/country/"))) == 0)
      stop(paste0("Country ", country_long, " not found in COVID19 library data"))
    system(paste0("mv ", Sys.Date(), "/country/*.csv ", covid19_data))
    system(paste0("rm -r ", Sys.Date()))
    
    download.file("https://opendata.ecdc.europa.eu/covid19/virusvariant/csv/data.csv", covid19_variants_data)
  }
  
  # Read and preprocess the data
  df_COVID19_ref_init <- read.csv(covid19_data)
  df_COVID19_ref_init[df_COVID19_ref_init == ""] <- NA
  df_COVID19_ref_init$confirmed[is.na(df_COVID19_ref_init$confirmed) & df_COVID19_ref_init$date <= "2020-04-01"] <- 0
  df_COVID19_ref_init$deaths[is.na(df_COVID19_ref_init$deaths) & df_COVID19_ref_init$date <= "2020-04-01"] <- 0
  
  df_COVID19_ref_init <- df_COVID19_ref_init %>%
    filter(is.na(administrative_area_level_2), is.na(administrative_area_level_3))
  
  df_COVID19_ref_init <- df_COVID19_ref_init %>%
    mutate(confirmed = na.approx(confirmed, na.rm = FALSE), deaths = na.approx(deaths, na.rm = FALSE), recovered = na.approx(recovered, na.rm = FALSE))
  
  df_COVID19_ref_init <- df_COVID19_ref_init %>%
    filter(!is.na(confirmed), !is.na(deaths), !is.na(recovered), date <= global_final_date)
  
  df_variants_init <- data.frame()
  if(variants){
    df_variants_init <- read.csv(covid19_variants_data)
    variants_countries <- unique(df_variants_init$country)
    
    if(!country_long %in% variants_countries)
      stop(paste0("Country ", country_long, " not found in variants data"))
    
    df_variants_init <- df_variants_init %>%
      filter(country == country_long, !is.na(new_cases), year_week <= format(as.Date(global_final_date), format="%Y-%U"), source == "GISAID", ! variant %in% variants_to_disregard) %>%
      mutate(year = as.integer(substr(year_week, 1, 4)),
             week = as.integer(substr(year_week, 6, 7)),
             number_detections_variant = as.integer(number_detections_variant),
             number_sequenced_known_variant = as.integer(number_sequenced_known_variant),
             percent_variant = number_detections_variant / number_sequenced_known_variant) %>%
      filter(!(year == 2020 & week %in% seq(1, 8)))
  
    df_variants_init <- df_variants_init %>%
      mutate(variant = gsub("/", "-", variant))
    df_variants_init$percent_variant[is.na(df_variants_init$percent_variant)] <- 0
    
    # Aggregate variants
    for(i in 1:length(variants_aggregated)){
      df_variants_init$variant[which(df_variants_init$variant %in% variants_aggregated[[i]])] <- variants_aggregated_names[[i]]
    }
    
    df_variants_init <- df_variants_init %>%
      group_by(year_week) %>%
      aggregate(percent_variant ~ year_week + variant + week + year, FUN=sum) %>%
      arrange(year_week)
    
    df_variants_init <- filter_variants(df_variants_init)
    
    df_variants_init$percent_variant[which(df_variants_init$percent_variant > 1.0)] <- 1.0
  }
  
  return(list(df_COVID19_ref_init, df_variants_init, updated_file))
}

# Filter out small values in the variants proportion
#
# Input:
#   - df_variants_init: dataframe with variants data
#
# Output:
#   . df_variants:      dataframe with variants data (filtered)
filter_variants <- function(df_variants_init){
  variants_names <- unique(df_variants_init$variant)
  
  other_proportion <- c()
  df_variants <- data.frame()
  for(v in variants_names){
    df_variants_local <- df_variants_init %>%
      filter(variant == v)
    
    n <- nrow(df_variants_local)
    
    peak <- first(which(df_variants_local$percent_variant == max(df_variants_local$percent_variant)))
    
    right_peak <- match(0, df_variants_local$percent_variant[seq(peak, n)]) + peak - 1
    left_peak <- peak - match(0, rev(df_variants_local$percent_variant[seq(1, peak)])) + 1
    
    if(is.na(right_peak)){
      right_peak <- n
    }
    
    if(is.na(left_peak)){
      left_peak <- 1
    }
    
    variants_local_other <- df_variants_local$percent_variant
    variants_local_other[left_peak:right_peak] <- 0
    if(length(other_proportion) == 0){
      other_proportion <- variants_local_other
    }
    else{
      other_proportion <- other_proportion + variants_local_other
    }
    
    df_variants_local$percent_variant[-c(left_peak:right_peak)] <- 0
    df_variants <- rbind(df_variants, df_variants_local)
  }
  
  df_variants$percent_variant[which(df_variants$variant == "Other")] <- df_variants$percent_variant[which(df_variants$variant == "Other")] + other_proportion
  df_variants$percent_variant[which(df_variants$variant == "Other" & df_variants$year == 2020 & df_variants$week < 45)] <- 1.0
  df_variants$percent_variant[which(df_variants$variant != "Other" & df_variants$year == 2020 & df_variants$week < 45)] <- 0.0
  
  return(df_variants)
}

# Generate the SIRD model.
#
# Inputs:
#   - dir_name:               name of the directory in which put the results
#   - df_COVID19_ref:         dataframe with Covid-19 data
#   - df_variants_ref:        dataframe with variants data
#   - immunization_end_rate:  immunization end rate
#   - global_final_date:      final date for the data
#   - variants                    true if we are considering variants, false otherwise
#   . new_data:               true if the files were not update, false otherwise
#
# Output:
#   - df_variants_ref:        dataframe with variants data (after preprocessing)
#   - df_COVID19_ref:         dataframe with Covid-19 data (after preprocessing)
#   - SIRD_all:               evolution of the infection using a SIRD model
#   - results_all:            infection, recovery and fatality rates extracted from the SIRD model
compute_data <- function(dir_name, df_COVID19_ref, df_variants_ref, immunization_end_rate, global_final_date, variants, new_data=FALSE){
  if(file.exists(paste0(dir_name, "/data/date.RData")) && !new_data){
    load(paste0(dir_name, "/data/date.RData"))
    new_data <- !(today == Sys.Date())
  }
  else{
    new_data <- TRUE
  }
  
  if(new_data){
    # Preprocess data
    N <- df_COVID19_ref$population[1]
     
    df_COVID19_ref <- df_COVID19_ref %>%
      mutate(total_cases = confirmed, total_deaths = deaths, total_recovered = recovered) %>%
      select(date, total_cases, total_deaths, total_recovered, population) %>%
      mutate(new_cases = diff(c(0, total_cases)), new_deaths = diff(c(0, total_deaths)), new_recovered = diff(c(0, total_recovered))) %>%
      filter(!is.na(new_cases), date >= "2020-02-24")
    
    df_COVID19_ref$date <- as.Date(df_COVID19_ref$date)
    
    df_COVID19_ref$week <- as.integer(substr(format(df_COVID19_ref$date, format="%Y-%V"), 6, 7))
    df_COVID19_ref$year <- as.integer(substr(format(df_COVID19_ref$date, format="%Y"), 1, 4))
    
    df_COVID19_ref$year[which(df_COVID19_ref$year == 2021 & df_COVID19_ref$week == 53)] <- 2020
    df_COVID19_ref$year[which(df_COVID19_ref$year == 2022 & df_COVID19_ref$week == 52 & df_COVID19_ref$date <= as.Date("2022-02-01"))] <- 2021
    df_COVID19_ref$year[which(df_COVID19_ref$year == 2023 & df_COVID19_ref$week == 52 & df_COVID19_ref$date <= as.Date("2023-02-01"))] <- 2022
    
    df_COVID19_ref_weekly <- df_COVID19_ref %>%
      group_by(week, year) %>%
      summarise(date = first(date))
    
    df_COVID19_ref_weekly <- df_COVID19_ref_weekly[order(df_COVID19_ref_weekly$date),]
    
    n <- nrow(df_COVID19_ref)
    
    # Build the SIRD model from the data
    S_local <- I_local <- R_local <- D_local <- rep(0, n)
    
    S_local[1] <- N - df_COVID19_ref$total_cases[1]
    I_local[1] <- df_COVID19_ref$total_cases[1]
    R_local[1] <- df_COVID19_ref$total_recovered[1]
    D_local[1] <- df_COVID19_ref$total_deaths[1]
    
    for(t in 2:n){
      S_local[t] <- S_local[t-1] - df_COVID19_ref$new_cases[t] + R_local[t-1] * immunization_end_rate
      I_local[t] <- I_local[t-1] + df_COVID19_ref$new_cases[t] - (df_COVID19_ref$new_deaths[t] + df_COVID19_ref$new_recovered[t])
      R_local[t] <- R_local[t-1] + df_COVID19_ref$new_recovered[t] - R_local[t-1] * immunization_end_rate
      D_local[t] <- D_local[t-1] + df_COVID19_ref$new_deaths[t]
    }
   
    SIRD_all <- data.frame(date=df_COVID19_ref$date, S=S_local, I=I_local, R=R_local, D=D_local)
    
    # Extract the rates
    results_all <- get_rates(SIRD_all[-nrow(SIRD_all),], SIRD_all[nrow(SIRD_all),], immunization_end_rate, rep(N, nrow(SIRD_all)-1))
    
    plot_rates(dir_name, results_all)
    
    if(variants){
      df_COVID19_ref_weekly_variants <- df_COVID19_ref_weekly %>%
        filter(!(year == df_variants_ref$year[nrow(df_variants_ref)] & week > df_variants_ref$week[nrow(df_variants_ref)]))
      
      df_variants_ref <- df_variants_ref %>%
        filter(!((year == df_COVID19_ref_weekly_variants$year[nrow(df_COVID19_ref_weekly_variants)] & week > df_COVID19_ref_weekly_variants$week[nrow(df_COVID19_ref_weekly_variants)]) | (year == df_COVID19_ref_weekly_variants$year[1] & week < df_COVID19_ref_weekly_variants$week[1]) | year > df_COVID19_ref_weekly_variants$year[nrow(df_COVID19_ref_weekly_variants)])) %>%
        arrange(variant)
      
      df_variants_ref$date <- rep(df_COVID19_ref_weekly_variants$date, length(unique(df_variants_ref$variant)))
    }
          
    today <- Sys.Date()
    save(df_variants_ref, df_COVID19_ref, S_local, I_local, R_local, D_local, SIRD_all, results_all, file=paste0(dir_name, "/data/data.RData"))
    save(today, file=paste0(dir_name, "/data/date.RData"))
  }
  else{
    load(paste0(dir_name, "/data/data.RData"))
  }
  
  return(list(df_variants_ref, df_COVID19_ref, SIRD_all, results_all))
}

# Computes the infection, recovery and  fatality rates starting from the system of ODEs of the SIRD model.
#
# Inputs:
#   - SIRD:                   evolution of the infection using a SIRD model
#   - after_date_SIRD:        size of I, R and D in the date after the end date (necessary in the equations)
#   - immunization_end_rate:  immunization end rate
#   - N:                      total population
#
# Output:
#   - results_all:            infection, recovery and fatality rates extracted from the SIRD model
get_rates <- function(SIRD, after_date_SIRD, immunization_end_rate, N){
  fat_rates <- c(diff(SIRD$D), after_date_SIRD$D - SIRD$D[nrow(SIRD)]) / SIRD$I
  rec_rates <- (c(diff(SIRD$R), after_date_SIRD$R - SIRD$R[nrow(SIRD)]) + SIRD$R * immunization_end_rate) / SIRD$I
  infection_rates <- (c(diff(SIRD$I), after_date_SIRD$I - SIRD$I[nrow(SIRD)]) + SIRD$I * (rec_rates + fat_rates)) * (N / (SIRD$S * SIRD$I))
  
  fat_rates[fat_rates < 0] <- 0
  rec_rates[rec_rates < 0] <- 0
  infection_rates[infection_rates < 0] <- 0
  
  results_all <- data.frame(date=SIRD$date, infection_rates, rec_rates, fat_rates)
  
  return(results_all)
}

# Generate variants information and plot
#
# Inputs:
#   - dir_name:           name of the directory in which put the results
#   - df_variants:        dataframe with variants data
#   - df_COVID19_all:     dataframe with Covid-19 data
#   - SIRD_all:           evolution of the infection using a SIRD model
#   - results_all:        infection, recovery and fatality rates extracted from the SIRD model
#
# Output:
#   - variants_global_df: infection, recovery and fatality rates extracted from the SIRD model
#   - df_COVID19_all:     dataframe with Covid-19 data
#   - SIRD_all:           evolution of the infection using a SIRD model
#   - results_all:        infection, recovery and fatality rates extracted from the SIRD model
generate_and_plot_variants_info <- function(dir_name, df_variants, df_COVID19_all, SIRD_all, results_all){
  variants_name <- unique(df_variants$variant)
  
  # Preprocess variants data
  variants_global_df <- data.frame()
  i <- 1
  for(v in variants_name){
    df_variants_local <- df_variants %>%
      filter(variant == v)
    
    n <- nrow(df_variants_local)
    
    df_variants_local$percent_variant <- rollapplyr(df_variants_local$percent_variant, 3, (mean), fill = if(v == "Other") 1 else 0)
    
    variants_spline <- splinefun(seq(1, n*7, 7), df_variants_local$percent_variant, method = "monoH.FC")
    variants_data_spline <- variants_spline(seq(1, length(seq.Date(df_variants_local$date[1], df_variants_local$date[nrow(df_variants_local)], 1))))
    variants_data_spline[variants_data_spline < 0] <- 0
    
    variants_spline_df <- data.frame(date=seq.Date(df_variants_local$date[1], df_variants_local$date[nrow(df_variants_local)], 1), y=variants_data_spline, variant=rep(v, length(variants_data_spline)))
    
    variants_global_df <- rbind(variants_global_df, variants_spline_df)
    
    plot_variant_proportion(dir_name, variants_spline_df, v, variants_name, i)
    
    i <- i + 1
  }
  
  plot_variants_proportion(dir_name, variants_global_df, variants_name)
  
  df_COVID19_all <- df_COVID19_all %>%
    filter(date <= df_variants$date[nrow(df_variants)])
  
  SIRD_all <- SIRD_all %>%
    filter(date <= df_variants$date[nrow(df_variants)])
  
  results_all <- results_all %>%
    filter(date <= df_variants$date[nrow(df_variants)])
      
  return(list(variants_global_df, df_COVID19_all, SIRD_all, results_all))
}

# Generate the SIvRD model.
#
# Inputs:
#   - dir_name:               name of the directory in which put the results
#   - df_variants:            dataframe with variants data
#   - SIRD_all:               evolution of the infection using a SIRD model
#   - results_all:            infection, recovery and fatality rates extracted from the SIRD model
#   - immunization_end_rate:  immunization end rate
#   - N:                      total population
#   - final_dates:            final dates
#
# Output:
#   - SIRD_all_variants:      evolution of the infection using a SIvRD model
#   - df_all_variants:        infection, recovery and fatality rates extracted from the SIvRD model for each variant
SIRD_variants <- function(dir_name, df_variants, SIRD_all, results_all, immunization_end_rate, N, final_dates){
  variants_name <- unique(df_variants$variant)
  
  # Generate the SIvRD model and compute the infection rates for each variant
  SIRD_all_variants <- data.frame()
  infection_rates_all_variants <- c()
  for(i in 1:length(variants_name)){
    df_variants_local <- df_variants %>%
      filter(variant == variants_name[i])
    
    I_variant <- SIRD_all$I * df_variants_local$y
    
    SIRD_variant <- data.frame(date=SIRD_all$date, S=SIRD_all$S, I=I_variant, R=SIRD_all$R, D=SIRD_all$D)
    infection_rates_variant <- (diff(SIRD_variant$I) + SIRD_variant$I[-nrow(SIRD_variant)] * (results_all$rec_rates[-nrow(SIRD_variant)] + results_all$fat_rates[-nrow(SIRD_variant)])) * (rep(N, nrow(SIRD_variant)-1) / (SIRD_variant$S[-nrow(SIRD_variant)] * SIRD_all$I[-nrow(SIRD_variant)]))
    infection_rates_variant[is.na(infection_rates_variant) | is.infinite(infection_rates_variant) | infection_rates_variant < 0] <- 0
    
    infection_rates_all_variants <- c(infection_rates_all_variants, infection_rates_variant)
    
    SIRD_all_variants <- rbind(SIRD_all_variants, data.frame(date=SIRD_variant$date, S=SIRD_variant$S, I=SIRD_variant$I, R=SIRD_variant$R, D=SIRD_variant$D, variant=variants_name[i]))
  }

  df_variants_names <- df_variants %>%
    filter(date != SIRD_all$date[nrow(SIRD_all)])
  
  df_all_variants <- data.frame(date=rep(SIRD_all$date[-nrow(SIRD_all)], length(variants_name)), infection_rates=infection_rates_all_variants, rec_rates=rep(results_all$rec_rates[-nrow(SIRD_variant)], length(variants_name)), fat_rates=rep(results_all$fat_rates[-nrow(SIRD_variant)], length(variants_name)), variant=df_variants_names$variant)

  plot_I_variants(dir_name, SIRD_all_variants, variants_name, final_dates)
  
  plot_infection_rates_variants(dir_name, df_all_variants)
  
  return(list(SIRD_all_variants, df_all_variants))
}

# Gets the data frames.
#
# Inputs:
#   - df_COVID19_ref:         dataframe with Covid-19 data (with the ground truth of the forecast)
#   - SIRD_all:               evolution of the infection using a SIRD model
#   - SIRD_all_variants:      evolution of the infection using a SIvRD model
#   - results_all:            infection, recovery and fatality rates extracted from the SIRD model
#   - results_all_variants:   infection, recovery and fatality rates extracted from the SIvRD model for each variant
#   - initial_date:           initial date (for training)
#   - final_date:             final date (for training)
#   - final_date_ref:         final date (for forecast)
#   - variants:               true if we are considering variants, false otherwise
#
# Output:
#   - df_COVID19_ref_used:  dataframe with Covid-19 data (with the ground truth of the forecast)
#   - df_COVID19_used:      dataframe with Covid-19 data
#   - SIRD_ref_used:        evolution of the infection (with the ground truth of the forecast)
#   - SIRD_used:            evolution of the infection
#   - results_ref_used:     infection, recovery and fatality rates (with the ground truth of the forecast)
#   - results_used:         infection, recovery and fatality rates
filter_data <- function(df_COVID19_ref, SIRD_all, SIRD_all_variants, results_all, results_all_variants, initial_date, final_date, final_date_ref, variants){
  # Reference dataframes
  df_COVID19_ref <- df_COVID19_ref %>%
    filter(date >= initial_date, date <= final_date_ref)
  
  SIRD_all <- SIRD_all %>%
    filter(date >= initial_date, date <= final_date_ref)
  
  results_all <- results_all %>%
    filter(date >= initial_date, date <= final_date_ref)
  
  
  # Dataframes
  df_COVID19 <- df_COVID19_ref %>%
    filter(date <= final_date)

  SIRD <- SIRD_all %>%
    filter(date <= final_date)
  
  results <- results_all %>%
    filter(date <= final_date)
  
  
  # Select the correct dataframes
  df_COVID19_ref_used <- df_COVID19_ref
  SIRD_ref_used <- SIRD_all
  results_ref_used <- results_all
  df_COVID19_used <-df_COVID19
  SIRD_used <- SIRD
  results_used <- results
  
  if(variants){
    SIRD_all_variants <- SIRD_all_variants %>%
      filter(date >= initial_date, date <= final_date_ref)
    
    results_all_variants <- results_all_variants %>%
      filter(date >= initial_date, date <= final_date_ref)
    
    SIRD_variants <- SIRD_all_variants %>%
      filter(date <= final_date)
    
    results_variants <- results_all_variants %>%
      filter(date <= final_date)
    
    SIRD_ref_used <- SIRD_all_variants
    results_ref_used <- results_all_variants
    SIRD_used <- SIRD_variants
    results_used <- results_variants
  }
  
  return(list(df_COVID19_ref_used, df_COVID19_used, SIRD_ref_used, SIRD_used, results_ref_used, results_used))
}

# Applies Prophet.
#
# Inputs:
#   - dir_name:                 name of the directory in which put the results
#   - date:                     training dates
#   - values:                   training values
#   - time_step:                time window to forecast
#   - file_name:                path in which to save/load the forecast
#   - mcmc_samples:             Markov Chain Monte Carlo (MCMC) samples (see Prophet documentation for more details)
#   - changepoint_prior_scale:  change point prior scale (see Prophet documentation for more details)
#
# Output:
#   - forecast:                 forecast
apply_Prophet <- function(dir_name, date, values, time_step, file_name, mcmc_samples=0, changepoint_prior_scale=0.05){
  profet_df <- data.frame(ds=date, y=values)
  
  path <- paste0(dir_name, "/prophet_models/model_", file_name, ".RData")
  if(file.exists(path)){
    load(path)
  }
  else{
    m <- prophet(profet_df, interval.width=0.8, changepoint.prior.scale = changepoint_prior_scale, mcmc.samples=mcmc_samples)
    save(m, file=path)
  }
  
  future <- make_future_dataframe(m, periods=time_step, freq="day", include_history = FALSE)
  forecast <- predict(m, future)
  
  return(forecast)
}

# Applies Neural Prophet.
#
# Inputs:
#   - dir_name:                 name of the directory in which put the results
#   - date:                     training dates
#   - values:                   training values
#   - time_step:                time window to forecast
#   - variant:                  variant
#
# Output:
#   - forecast:                 forecast
apply_NeuralProphet <- function(dir_name, date, values, time_step, variant){
  prophet_df <- data.frame(ds=date, y=values)

  write.csv(prophet_df, paste0(dir_name, "neural_prophet_models/neural_prophet_data_", variant, ".csv"), row.names = FALSE)
  
  system(paste0("bash neural_prophet.sh ", dir_name, "neural_prophet_models/ ", variant))
}

# Applies ARIMA.
#
# Inputs:
#   - dir_name:                 name of the directory in which put the results
#   - date:                     training dates
#   - values:                   training values
#   - time_step:                time window to forecast
#   - file_name:                path in which to save/load the forecast
#
# Output:
#   - forecast:                 forecast
apply_Arima<- function(dir_name, date, values, time_step, file_name){
  path <- paste0(dir_name, "/arima_models/model_", file_name, ".RData")
  
  if(file.exists(path)){
    load(path)
  }
  else{
    m <- auto.arima(values, seasonal = FALSE)
    save(m, file=path)
  }
  
  forecast <- forecast(m, h = time_step)
  
  return(forecast)
}

# Applies SARIMA.
#
# Inputs:
#   - dir_name:                 name of the directory in which put the results
#   - date:                     training dates
#   - values:                   training values
#   - time_step:                time window to forecast
#   - file_name:                path in which to save/load the forecast
#
# Output:
#   - forecast:                 forecast
apply_seasonal_Arima <- function(dir_name, date, values, time_step, file_name){
  path <- paste0(dir_name, "/sarima_models/model_", file_name, ".RData")
  
  if(file.exists(path)){
    load(path)
  }
  else{
    m <- auto.arima(values, seasonal = TRUE)
    save(m, file=path)
  }
  
  forecast <- forecast(m, h = time_step)
  
  return(forecast)
}

# Applies LSTM (python)
#
# Inputs:
#   - dir_name:                 name of the directory in which put the results
#   - date:                     training dates
#   - values:                   training values
#   - time_step:                time window to forecast
#   - variant:                  variant
#
# Output:
#   - forecast:                 forecast
apply_LSTM <- function(dir_name, date, values, time_step, variant){
  lstm_df <- data.frame(ds=date, y=values)
  
  write.csv(lstm_df, paste0(dir_name, "lstm_models/lstm_data_", variant, ".csv"), row.names = FALSE)
  
  system(paste0("bash lstm.sh ", dir_name, "lstm_models/ ", variant))
}

# Applies GRU (python)
#
# Inputs:
#   - dir_name:                 name of the directory in which put the results
#   - date:                     training dates
#   - values:                   training values
#   - time_step:                time window to forecast
#   - variant:                  variant
#
# Output:
#   - forecast:                 forecast
apply_GRU <- function(dir_name, date, values, time_step, variant){
  gru_df <- data.frame(ds=date, y=values)
  
  write.csv(gru_df, paste0(dir_name, "gru_models/gru_data_", variant, ".csv"), row.names = FALSE)

  system(paste0("bash gru.sh ", dir_name, "gru_models/ ", variant))
}

# Applies EpiNow.
#
# Inputs:
#   - date:                     training dates
#   - values:                   training values
#
# Output:
#   - forecast:                 forecast
apply_EpiNow <- function(date, values){
  epinow_df <- data.frame(date=as.Date(as.numeric(date)), confirm=as.integer(exp(values)))
  
  generation_time <- get_generation_time(
    disease = "SARS-CoV-2", source = "ganyani", max = 10, fixed = TRUE
  )
  
  incubation_period <- get_incubation_period(
    disease = "SARS-CoV-2", source = "lauer", max = 10, fixed = TRUE
  )
  
  reporting_delay <- dist_spec(
    mean = convert_to_logmean(2, 1), sd = convert_to_logsd(2, 1), max = 10,
    dist = "lognormal"
  )
  
  estimates <- epinow(
    reported_cases = epinow_df,
    generation_time = generation_time_opts(generation_time),
    delays = delay_opts(incubation_period + reporting_delay),
    rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
    stan = stan_opts(cores = 4, control = list(adapt_delta = 0.99)),
    horizon = 28,
    verbose = TRUE)
  
  forecast <- estimates$estimated_reported_cases$summarised$mean[(length(date)+1):length(estimates$estimated_reported_cases$summarised$mean)]

  return(forecast)
}

# Evolution of the deterministic SIRD model.
#
# Inputs:
#   - n:                      length of the training time series
#   - n_ref:                  length of the training time series + the forecast window
#   - N:                      total population
#   - SIRD:                   evolution of the infection using a SIRD/SIvRD model
#   - infection_rates:        infection rates for each variant
#   - global_infection_rates: global infection rates
#   - global_recovery_rates:  global recovery rates
#   - global_fatality_rates:  global fatality rates
#   - immunization_end_rate:  immunization end rate
#   - variants:               true if we are considering variants, false otherwise
#   - time_step:              time window to forecast
#
# Output:
#   - SIRD_ev:                evolution of the SIRD/SIvRD in the considered forecast window model using the forecasted rates
SIRD_det <- function(n, n_ref, N, SIRD, infection_rates, global_infection_rates, global_recovery_rates, global_fatality_rates, immunization_end_rate, variants, time_step){
  if(variants){
    SIRD_all_variants <- SIRD %>%
      group_by(date) %>%
      summarize(S=first(S), I=sum(I), R=first(R), D=first(D))
    
    n <- nrow(SIRD_all_variants)
    n_ref <- n + time_step
    
    S_local_all_variants <- I_local_all_variants <- R_local_all_variants <- D_local_all_variants <- rep(NA, n_ref)
    
    S_local_all_variants[1:n] <- SIRD_all_variants$S
    I_local_all_variants[1:n] <- SIRD_all_variants$I
    R_local_all_variants[1:n] <- SIRD_all_variants$R
    D_local_all_variants[1:n] <- SIRD_all_variants$D
    
    for(t in n:(n_ref-1)){
      S_local_all_variants[t+1] <- S_local_all_variants[t] - global_infection_rates$mean[(t-n)+1] * I_local_all_variants[t] * S_local_all_variants[t] / N + R_local_all_variants[t] * immunization_end_rate
      I_local_all_variants[t+1] <- I_local_all_variants[t] + global_infection_rates$mean[(t-n)+1] * I_local_all_variants[t] * S_local_all_variants[t] / N - I_local_all_variants[t] * (global_recovery_rates$mean[(t-n)+1] + global_fatality_rates$mean[(t-n)+1])
      R_local_all_variants[t+1] <- R_local_all_variants[t] + I_local_all_variants[t] * global_recovery_rates$mean[(t-n)+1] - R_local_all_variants[t] * immunization_end_rate
      D_local_all_variants[t+1] <- D_local_all_variants[t] + I_local_all_variants[t] * global_fatality_rates$mean[(t-n)+1]
    }
    
    variants_name <- unique(infection_rates$variant)
    
    I_local <- data.frame()
    for(i in 1:length(variants_name)){
      infection_rates_variant <- infection_rates %>%
        filter(variant == variants_name[i])
      
      SIRD_variant <- SIRD %>%
        filter(variant == variants_name[i])
      
      I_local_variant <- rep(NA, n_ref)
      I_local_variant[1:n] <- SIRD_variant$I
      
      for(t in n:(n_ref-1)){
        I_local_variant[t+1] <- I_local_variant[t] + infection_rates_variant$mean[(t-n)+1] * I_local_all_variants[t] * S_local_all_variants[t] / N - I_local_variant[t] * (global_recovery_rates$mean[(t-n)+1] + global_fatality_rates$mean[(t-n)+1])
      }
      
      I_local <- rbind(I_local, data.frame(I=I_local_variant, variant=rep(variants_name[i], n_ref)))
    }
    
    SIRD_ev <- data.frame(date=seq.Date(SIRD_all_variants$date[1], SIRD_all_variants$date[n]+time_step, 1), S=rep(S_local_all_variants, length(variants_name)), I=I_local$I, R=rep(R_local_all_variants, length(variants_name)), D=rep(D_local_all_variants, length(variants_name)), variant=I_local$variant)
  }
  else{
    S_local <- I_local <- R_local <- D_local <- rep(NA, n_ref)
    
    S_local[1:n] <- SIRD$S
    I_local[1:n] <- SIRD$I
    R_local[1:n] <- SIRD$R
    D_local[1:n] <- SIRD$D

    for(t in n:(n_ref-1)){
      S_local[t+1] <- S_local[t] - infection_rates$mean[(t-n)+1] * I_local[t] * S_local[t] / N + R_local[t] * immunization_end_rate
      I_local[t+1] <- I_local[t] + infection_rates$mean[(t-n)+1] * I_local[t] * S_local[t] / N - I_local[t] * (global_recovery_rates$mean[(t-n)+1] + global_fatality_rates$mean[(t-n)+1])
      R_local[t+1] <- R_local[t] + I_local[t] * global_recovery_rates$mean[(t-n)+1] - R_local[t] * immunization_end_rate
      D_local[t+1] <- D_local[t] + I_local[t] * global_fatality_rates$mean[(t-n)+1]
    }
    
    SIRD_ev <- data.frame(date=seq.Date(SIRD$date[1], SIRD$date[n] + time_step, 1), S=S_local, I=I_local, R=R_local, D=D_local)
  }
  
  return(SIRD_ev)
}

# Evolution of the deterministic SIRD model and plot.
#
# Inputs:
#   - dir_name:               name of the directory in which put the results
#   - time_step:              time window to forecast
#   - ref_data_flag:          true if ground truth for the forecast window exists, false otherwise
#   - final_date:             final date (for training)
#   - infection_rates:        infection rates for each variant
#   - global_infection_rates: global infection rates
#   - global_recovery_rates:  global recovery rates
#   - global_fatality_rates:  global fatality rates
#   - immunization_end_rate:  immunization end rate
#   - fc_I:                   forecast on I
#   - SIRD:                   evolution of the infection using a SIRD/SIvRD model
#   - SIRD_ref:               evolution of the infection using a SIRD/SIvRD model + the forecast window
#   - N:                      total population
#   - variants:               true if we are considering variants, false otherwise
#
# Output:
#   - SIRD_ev:                evolution of the SIRD/SIvRD in the considered forecast window model using the forecasted rates
SIRD_evolution <- function(dir_name, time_step, ref_data_flag, final_date, infection_rates, global_infection_rates, global_recovery_rates, global_fatality_rates, immunization_end_rate, fc_I, SIRD, SIRD_ref, N, variants){
  n <- length(unique(SIRD$date))
  n_ref <- n + time_step
  
  SIRD_ev <- SIRD_det(n, n_ref, N, SIRD, infection_rates, global_infection_rates, global_recovery_rates, global_fatality_rates, immunization_end_rate, variants, time_step)
  
  plot_SIRD_evolution(SIRD_ev, n, n_ref, dir_name, time_step, ref_data_flag, final_date, infection_rates, fc_I, SIRD, SIRD_ref, variants)
  
  return(SIRD_ev)
}

# Compute Root Mean Squared Error
#
# Input:
#   - actual:    ground-truth
#   - predicted: predictions
#
# Output:
#   - error:     rmse between actual and predicted
rmse <- function(actual, predicted) {
  error <- sqrt(mean((actual - predicted)^2))
  
  return(error)
}

# Compute the forecast error between the two considered approaches.
#
# Input:
#   - real:           ground truth
#   - computed_I:     computed I
#   - computed_rates: computed rates
#   - final_date:     final date (for training)
#   - time_step:      time window to forecast
#   - dir_name:       name of the directory in which put the results
#   - variants:       true if we are considering variants, false otherwise
compute_error <- function(real, computed_I, computed_I_Arima, computed_I_seasonal_Arima, computed_I_EpiNow, computed_rates, final_date, time_step, dir_name, variants){
  if(!file.exists(paste0(dir_name, "/errors"))){
    system(paste0("mkdir ", dir_name, "/errors"))
  }
  
  real <- real %>%
    filter(date > final_date - time_step)
  
  computed_I <- computed_I %>%
    filter(date > final_date - time_step)
  
  computed_I_Arima <- computed_I_Arima %>%
    filter(date > final_date - time_step)
  
  computed_I_seasonal_Arima <- computed_I_seasonal_Arima %>%
    filter(date > final_date - time_step)
  
  computed_I_EpiNow <- computed_I_EpiNow %>%
    filter(date > final_date - time_step)
  
  computed_rates <- computed_rates %>%
    filter(date > final_date - time_step)
  
  if(variants){
    variants_name <- unique(computed_rates$variant)
    for(v in variants_name){
      computed_I_neural_prophet_local <- rep(0, time_step)
      computed_I_lstm_local <- rep(0, time_step)
      computed_I_gru_local <- rep(0, time_step)
      if(file.exists(paste0(dir_name, "neural_prophet_models/forecast_", time_step, "_", v, ".csv"))){
        computed_I_neural_prophet_local <- read.csv(paste0(dir_name, "neural_prophet_models/forecast_", time_step, "_", v, ".csv"), col.names = c("time", "value"))$value
      }
      
      if(file.exists(paste0(dir_name, "lstm_models/forecast_", time_step, "_", v, ".csv"))){
        computed_I_lstm_local <- read.csv(paste0(dir_name, "lstm_models/forecast_", time_step, "_", v, ".csv"), col.names = c("time", "value"))$value
      }

      if(file.exists(paste0(dir_name, "gru_models/forecast_", time_step, "_", v, ".csv"))){
        computed_I_gru_local <- read.csv(paste0(dir_name, "gru_models/forecast_", time_step, "_", v, ".csv"), col.names = c("time", "value"))$value
      }
            
      real_local <- real %>%
        filter(variant == v)
      
      computed_I_local <- computed_I %>%
        filter(variant == v)
      
      computed_I_Arima_local <- computed_I_Arima %>%
        filter(variant == v)
      
      computed_I_seasonal_Arima_local <- computed_I_seasonal_Arima %>%
        filter(variant == v)
      
      computed_I_EpiNow_local <- computed_I_EpiNow %>%
        filter(variant == v)
      
      computed_rates_local <- computed_rates %>%
        filter(variant == v)
      
      
      rmse_I_local <- rmse(real_local$I, computed_I_local$mean)
      rmse_I_Arima_local <- rmse(real_local$I, computed_I_Arima_local$mean)
      rmse_I_seasonal_Arima_local <- rmse(real_local$I, computed_I_seasonal_Arima_local$mean)
      rmse_I_neural_prophet_local <- rmse(real_local$I, computed_I_neural_prophet_local)
      rmse_I_lstm_local <- rmse(real_local$I, computed_I_lstm_local)
      rmse_I_gru_local <- rmse(real_local$I, computed_I_gru_local)
      rmse_I_EpiNow_local <- rmse(real_local$I, computed_I_EpiNow_local)
      rmse_rates_local <- rmse(real_local$I, computed_rates_local$I)
      
      rmse_df_local <- data.frame(type=c("Prophet", "ARIMA", "SARIMA", "Neural Prophet", "LSTM", "GRU", "EpiNow", "Sybil"), value=c(rmse_I_local, rmse_I_Arima_local, rmse_I_seasonal_Arima_local, rmse_I_neural_prophet_local, rmse_I_lstm_local, rmse_I_gru_local, rmse_I_EpiNow_local, rmse_rates_local))
      write.csv(file = paste0(dir_name, "/errors/rmse_", v, "_", time_step, ".csv"), x = rmse_df_local, row.names = FALSE, col.names = FALSE)
    }
  }
  else{
    computed_I_neural_prophet <- read_csv(paste0(dir_name, "neural_prophet_models/forecast_", time_step, "_all.csv"), col_names = c("time", "value"))
    computed_I_lstm <- read_csv(paste0(dir_name, "lstm_models/forecast_", time_step, "_all.csv"), col_names = c("time", "value"))
    computed_I_gru <- read_csv(paste0(dir_name, "gru_models/forecast_", time_step, "_all.csv"), col_names = c("time", "value"))
    
    rmse_I <- rmse(real$I, computed_I$mean)
    rmse_I_Arima <- rmse(real$I, computed_I_Arima$mean)
    rmse_I_seasonal_Arima <- rmse(real$I, computed_I_seasonal_Arima$mean)
    rmse_I_neural_prophet <- rmse(real$I, computed_I_neural_prophet$mean)
    rmse_I_lstm <- rmse(real$I, computed_I_lstm)
    rmse_I_gru <- rmse(real$I, computed_I_gru)
    rmse_I_EpiNow <- rmse(real$I, computed_I_EpiNow)
    rmse_rates <- rmse(real$I, computed_rates$I)
  
    
    rmse_df <- data.frame(type=c("Prophet", "ARIMA", "SARIMA", "Neural Prophet", "LSTM", "GRU", "EpiNow", "Sybil"), value=c(rmse_I, rmse_I_Arima, rmse_I_seasonal_Arima, rmse_I_neural_prophet, rmse_I_lstm, rmse_I_gru, rmse_I_EpiNow, rmse_rates))
    write.csv(file = paste0(dir_name, "/errors/rmse_", time_step, ".csv"), x = rmse_df, row.names = FALSE, col.names = FALSE)  
  }
}