# Sybil
#
# Author: Daniele Baccega
# Prophet: https://facebook.github.io/prophet/docs/quick_start.html

# Generate the SIRD model.
#
# Inputs:
#   - dir_name:               name of the directory in which put the results
#   - df_disease_ref:         dataframe with disease data
#   - df_variants_ref:        dataframe with variants data
#   - immunization_end_rate:  immunization end rate
#   - recovery_rate:          recovery rate
#
# Output:
#   - df_variants_ref:        dataframe with variants data (after preprocessing)
#   - df_disease_ref:         dataframe with disease data (after preprocessing)
#   - SIRD_all:               evolution of the infection using a SIRD model
#   - results_all:            infection, recovery and fatality rates extracted from the SIRD model
compartmental_models <- function(dir_name, df_disease_ref, df_variants_ref, immunization_end_rate, recovery_rate){
  if(file.exists(paste0(dir_name, "/data/date.RData"))){
    load(paste0(dir_name, "/data/date.RData"))
    new_data <- !(today == Sys.Date())
  }
  else{
    new_data <- TRUE
  }
  
  if(new_data){
    N <- df_disease_ref$population[1]
    n <- nrow(df_disease_ref)
    
    if(!daily_spline){
      # Build the SIRD model from the data
      S_local <- I_local <- R_local <- D_local <- rep(0, n)
      
      S_local[1] <- N - df_disease_ref$total_cases[1]
      I_local[1] <- df_disease_ref$total_cases[1]
      R_local[1] <- 0
      D_local[1] <- df_disease_ref$total_deaths[1]
      
      for(t in 2:n){
        S_local[t] <- S_local[t-1] - df_disease_ref$new_cases[t] + R_local[t-1] * immunization_end_rate
        I_local[t] <- I_local[t-1] + df_disease_ref$new_cases[t] - (df_disease_ref$new_deaths[t] + recovery_rate * I_local[t-1])
        R_local[t] <- R_local[t-1] + recovery_rate * I_local[t-1] - R_local[t-1] * immunization_end_rate
        D_local[t] <- D_local[t-1] + df_disease_ref$new_deaths[t]
      }
    }
    else{
      # Weekly data (we need this data to generate daily spline data)
      S_local_weekly <- I_local_weekly <- R_local_weekly <- D_local_weekly <- rep(0, n_weekly)
      
      S_local_weekly[1] <- N - df_disease_ref$total_cases[1]
      I_local_weekly[1] <- df_disease_ref$total_cases[1]
      R_local_weekly[1] <- 0
      D_local_weekly[1] <- df_disease_ref$total_deaths[1]
      
      recovery_rate_weekly <- recovery_rate * 7
      immunization_end_rate_weekly <- immunization_end_rate * 7
      
      for(t in 2:n_weekly){
        S_local_weekly[t] <- S_local_weekly[t-1] - df_disease_ref$new_cases[t] + R_local_weekly[t-1] * immunization_end_rate_weekly
        I_local_weekly[t] <- I_local_weekly[t-1] + df_disease_ref$new_cases[t] - (df_disease_ref$new_deaths[t] + recovery_rate_weekly * I_local_weekly[t-1])
        R_local_weekly[t] <- R_local_weekly[t-1] + recovery_rate_weekly * I_local_weekly[t-1] - R_local_weekly[t-1] * immunization_end_rate_weekly
        D_local_weekly[t] <- D_local_weekly[t-1] + df_disease_ref$new_deaths[t]
      }
      
      
      # Spline data
      S_spline <- splinefun(seq(1, n_weekly*7, 7), S_local_weekly, method = "monoH.FC")
      I_spline <- splinefun(seq(1, n_weekly*7, 7), I_local_weekly, method = "monoH.FC")
      R_spline <- splinefun(seq(1, n_weekly*7, 7), R_local_weekly, method = "monoH.FC")
      D_spline <- splinefun(seq(1, n_weekly*7, 7), D_local_weekly, method = "monoH.FC")
      
      S_local <- S_spline(seq(1, length(df_disease_ref$date)))
      I_local <- I_spline(seq(1, length(df_disease_ref$date)))
      R_local <- R_spline(seq(1, length(df_disease_ref$date)))
      D_local <- D_spline(seq(1, length(df_disease_ref$date)))
    }
   
    SIRD_all <- data.frame(date=df_disease_ref$date, S=S_local, I=I_local, R=R_local, D=D_local)
    
    # Extract the rates
    results_all <- get_rates(SIRD_all[-nrow(SIRD_all),], SIRD_all[nrow(SIRD_all),], immunization_end_rate, rep(N, nrow(SIRD_all)-1))
      
    plot_rates(dir_name, results_all)

    today <- Sys.Date()
    save(df_variants_ref, df_disease_ref, S_local, I_local, R_local, D_local, SIRD_all, results_all, file=paste0(dir_name, "/data/data.RData"))
    save(today, file=paste0(dir_name, "/data/date.RData"))
  }
  else{
    load(paste0(dir_name, "/data/data.RData"))
  }
  
  return(list(df_variants_ref, df_disease_ref, SIRD_all, results_all))
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
#   - dir_name:               name of the directory in which put the results
#   - df_variants:            dataframe with variants data
#   - df_disease_all:         dataframe with disease data
#   - SIRD_all:               evolution of the infection using a SIRD model
#   - results_all:            infection, recovery and fatality rates extracted from the SIRD model
#   - daily_variants_data:    true if data related to variants are daily, false if thery are weekly
#
# Output:
#   - variants_global_df:     infection, recovery and fatality rates extracted from the SIRD model
#   - df_disease_all:         dataframe with disease data
#   - SIRD_all:               evolution of the infection using a SIRD model
#   - results_all:            infection, recovery and fatality rates extracted from the SIRD model
generate_and_plot_variants_info <- function(dir_name, df_variants, df_disease_all, SIRD_all, results_all, daily_variants_data){
  variants_name <- unique(df_variants$variant)
  
  # Preprocess variants data
  variants_global_df <- data.frame()
  i <- 1
  for(v in variants_name){
    df_variants_local <- df_variants %>%
      filter(variant == v)
    
    n <- nrow(df_variants_local)
    
    df_variants_local$percent_variant <- c(rep(df_variants_local$percent_variant[1], 2), rollapplyr(df_variants_local$percent_variant, 3, (mean), align = "right"))
    
    variants_data_spline <- df_variants_local
    if(!daily_variants_data){
      variants_spline <- splinefun(seq(1, n*7, 7), df_variants_local$percent_variant, method = "monoH.FC")
      variants_data_spline <- variants_spline(seq(1, length(seq.Date(df_variants_local$date[1], df_variants_local$date[nrow(df_variants_local)], 1))))
    }
    
    variants_data_spline[variants_data_spline < 0] <- 0
      
    variants_spline_df <- data.frame(date=seq.Date(df_variants_local$date[1], df_variants_local$date[nrow(df_variants_local)], 1), y=variants_data_spline, variant=rep(v, length(variants_data_spline)))
    
    variants_global_df <- rbind(variants_global_df, variants_spline_df)
    
    plot_variant_proportion(dir_name, variants_spline_df, v, variants_name, i)
    
    i <- i + 1
  }
  
  plot_variants_proportion(dir_name, variants_global_df, variants_name)
  
  df_disease_all <- df_disease_all %>%
    filter(date <= df_variants$date[nrow(df_variants)])
  
  SIRD_all <- SIRD_all %>%
    filter(date <= df_variants$date[nrow(df_variants)])
  
  results_all <- results_all %>%
    filter(date <= df_variants$date[nrow(df_variants)])
      
  return(list(variants_global_df, df_disease_all, SIRD_all, results_all))
}

# Generate the SIvRD model.
#
# Inputs:
#   - dir_name:                   name of the directory in which put the results
#   - df_variants:                dataframe with variants data
#   - SIRD_all:                   evolution of the infection using a SIRD model
#   - results_all:                infection, recovery and fatality rates extracted from the SIRD model
#   - immunization_end_rate:      immunization end rate
#   - N:                          total population
#
# Output:
#   - SIRD_all_variants:      evolution of the infection using a SIvRD model
#   - df_all_variants:        infection, recovery and fatality rates extracted from the SIvRD model for each variant
SIRD_variants <- function(dir_name, df_variants, SIRD_all, results_all, immunization_end_rate, N){
  variants_name <- unique(df_variants$variant)
  
  SIRD_all_used <- SIRD_all
  results_all_used <- results_all
  
  # Generate the SIvRD model and compute the infection rates for each variant
  SIRD_all_variants <- data.frame()
  infection_rates_all_variants <- c()
  for(i in 1:length(variants_name)){
    df_variants_local <- df_variants %>%
      filter(variant == variants_name[i])
    
    I_variant <- SIRD_all_used$I * df_variants_local$y
    
    SIRD_variant <- data.frame(date=SIRD_all_used$date, S=SIRD_all_used$S, I=I_variant, R=SIRD_all_used$R, D=SIRD_all_used$D)
    infection_rates_variant <- (diff(SIRD_variant$I) + SIRD_variant$I[-nrow(SIRD_variant)] * (results_all_used$rec_rates[-nrow(SIRD_variant)] + results_all_used$fat_rates[-nrow(SIRD_variant)])) * (rep(N, nrow(SIRD_variant)-1) / (SIRD_variant$S[-nrow(SIRD_variant)] * SIRD_all_used$I[-nrow(SIRD_variant)]))
    infection_rates_variant[is.na(infection_rates_variant) | is.infinite(infection_rates_variant) | infection_rates_variant < 0] <- 0
    
    infection_rates_all_variants <- c(infection_rates_all_variants, infection_rates_variant)
    
    SIRD_all_variants <- rbind(SIRD_all_variants, data.frame(date=SIRD_variant$date, S=SIRD_variant$S, I=SIRD_variant$I, R=SIRD_variant$R, D=SIRD_variant$D, variant=variants_name[i]))
  }

  df_variants_names <- df_variants %>%
    filter(date != SIRD_all_used$date[nrow(SIRD_all_used)])
  
  df_all_variants <- data.frame(date=rep(SIRD_all_used$date[-nrow(SIRD_all_used)], length(variants_name)), infection_rates=infection_rates_all_variants, rec_rates=rep(results_all_used$rec_rates[-nrow(SIRD_variant)], length(variants_name)), fat_rates=rep(results_all_used$fat_rates[-nrow(SIRD_variant)], length(variants_name)), variant=df_variants_names$variant)
  
  plot_infection_rates_variants(dir_name, df_all_variants)
  
  return(list(SIRD_all_variants, df_all_variants))
}

# Save data in a csv file.
#
# Inputs:
#   - dir_name: name of the directory in which put the results
#   - data:     data to save in a csv
#   - type:     file name
save_csv <- function(dir_name, results_all, type){
  write.csv(results_all, paste0(dir_name, "/", type, ".csv"))
}

# Gets the data frames.
#
# Inputs:
#   - df_disease_ref:         dataframe with disease data (with the ground truth of the forecast)
#   - SIRD_all:               evolution of the infection using a SIRD model
#   - SIRD_all_variants:      evolution of the infection using a SIvRD model
#   - results_all:            infection, recovery and fatality rates extracted from the SIRD model
#   - results_all_variants:   infection, recovery and fatality rates extracted from the SIvRD model for each variant
#   - initial_date:           initial date (for training)
#   - final_date:             final date (for training)
#   - final_date_ref:         final date (for forecast)
#   - daily_spline            true if we approximate daily data with a spline, false otherwise
#   - variants:               true if we are considering variants, false otherwise
#
# Output:
#   - df_disease_ref_used:  dataframe with disease data (with the ground truth of the forecast)
#   - df_disease_used:      dataframe with disease data
#   - SIRD_ref_used:        evolution of the infection (with the ground truth of the forecast)
#   - SIRD_used:            evolution of the infection
#   - results_ref_used:     infection, recovery and fatality rates (with the ground truth of the forecast)
#   - results_used:         infection, recovery and fatality rates
filter_data <- function(df_disease_ref, SIRD_all, SIRD_all_variants, results_all, results_all_variants, initial_date, final_date, final_date_ref, daily_spline, variants){
  # Reference dataframes
  df_disease_ref <- df_disease_ref %>%
    filter(date >= initial_date, date <= final_date_ref)
  
  SIRD_all <- SIRD_all %>%
    filter(date >= initial_date, date <= final_date_ref)
  
  results_all <- results_all %>%
    filter(date >= initial_date, date <= final_date_ref)
  
  
  # Dataframes
  df_disease <- df_disease_ref %>%
    filter(date <= final_date)

  SIRD <- SIRD_all %>%
    filter(date <= final_date)
  
  results <- results_all %>%
    filter(date <= final_date)
  
  
  # Select the correct dataframes
  df_disease_ref_used <- df_disease_ref
  SIRD_ref_used <- SIRD_all
  results_ref_used <- results_all
  df_disease_used <-df_disease
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
  
  return(list(df_disease_ref_used, df_disease_used, SIRD_ref_used, SIRD_used, results_ref_used, results_used))
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
apply_Prophet <- function(dir_name, date, values, time_step, file_name, mcmc_samples=1000, changepoint_prior_scale=0.05){
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

# Evolution of the deterministic SIRD model.
#
# Inputs:
#   - n:                      length of the training time series
#   - n_ref:                  length of the training time series + the forecast window
#   - N:                      total population
#   - SIRD:                   evolution of the infection using a SIRD/SIvRD model
#   - infection_rates:        infection rates for each variant
#   - global_infection_rates: global infection rates
#   - recovery_rate:          recovery rate
#   - global_fatality_rates:  global fatality rates
#   - immunization_end_rate:  immunization end rate
#   - variants:               true if we are considering variants, false otherwise
#   - time_step:              time window to forecast
#
# Output:
#   - SIRD_ev:                evolution of the SIRD/SIvRD in the considered forecast window model using the forecasted rates
SIRD_det <- function(n, n_ref, N, SIRD, infection_rates, global_infection_rates, recovery_rate, global_fatality_rates, immunization_end_rate, variants, time_step){
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
      I_local_all_variants[t+1] <- I_local_all_variants[t] + global_infection_rates$mean[(t-n)+1] * I_local_all_variants[t] * S_local_all_variants[t] / N - I_local_all_variants[t] * (recovery_rate + global_fatality_rates$mean[(t-n)+1])
      R_local_all_variants[t+1] <- R_local_all_variants[t] + I_local_all_variants[t] * recovery_rate - R_local_all_variants[t] * immunization_end_rate
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
        I_local_variant[t+1] <- I_local_variant[t] + infection_rates_variant$mean[(t-n)+1] * I_local_all_variants[t] * S_local_all_variants[t] / N - I_local_variant[t] * (recovery_rate + global_fatality_rates$mean[(t-n)+1])
      }
      
      I_local <- rbind(I_local, data.frame(I=I_local_variant, variant=rep(variants_name[i], n_ref)))
    }
    
    SIRD_ev <- data.frame(date=seq.Date(SIRD_all_variants$date[1], SIRD_all_variants$date[n] + time_step, 1), S=rep(S_local_all_variants, length(variants_name)), I=I_local$I, R=rep(R_local_all_variants, length(variants_name)), D=rep(D_local_all_variants, length(variants_name)), variant=I_local$variant)
  }
  else{
    S_local <- I_local <- R_local <- D_local <- rep(NA, n_ref)
    
    S_local[1:n] <- SIRD$S
    I_local[1:n] <- SIRD$I
    R_local[1:n] <- SIRD$R
    D_local[1:n] <- SIRD$D

    for(t in n:(n_ref-1)){
      S_local[t+1] <- S_local[t] - infection_rates$mean[(t-n)+1] * I_local[t] * S_local[t] / N + R_local[t] * immunization_end_rate
      I_local[t+1] <- I_local[t] + infection_rates$mean[(t-n)+1] * I_local[t] * S_local[t] / N - I_local[t] * (recovery_rate + global_fatality_rates$mean[(t-n)+1])
      R_local[t+1] <- R_local[t] + I_local[t] * recovery_rate - R_local[t] * immunization_end_rate
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
#   - recovery_rate:          recovery rate
#   - global_fatality_rates:  global fatality rates
#   - immunization_end_rate:  immunization end rate
#   - SIRD:                   evolution of the infection using a SIRD/SIvRD model
#   - SIRD_ref:               evolution of the infection using a SIRD/SIvRD model + the forecast window
#   - N:                      total population
#   - variants:               true if we are considering variants, false otherwise
#
# Output:
#   - SIRD_ev:                evolution of the SIRD/SIvRD in the considered forecast window model using the forecasted rates
SIRD_evolution <- function(dir_name, time_step, ref_data_flag, final_date, infection_rates, global_infection_rates, recovery_rate, global_fatality_rates, immunization_end_rate, SIRD, SIRD_ref, N, variants){
  n <- length(unique(SIRD$date))
  n_ref <- n + time_step
  
  SIRD_ev <- SIRD_det(n, n_ref, N, SIRD, infection_rates, global_infection_rates, recovery_rate, global_fatality_rates, immunization_end_rate, variants, time_step)
  
  plot_SIRD_evolution(SIRD_ev, n, n_ref, dir_name, time_step, ref_data_flag, final_date, infection_rates, SIRD, SIRD_ref, variants)
  
  return(SIRD_ev)
}