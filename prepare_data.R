# Prophet with Covid-19 data
#
# Author: Daniele Baccega
# Data: COVID-19 R library
# Prophet: https://facebook.github.io/prophet/docs/quick_start.html

# Custom functions to prepare the data to feed Sybil

# Download files and load data
#
# Inputs:
#   - country_long:               name of the interested country (e.g. Italy, Austria)
#   - global_initial_date:        initial date for the data
#   - global_final_date:          final date for the data
#   - reproduce:                  reproduce the results of the paper
#   - variants:                   true if we are considering variants, false otherwise
#   - variants_to_disregard:      variants not to be considered
#   - variants_aggregated:        aggregation of variants (must be a list)
#   - variants_aggregated_names:  names of the aggregated variants (must have the same length of variants_aggregated)
#
# Output:
#   - df_disease_ref_init:        dataframe with disease data
#   - df_variants_init:           dataframe with variants data
download_files_and_load_data <- function(country_long, global_initial_date, global_final_date, reproduce, variants, variants_to_disregard = list(), variants_aggregated = list(), variants_aggregated_names = list()){
  if(!is.list(variants_aggregated) || !is.list(variants_aggregated_names))
    stop("Variables variants_aggregated and variants_aggregated_names must be lists!")
  
  if(length(variants_aggregated) != length(variants_aggregated_names))
    stop("Variables variants_aggregated and variants_aggregated_names must have the same size!")
  
  if(!file.exists("datasets")){
    system("mkdir datasets")
  }
  
  disease_data <- paste0("datasets/", country_long, "_", Sys.Date(), ".csv")
  disease_variants_data <- paste0("datasets/variants_data_", Sys.Date(), ".csv")
  
  # Check if the files are updated
  updated_file <- file.exists(disease_data) && file.exists(disease_variants_data)
  
  if(reproduce){
    updated_file <- TRUE
    disease_data <- paste0("datasets/", country_long, "_2023-11-22.csv")
    disease_variants_data <- "datasets/variants_data_2023-07-25.csv"
  }
  
  if(!updated_file){
    # Download the updated data
    covid19(country = gsub("_", " ", country_long), level = 3, start = global_initial_date, end = global_final_date, dir = '.')
    system(paste0("rm ", Sys.Date(), "/country/index.csv"))
    if(length(list.files(paste0(Sys.Date(), "/country/"))) == 0)
      stop(paste0("Country ", gsub("_", " ", country_long), " not found in covid19 library data"))
    system(paste0("mv ", Sys.Date(), "/country/*.csv ", disease_data))
    system(paste0("rm -r ", Sys.Date()))
    
    download.file("https://opendata.ecdc.europa.eu/covid19/virusvariant/csv/data.csv", disease_variants_data)
  }
  
  # Read and preprocess the data
  df_disease_ref_init <- read.csv(disease_data)
  df_disease_ref_init[df_disease_ref_init == ""] <- NA
  df_disease_ref_init$confirmed[is.na(df_disease_ref_init$confirmed) & df_disease_ref_init$date <= "2020-04-01"] <- 0
  df_disease_ref_init$deaths[is.na(df_disease_ref_init$deaths) & df_disease_ref_init$date <= "2020-04-01"] <- 0
  
  df_disease_ref_init <- df_disease_ref_init %>%
    filter(is.na(administrative_area_level_2), is.na(administrative_area_level_3))
  
  df_disease_ref_init <- df_disease_ref_init %>%
    mutate(confirmed = na.approx(confirmed, na.rm = FALSE), deaths = na.approx(deaths, na.rm = FALSE))
  
  df_disease_ref_init <- df_disease_ref_init %>%
    filter(!is.na(confirmed), !is.na(deaths), date <= global_final_date, date >= global_initial_date)
  
  df_variants_init <- data.frame()
  if(variants){
    df_variants_init <- read.csv(disease_variants_data)
    variants_countries <- unique(df_variants_init$country)
    
    if(!gsub("_", " ", country_long) %in% variants_countries)
      stop(paste0("Country ", gsub("_", " ", country_long), " not found in variants data"))
    
    df_variants_init <- df_variants_init %>%
      filter(country == gsub("_", " ", country_long), !is.na(new_cases), year_week <= format(as.Date(global_final_date), format="%Y-%U"), year_week >= format(as.Date(global_initial_date), format="%Y-%U"), source == "GISAID", ! variant %in% variants_to_disregard) %>%
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
  
  return(list(df_disease_ref_init, df_variants_init))
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

# Prepare data.
#
# Inputs:
#   - df_disease_ref:         dataframe with disease data
#   - df_variants_ref:        dataframe with variants data
#   - global_initial_date:    initial date for the data
#   - global_final_date:          final date for the data
#   - immunization_end_rate:  immunization end rate
#   - recovery_rate:          recovery rate
#   - variants                true if we are considering variants, false otherwise
#   - daily_spline            true if we approximate daily data with a spline, false otherwise
#   - country:                country name
#
# Output:
#   - df_variants_ref:        dataframe with variants data (after preprocessing)
#   - df_disease_ref:         dataframe with disease data (after preprocessing)
compute_data <- function(df_disease_ref, df_variants_ref, global_initial_date, global_final_date, immunization_end_rate, recovery_rate, variants, daily_spline, country){
  # Preprocess data
  N <- df_disease_ref$population[1]
  
  df_disease_ref <- df_disease_ref %>%
    mutate(total_deaths = deaths) %>%
    select(date, total_deaths, population) %>%
    mutate(new_deaths = diff(c(0, total_deaths))) %>%
    filter(date >= global_initial_date)
  
  initial_month <- as.numeric(format(global_initial_date, "%m"))
  initial_year <- as.numeric(format(global_initial_date, "%Y"))
  final_month <- as.numeric(format(global_final_date, "%m"))
  final_year <- as.numeric(format(global_final_date, "%Y"))
  
  initial_quarter_number <- ceiling(initial_month / 3)
  final_quarter_number <- ceiling(final_month / 3)
  
  dirs <- data.frame(directory=list.dirs("aggregatesUMD", recursive = FALSE)) %>%
    filter(directory >= paste0("aggregatesUMD/", initial_year, "-", initial_month), directory <= paste0("aggregatesUMD/", final_year, "-", final_month))
  
  coronasurveys_data <- NA
  for(i in 1:nrow(dirs)){
    if(!file.exists(paste0(dirs$directory[i], "/aggregates/country/", codelist$iso2c[which(codelist$country.name.en == gsub("_", " ", country))], ".csv")))
      stop(paste0("There is no file for country ", country, " in ", dirs$directory[i], "/aggregates/country/", codelist$iso2c[which(codelist$country.name.en == gsub("_", " ", country))], ".csv"))
      
    
    coronasurveys_data_local <- read.csv(paste0(dirs$directory[i], "/aggregates/country/", codelist$iso2c[which(codelist$country.name.en == gsub("_", " ", country))], ".csv"))
    coronasurveys_data_local <- coronasurveys_data_local %>%
      select(date, p_cli)
    
    if(!is.data.frame(coronasurveys_data)){
      coronasurveys_data <- coronasurveys_data_local
    }
    else{
      coronasurveys_data <- rbind(coronasurveys_data, coronasurveys_data_local)
    }
  }
  
  coronasurveys_data$p_cli <- rollmean(coronasurveys_data$p_cli * N, 7, align = "right", fill = NA)
  coronasurveys_data <- coronasurveys_data %>%
    filter(!is.na(p_cli))
  
  
  
  df_disease_ref <- df_disease_ref %>%
    filter(date >= min(coronasurveys_data$date), date <= max(coronasurveys_data$date))
  df_disease_ref <- df_disease_ref %>%
    mutate(new_cases = c(0, diff(coronasurveys_data$p_cli) + recovery_rate * coronasurveys_data$p_cli[1:(nrow(coronasurveys_data)-1)] + new_deaths[2:length(new_deaths)]))
  df_disease_ref <- df_disease_ref %>%
    filter(date > min(coronasurveys_data$date))
  
  df_disease_ref$new_cases[which(df_disease_ref$new_cases < 0)] <- 0
  
  df_disease_ref$date <- as.Date(df_disease_ref$date)
  
  df_disease_ref$week <- as.integer(substr(format(df_disease_ref$date, format="%Y-%V"), 6, 7))
  df_disease_ref$year <- as.integer(substr(format(df_disease_ref$date, format="%Y"), 1, 4))
  
  df_disease_ref$year[which(df_disease_ref$year == 2021 & df_disease_ref$week == 53)] <- 2020
  df_disease_ref$year[which(df_disease_ref$year == 2022 & df_disease_ref$week == 52 & df_disease_ref$date <= as.Date("2022-02-01"))] <- 2021
  df_disease_ref$year[which(df_disease_ref$year == 2023 & df_disease_ref$week == 52 & df_disease_ref$date <= as.Date("2023-02-01"))] <- 2022
  
  variants_date <- df_disease_ref %>%
    group_by(year, week) %>%
    mutate(date = first(date))
  variants_date <- unique(variants_date$date)
  
  if(daily_spline){
    df_disease_ref <- df_disease_ref %>%
      group_by(week, year) %>%
      summarize(new_cases = sum(new_cases),
                new_deaths = sum(new_deaths),
                date = first(date)) %>%
      filter(!is.na(new_cases))

    df_disease_ref <- df_disease_ref[order(df_disease_ref$date),]

    df_disease_ref$population <- rep(N, nrow(df_disease_ref))
  }
  
  if(variants){
    df_variants_ref <- df_variants_ref %>%
      filter(!(year == df_disease_ref$year[nrow(df_disease_ref)] & week > df_disease_ref$week[nrow(df_disease_ref)]), !(year > df_disease_ref$year[nrow(df_disease_ref)]), !(year == df_disease_ref$year[1] & week < df_disease_ref$week[1]), !(year < df_disease_ref$year[1])) %>%
      arrange(variant)
    
    df_variants_ref$date <- rep(variants_date, length(unique(df_variants_ref$variant)))
  }
  
  return(list(df_variants_ref, df_disease_ref))
}



# Prepare the data for Sybil.
#
# Inputs:
#   - country:                country name
#   - global_initial_date:    initial date for the data
#   - global_final_date:      final date for the data
#   - immunization_end_rate:  immunization end rate
#   - recovery_rate:          recovery rate
#   - reproduce:              reproduce paper's results
#   - variants                true if we are considering variants, false otherwise
#   - variants_to_disregard:      variants not to be considered
#   - variants_aggregated:        aggregation of variants (must be a list)
#   - variants_aggregated_names:  names of the aggregated variants (must have the same length of variants_aggregated)
#   - daily_spline            true if we approximate daily data with a spline, false otherwise
#
# Output:
#   - df_variants_ref:        dataframe with variants data (after preprocessing)
#   - df_disease_ref:         dataframe with disease data (after preprocessing)
prepare_data <- function(country, global_initial_date, global_final_date, immunization_end_rate, recovery_rate, reproduce, variants, variants_to_disregard, variants_aggregated, variants_aggregated_names, daily_spline){
  # Download file and load data
  data <- download_files_and_load_data(country, global_initial_date, global_final_date, reproduce, variants, variants_to_disregard, variants_aggregated, variants_aggregated_names)
  df_disease_init <- data[[1]]
  df_variants_init <- data[[2]]

  data <- compute_data(df_disease_init, df_variants_init, global_initial_date, global_final_date, immunization_end_rate, recovery_rate, variants, daily_spline, country)
  df_variants_all <- data[[1]]
  df_disease_all <- data[[2]]
  
  if(nrow(df_variants_all) > 0){
    df_variants_all <- df_variants_all %>%
      select(date, variant, percent_variant)
  }
  
  df_disease_all <- df_disease_all %>%
    select(date, new_cases, new_deaths, population)
  
  return(list(df_variants_all, df_disease_all))
}