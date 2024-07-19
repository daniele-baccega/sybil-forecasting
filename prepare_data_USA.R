# Prophet with Covid-19 data
#
# Author: Daniele Baccega
# Data: COVID-19 R library
# Prophet: https://facebook.github.io/prophet/docs/quick_start.html

# Custom functions to prepare the data to feed Sybil

# USA

# Download files and load data
#
# Inputs:
#   - country_long:               name of the interested country (e.g. Italy, Austria)
#   - global_initial_date:        initial date for the data
#   - global_final_date:          final date for the data
#   - variants:                   true if we are considering variants, false otherwise
#   - variants_to_disregard:      variants not to be considered
#   - variants_aggregated:        aggregation of variants (must be a list)
#   - variants_aggregated_names:  names of the aggregated variants (must have the same length of variants_aggregated)
#
# Output:
#   - df_disease_ref_init:        dataframe with disease data
#   - df_variants_init:           dataframe with variants data
download_files_and_load_data_USA <- function(country_long, global_initial_date, global_final_date, variants, variants_to_disregard = list(), variants_aggregated = list(), variants_aggregated_names = list(), region = "", city = ""){
  if(!is.list(variants_aggregated) || !is.list(variants_aggregated_names))
    stop("Variables variants_aggregated and variants_aggregated_names must be lists!")
  
  if(length(variants_aggregated) != length(variants_aggregated_names))
    stop("Variables variants_aggregated and variants_aggregated_names must have the same size!")
  
  if(!file.exists("datasets")){
    system("mkdir datasets")
  }
  
  disease_data <- paste0("datasets/", country_long, "_2024-06-13.csv")
  disease_variants_data <- paste0("datasets/USA_variants.csv")
  
  # Check if the files are updated
  updated_file <- file.exists(disease_data)
  
  if(!updated_file){
    # Download the updated data
    covid19(country = gsub("_", " ", country_long), level = 3, start = global_initial_date, end = global_final_date, dir = '.')
    system(paste0("rm ", Sys.Date(), "/country/index.csv"))
    if(length(list.files(paste0(Sys.Date(), "/country/"))) == 0)
      stop(paste0("Country ", gsub("_", " ", country_long), " not found in covid19 library data"))
    system(paste0("mv ", Sys.Date(), "/country/*.csv ", disease_data))
    system(paste0("rm -r ", Sys.Date()))
  }
  
  # Read and preprocess the data
  df_disease_ref_init <- read.csv(disease_data)

  df_disease_ref_init[df_disease_ref_init == ""] <- NA
  df_disease_ref_init$confirmed[is.na(df_disease_ref_init$confirmed) & df_disease_ref_init$date <= "2020-04-01"] <- 0
  df_disease_ref_init$deaths[is.na(df_disease_ref_init$deaths) & df_disease_ref_init$date <= "2020-04-01"] <- 0
  
  df_disease_ref_init <- df_disease_ref_init %>%
    filter(((region == "" & is.na(administrative_area_level_2)) | administrative_area_level_2 == region) & ((city == "" & is.na(administrative_area_level_3)) | administrative_area_level_3 == city))
  
  df_disease_ref_init <- df_disease_ref_init %>%
    mutate(confirmed = na.approx(confirmed, na.rm = FALSE), deaths = na.approx(deaths, na.rm = FALSE))
  
  df_disease_ref_init <- df_disease_ref_init %>%
    filter(!is.na(confirmed), !is.na(deaths), date <= global_final_date, date >= global_initial_date)
  
  df_variants_init <- data.frame()
  if(variants){
    df_variants_init <- read.csv(disease_variants_data)
    
    df_variants_init <- df_variants_init %>%
      filter(time_interval == "weekly", modeltype == "weighted")

    df_variants_init$week_ending <- as.Date(strptime(df_variants_init$week_ending, format="%m/%d/%Y %I:%M:%S %p"))
    
    df_variants_init <- df_variants_init %>%
      distinct(usa_or_hhsregion, week_ending, variant, .keep_all = TRUE) %>%
      mutate(date = week_ending, percent_variant = share) %>%
      select(usa_or_hhsregion, date, variant, percent_variant) %>%
      arrange(date)
    
    usa_or_hhsregions <- unique(df_variants_init$usa_or_hhsregion)
    usa_or_hhsregions_mapping <- data.frame(usa_or_hhsregion=c(1, 1, 1, 1, 1, 1,
                                                               2, 2, 2, 2,
                                                               3, 3, 3, 3, 3, 3,
                                                               4, 4, 4, 4, 4, 4, 4, 4,
                                                               5, 5, 5, 5, 5, 5,
                                                               6, 6, 6, 6, 6,
                                                               7, 7, 7, 7,
                                                               8, 8, 8, 8, 8, 8,
                                                               9, 9, 9, 9, 9, 9, 9,
                                                               10, 10, 10, 10,
                                                               "USA"),
                                            state=c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont",
                                                    "New Jersey", "New York", "Puerto Rico", "Virgin Islands",
                                                    "Delaware", "District of Columbia", "Maryland", "Pennsylvania", "Virginia", "West Virginia",
                                                    "Alabama", "Florida", "Georgia", "Kentucky", "Mississippi", "North Carolina", "South Carolina", "Tennessee",
                                                    "Illinois", "Indiana", "Michigan", "Minnesota", "Ohio", "Wisconsin",
                                                    "Arkansas", "Louisiana", "New Mexico", "Oklahoma", "Texas",
                                                    "Iowa", "Kansas", "Missouri", "Nebraska",
                                                    "Colorado", "Montana", "North Dakota", "South Dakota", "Utah", "Wyoming",
                                                    "Arizona", "California", "Hawaii", "Nevada", "American Samoa", "Northern Mariana Islands", "Guam",
                                                    "Alaska", "Idaho", "Oregon", "Washington",
                                                    "USA"))
    
    df_variants_init_global <- data.frame()
    for(r in usa_or_hhsregions){
      df_variants_init_local <- df_variants_init %>%
        filter(usa_or_hhsregion == r)
      
      usa_or_hhsregions_mapping_local <- usa_or_hhsregions_mapping %>%
        filter(usa_or_hhsregion == r)
      
      new_dates <- sort(seq.Date(df_variants_init_local$date[1]-7, global_initial_date, -7))
      
      df_variants_init_local <- rbind(data.frame(usa_or_hhsregion=rep(r, length(new_dates)), date=sort(rep(new_dates, length(unique(df_variants_init_local$variant)))), variant=rep(unique(df_variants_init_local$variant), length(new_dates)), percent_variant=rep(0, length(unique(df_variants_init_local$variant))*length(new_dates))), df_variants_init_local)
      
      df_variants_init_local$percent_variant[which(df_variants_init_local$variant == "Other" & df_variants_init_local$date <= new_dates[length(new_dates)])] <- 1
      
      dates <- unique(df_variants_init_local$date[df_variants_init_local$variant == "Other"])
      new_Alpha_dates <- dates[which(!dates %in% df_variants_init_local$date[df_variants_init_local$variant == variants_aggregated$Alpha])]
      new_Delta_dates <- dates[which(!dates %in% df_variants_init_local$date[df_variants_init_local$variant == variants_aggregated$Delta])]
      new_Omicron_dates <- dates[which(!dates %in% df_variants_init_local$date[df_variants_init_local$variant %in% variants_aggregated$Omicron])]
      
      df_variants_init_local <- rbind(df_variants_init_local, data.frame(usa_or_hhsregion=rep(r, length(new_Alpha_dates)), date=new_Alpha_dates, variant=rep(variants_aggregated$Alpha, length(new_Alpha_dates)), percent_variant=rep(0, length(new_Alpha_dates))))
      df_variants_init_local <- rbind(df_variants_init_local, data.frame(usa_or_hhsregion=rep(r, length(new_Delta_dates)), date=new_Delta_dates, variant=rep(variants_aggregated$Delta, length(new_Delta_dates)), percent_variant=rep(0, length(new_Delta_dates))))
      df_variants_init_local <- rbind(df_variants_init_local, data.frame(usa_or_hhsregion=rep(r, length(new_Omicron_dates)), date=new_Omicron_dates, variant=rep(variants_aggregated$Omicron[1], length(new_Omicron_dates)), percent_variant=rep(0, length(new_Omicron_dates))))
      
      df_variants_init_local <- df_variants_init_local %>%
        filter(date <= global_final_date, !variant %in% variants_to_disregard, !date <= "2020-02-24")
      
      df_variants_init_local <- df_variants_init_local %>%
        mutate(variant = gsub("/", "-", variant))
      df_variants_init_local$percent_variant[is.na(df_variants_init_local$percent_variant)] <- 0
      
      # Aggregate variants
      for(i in 1:length(variants_aggregated)){
        df_variants_init_local$variant[which(df_variants_init_local$variant %in% variants_aggregated[[i]])] <- variants_aggregated_names[[i]]
      }
      
      df_variants_init_local <- df_variants_init_local %>%
        group_by(date) %>%
        aggregate(percent_variant ~ date + variant, FUN=sum) %>%
        arrange(date)
      
      df_variants_init_local <- filter_variants_USA(df_variants_init_local)
      
      df_variants_init_local$percent_variant[which(df_variants_init_local$percent_variant > 1.0)] <- 1.0
      
      df_variants_init_local <- df_variants_init_local %>%
        mutate(week = as.integer(substr(format(date, format="%Y-%V"), 6, 7)), year = as.integer(substr(format(date, format="%Y"), 1, 4)))
      
      df_variants_init_local_states <- data.frame()
      for(state in usa_or_hhsregions_mapping_local$state){
        df_variants_init_local_state <- df_variants_init_local
        df_variants_init_local_state$state <- state
          
        df_variants_init_local_states <- rbind(df_variants_init_local_states, df_variants_init_local_state)
      }
      
      df_variants_init_global <- rbind(df_variants_init_global, df_variants_init_local_states)
    }
    
    df_variants_init <- df_variants_init_global %>%
      filter(state == "USA")
    
    if(region != "")
      df_variants_init <- df_variants_init_global %>%
      filter(state == region)
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
filter_variants_USA <- function(df_variants_init){
  variants_names <- unique(df_variants_init$variant)
  
  other_proportion <- c()
  df_variants <- data.frame()
  for(v in variants_names){
    df_variants_local <- df_variants_init %>%
      filter(variant == v)
    
    if(v != "Other"){
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
      if(length(other_proportion) == 0){
        other_proportion <- variants_local_other
      }
      else{
        other_proportion <- other_proportion + variants_local_other
      }
      
      df_variants_local$percent_variant[-c(left_peak:right_peak)] <- 0
    }
    
    df_variants <- rbind(df_variants, df_variants_local)
  }
  
  df_variants$percent_variant[which(df_variants$variant == "Other")] <- 1 - other_proportion
  
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
compute_data_USA <- function(df_disease_ref, df_variants_ref, global_initial_date, global_final_date, immunization_end_rate, recovery_rate, variants, daily_spline, country){
  # Preprocess data
  N <- df_disease_ref$population[1]
  
  df_disease_ref <- df_disease_ref %>%
    mutate(total_deaths = deaths, total_cases = confirmed) %>%
    select(date, total_deaths, total_cases, population) %>%
    mutate(new_deaths = diff(c(0, total_deaths)), new_cases = diff(c(0, total_cases))) %>%
    filter(date >= global_initial_date)
  
  df_disease_ref$new_cases[which(df_disease_ref$new_cases < 0)] <- 0
  df_disease_ref$new_deaths[which(df_disease_ref$new_deaths < 0)] <- 0
  
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
  
  SIRDS_initial_marking <- c(unique(df_disease_ref$population) - df_disease_ref$total_cases[1] - df_disease_ref$total_deaths[1],
                             df_disease_ref$total_cases[1],
                             0,
                             df_disease_ref$total_deaths[1])
  
  return(list(df_variants_ref, df_disease_ref, SIRDS_initial_marking))
}



# Prepare the data for Sybil.
#
# Inputs:
#   - country:                    country name
#   - global_initial_date:        initial date for the data
#   - global_final_date:          final date for the data
#   - immunization_end_rate:      immunization end rate
#   - recovery_rate:              recovery rate
#   - variants                    true if we are considering variants, false otherwise
#   - variants_to_disregard:      variants not to be considered
#   - variants_aggregated:        aggregation of variants (must be a list)
#   - variants_aggregated_names:  names of the aggregated variants (must have the same length of variants_aggregated)
#   - daily_spline                true if we approximate daily data with a spline, false otherwise
#
# Output:
#   - df_variants_ref:        dataframe with variants data (after preprocessing)
#   - df_disease_ref:         dataframe with disease data (after preprocessing)
prepare_data_USA <- function(country, global_initial_date, global_final_date, immunization_end_rate, recovery_rate, variants, variants_to_disregard, variants_aggregated, variants_aggregated_names, daily_spline, region = "", city = ""){
  # Download file and load data
  data <- download_files_and_load_data_USA(country, global_initial_date, global_final_date, variants, variants_to_disregard, variants_aggregated, variants_aggregated_names, region, city)
  df_disease_init <- data[[1]]
  df_variants_init <- data[[2]]
  
  data <- compute_data_USA(df_disease_init, df_variants_init, global_initial_date, global_final_date, immunization_end_rate, recovery_rate, variants, daily_spline, country)
  df_variants_all <- data[[1]]
  df_disease_all <- data[[2]]
  SIRDS_initial_marking <- data[[3]]
  
  if(nrow(df_variants_all) > 0){
    df_variants_all <- df_variants_all %>%
      select(date, variant, percent_variant)
  }
  
  df_disease_all <- df_disease_all %>%
    select(date, new_cases, total_cases, new_deaths, total_deaths, population)
  
  return(list(df_variants_all, df_disease_all, SIRDS_initial_marking))
}