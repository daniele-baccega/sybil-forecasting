# Prophet with Covid-19 data
#
# Author: Daniele Baccega
# Data: COVID19 R library
# Prophet: https://facebook.github.io/prophet/docs/quick_start.html


# Import the necessary libraries
library(dplyr)
library(ggplot2)
library(zoo)
library(forecast)
library(COVID19)
library(prophet)
library(stringr)
library(scales)
library(patchwork)


# Include the common functions
source("common_functions.R")

Sys.setlocale("LC_TIME", "en_US.UTF-8")


# Select the configuration (with or without variants)
variants <- TRUE

# Select the variants aggregation to use (if variants is set to TRUE)
who_labels <- TRUE

# Select the country
country <- "Italy"


# Download file and load data
data <- download_files_and_load_data(country, who_labels)
df_COVID19_ref_init <- data[[1]]
df_variants_init <- data[[2]]
updated_file <- data[[3]]


# Initialize other variables
immunization_end_rate <- 1 / 180

mcmc_samples <- 1000

ref_data_flag <- rep(FALSE, 4)
time_steps <- c(7, 14, 21, 28)
internal_dir_name <- paste0("SIRD", if(variants) "_Variants" else "", if(immunization_end_rate > 0) "_Reinfection" else "", "_MCMC", mcmc_samples, "/")


# Select the scenarios
if(who_labels){
  external_dir_name <- c(paste0(country, "_ProphetWHOApril2020/"),
                         paste0(country, "_ProphetWHOJuly2020/"),
                         paste0(country, "_ProphetWHOJanuary2022/"),
                         paste0(country, "_ProphetWHOJuly2022/"),
                         paste0(country, "_ProphetWHOFebruary2023/"))
  
  initial_dates <- c(as.Date("2020-03-14"),
                     as.Date("2020-05-30"),
                     as.Date("2021-12-13"),
                     as.Date("2022-06-14"),
                     as.Date("2023-01-07"))
  
  final_dates <- c(as.Date("2020-04-14"),
                  as.Date("2020-06-30"),
                  as.Date("2022-01-13"),
                  as.Date("2022-07-14"),
                  as.Date("2023-02-07"))
}else{
  external_dir_name <- c(paste0(country, "_ProphetJanuary-10-2022/"),
                         paste0(country, "_ProphetJanuary-11-2022/"),
                         paste0(country, "_ProphetJanuary-12-2022/"),
                         paste0(country, "_ProphetJanuary-13-2022/"),
                         paste0(country, "_ProphetJanuary-14-2022/"),
                         paste0(country, "_ProphetJanuary-15-2022/"),
                         paste0(country, "_ProphetJanuary-16-2022/"))
  
  initial_dates <- c(as.Date("2021-12-10"),
                     as.Date("2021-12-11"),
                     as.Date("2021-12-12"),
                     as.Date("2021-12-13"),
                     as.Date("2021-12-14"),
                     as.Date("2021-12-15"),
                     as.Date("2021-12-16"))
  
  final_dates <- c(as.Date("2022-01-10"),
                   as.Date("2022-01-11"),
                   as.Date("2022-01-12"),
                   as.Date("2022-01-13"),
                   as.Date("2022-01-14"),
                   as.Date("2022-01-15"),
                   as.Date("2022-01-16"))
}


# Loop on different 'training' windows
for(j in seq(1, length(initial_dates))){
  dir_name <- paste0(external_dir_name[j], internal_dir_name, "OneMonth")
  
  # Create the main directories
  if(!file.exists(external_dir_name[j])){
    system(paste0("mkdir ", external_dir_name[j]))
  }
  
  if(!file.exists(internal_dir_name)){
    system(paste0("mkdir ", external_dir_name[j], internal_dir_name))
  }
  
  # Compute the final dates
  final_dates_ref <- c(final_dates[j] + time_steps[1])
  
  for(i in seq(2, length(time_steps))){
    final_dates_ref <- c(final_dates_ref, final_dates[j] + time_steps[i])
  }
  
  
  # Create the specific directories
  if(!file.exists(dir_name)){
    system(paste0("mkdir ", dir_name))
    system(paste0("mkdir ", dir_name, "/prophet_models"))
    system(paste0("mkdir ", dir_name, "/data"))
    system(paste0("mkdir ", dir_name, "/forecast_plot"))
    
    system(paste0("mkdir ", dir_name, "/forecast_plot/I"))
    system(paste0("mkdir ", dir_name, "/forecast_plot/infection_rates"))
    system(paste0("mkdir ", dir_name, "/forecast_plot/comparison"))
    system(paste0("mkdir ", dir_name, "/forecast_plot/RData"))
  }
  
  # Compute and save all the data
  data <- compute_data(dir_name, df_COVID19_ref_init, df_variants_init, immunization_end_rate, !updated_file)
  df_variants_all <- data[[1]]
  df_COVID19_all <- data[[2]]
  SIRD_all <- data[[3]]
  results_all <- data[[4]]
  
  # Check if we can reproduce the real data using a SIRD model from initial_dates[j] to final_dates[j] (with the previously computed rates)
  SIRD_check(dir_name, SIRD_all, results_all$infection_rates, results_all$rec_rates, results_all$fat_rates, immunization_end_rate, df_COVID19_all$population[1])
  plot_I(dir_name, SIRD_all)
  
  # Plot variants info
  data <- generate_and_plot_variants_info(paste0(external_dir_name[j], internal_dir_name), df_variants_all, df_COVID19_all, SIRD_all, results_all)
  df_variants_processed <- data[[1]]
  df_COVID19_all <- data[[2]]
  SIRD_all <- data[[3]]
  results_all <- data[[4]]
  
  variants_data <- SIRD_variants(dir_name, df_variants_processed, SIRD_all, results_all, immunization_end_rate, df_COVID19_all$population[1])
  SIRD_all_variants <- variants_data[[1]]
  results_all_variants <- variants_data[[2]]
  
  variants_name <- unique(df_variants_processed$variant)
  
  for(i in seq(1, length(time_steps))){
    filtered_data <- filter_data(df_COVID19_all, SIRD_all, SIRD_all_variants, results_all, results_all_variants, initial_dates[j], final_dates[j], final_dates_ref[i], variants)
    df_COVID19_ref_used <- filtered_data[[1]]
    df_COVID19_used <- filtered_data[[2]]
    SIRD_ref_used <- filtered_data[[3]]
    SIRD_used <- filtered_data[[4]]
    results_ref_used <- filtered_data[[5]]
    results_used <- filtered_data[[6]]
    
    N <- df_COVID19_used$population
    N_ref <- df_COVID19_ref_used$population
    
    n <- nrow(df_COVID19_used)
    n_ref <- n + time_steps[i]
    
    
    ref_data_flag[i] <- df_COVID19_all$date[nrow(df_COVID19_all)-1] >= (df_COVID19_used$date[n] + time_steps[i])
    
    # Forecast on recovery rates
    results_used$rec_rates[which(results_used$rec_rates == 0)] <- min(results_used$rec_rates[which(results_used$rec_rates > 0)])
    rec_rates_log <- log(unique(results_used$rec_rates))
    fc_rec_rate <- apply_Prophet(dir_name, df_COVID19_used$date, rec_rates_log, time_steps[i], "rec_rate", "/forecast_plot", mcmc_samples)
    fc_rec_rate$yhat <- exp(fc_rec_rate$yhat)
    fc_rec_rate$yhat_lower <- exp(fc_rec_rate$yhat_lower)
    fc_rec_rate$yhat_upper <- exp(fc_rec_rate$yhat_upper)
    forecast_plot(paste0(dir_name, "/forecast_plot"), 'Prophet', ref_data_flag[i], final_dates_ref[i], n, n_ref, df_COVID19_used$date, df_COVID19_ref_used$date, unique(results_ref_used$rec_rates), fc_rec_rate, time_steps[i], "recovery_rates")

    
    # Forecast on fatality_rates
    results_used$fat_rates[which(results_used$fat_rates == 0)] <- min(results_used$fat_rates[which(results_used$fat_rates > 0)])
    fat_rates_log <- log(unique(results_used$fat_rates))
    fc_fat_rate <- apply_Prophet(dir_name, df_COVID19_used$date, fat_rates_log, time_steps[i], "fat_rate", "/forecast_plot", mcmc_samples)
    fc_fat_rate$yhat <- exp(fc_fat_rate$yhat)
    fc_fat_rate$yhat_lower <- exp(fc_fat_rate$yhat_lower)
    fc_fat_rate$yhat_upper <- exp(fc_fat_rate$yhat_upper)
    forecast_plot(paste0(dir_name, "/forecast_plot"), 'Prophet', ref_data_flag[i], final_dates_ref[i], n, n_ref, df_COVID19_used$date, df_COVID19_ref_used$date, unique(results_ref_used$fat_rates), fc_fat_rate, time_steps[i], "fatality_rates")


    if(variants){
      infection_rates <- data.frame()
      I <- data.frame()
      
      for(k in 1:length(variants_name)){
        results_used_local <- results_used %>%
          filter(variant == variants_name[k])
        
        results_ref_used_local <- results_ref_used %>%
          filter(variant == variants_name[k])
        
        SIRD_used_local <- SIRD_used %>%
          filter(variant == variants_name[k])
        
        SIRD_ref_used_local <- SIRD_ref_used %>%
          filter(variant == variants_name[k])
        
        if(sum(results_used_local$infection_rates) > 1e-5){
          # Forecast on infection rates
          results_used_local$infection_rates[which(results_used_local$infection_rates == 0)] <- min(results_used_local$infection_rates[which(results_used_local$infection_rates > 0)])
          
          infection_rates_log <- log(results_used_local$infection_rates)
          fc_inf_rate <- apply_Prophet(dir_name, df_COVID19_used$date[(nrow(df_COVID19_used)-length(infection_rates_log)+1):nrow(df_COVID19_used)], infection_rates_log, time_steps[i], paste0("inf_rate_", variants_name[k]), "/forecast_plot/infection_rates", mcmc_samples)
          fc_inf_rate$yhat <- exp(fc_inf_rate$yhat)
          fc_inf_rate$yhat_lower <- exp(fc_inf_rate$yhat_lower)
          fc_inf_rate$yhat_upper <- exp(fc_inf_rate$yhat_upper)
        }
        else{
          fc_inf_rate <- data.frame(yhat=rep(0, time_steps[i]), yhat_lower=rep(0, time_steps[i]), yhat_upper=rep(0, time_steps[i]))
        }

        forecast_plot(paste0(dir_name, "/forecast_plot/infection_rates"), 'Prophet', ref_data_flag[i], final_dates_ref[i], n, n_ref, df_COVID19_used$date, df_COVID19_ref_used$date, results_ref_used_local$infection_rates, fc_inf_rate, time_steps[i], paste0("infection_rates_", variants_name[k]))
        
        infection_rates_local <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), variant=rep(variants_name[k], time_steps[i]), mean=fc_inf_rate$yhat, lower=fc_inf_rate$yhat_lower, upper=fc_inf_rate$yhat_upper)
        infection_rates <- rbind(infection_rates, infection_rates_local)
        
        if(sum(SIRD_used_local$I) > 0){
          # Forecast on I
          SIRD_used_local$I[which(SIRD_used_local$I == 0)] <- min(SIRD_used_local$I[which(SIRD_used_local$I != 0)])
          I_log <- log(SIRD_used_local$I)
          fc_I <- apply_Prophet(dir_name, df_COVID19_used$date, I_log, time_steps[i], paste0("I_", variants_name[k]), "/forecast_plot/I", mcmc_samples)
          fc_I$yhat <- exp(fc_I$yhat)
          fc_I$yhat_lower <- exp(fc_I$yhat_lower)
          fc_I$yhat_upper <- exp(fc_I$yhat_upper)
        }
        else{
          fc_I <- data.frame(yhat=rep(0, time_steps[i]), yhat_lower=rep(0, time_steps[i]), yhat_upper=rep(0, time_steps[i]))
        }
        
        forecast_plot(paste0(dir_name, "/forecast_plot/I"), 'Prophet', ref_data_flag[i], final_dates_ref[i], n, n_ref, df_COVID19_used$date, df_COVID19_ref_used$date, SIRD_ref_used_local$I, fc_I, time_steps[i], paste0("I_", variants_name[k]))
        
        I_local <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), variant=rep(variants_name[k], time_steps[i]), mean=fc_I$yhat, lower=fc_I$yhat_lower, upper=fc_I$yhat_upper)
        I <- rbind(I, I_local)
      }
      
      global_infection_rates <- results_all %>%
        filter(date >= initial_dates[j], date <= final_dates[j])
      
      global_infection_rates_ref <- results_all %>%
        filter(date >= initial_dates[j], date <= final_dates_ref[i])
      
      # Forecast on global infection rates
      infection_rates_log <- log(global_infection_rates$infection_rates)
      infection_rates_log[which(is.infinite(infection_rates_log) | is.na(infection_rates_log))] <- 0
      fc_inf_rate <- apply_Prophet(dir_name, global_infection_rates$date, infection_rates_log, time_steps[i], "global_inf_rate", "/forecast_plot", mcmc_samples)
      fc_inf_rate$yhat <- exp(fc_inf_rate$yhat)
      fc_inf_rate$yhat_lower <- exp(fc_inf_rate$yhat_lower)
      fc_inf_rate$yhat_upper <- exp(fc_inf_rate$yhat_upper)
      forecast_plot(paste0(dir_name, "/forecast_plot"), 'Prophet', ref_data_flag[i], final_dates_ref[i], n, n_ref, global_infection_rates$date, global_infection_rates_ref$date, global_infection_rates_ref$infection_rates, fc_inf_rate, time_steps[i], "global_infection_rates")
      
      global_infection_rates <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), mean=fc_inf_rate$yhat, lower=fc_inf_rate$yhat_lower, upper=fc_inf_rate$yhat_upper)
      global_recovery_rates <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), mean=fc_rec_rate$yhat, lower=fc_rec_rate$yhat_lower, upper=fc_rec_rate$yhat_upper)
      global_fatality_rates <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), mean=fc_fat_rate$yhat, lower=fc_fat_rate$yhat_lower, upper=fc_fat_rate$yhat_upper)


      # Plot the forecast and the comparisons
      SIRD_final <- SIRD_evolution(paste0(dir_name, "/forecast_plot"), 'Prophet', time_steps[i], ref_data_flag[i], final_dates_ref[i], infection_rates, global_infection_rates, global_recovery_rates, global_fatality_rates, immunization_end_rate, I, SIRD_used, SIRD_ref_used, N[1], variants)
    }
    else{
      # Forecast on infection rates
      infection_rates_log <- log(results_used$infection_rates)
      infection_rates_log[which(is.infinite(infection_rates_log) | is.na(infection_rates_log))] <- 0
      fc_inf_rate <- apply_Prophet(dir_name, SIRD_used$date, infection_rates_log, time_steps[i], "inf_rate", "/forecast_plot", mcmc_samples)
      fc_inf_rate$yhat <- exp(fc_inf_rate$yhat)
      fc_inf_rate$yhat_lower <- exp(fc_inf_rate$yhat_lower)
      fc_inf_rate$yhat_upper <- exp(fc_inf_rate$yhat_upper)
      forecast_plot(paste0(dir_name, "/forecast_plot"), 'Prophet', ref_data_flag[i], final_dates_ref[i], n, n_ref, SIRD_used$date, SIRD_ref_used$date, results_ref_used$infection_rates, fc_inf_rate, time_steps[i], "infection_rates")
    
      # Forecast on I
      I_log <- log(SIRD_used$I)
      I_log[which(is.infinite(I_log) | is.na(I_log))] <- 0
      fc_I <- apply_Prophet(dir_name, SIRD_used$date, I_log, time_steps[i], "I", "/forecast_plot", mcmc_samples)
      fc_I$yhat <- exp(fc_I$yhat)
      fc_I$yhat_lower <- exp(fc_I$yhat_lower)
      fc_I$yhat_upper <- exp(fc_I$yhat_upper)
      forecast_plot(paste0(dir_name, "/forecast_plot"), 'Prophet', ref_data_flag[i], final_dates_ref[i], n, n_ref, SIRD_used$date, SIRD_ref_used$date, SIRD_ref_used$I, fc_I, time_steps[i], "I")  
    
      infection_rates <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), mean=fc_inf_rate$yhat, lower=fc_inf_rate$yhat_lower, upper=fc_inf_rate$yhat_upper)
      recovery_rates <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), mean=fc_rec_rate$yhat, lower=fc_rec_rate$yhat_lower, upper=fc_rec_rate$yhat_upper)
      fatality_rates <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), mean=fc_fat_rate$yhat, lower=fc_fat_rate$yhat_lower, upper=fc_fat_rate$yhat_upper)


      # Plot the forecast and the comparisons
      SIRD_final <- SIRD_evolution(paste0(dir_name, "/forecast_plot"), 'Prophet', time_steps[i], ref_data_flag[i], final_dates_ref[i], infection_rates, infection_rates, recovery_rates, fatality_rates, immunization_end_rate, fc_I$yhat, SIRD_used, SIRD_ref_used, N[1], variants)
    }
  }
  
  final_plots(dir_name)
}

if(!who_labels){
  load(paste0(external_dir_name[1], internal_dir_name, "OneMonth/forecast_plot/RData/SIRD_forecast_28_days.RData"))
  p1 <- plot + labs(title = expression("January 10"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_name[2], internal_dir_name, "OneMonth/forecast_plot/RData/SIRD_forecast_28_days.RData"))
  p2 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_name[3], internal_dir_name, "OneMonth/forecast_plot/RData/SIRD_forecast_28_days.RData"))
  p3 <- plot + labs(title = expression("January 12"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_name[4], internal_dir_name, "OneMonth/forecast_plot/RData/SIRD_forecast_28_days.RData"))
  p4 <- plot + labs(title = expression("January 13"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_name[5], internal_dir_name, "OneMonth/forecast_plot/RData/SIRD_forecast_28_days.RData"))
  p5 <- plot + labs(title = expression("January 14"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_name[6], internal_dir_name, "OneMonth/forecast_plot/RData/SIRD_forecast_28_days.RData"))
  p6 <- plot + labs(title = expression("January 15"^"th")) + theme(plot.title=element_text(size=34))
  
  p <- (p1 + p2 + p3) / (p4 + p5 + p6) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png("SIRD_forecast_evolution.png", units="in", width=34, height=15, res=300)
  print(p)
  dev.off()
}