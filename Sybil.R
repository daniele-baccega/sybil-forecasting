# Sybil
#
# Author: Daniele Baccega
# Data: COVID19 R library
# Prophet: https://facebook.github.io/prophet/docs/quick_start.html

# Forecast
#
#
# Inputs:
#   - variants                    true if we are considering variants, false otherwise
#   - global_final_date:          final date for the data
#   - country:                    name of the interested country (e.g. Italy, Austria)
#   - external_dir_names:         names of the external directories
#   - immunization_end_rate:      immunization end rate
#   - reproduce:                  reproduce the results of the paper
#   - initial_dates:              initial dates
#   - final_dates:                final dates
#   - variants_to_disregard:      variants not to be considered
#   - variants_aggregated:        aggregation of variants (must be a list)
#   - variants_aggregated_names:  names of the aggregated variants (must have the same length of variants_aggregated)
Sybil <- function(variants = TRUE, global_final_date = as.Date("2023-06-04"), country = "Italy", external_dir_names = paste0(country, "/FirstScenario/"), immunization_end_rate = 1 / 180, reproduce = FALSE, initial_dates = c(as.Date("2020-03-14")), final_dates = c(as.Date("2020-04-14")), variants_to_disregard = list(), variants_aggregated = list(), variants_aggregated_names = list()){
  if(length(initial_dates) != length(final_dates) || length(initial_dates) != length(external_dir_names))
    stop("Variables initial_dates, final_dates and external_dir_names must have the same size!")
  
  # Download file and load data
  data <- download_files_and_load_data(country, global_final_date, reproduce, variants, variants_to_disregard, variants_aggregated, variants_aggregated_names)
  df_COVID19_init <- data[[1]]
  df_variants_init <- data[[2]]
  updated_file <- data[[3]]

  
  mcmc_samples <- 1000
  
  ref_data_flag <- rep(FALSE, 4)
  time_steps <- c(7, 14, 21, 28)
  internal_dir_name <- paste0("SIRD", if(variants) "_Variants" else "", if(immunization_end_rate > 0) "_Reinfection" else "", "/")
  
  relative_errors_df <- data.frame()
  # Loop on different 'training' windows
  for(j in seq(1, length(external_dir_names))){
    dir_name <- paste0(external_dir_names[j], internal_dir_name)
    
    # Create the main directories
    if(!file.exists(external_dir_names[j])){
      system(paste0("mkdir -p ", external_dir_names[j]))
    }
    
    # Create the specific directories
    if(!file.exists(dir_name)){
      system(paste0("mkdir -p ", dir_name))
      system(paste0("mkdir -p ", dir_name, "/prophet_models"))
      system(paste0("mkdir -p ", dir_name, "/neural_prophet_models"))
      system(paste0("mkdir -p ", dir_name, "/arima_models"))
      system(paste0("mkdir -p ", dir_name, "/sarima_models"))
      system(paste0("mkdir -p ", dir_name, "/lstm_models"))
      system(paste0("mkdir -p ", dir_name, "/gru_models"))
      system(paste0("mkdir -p ", dir_name, "/data"))
      system(paste0("mkdir -p ", dir_name, "/forecast_plot"))
      
      system(paste0("mkdir -p ", dir_name, "/forecast_plot/I"))
      system(paste0("mkdir -p ", dir_name, "/forecast_plot/infection_rates"))
      system(paste0("mkdir -p ", dir_name, "/forecast_plot/comparison"))
      system(paste0("mkdir -p ", dir_name, "/forecast_plot/RData"))
    }
    
    # Compute and save all the data
    data <- compute_data(dir_name, df_COVID19_init, df_variants_init, immunization_end_rate, global_final_date, variants, !updated_file || reproduce)
    df_variants_all <- data[[1]]
    df_COVID19_all <- data[[2]]
    SIRD_all <- data[[3]]
    results_all <- data[[4]]
    
    # Check if we can reproduce the real data using a SIRD model with the previously computed rates
    SIRD_check(dir_name, SIRD_all, results_all$infection_rates, results_all$rec_rates, results_all$fat_rates, immunization_end_rate, df_COVID19_all$population[1])
    
    # Compute the final dates
    final_dates_ref <- c()
    for(i in seq(1, length(time_steps))){
      final_dates_ref <- c(final_dates_ref, final_dates[j] + time_steps[i])
    }
    
    plot_I(dir_name, SIRD_all, final_dates)
    
    SIRD_all_variants <- data.frame()
    results_all_variants <- data.frame()
    if(variants){
      # Plot variants info
      data <- generate_and_plot_variants_info(paste0(external_dir_names[j], internal_dir_name), df_variants_all, df_COVID19_all, SIRD_all, results_all)
      df_variants_processed <- data[[1]]
      df_COVID19_all <- data[[2]]
      SIRD_all <- data[[3]]
      results_all <- data[[4]]
      
      variants_data <- SIRD_variants(dir_name, df_variants_processed, SIRD_all, results_all, immunization_end_rate, df_COVID19_all$population[1], final_dates)
      SIRD_all_variants <- variants_data[[1]]
      results_all_variants <- variants_data[[2]]
    }
    
    if(final_dates[j] > SIRD_all$date[nrow(SIRD_all)]){
      print(paste0("Warning: ", final_dates[j], " is greater than the last date in the data! Skip"))
      next
    }
    
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
      
      # Forecast on global recovery rates
      results_used$rec_rates[which(results_used$rec_rates <= 1e-10)] <- min(results_used$rec_rates[which(results_used$rec_rates > 1e-10)])
      rec_rates_log <- log(unique(results_used$rec_rates))
      fc_rec_rate <- apply_Prophet(dir_name, df_COVID19_used$date[(nrow(df_COVID19_used)-length(rec_rates_log)+1):nrow(df_COVID19_used)], rec_rates_log, time_steps[i], "rec_rate", mcmc_samples)
      fc_rec_rate$yhat <- exp(fc_rec_rate$yhat)
      forecast_plot(paste0(dir_name, "/forecast_plot"), ref_data_flag[i], final_dates_ref[i], n, n_ref, df_COVID19_used$date, df_COVID19_ref_used$date, results_ref_used$rec_rates, fc_rec_rate, time_steps[i], "recovery_rates")
      
      
      # Forecast on global fatality_rates
      results_used$fat_rates[which(results_used$fat_rates <= 1e-10)] <- min(results_used$fat_rates[which(results_used$fat_rates > 1e-10)])
      fat_rates_log <- log(unique(results_used$fat_rates))
      fc_fat_rate <- apply_Prophet(dir_name, df_COVID19_used$date[(nrow(df_COVID19_used)-length(fat_rates_log)+1):nrow(df_COVID19_used)], fat_rates_log, time_steps[i], "fat_rate", mcmc_samples)
      fc_fat_rate$yhat <- exp(fc_fat_rate$yhat)
      forecast_plot(paste0(dir_name, "/forecast_plot"), ref_data_flag[i], final_dates_ref[i], n, n_ref, df_COVID19_used$date, df_COVID19_ref_used$date, results_ref_used$fat_rates, fc_fat_rate, time_steps[i], "fatality_rates")
      
      if(variants){
        variants_name <- unique(df_variants_processed$variant)
        plot_I_variants(dir_name, SIRD_all_variants, variants_name, final_dates)
        
        infection_rates <- data.frame(date=NULL, variant=NULL, mean=NULL)
        I <- data.frame(date=NULL, variant=NULL, mean=NULL)
        I_Arima <- data.frame(date=NULL, variant=NULL, mean=NULL)
        I_seasonal_Arima <- data.frame(date=NULL, variant=NULL, mean=NULL)
        I_EpiNow <- data.frame(date=NULL, variant=NULL, mean=NULL)

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
            results_used_local$infection_rates[which(results_used_local$infection_rates <= 1e-10)] <- min(results_used_local$infection_rates[which(results_used_local$infection_rates > 1e-10)])
            
            infection_rates_log <- log(results_used_local$infection_rates)
            fc_inf_rate <- apply_Prophet(dir_name, df_COVID19_used$date[(nrow(df_COVID19_used)-length(infection_rates_log)+1):nrow(df_COVID19_used)], infection_rates_log, time_steps[i], paste0("inf_rate_", variants_name[k]), mcmc_samples)
            fc_inf_rate$yhat <- exp(fc_inf_rate$yhat)
          }
          else{
            fc_inf_rate <- data.frame(yhat=rep(0.0, time_steps[i]))
          }
          
          forecast_plot(paste0(dir_name, "/forecast_plot/infection_rates"), ref_data_flag[i], final_dates_ref[i], n, n_ref, df_COVID19_used$date, df_COVID19_ref_used$date, results_ref_used_local$infection_rates, fc_inf_rate, time_steps[i], paste0("infection_rates_", variants_name[k]))
          
          infection_rates_local <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), variant=rep(variants_name[k], time_steps[i]), mean=fc_inf_rate$yhat)
          infection_rates <- rbind(infection_rates, infection_rates_local)
          
          if(sum(SIRD_used_local$I) > 0){
            # Forecast on I
            SIRD_used_local$I[which(SIRD_used_local$I == 0)] <- min(SIRD_used_local$I[which(SIRD_used_local$I != 0)])
            I_plain <- SIRD_used_local$I
            
            fc_I <- apply_Prophet(dir_name, df_COVID19_used$date, I_plain, time_steps[i], paste0("I_", variants_name[k]), mcmc_samples)

            fc_I_Arima <- apply_Arima(dir_name, df_COVID19_used$date, I_plain, time_steps[i], paste0("I_", variants_name[k], "_ARIMA"))

            fc_I_seasonal_Arima <- apply_seasonal_Arima(dir_name, df_COVID19_used$date, I_plain, time_steps[i], paste0("I_", variants_name[k], "_SARIMA"))

            if(i == 1){
              apply_NeuralProphet(dir_name, df_COVID19_used$date, I_plain, time_steps[i], variants_name[k])
              apply_LSTM(dir_name, df_COVID19_used$date, I_plain, time_steps[i], variants_name[k])
              apply_GRU(dir_name, df_COVID19_used$date, I_plain, time_steps[i], variants_name[k])
              epinow_forecast <- apply_EpiNow(df_COVID19_used$date, I_plain)
            }
            
            I_local_Arima <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), variant=rep(variants_name[k], time_steps[i]), mean=as.vector(fc_I_Arima$mean))
            I_local_seasonal_Arima <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), variant=rep(variants_name[k], time_steps[i]), mean=as.vector(fc_I_seasonal_Arima$mean))
          }
          else{
            fc_I <- data.frame(yhat=rep(0.0, time_steps[i]))
            fc_I_Arima <- data.frame(mean=rep(0.0, time_steps[i]))
            fc_I_seasonal_Arima <- data.frame(mean=rep(0.0, time_steps[i]))
            epinow_forecast <- rep(0.0, time_steps[i])
            
            I_local_Arima <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), variant=rep(variants_name[k], time_steps[i]), mean=fc_I_Arima$mean)
            I_local_seasonal_Arima <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), variant=rep(variants_name[k], time_steps[i]), mean=fc_I_seasonal_Arima$mean)
          }
          
          forecast_plot(paste0(dir_name, "/forecast_plot/I"), ref_data_flag[i], final_dates_ref[i], n, n_ref, df_COVID19_used$date, df_COVID19_ref_used$date, SIRD_ref_used_local$I, fc_I, time_steps[i], paste0("I_", variants_name[k]))
          
          I_local <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), variant=rep(variants_name[k], time_steps[i]), mean=fc_I$yhat)
          I_local_EpiNow <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), variant=rep(variants_name[k], time_steps[i]), mean=epinow_forecast[1:time_steps[i]])
          
          I <- rbind(I, I_local)
          I_Arima <- rbind(I_Arima, I_local_Arima)
          I_seasonal_Arima <- rbind(I_seasonal_Arima, I_local_seasonal_Arima)
          I_EpiNow <- rbind(I_EpiNow, I_local_EpiNow)
        }
        
        global_infection_rates <- results_all %>%
          filter(date >= initial_dates[j], date <= final_dates[j])
        
        global_infection_rates_ref <- results_all %>%
          filter(date >= initial_dates[j], date <= final_dates_ref[i])
        
        # Forecast on global infection rates
        infection_rates_log <- log(global_infection_rates$infection_rates)
        infection_rates_log[which(is.infinite(infection_rates_log) | is.na(infection_rates_log))] <- 0
        fc_inf_rate <- apply_Prophet(dir_name, global_infection_rates$date, infection_rates_log, time_steps[i], "global_inf_rate", mcmc_samples)
        fc_inf_rate$yhat <- exp(fc_inf_rate$yhat)
        forecast_plot(paste0(dir_name, "/forecast_plot"), ref_data_flag[i], final_dates_ref[i], n, n_ref, global_infection_rates$date, global_infection_rates_ref$date, global_infection_rates_ref$infection_rates, fc_inf_rate, time_steps[i], "infection_rates")
        
        global_infection_rates <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), mean=fc_inf_rate$yhat)
        global_recovery_rates <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), mean=fc_rec_rate$yhat)
        global_fatality_rates <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), mean=fc_fat_rate$yhat)
        
        
        # Plot the forecast and the comparisons
        SIRD_final <- SIRD_evolution(paste0(dir_name, "/forecast_plot"), time_steps[i], ref_data_flag[i], final_dates_ref[i], infection_rates, global_infection_rates, global_recovery_rates, global_fatality_rates, immunization_end_rate, I, SIRD_used, SIRD_ref_used, N[1], variants)
        compute_error(SIRD_ref_used, I, I_Arima, I_seasonal_Arima, I_EpiNow, SIRD_final, final_dates_ref[i], time_steps[i], dir_name, variants)
      }
      else{
        # Forecast on infection rates
        infection_rates_log <- log(results_used$infection_rates)
        infection_rates_log[which(is.infinite(infection_rates_log) | is.na(infection_rates_log))] <- 0
        fc_inf_rate <- apply_Prophet(dir_name, SIRD_used$date, infection_rates_log, time_steps[i], "inf_rate", mcmc_samples)
        fc_inf_rate$yhat <- exp(fc_inf_rate$yhat)
        forecast_plot(paste0(dir_name, "/forecast_plot"), ref_data_flag[i], final_dates_ref[i], n, n_ref, SIRD_used$date, SIRD_ref_used$date, results_ref_used$infection_rates, fc_inf_rate, time_steps[i], "infection_rates")
        
        # Forecast on I
        I_plain <- SIRD_used$I
        I_plain[which(is.infinite(I_plain) | is.na(I_plain))] <- 0
        fc_I <- apply_Prophet(dir_name, SIRD_used$date, I_plain, time_steps[i], "I", mcmc_samples)
        forecast_plot(paste0(dir_name, "/forecast_plot"), ref_data_flag[i], final_dates_ref[i], n, n_ref, SIRD_used$date, SIRD_ref_used$date, SIRD_ref_used$I, fc_I, time_steps[i], "I")  
        
        # Apply auto.arima and auto.arima with seasonality
        fc_I_Arima <- apply_Arima(dir_name, df_COVID19_used$date, I_plain, time_steps[i], "I_ARIMA")

        fc_I_seasonal_Arima <- apply_seasonal_Arima(dir_name, df_COVID19_used$date, I_plain, time_steps[i], "I_SARIMA")

        if(i == 1){
          apply_NeuralProphet(dir_name, df_COVID19_used$date, I_plain, time_steps[i], "all")
          apply_LSTM(dir_name, df_COVID19_used$date, I_plain, time_steps[i], "all")
          apply_GRU(dir_name, df_COVID19_used$date, I_plain, time_steps[i], "all")
          epinow_forecast <- apply_EpiNow(df_COVID19_used$date, I_plain)
        }

        infection_rates <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), mean=fc_inf_rate$yhat)
        recovery_rates <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), mean=fc_rec_rate$yhat)
        fatality_rates <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), mean=fc_fat_rate$yhat)
        
        I <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), mean=fc_I$yhat)
        I_Arima <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), variant=rep(variants_name[k], time_steps[i]), mean=fc_I_Arima$mean)
        I_seasonal_Arima <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), variant=rep(variants_name[k], time_steps[i]), mean=fc_I_seasonal_Arima$mean)
        I_EpiNow <- data.frame(date=seq.Date(df_COVID19_used$date[n]+1, df_COVID19_used$date[n]+time_steps[i], 1), variant=rep(variants_name[k], time_steps[i]), mean=epinow_forecast[1:time_steps[i]])
        
        # Plot the forecast and the comparisons
        SIRD_final <- SIRD_evolution(paste0(dir_name, "/forecast_plot"), time_steps[i], ref_data_flag[i], final_dates_ref[i], infection_rates, infection_rates, recovery_rates, fatality_rates, immunization_end_rate, fc_I$yhat, SIRD_used, SIRD_ref_used, N[1], variants)
        compute_error(SIRD_ref_used, I, I_Arima, I_seasonal_Arima, I_EpiNow, SIRD_final, final_dates_ref[i], time_steps[i], dir_name, variants)
      }
    }
    
    variants_name <- unique(SIRD_final$variant)
    
    final_plots(dir_name, variants_name)
  }
}