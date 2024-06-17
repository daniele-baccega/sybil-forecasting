# Sybil
#
# Author: Daniele Baccega
# Prophet: https://facebook.github.io/prophet/docs/quick_start.html

# Forecast
#
#
# Inputs:
#   - df_disease_all              dataframe with disease data (after preprocessing)
#   - df_variants_all             dataframe with variants data (after preprocessing)
#   - variants                    true if we are considering variants, false otherwise
#   - daily_spline                true if we approximate daily data with a spline, false otherwise
#   - external_dir_names:         names of the external directories
#   - immunization_end_rate:      immunization end rate
#   - recovery_rate:              recovery rate
#   - forecast:                   true if you want to do the forecasts, false if you only want to extract the rates
#   - initial_dates:              initial dates
#   - final_dates:                final dates
Sybil <- function(df_disease_all, df_variants_all, variants = TRUE, daily_variants_data = TRUE, daily_spline = FALSE, external_dir_names = paste0("Scenario_", as.numeric(Sys.time())), immunization_end_rate = 1 / 180, recovery_rate = 1 / 14, forecast = FALSE, initial_dates = c(), final_dates = c(), region_abbrv = NA){
  if(forecast && (length(initial_dates) != length(final_dates) || length(initial_dates) != length(external_dir_names)))
    stop("Variables initial_dates, final_dates and external_dir_names must have the same size!")
  
  # recovery_data <- FALSE
  # if("new_recoveries" %in% names(df_disease_all))
  #   recovery_data <- TRUE
  
  ref_data_flag <- rep(FALSE, 4)
  time_steps <- c(7, 14, 21, 28)
  internal_dir_name <- paste0("SIRD", if(variants) "_Variants" else "", if(immunization_end_rate > 0) "_Reinfection" else "", if(daily_spline) "_DailySpline" else "", "_FixedRecoveryRate/")
  
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
      system(paste0("mkdir -p ", dir_name, "/data"))
      system(paste0("mkdir -p ", dir_name, "/forecast_plot"))
      
      system(paste0("mkdir -p ", dir_name, "/forecast_plot/I"))
      system(paste0("mkdir -p ", dir_name, "/forecast_plot/infection_rates"))
      system(paste0("mkdir -p ", dir_name, "/forecast_plot/comparison"))
      system(paste0("mkdir -p ", dir_name, "/forecast_plot/RData"))
    }
    
    # Compute and save all the data
    data <- compartmental_models(dir_name, df_disease_all, df_variants_all, immunization_end_rate, recovery_rate)
    df_variants_all <- data[[1]]
    df_disease_all <- data[[2]]
    SIRD_all <- data[[3]]
    results_all <- data[[4]]

    # Check if we can reproduce the real data using a SIRD model with the previously computed rates
    SIRD_check(dir_name, SIRD_all, results_all$infection_rates, results_all$rec_rates, results_all$fat_rates, immunization_end_rate, df_disease_all$population[1])
    
    SIRD_all_variants <- data.frame()
    results_all_variants <- data.frame()
    # Plot variants info
    if(variants){
      data <- generate_and_plot_variants_info(paste0(external_dir_names[j], internal_dir_name), df_variants_all, df_disease_all, SIRD_all, results_all, daily_variants_data)
      df_variants_processed <- data[[1]]
      df_disease_all <- data[[2]]
      SIRD_all <- data[[3]]
      results_all <- data[[4]]

      variants_data <- SIRD_variants(dir_name, df_variants_processed, SIRD_all, results_all, immunization_end_rate, df_disease_all$population[1])
      SIRD_all_variants <- variants_data[[1]]
      results_all_variants <- variants_data[[2]]
      
      save_csv(dir_name, results_all_variants, "rates_variants")
      save_csv(dir_name, SIRD_all_variants, "evolution_variants")
    }
    
    save_csv(dir_name, results_all, "rates")
    save_csv(dir_name, SIRD_all, "evolution")
    save_csv(dir_name, df_disease_all, "data")
    
    if(forecast){
      if(final_dates[j] > SIRD_all$date[nrow(SIRD_all)]){
        print(paste0("Warning: ", final_dates[j], " is greater than the last date in the data! Skip"))
        next
      }
      
      # Compute the final dates
      final_dates_ref <- c()
      for(i in seq(1, length(time_steps))){
        final_dates_ref <- c(final_dates_ref, final_dates[j] + time_steps[i])
      }
      
      if(daily_spline)
        plot_I(dir_name, SIRD_all_spline)
      else
        plot_I(dir_name, SIRD_all)
      
      
      for(i in seq(1, length(time_steps))){
        filtered_data <- filter_data(df_disease_all, SIRD_all, SIRD_all_variants, results_all, results_all_variants, initial_dates[j], final_dates[j], final_dates_ref[i], daily_spline, variants)
        df_disease_ref_used <- filtered_data[[1]]
        df_disease_used <- filtered_data[[2]]
        SIRD_ref_used <- filtered_data[[3]]
        SIRD_used <- filtered_data[[4]]
        results_ref_used <- filtered_data[[5]]
        results_used <- filtered_data[[6]]
        
        N <- df_disease_used$population
        N_ref <- df_disease_ref_used$population
        
        n <- nrow(df_disease_used)
        n_ref <- n + time_steps[i]
        
        
        ref_data_flag[i] <- df_disease_all$date[nrow(df_disease_all)-1] >= (df_disease_used$date[n] + time_steps[i])
        
        # Forecast on fatality_rates
        results_used$fat_rates[which(results_used$fat_rates <= 1e-10)] <- min(results_used$fat_rates[which(results_used$fat_rates > 1e-10)])
        fat_rates_log <- log(unique(results_used$fat_rates))
        fc_fat_rate <- apply_Prophet(dir_name, df_disease_used$date[(nrow(df_disease_used)-length(fat_rates_log)+1):nrow(df_disease_used)], fat_rates_log, time_steps[i], "fat_rate")
        fc_fat_rate$yhat <- exp(fc_fat_rate$yhat)
        fc_fat_rate$yhat_lower <- exp(fc_fat_rate$yhat_lower)
        fc_fat_rate$yhat_upper <- exp(fc_fat_rate$yhat_upper)
        forecast_plot(paste0(dir_name, "/forecast_plot"), ref_data_flag[i], final_dates_ref[i], n, n_ref, df_disease_used$date, df_disease_ref_used$date, results_ref_used$fat_rates, fc_fat_rate, time_steps[i], "fatality_rates")
        
        if(variants){
          variants_name <- unique(df_variants_processed$variant)
          plot_I_variants(dir_name, SIRD_all_variants, variants_name, final_dates)
          
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
              results_used_local$infection_rates[which(results_used_local$infection_rates <= 1e-10)] <- min(results_used_local$infection_rates[which(results_used_local$infection_rates > 1e-10)])
              
              infection_rates_log <- log(results_used_local$infection_rates)
              fc_inf_rate <- apply_Prophet(dir_name, df_disease_used$date[(nrow(df_disease_used)-length(infection_rates_log)+1):nrow(df_disease_used)], infection_rates_log, time_steps[i], paste0("inf_rate_", variants_name[k]))
              fc_inf_rate$yhat <- exp(fc_inf_rate$yhat)
              fc_inf_rate$yhat_lower <- exp(fc_inf_rate$yhat_lower)
              fc_inf_rate$yhat_upper <- exp(fc_inf_rate$yhat_upper)
            }
            else{
              fc_inf_rate <- data.frame(yhat=rep(0, time_steps[i]), yhat_lower=rep(0, time_steps[i]), yhat_upper=rep(0, time_steps[i]))
            }
            
            forecast_plot(paste0(dir_name, "/forecast_plot/infection_rates"), ref_data_flag[i], final_dates_ref[i], n, n_ref, df_disease_used$date, df_disease_ref_used$date, results_ref_used_local$infection_rates, fc_inf_rate, time_steps[i], paste0("infection_rates_", variants_name[k]))
            
            infection_rates_local <- data.frame(date=seq.Date(df_disease_used$date[n]+1, df_disease_used$date[n]+time_steps[i], 1), variant=rep(variants_name[k], time_steps[i]), mean=fc_inf_rate$yhat, lower=fc_inf_rate$yhat_lower, upper=fc_inf_rate$yhat_upper)
            infection_rates <- rbind(infection_rates, infection_rates_local)
            
            if(sum(SIRD_used_local$I) > 0){
              # Forecast on I
              SIRD_used_local$I[which(SIRD_used_local$I == 0)] <- min(SIRD_used_local$I[which(SIRD_used_local$I != 0)])
              I_log <- log(SIRD_used_local$I)
              fc_I <- apply_Prophet(dir_name, df_disease_used$date, I_log, time_steps[i], paste0("I_", variants_name[k]))
              fc_I$yhat <- exp(fc_I$yhat)
              fc_I$yhat_lower <- exp(fc_I$yhat_lower)
              fc_I$yhat_upper <- exp(fc_I$yhat_upper)
            }
            else{
              fc_I <- data.frame(yhat=rep(0, time_steps[i]), yhat_lower=rep(0, time_steps[i]), yhat_upper=rep(0, time_steps[i]))
            }
            
            forecast_plot(paste0(dir_name, "/forecast_plot/I"), ref_data_flag[i], final_dates_ref[i], n, n_ref, df_disease_used$date, df_disease_ref_used$date, SIRD_ref_used_local$I, fc_I, time_steps[i], paste0("I_", variants_name[k]))
            
            I_local <- data.frame(date=seq.Date(df_disease_used$date[n]+1, df_disease_used$date[n]+time_steps[i], 1), variant=rep(variants_name[k], time_steps[i]), mean=fc_I$yhat, lower=fc_I$yhat_lower, upper=fc_I$yhat_upper)
            I <- rbind(I, I_local)
          }
          
          global_infection_rates <- results_all
          if(daily_spline)
            global_infection_rates <- results_all_spline
          
          global_infection_rates_ref <- global_infection_rates %>%
            filter(date >= initial_dates[j], date <= final_dates_ref[i])
          
          global_infection_rates <- global_infection_rates %>%
            filter(date >= initial_dates[j], date <= final_dates[j])
          
          # Forecast on global infection rates
          infection_rates_log <- log(global_infection_rates$infection_rates)
          infection_rates_log[which(is.infinite(infection_rates_log) | is.na(infection_rates_log))] <- 0
          fc_inf_rate <- apply_Prophet(dir_name, global_infection_rates$date, infection_rates_log, time_steps[i], "global_inf_rate")
          fc_inf_rate$yhat <- exp(fc_inf_rate$yhat)
          fc_inf_rate$yhat_lower <- exp(fc_inf_rate$yhat_lower)
          fc_inf_rate$yhat_upper <- exp(fc_inf_rate$yhat_upper)
          forecast_plot(paste0(dir_name, "/forecast_plot"), ref_data_flag[i], final_dates_ref[i], n, n_ref, global_infection_rates$date, global_infection_rates_ref$date, global_infection_rates_ref$infection_rates, fc_inf_rate, time_steps[i], "infection_rates")
          
          global_infection_rates <- data.frame(date=seq.Date(df_disease_used$date[n]+1, df_disease_used$date[n]+time_steps[i], 1), mean=fc_inf_rate$yhat, lower=fc_inf_rate$yhat_lower, upper=fc_inf_rate$yhat_upper)
          global_fatality_rates <- data.frame(date=seq.Date(df_disease_used$date[n]+1, df_disease_used$date[n]+time_steps[i], 1), mean=fc_fat_rate$yhat, lower=fc_fat_rate$yhat_lower, upper=fc_fat_rate$yhat_upper)
          
          
          # Plot the forecast and the comparisons
          SIRD_final <- SIRD_evolution(paste0(dir_name, "/forecast_plot"), time_steps[i], ref_data_flag[i], final_dates_ref[i], infection_rates, global_infection_rates, recovery_rate, global_fatality_rates, immunization_end_rate, SIRD_used, SIRD_ref_used, N[1], variants)
          #if(!is.na(region_abbrv))
           # compute_error(SIRD_ref_used, SIRD_final, final_dates_ref[i], time_steps[i], dir_name, variants, region_abbrv)
        }
        else{
          # Forecast on infection rates
          infection_rates_log <- log(results_used$infection_rates)
          infection_rates_log[which(is.infinite(infection_rates_log) | is.na(infection_rates_log))] <- 0
          fc_inf_rate <- apply_Prophet(dir_name, SIRD_used$date, infection_rates_log, time_steps[i], "inf_rate")
          fc_inf_rate$yhat <- exp(fc_inf_rate$yhat)
          fc_inf_rate$yhat_lower <- exp(fc_inf_rate$yhat_lower)
          fc_inf_rate$yhat_upper <- exp(fc_inf_rate$yhat_upper)
          forecast_plot(paste0(dir_name, "/forecast_plot"), ref_data_flag[i], final_dates_ref[i], n, n_ref, SIRD_used$date, SIRD_ref_used$date, results_ref_used$infection_rates, fc_inf_rate, time_steps[i], "infection_rates")
          
          # Forecast on I
          I_log <- log(SIRD_used$I)
          I_log[which(is.infinite(I_log) | is.na(I_log))] <- 0
          fc_I <- apply_Prophet(dir_name, SIRD_used$date, I_log, time_steps[i], "I")
          fc_I$yhat <- exp(fc_I$yhat)
          fc_I$yhat_lower <- exp(fc_I$yhat_lower)
          fc_I$yhat_upper <- exp(fc_I$yhat_upper)
          forecast_plot(paste0(dir_name, "/forecast_plot"), ref_data_flag[i], final_dates_ref[i], n, n_ref, SIRD_used$date, SIRD_ref_used$date, SIRD_ref_used$I, fc_I, time_steps[i], "I")  
          
          infection_rates <- data.frame(date=seq.Date(df_disease_used$date[n]+1, df_disease_used$date[n]+time_steps[i], 1), mean=fc_inf_rate$yhat, lower=fc_inf_rate$yhat_lower, upper=fc_inf_rate$yhat_upper)
          fatality_rates <- data.frame(date=seq.Date(df_disease_used$date[n]+1, df_disease_used$date[n]+time_steps[i], 1), mean=fc_fat_rate$yhat, lower=fc_fat_rate$yhat_lower, upper=fc_fat_rate$yhat_upper)
          
          I <- data.frame(date=seq.Date(df_disease_used$date[n]+1, df_disease_used$date[n]+time_steps[i], 1), mean=fc_I$yhat, lower=fc_I$yhat_lower, upper=fc_I$yhat_upper)
          
          # Plot the forecast and the comparisons
          SIRD_final <- SIRD_evolution(paste0(dir_name, "/forecast_plot"), time_steps[i], ref_data_flag[i], final_dates_ref[i], infection_rates, infection_rates, recovery_rate, fatality_rates, immunization_end_rate, SIRD_used, SIRD_ref_used, N[1], variants)
          if(!is.na(region_abbrv))
            compute_error(SIRD_ref_used, SIRD_final, final_dates_ref[i], time_steps[i], dir_name, variants, region_abbrv)
        }
      }
      
      variants_name <- unique(SIRD_final$variant)
      
      final_plots(dir_name, variants_name)
    }
  }
}