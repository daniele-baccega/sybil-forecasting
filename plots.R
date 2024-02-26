# Sybil
#
# Author: Daniele Baccega
# Data: COVID19 R library
# Prophet: https://facebook.github.io/prophet/docs/quick_start.html

# Checks if we can re-obtain the real data starting from the previously computed
# rates; in particular, we used a deterministic SIRD model.
#
# Inputs:
#   - dir_name:               name of the directory in which put the results
#   - SIRD:                   evolution of the infection using a SIRD model
#   - infection_rates:        infection rates
#   - rec_rates:              recovery rates
#   - fat_rates:              fatality rates
#   - immunization_end_rate:  immunization end rate
#   - N:                      total population
SIRD_check <- function(dir_name, SIRD, infection_rates, rec_rates, fat_rates, immunization_end_rate, N){
  # Reproduce the evolution of the SIRD model starting from the extracted rates
  n <- nrow(SIRD)
  
  S_local <- I_local <- R_local <- D_local <- rep(0, n)
  
  S_local[1] <- SIRD$S[1]
  I_local[1] <- SIRD$I[1]
  R_local[1] <- SIRD$R[1]
  D_local[1] <- SIRD$D[1]
  
  for(t in 1:(n-1)){
    S_local[t+1] <- S_local[t] - infection_rates[t] * I_local[t] * S_local[t] / N + R_local[t] * immunization_end_rate
    I_local[t+1] <- I_local[t] + infection_rates[t] * I_local[t] * S_local[t] / N - I_local[t] * (rec_rates[t] + fat_rates[t])
    R_local[t+1] <- R_local[t] + I_local[t] * rec_rates[t] - R_local[t] * immunization_end_rate
    D_local[t+1] <- D_local[t] + I_local[t] * fat_rates[t]
  }
  
  date <- rep(SIRD$date, 4)
  value <- c(S_local, I_local, R_local, D_local)
  place <- c(rep("S", n), rep("I", n), rep("R", n), rep("D", n))
  
  df_plot <- data.frame(date, value, place)
  df_plot$place = factor(df_plot$place, levels = c("S", "I", "R", "D"))
  
  value <- c(SIRD$S, SIRD$I, SIRD$R, SIRD$D)
  
  df_plot_ref <- data.frame(date, value, place)
  df_plot_ref$place = factor(df_plot_ref$place, levels = c("S", "I", "R", "D"))
  
  df_plot <- df_plot %>%
    mutate(type = "SIRD")
  
  df_plot_ref <- df_plot_ref %>%
    mutate(type = "Real")
  
  df_plot <- rbind(df_plot, df_plot_ref)
  
  png(paste0(dir_name, "/SIRD_check.png"), units="in", width=34, height=15, res=300)
  plot <- ggplot(df_plot) +
    geom_line(aes(x=date, y=value, col=type), linewidth=1.5) +
    facet_wrap(~place, scales="free_y") +
    scale_colour_manual(values=c("#000000", "#F3474D")) +
    scale_y_continuous(labels = label_scientific())
  print(plot)
  dev.off()
}

# Plot the rates
#
# Inputs:
#   - dir_name:     name of the directory in which put the results
#   - results_all:  infection and fatality rates extracted from the SIRD model
plot_rates <- function(dir_name, results_all, type = ""){
  png(paste0(dir_name, "/infection_rates", type, ".png"), units="in", width=34, height=15, res=300)
  plot <- ggplot(results_all) +
    geom_line(aes(x=date, y=infection_rates), linewidth=2) +
    theme(legend.position = "bottom", legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=25), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
    labs(x="Date", y="Infection rates") +
    scale_y_continuous(labels = label_scientific())
  print(plot)
  dev.off()
  
  png(paste0(dir_name, "/fatality_rates", type, ".png"), units="in", width=34, height=15, res=300)
  plot <- ggplot(results_all) +
    geom_line(aes(x=date, y=fat_rates), linewidth=2) +
    theme(legend.position = "bottom", legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=25), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
    labs(x="Date", y="Fatality rates") +
    scale_y_continuous(labels = label_scientific())
  print(plot)
  dev.off()
}

# Plot the infection rates for each variant
#
# Inputs:
#   - dir_name:     name of the directory in which put the results
#   - results_all:  infection rates extracted from the SIvRD model
plot_infection_rates_variants <- function(dir_name, results_all, type = ""){
  variants_name <- unique((results_all$variant))
  
  png(paste0(dir_name, "/infection_rates_variants", type, ".png"), units="in", width=34, height=15, res=300)
  plot <- ggplot(results_all) +
    geom_line(aes(x=date, y=infection_rates, color=variant), linewidth=2) +
    theme(legend.position = "bottom", legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=25), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
    labs(x="Date", y="Infection rates", color="Variant") +
    scale_colour_manual(values=hue_pal()(length(variants_name))) +
    scale_y_continuous(labels = label_scientific())
  print(plot)
  dev.off()
}

# Plot SIRD$I with and without the scenarios
#
# Inputs:
#   - dir_name: name of the directory in which put the results
#   - SIRD_all: evolution of the infection using a SIRD model
plot_I <- function(dir_name, SIRD_all){
  png(paste0(dir_name, "/I.png"), units="in", width=34, height=15, res=300)
  plot <- ggplot(SIRD_all, aes(x=date, y=I)) +
    geom_line(linewidth=1.5) +
    theme(legend.position = "bottom", legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=25), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
    labs(x="Date", y="Population") +
    scale_y_continuous(labels = label_scientific())
  print(plot)
  dev.off()
}

# Plot the proportion of a variant
#
# Inputs:
#   - dir_name:           name of the directory in which put the results
#   - variants_spline_df: proportion of a variant
#   - v:                  name of the variant
#   - variants_name:      names of the variants
#   - i:                  index of the variant
plot_variant_proportion <- function(dir_name, variants_spline_df, v, variants_name, i){
  png(paste0(dir_name, "variants_", v, "_spline_check.png"), units="in", width=34, height=15, res=300)
  plot <- ggplot(variants_spline_df) +
    geom_line(aes(x=date, y=y, col=variant), linewidth=1.5) +
    scale_colour_manual(values=hue_pal()(length(variants_name))[i]) +
    theme(legend.position = "bottom", legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=25), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
    labs(x="Date", y="Proportion", col="Variant")
  print(plot)
  dev.off()
}

# Plot the proportion of each variant
#
# Inputs:
#   - dir_name:           name of the directory in which put the results
#   - variants_global_df: proportion of each variant
#   - variants_name:      names of the variants
plot_variants_proportion <- function(dir_name, variants_global_df, variants_name){
  png(paste0(dir_name, "variants_check.png"), units="in", width=34, height=15, res=300)
  plot <- ggplot(variants_global_df) +
    geom_line(aes(x=date, y=y, col=variant), linewidth=1.5) +
    scale_colour_manual(values=hue_pal()(length(variants_name))) +
    theme(legend.position = "bottom", legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=25), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
    labs(x="Date", y="Proportion", col="Variants")
  print(plot)
  dev.off()
}

# Plot SIvRD$I with and without the scenarios
#
# Inputs:
#   - dir_name:       name of the directory in which put the results
#   - SIRD_all:       evolution of the infection using a SIvRD model
#   - variants_name:  names of the variants
#   - final_dates:                final dates
plot_I_variants <- function(dir_name, SIRD_all_variants, variants_name, final_dates){
  png(paste0(dir_name, "/I_variants_all_scenarios.png"), units="in", width=34, height=15, res=300)
  plot <- ggplot(SIRD_all_variants, aes(date, I, color=variant))
  for(i in 1:length(variants_name)){
    plot <- plot + geom_line(linewidth=2)
  }
  plot <- plot +
    geom_vline(aes(xintercept = final_dates[1]), color="#F3474D", linetype="dashed", linewidth=2) +
    geom_vline(aes(xintercept = final_dates[2]), color="#F3474D", linetype="dashed", linewidth=2) +
    geom_vline(aes(xintercept = final_dates[3]), color="#F3474D", linetype="dashed", linewidth=2) +
    geom_text(aes(x = final_dates[1]-30, label="\n1st forecast", y=max(I) - max(I)/8 ), size = 13, colour="#F3474D", angle=90) +
    geom_text(aes(x = final_dates[2]-30, label="\n2nd forecast", y=max(I) - max(I)/8), size = 13, colour="#F3474D", angle=90) +
    geom_text(aes(x = final_dates[3]-30, label="\n3rd forecast", y=max(I) - max(I)/8), size = 13, colour="#F3474D", angle=90) +
    scale_colour_manual(values=hue_pal()(length(variants_name))) +
    theme(legend.position = "bottom", legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=25), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
    labs(x="Date", y="Population", col="Variants") +
    scale_y_continuous(labels = label_scientific())
  print(plot)
  dev.off()
  
  png(paste0(dir_name, "/I_variants.png"), units="in", width=34, height=15, res=300)
  plot <- ggplot(SIRD_all_variants, aes(date, I, color=variant))
  for(i in 1:length(variants_name)){
    plot <- plot + geom_line(linewidth=1.5)
  }
  
  plot <- plot +
    scale_colour_manual(values=hue_pal()(length(variants_name))) +
    theme(legend.position = "bottom", legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=25), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
    labs(x="Date", y="Population", col="Variants") +
    scale_y_continuous(labels = label_scientific())
  print(plot)
  dev.off()
}

# Generates the forecast plot.
#
# Inputs:
#   - dir_name:       name of the directory in which put the results
#   - ref_data_flag:  true if ground truth for the forecast window exists, false otherwise
#   - final_date:     final date (for training)
#   - n:              length of the training time series
#   - n_ref:          length of the training time series + the forecast window
#   - dates:          dates of the training time series
#   - dates_ref:      dates of the training time series + the forecast window
#   - data_fc:        forecast
#   - i:              time window
#   - title:          text on the y axis
forecast_plot <- function(dir_name, ref_data_flag, final_date, n, n_ref, dates, dates_ref, data_ref, data_fc, i, title){
  if(ref_data_flag){
    type <- rep(NA, n_ref + (n_ref - n) + 2)
    
    type[1:n] <- rep("training data", n)
    type[(n+1):(n_ref+1)] <- rep("ground truth for validation", n_ref-n+1)
    type[(n_ref+2):(n_ref+(n_ref-n)+2)] <- rep("forecast", n_ref-n+1)
    
    date <- c(dates_ref[1:n], dates_ref[n:n_ref], dates_ref[n:n_ref])
    
    value <- c(data_ref[1:n], data_ref[n:n_ref], data_ref[n], data_fc$yhat)
    low <- c(rep(NA, n_ref+1), data_ref[n], data_fc$yhat_lower)
    up <- c(rep(NA, n_ref+1), data_ref[n], data_fc$yhat_upper)
    
    df_plot <- data.frame(date, value, low, up, type)
  }
  else{
    type <- rep(NA, n_ref + 1)
    
    type[1:n] <- rep("training data", n)
    type[(n+1):(n_ref+1)] <- rep("forecast", n_ref-n+1)
    
    date <- c(dates, seq(as.Date(dates[n]), final_date, 1))
    
    value <- c(data_ref[1:n], data_ref[n], data_fc$yhat)
    low <- c(rep(NA, n), data_ref[n], data_fc$yhat_lower)
    up <- c(rep(NA, n), data_ref[n], data_fc$yhat_upper)
    
    df_plot <- data.frame(date, value, low, up, type)
  }
  
  plot <- ggplot(df_plot, aes(x=date, col=type)) +
    geom_line(aes(y=value), linewidth=1.5) +
    labs(title=paste0(i, " days"), x="Date", y=if(title[1] == "I") "Population" else gsub("fatality", "Fatality", gsub("recovery", "Recovery", gsub("infection", "Infection", gsub("_", " ", gsub("_rates*", "_rates", title))))), color="Variants", linetype="Type") +
    scale_colour_manual(values=c("#F3474D", "#6B95DB", "#000000")) +
    theme(legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=25), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
    guides(color=guide_legend(override.aes=list(fill=NA)))
  if(substr(title, 1, 1) == "I"){
    plot <- plot + scale_y_continuous(labels = label_scientific())
  }
  save(plot, file = paste0(sub("/I/", "/", sub("/infection_rates/", "/", paste0(dir_name, "/"))), "/RData/forecast_", title, "_", i, "_days.RData"))
}

# Plot the evolution of the deterministic SIRD/SIvRD model.
#
# Inputs:
#   - dir_name:         name of the directory in which put the results
#   - n:                length of the training time series
#   - n_ref:            length of the training time series + the forecast window
#   - time_step:        time window to forecast
#   - ref_data_flag:    true if ground truth for the forecast window exists, false otherwise
#   - final_date:       final date (for training)
#   - infection_rates:  infection rates for each variant
#   - fc_I:             forecast on I
#   - SIRD:             evolution of the infection using a SIRD/SIvRD model
#   - SIRD_ref:         evolution of the infection using a SIRD/SIvRD model + the forecast window
#   - variants:         true if we are considering variants, false otherwise
plot_SIRD_evolution <- function(df_local, n, n_ref, dir_name, time_step, ref_data_flag, final_date, infection_rates, fc_I, SIRD, SIRD_ref, variants){
  if(variants){
    variants_name <- unique(infection_rates$variant)
    
    if(ref_data_flag){
      type <- rep(NA, n_ref + (n_ref - n) + 2)
      
      type[1:n] <- rep("training data", n)
      type[(n+1):(n_ref+1)] <- rep("forecast", n_ref-n+1)
      type[(n_ref+2):(n_ref+2+(n_ref-n))] <- rep("ground truth for validation", n_ref-n+1)
      type <- rep(type, 3 + length(variants_name))
      
      date <- rep(c(unique(SIRD_ref$date), SIRD_ref$date[n], SIRD_ref$date[n:n_ref]), 3 + length(variants_name))
      
      value <- c()
      place <- c()
      variant <- c()
      for(i in 1:length(variants_name)){
        df_local_variant <- df_local %>%
          filter(variant == variants_name[i])
        
        SIRD_ref_local <- SIRD_ref %>%
          filter(variant == variants_name[i])
        
        value <- c(value, c(df_local_variant$I, SIRD_ref_local$I[n], SIRD_ref_local$I[n:n_ref]))
        place <- c(place, c(rep("I", n_ref+2+(n_ref-n))))
        variant <- c(variant, rep(variants_name[i], n_ref+2+(n_ref-n)))
        
        if(i == length(variants_name)){
          value <- c(value, c(df_local_variant$S, SIRD_ref$S[n], SIRD_ref$S[n:n_ref]), c(df_local_variant$R, SIRD_ref$R[n], SIRD_ref$R[n:n_ref]), c(df_local_variant$D, SIRD_ref$D[n], SIRD_ref$D[n:n_ref]))
        }
      }
      
      place <- c(place, rep("S", n_ref+2+(n_ref-n)), rep("R", n_ref+2+(n_ref-n)), rep("D", n_ref+2+(n_ref-n)))
      variant <- c(variant, rep("-", n_ref+2+(n_ref-n)), rep("-", n_ref+2+(n_ref-n)), rep("-", n_ref+2+(n_ref-n)))
    }
    else{
      type <- rep(NA, n_ref + 1)
      
      type[1:n] <- rep("training data", n)
      type[(n+1):(n_ref+1)] <- rep("forecast", n_ref-n+1)
      type <- rep(type, 3 + length(variants_name))
      
      date <- rep(c(unique(SIRD$date), seq(SIRD$date[n], final_date, 1)), 3 + length(variants_name))
      
      value <- c()
      place <- c()
      variant <- c()
      for(i in 1:length(variants_name)){
        df_local_variant <- df_local %>%
          filter(variant == variants_name[i])
        
        SIRD_ref_local <- SIRD_ref %>%
          filter(variant == variants_name[i])
        
        value <- c(value, c(df_local_variant$I[1:n], df_local_variant$I[n:n_ref]))
        place <- c(place, c(rep("I", n_ref+1)))
        variant <- c(variant, rep(variants_name[i], n_ref+1))
        
        if(i == length(variants_name)){
          value <- c(value, value <- c(value, c(df_local_variant$S[1:n], df_local$S[n:n_ref]), c(df_local_variant$R[1:n], df_local$R[n:n_ref]), c(df_local_variant$D[1:n], df_local$D[n:n_ref])))
        }
      }
      
      place <- c(place, rep("S", n_ref+1), rep("R", n_ref+1), rep("D", n_ref+1))
      variant <- c(variant, rep("-", n_ref+1), rep("-", n_ref+1), rep("-", n_ref+1))
    }
    
    df_plot <- data.frame(date, value, place, type, variant)
    df_plot$place = factor(df_plot$place, levels = c("S", "I", "R", "D"))
    df_plot$variant = factor(df_plot$variant, levels = c(variants_name, "-"))
    
    df_plot <- df_plot %>%
      filter(place == "I")
    
    plot <- ggplot(df_plot) +
      geom_line(aes(date, value, col=variant, linetype=type), linewidth=1.5) +
      facet_wrap(~place, scales="free_y") +
      scale_colour_manual(values=c(hue_pal()(length(variants_name)), "#808080")) +
      theme(panel.spacing = unit(1, "cm"), legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=25), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38), strip.text.x = element_blank()) +
      labs(title=paste0(time_step, " days"), x="Date", y="Population", color="Variants", linetype="Type") +
      scale_y_continuous(labels = label_scientific())
    save(plot, file = paste0(dir_name, "/RData/SIRD_forecast_", time_step, "_days.RData"))
    
    for(i in 1:length(variants_name)){
      df_local_I <- df_local %>%
        filter(variant == variants_name[i])
      
      SIRD_ref_local <- SIRD_ref %>%
        filter(variant == variants_name[i])
      
      SIRD_local <- SIRD %>%
        filter(variant == variants_name[i])
      
      fc_I_local <- fc_I %>%
        filter(variant == variants_name[i])
      
      comparison(dir_name, time_step, paste0("I_", variants_name[i]), ref_data_flag, final_date, SIRD_local$date, SIRD_ref_local$date, df_local_I$I, fc_I_local$mean, SIRD_ref_local$I) 
    }
  }
  else{
    if(ref_data_flag){
      type <- rep(NA, n_ref + (n_ref - n) + 2)
      
      type[1:n] <- rep("training data", n)
      type[(n+1):(n_ref+1)] <- rep("forecast", n_ref-n+1)
      type[(n_ref+2):(n_ref+2+(n_ref-n))] <- rep("ground truth for validation", n_ref-n+1)
      
      date <- rep(c(SIRD_ref$date, SIRD_ref$date[n], SIRD_ref$date[n:n_ref]), 4)
      value <- c(c(df_local$S, SIRD_ref$S[n], SIRD_ref$S[n:n_ref]), c(df_local$I, SIRD_ref$I[n], SIRD_ref$I[n:n_ref]), c(df_local$R, SIRD_ref$R[n], SIRD_ref$R[n:n_ref]), c(df_local$D, SIRD_ref$D[n], SIRD_ref$D[n:n_ref]))
      place <- c(rep("S", n_ref+2+(n_ref-n)), rep("I", n_ref+2+(n_ref-n)), rep("R", n_ref+2+(n_ref-n)), rep("D", n_ref+2+(n_ref-n)))
      type <- rep(type, 4)
    }
    else{
      type <- rep(NA, n_ref + 1)
      
      type[1:n] <- rep("training data", n)
      type[(n+1):(n_ref+1)] <- rep("forecast", n_ref-n+1)
      
      date <- rep(c(SIRD$date, seq(SIRD$date[n], final_date, 1)), 4)
      value <- c(c(df_local$S[1:n], df_local$S[n], df_local$S[(n+1):length(df_local$S)]), c(df_local$I[1:n], df_local$I[n], df_local$I[(n+1):length(df_local$I)]), c(df_local$R[1:n], df_local$R[n], df_local$R[(n+1):length(df_local$R)]), c(df_local$D[1:n], df_local$D[n], df_local$D[(n+1):length(df_local$D)]))
      place <- c(rep("S", n_ref+1), rep("I", n_ref+1), rep("R", n_ref+1), rep("D", n_ref+1))
      type <- rep(type, 4)
    }
    
    df_plot <- data.frame(date, value, place, type)
    df_plot$place = factor(df_plot$place, levels = c("S", "I", "R", "D"))
    
    df_plot <- df_plot %>%
      filter(place == "I")
    
    plot <- ggplot(df_plot) +
      geom_line(aes(date, value, col=type), linewidth=1.5) +
      facet_wrap(~place, scales="free_y") +
      scale_colour_manual(values=c("#F3474D", "#6B95DB", "#000000")) +
      theme(panel.spacing = unit(1, "cm"), legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=25), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38), strip.text.x = element_blank()) +
      labs(title=paste0(time_step, " days"), x="Date", y="Population", color="Variants", linetype="Type") +
      scale_y_continuous(labels = label_scientific())
    save(plot, file = paste0(dir_name, "/RData/SIRD_forecast_", time_step, "_days.RData"))
    
    comparison(dir_name, time_step, "I", ref_data_flag, final_date, SIRD$date, SIRD_ref$date, df_local$I, fc_I, SIRD_ref$I)
  }
}

# Generates a comparison plot between the two considered forecast approaches.
#
# Inputs:
#   - dir_name:               name of the directory in which put the results
#   - time_step:              time window to forecast
#   - variable_name_variants: string used for save the plot
#   - ref_data_flag:          true if ground truth for the forecast window exists, false otherwise
#   - final_date_local:       final date (for training)
#   - dates:                  dates of the training time series
#   - dates_ref:              dates of the training time series + the forecast window
#   - variable_local:         ground truth
#   - variable_fc:            forecast
#   - variable_ref:           groung truth in the forecast window
comparison <- function(dir_name, time_step, variable_name_variants, ref_data_flag, final_date_local, dates, dates_ref, variable_local, variable_fc, variable_ref){
  n <- length(dates)
  n_ref <- n + time_step
  
  if(ref_data_flag){
    type <- rep(NA, n_ref + (n_ref - n) * 2 + 3)
    
    type[1:n] <- rep("training data", n)
    type[(n+1):(n_ref+1)] <- rep("forecast with Sybil", n_ref-n+1)
    type[(n_ref+2):(n_ref+2+(n_ref-n))] <- rep("ground truth for validation", n_ref-n+1)
    type[(n_ref+3+(n_ref-n)):(n_ref+3+(n_ref-n)*2)] <- rep(paste0("forecast with Prophet"), n_ref-n+1)
    
    date <- c(dates_ref, dates_ref[n], dates_ref[n:n_ref], dates_ref[n:n_ref])
    value <- c(variable_local, variable_ref[n], variable_ref[n:n_ref], variable_ref[n], variable_fc)
    place <- rep("I", n_ref + (n_ref - n) * 2 + 3)
  }
  else{
    type <- rep(NA, n_ref + (n_ref - n) + 2)
    
    type[1:n] <- rep("training data", n)
    type[(n+1):(n_ref+1)] <- rep("forecast with Sybil", n_ref-n+1)
    type[(n_ref+2):(n_ref+(n_ref-n)+2)] <- rep("forecast with Prophet", n_ref-n+1)
    
    date <- c(dates, seq(dates[n], final_date_local, 1), seq(dates[n], final_date_local, 1))
    value <- c(variable_local[1:n], variable_local[n], variable_local[(n+1):length(variable_local)], variable_local[n], variable_fc)
    place <- rep("I", n_ref + (n_ref - n) + 2)
  }
  
  df_plot <- data.frame(date, value, place, type)
  
  plot <- ggplot(df_plot, aes(date, value, col=type)) +
    geom_line(linewidth=1.5) +
    scale_colour_manual(values=c("#F3474D", "#6C9F6B", "#6B95DB", "#000000")) +
    theme(legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=25), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
    labs(title=paste0(time_step, " days"), x="Date", y="Population", color="Variants", linetype="Type") +
    scale_y_continuous(labels = label_scientific())
  save(plot, file=paste0(dir_name, "/RData/", variable_name_variants, "_compare_", time_step, "_days.RData"))
}

# Generate final plots.
#
# Inputs:
#   - dir_name:               name of the directory in which put the results
#   - variants_name           names of the variants
final_plots <- function(dir_name, variants_name){
  load(paste0(dir_name, "/forecast_plot/RData/SIRD_forecast_7_days.RData"))
  p1 <- plot
  load(paste0(dir_name, "/forecast_plot/RData/SIRD_forecast_14_days.RData"))
  p2 <- plot
  load(paste0(dir_name, "/forecast_plot/RData/SIRD_forecast_21_days.RData"))
  p3 <- plot
  load(paste0(dir_name, "/forecast_plot/RData/SIRD_forecast_28_days.RData"))
  p4 <- plot
  
  p <- (p1 + p2) / (p3 + p4) +
    plot_layout(guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png(paste0(dir_name, "/forecast_plot/SIRD_forecast.png"), units="in", width=34, height=15, res=300)
  print(p)
  dev.off()
  
  p <- p1 / p2 / p3 / p4 +
    plot_layout(guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png(paste0(dir_name, "/forecast_plot/SIRD_forecast_onexrow.png"), units="in", width=28, height=34, res=300)
  print(p)
  dev.off()
  
  
  load(paste0(dir_name, "/forecast_plot/RData/forecast_fatality_rates_7_days.RData"))
  p1 <- plot 
  load(paste0(dir_name, "/forecast_plot/RData/forecast_fatality_rates_14_days.RData"))
  p2 <- plot 
  load(paste0(dir_name, "/forecast_plot/RData/forecast_fatality_rates_21_days.RData"))
  p3 <- plot 
  load(paste0(dir_name, "/forecast_plot/RData/forecast_fatality_rates_28_days.RData"))
  p4 <- plot 
  
  p <- (p1 + p2) / (p3 + p4) +
    plot_layout(guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png(paste0(dir_name, "/forecast_plot/forecast_fatality_rates.png"), units="in", width=34, height=15, res=300)
  print(p)
  dev.off()
  
  p <- p1 / p2 / p3 / p4 +
    plot_layout(guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png(paste0(dir_name, "/forecast_plot/forecast_fatality_rates_onexrow.png"), units="in", width=28, height=34, res=300)
  print(p)
  dev.off()
  
  
  if(variants){
    load(paste0(dir_name, "/forecast_plot/RData/forecast_infection_rates_7_days.RData"))
    p1 <- plot 
    load(paste0(dir_name, "/forecast_plot/RData/forecast_infection_rates_14_days.RData"))
    p2 <- plot 
    load(paste0(dir_name, "/forecast_plot/RData/forecast_infection_rates_21_days.RData"))
    p3 <- plot 
    load(paste0(dir_name, "/forecast_plot/RData/forecast_infection_rates_28_days.RData"))
    p4 <- plot
    
    p <- (p1 + p2) / (p3 + p4) +
      plot_layout(guides = "collect", axis_titles = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(dir_name, "/forecast_plot/infection_rates/forecast_infection_rates.png"), units="in", width=34, height=15, res=300)
    print(p)
    dev.off()
    
    p <- p1 / p2 / p3 / p4 +
      plot_layout(guides = "collect", axis_titles = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(dir_name, "/forecast_plot/infection_rates/forecast_infection_rates_onexrow.png"), units="in", width=28, height=34, res=300)
    print(p)
    dev.off()
    
    
    for(k in 1:length(variants_name)){
      load(paste0(dir_name, "/forecast_plot/RData/forecast_I_", variants_name[k], "_7_days.RData"))
      p1 <- plot 
      load(paste0(dir_name, "/forecast_plot/RData/forecast_I_", variants_name[k], "_14_days.RData"))
      p2 <- plot 
      load(paste0(dir_name, "/forecast_plot/RData/forecast_I_", variants_name[k], "_21_days.RData"))
      p3 <- plot 
      load(paste0(dir_name, "/forecast_plot/RData/forecast_I_", variants_name[k], "_28_days.RData"))
      p4 <- plot 
      
      p <- (p1 + p2) / (p3 + p4) +
        plot_layout(guides = "collect", axis_titles = "collect") &
        theme(legend.position = "bottom", legend.box = "vertical")
      
      png(paste0(dir_name, "/forecast_plot/I/forecast_I_", variants_name[k], ".png"), units="in", width=34, height=15, res=300)
      print(p)
      dev.off()
      
      p <- p1 / p2 / p3 / p4 +
        plot_layout(guides = "collect", axis_titles = "collect") &
        theme(legend.position = "bottom", legend.box = "vertical")
      
      png(paste0(dir_name, "/forecast_plot/I/forecast_I_", variants_name[k], "_onexrow.png"), units="in", width=28, height=34, res=300)
      print(p)
      dev.off()
      
      
      load(paste0(dir_name, "/forecast_plot/RData/forecast_infection_rates_", variants_name[k], "_7_days.RData"))
      p1 <- plot 
      load(paste0(dir_name, "/forecast_plot/RData/forecast_infection_rates_", variants_name[k], "_14_days.RData"))
      p2 <- plot 
      load(paste0(dir_name, "/forecast_plot/RData/forecast_infection_rates_", variants_name[k], "_21_days.RData"))
      p3 <- plot 
      load(paste0(dir_name, "/forecast_plot/RData/forecast_infection_rates_", variants_name[k], "_28_days.RData"))
      p4 <- plot 
      
      p <- (p1 + p2) / (p3 + p4) +
        plot_layout(guides = "collect", axis_titles = "collect") &
        theme(legend.position = "bottom", legend.box = "vertical")
      
      png(paste0(dir_name, "/forecast_plot/infection_rates/forecast_infection_rates_", variants_name[k], ".png"), units="in", width=34, height=15, res=300)
      print(p)
      dev.off()
      
      p <- p1 / p2 / p3 / p4 +
        plot_layout(guides = "collect", axis_titles = "collect") &
        theme(legend.position = "bottom", legend.box = "vertical")
      
      png(paste0(dir_name, "/forecast_plot/infection_rates/forecast_infection_rates_", variants_name[k], "_onexrow.png"), units="in", width=28, height=34, res=300)
      print(p)
      dev.off()
      
      
      load(paste0(dir_name, "/forecast_plot/RData/I_", variants_name[k], "_compare_7_days.RData"))
      p1 <- plot 
      load(paste0(dir_name, "/forecast_plot/RData/I_", variants_name[k], "_compare_14_days.RData"))
      p2 <- plot 
      load(paste0(dir_name, "/forecast_plot/RData/I_", variants_name[k], "_compare_21_days.RData"))
      p3 <- plot 
      load(paste0(dir_name, "/forecast_plot/RData/I_", variants_name[k], "_compare_28_days.RData"))
      p4 <- plot 
      
      p <- (p1 + p2) / (p3 + p4) +
        plot_layout(guides = "collect", axis_titles = "collect") &
        theme(legend.position = "bottom", legend.box = "vertical")
      
      png(paste0(dir_name, "/forecast_plot/comparison/I_", variants_name[k], "_compare.png"), units="in", width=34, height=15, res=300)
      print(p)
      dev.off()
      
      p <- p1 / p2 / p3 / p4 +
        plot_layout(guides = "collect", axis_titles = "collect") &
        theme(legend.position = "bottom", legend.box = "vertical")
      
      png(paste0(dir_name, "/forecast_plot/comparison/I_", variants_name[k], "_compare_onexrow.png"), units="in", width=28, height=34, res=300)
      print(p)
      dev.off()
    }
  }
  else{
    load(paste0(dir_name, "/forecast_plot/RData/forecast_infection_rates_7_days.RData"))
    p1 <- plot 
    load(paste0(dir_name, "/forecast_plot/RData/forecast_infection_rates_14_days.RData"))
    p2 <- plot 
    load(paste0(dir_name, "/forecast_plot/RData/forecast_infection_rates_21_days.RData"))
    p3 <- plot 
    load(paste0(dir_name, "/forecast_plot/RData/forecast_infection_rates_28_days.RData"))
    p4 <- plot 
    
    p <- (p1 + p2) / (p3 + p4) +
      plot_layout(guides = "collect", axis_titles = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(dir_name, "/forecast_plot/infection_rates/forecast_infection_rates.png"), units="in", width=34, height=15, res=300)
    print(p)
    dev.off()
    
    p <- p1 / p2 / p3 / p4 +
      plot_layout(guides = "collect", axis_titles = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(dir_name, "/forecast_plot/infection_rates/forecast_infection_rates_onexrow.png"), units="in", width=28, height=34, res=300)
    print(p)
    dev.off()
    
    
    load(paste0(dir_name, "/forecast_plot/RData/forecast_I_7_days.RData"))
    p1 <- plot 
    load(paste0(dir_name, "/forecast_plot/RData/forecast_I_14_days.RData"))
    p2 <- plot 
    load(paste0(dir_name, "/forecast_plot/RData/forecast_I_21_days.RData"))
    p3 <- plot 
    load(paste0(dir_name, "/forecast_plot/RData/forecast_I_28_days.RData"))
    p4 <- plot 
    
    p <- (p1 + p2) / (p3 + p4) +
      plot_layout(guides = "collect", axis_titles = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(dir_name, "/forecast_plot/I/forecast_I.png"), units="in", width=34, height=15, res=300)
    print(p)
    dev.off()
    
    p <- p1 / p2 / p3 / p4 +
      plot_layout(guides = "collect", axis_titles = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(dir_name, "/forecast_plot/I/forecast_I_onexrow.png"), units="in", width=28, height=34, res=300)
    print(p)
    dev.off()
    
    
    load(paste0(dir_name, "/forecast_plot/RData/I_compare_7_days.RData"))
    p1 <- plot 
    load(paste0(dir_name, "/forecast_plot/RData/I_compare_14_days.RData"))
    p2 <- plot 
    load(paste0(dir_name, "/forecast_plot/RData/I_compare_21_days.RData"))
    p3 <- plot 
    load(paste0(dir_name, "/forecast_plot/RData/I_compare_28_days.RData"))
    p4 <- plot 
    
    p <- (p1 + p2) / (p3 + p4) +
      plot_layout(guides = "collect", axis_titles = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(dir_name, "/forecast_plot/comparison/I_compare.png"), units="in", width=34, height=15, res=300)
    print(p)
    dev.off()
    
    p <- p1 / p2 / p3 / p4 +
      plot_layout(guides = "collect", axis_titles = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(dir_name, "/forecast_plot/comparison/I_compare_onexrow.png"), units="in", width=28, height=34, res=300)
    print(p)
    dev.off()
  }
}