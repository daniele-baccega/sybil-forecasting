plots_first_ascending_scenario <- function(external_dir_names, country, daily_spline){
  type <- ""
  if(daily_spline)
    type <- "_DailySpline"
  
  for(i in 1:4){
    load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p1 <- plot + labs(title = expression("December 9"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p2 <- plot + labs(title = expression("December 12"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p3 <- plot + labs(title = expression("December 15"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p4 <- plot + labs(title = expression("December 18"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p5 <- plot + labs(title = expression("December 21"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p6 <- plot + labs(title = expression("December 24"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[7], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p7 <- plot + labs(title = expression("December 27"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[8], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p8 <- plot + labs(title = expression("December 30"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[9], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p9 <- plot + labs(title = expression("January 2"^"nd")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[10], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p10 <- plot + labs(title = expression("January 5"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[11], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p11 <- plot + labs(title = expression("January 8"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[12], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p12 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34))
    
    p <- (p1 + p2 + p3) / (p4 + p5 + p6) / (p7 + p8 + p9) / (p10 + p11 + p12) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(country, "/V4/ThirdScenario/ThirdScenario_", i*7, "days", type, ".png"), units="in", width=34, height=22, res=300)
    print(p)
    dev.off()
  }
  
  for(i in 1:4){
    load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
    p1 <- plot + labs(title = expression("December 9"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
    p2 <- plot + labs(title = expression("December 12"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
    p3 <- plot + labs(title = expression("December 15"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
    p4 <- plot + labs(title = expression("December 18"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
    p5 <- plot + labs(title = expression("December 21"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
    p6 <- plot + labs(title = expression("December 24"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[7], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
    p7 <- plot + labs(title = expression("December 27"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[8], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
    p8 <- plot + labs(title = expression("December 30"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[9], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
    p9 <- plot + labs(title = expression("January 2"^"nd")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[10], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
    p10 <- plot + labs(title = expression("January 5"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[11], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
    p11 <- plot + labs(title = expression("January 8"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[12], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
    p12 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34))
    
    p <- (p1 + p2 + p3) / (p4 + p5 + p6) / (p7 + p8 + p9) / (p10 + p11 + p12) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(country, "/V4/ThirdScenario/Omicron_infection_rates_evolution_", i*7, "days", type, ".png"), units="in", width=34, height=22, res=300)
    print(p)
    dev.off()
  }
}

plots_second_ascending_scenario <- function(external_dir_names, country, daily_spline){
  type <- ""
  if(daily_spline)
    type <- "_DailySpline"
  
  for(i in 1:4){
    load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p1 <- plot + labs(title = expression("February 15"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p2 <- plot + labs(title = expression("February 18"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p3 <- plot + labs(title = expression("February 21"^"st")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p4 <- plot + labs(title = expression("February 24"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p5 <- plot + labs(title = expression("February 27"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p6 <- plot + labs(title = expression("March 2"^"nd")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[7], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p7 <- plot + labs(title = expression("March 5"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[8], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p8 <- plot + labs(title = expression("March 8"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[9], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p9 <- plot + labs(title = expression("March 11"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[10], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p10 <- plot + labs(title = expression("March 14"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[11], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p11 <- plot + labs(title = expression("March 17"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[12], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p12 <- plot + labs(title = expression("March 20"^"th")) + theme(plot.title=element_text(size=34))
    
    p <- (p1 + p2 + p3) / (p4 + p5 + p6) / (p7 + p8 + p9) / (p10 + p11 + p12) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(country, "/V4/SecondAscendingScenario/SecondAscendingScenario_", i*7, "days", type, ".png"), units="in", width=34, height=22, res=300)
    print(p)
    dev.off()
  }
  
  for(i in 1:4){
    load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
    p1 <- plot + labs(title = expression("February 15"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
    p2 <- plot + labs(title = expression("February 18"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
    p3 <- plot + labs(title = expression("February 21"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
    p4 <- plot + labs(title = expression("February 24"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
    p5 <- plot + labs(title = expression("February 27"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
    p6 <- plot + labs(title = expression("March 2"^"nd")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[7], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
    p7 <- plot + labs(title = expression("March 5"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[8], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
    p8 <- plot + labs(title = expression("March 8"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[9], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
    p9 <- plot + labs(title = expression("March 11"^"nd")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[10], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
    p10 <- plot + labs(title = expression("March 14"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[11], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
    p11 <- plot + labs(title = expression("March 17"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[12], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
    p12 <- plot + labs(title = expression("March 20"^"th")) + theme(plot.title=element_text(size=34))
    
    p <- (p1 + p2 + p3) / (p4 + p5 + p6) / (p7 + p8 + p9) / (p10 + p11 + p12) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(country, "/V4/SecondAscendingScenario/Alpha_infection_rates_evolution_", i*7, "days", type, ".png"), units="in", width=34, height=22, res=300)
    print(p)
    dev.off()
  }
}

plots_v11_scenario <- function(external_dir_names, country, daily_spline){
  type <- ""
  if(daily_spline)
    type <- "_DailySpline"
  
  for(i in 1:4){
    load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p1 <- plot + labs(title = expression("January 10"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p2 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p3 <- plot + labs(title = expression("January 12"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p4 <- plot + labs(title = expression("January 13"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p5 <- plot + labs(title = expression("January 14"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p6 <- plot + labs(title = expression("January 15"^"th")) + theme(plot.title=element_text(size=34))
    
    p <- (p1 + p2 + p3) / (p4 + p5 + p6) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(country, "/V11/SIRD_forecast_evolution_", i*7, "days", type, ".png"), units="in", width=34, height=15, res=300)
    print(p)
    dev.off()
  }
  
  for(i in 1:4){
    load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
    p1 <- plot + labs(title = expression("January 10"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
    p2 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
    p3 <- plot + labs(title = expression("January 12"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
    p4 <- plot + labs(title = expression("January 13"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
    p5 <- plot + labs(title = expression("January 14"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
    p6 <- plot + labs(title = expression("January 15"^"th")) + theme(plot.title=element_text(size=34))
    
    p <- (p1 + p2 + p3) / (p4 + p5 + p6) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(country, "/V11/BA.1_infection_rates_evolution_", i*7, "days", type, ".png"), units="in", width=34, height=15, res=300)
    print(p)
    dev.off()
  }
  
  for(i in 1:4){
    load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_BA.2_", i*7, "_days.RData"))
    p1 <- plot + labs(title = expression("January 10"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_BA.2_", i*7, "_days.RData"))
    p2 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_BA.2_", i*7, "_days.RData"))
    p3 <- plot + labs(title = expression("January 12"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_BA.2_", i*7, "_days.RData"))
    p4 <- plot + labs(title = expression("January 13"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_BA.2_", i*7, "_days.RData"))
    p5 <- plot + labs(title = expression("January 14"^"th")) + theme(plot.title=element_text(size=34))
    load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection_MCMC1000", type, "/forecast_plot/RData/forecast_infection_rates_BA.2_", i*7, "_days.RData"))
    p6 <- plot + labs(title = expression("January 15"^"th")) + theme(plot.title=element_text(size=34))
    
    p <- (p1 + p2 + p3) / (p4 + p5 + p6) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(country, "/V11/BA.2_infection_rates_evolution_", i*7, "days", type, ".png"), units="in", width=34, height=15, res=300)
    print(p)
    dev.off()
  }
}