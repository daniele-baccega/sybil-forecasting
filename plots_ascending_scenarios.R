plots_first_ascending_scenario <- function(external_dir_names, country, variants){
  type_variants <- ""
  if(variants)
    type_variants <- "_Variants"
  
  for(i in 1:4){
    load(paste0(external_dir_names[1], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p1 <- plot + labs(title = expression("December 9"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[2], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p2 <- plot + labs(title = expression("December 12"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[3], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p3 <- plot + labs(title = expression("December 15"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[4], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p4 <- plot + labs(title = expression("December 18"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[5], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p5 <- plot + labs(title = expression("December 21"^"st")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[6], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p6 <- plot + labs(title = expression("December 24"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[7], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p7 <- plot + labs(title = expression("December 27"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[8], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p8 <- plot + labs(title = expression("December 30"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[9], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p9 <- plot + labs(title = expression("January 2"^"nd")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[10], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p10 <- plot + labs(title = expression("January 5"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[11], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p11 <- plot + labs(title = expression("January 8"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[12], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p12 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    
    p <- (p1 + p2) / (p3 + p4) / (p5 + p6) / (p7 + p8) / (p9 + p10) / (p11 + p12) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(country, "/V4/ThirdScenario/ThirdScenario_", i*7, "days", type_variants, ".png"), units="in", width=34, height=22, res=300)
    print(p)
    dev.off()
    
    if(i == 1){
      p <- p4 / p7 +
        plot_layout(guides = "collect", axis_titles = "collect") &
        theme(legend.position = "bottom", legend.box = "vertical")
      
      png(paste0(country, "/V4/ThirdScenario/ThirdScenario_December18-27_", i*7, "days.png"), units="in", width=28, height=26, res=300)
      print(p)
      dev.off()
    }
  }
  
  for(i in 1:4){
    load(paste0(external_dir_names[1], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Omicron_" else "", "", i*7, "_days.RData"))
    p1 <- plot + labs(title = expression("December 9"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[2], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Omicron_" else "", "", i*7, "_days.RData"))
    p2 <- plot + labs(title = expression("December 12"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[3], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Omicron_" else "", "", i*7, "_days.RData"))
    p3 <- plot + labs(title = expression("December 15"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[4], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Omicron_" else "", "", i*7, "_days.RData"))
    p4 <- plot + labs(title = expression("December 18"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[5], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Omicron_" else "", "", i*7, "_days.RData"))
    p5 <- plot + labs(title = expression("December 21"^"st")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[6], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Omicron_" else "", "", i*7, "_days.RData"))
    p6 <- plot + labs(title = expression("December 24"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[7], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Omicron_" else "", "", i*7, "_days.RData"))
    p7 <- plot + labs(title = expression("December 27"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[8], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Omicron_" else "", "", i*7, "_days.RData"))
    p8 <- plot + labs(title = expression("December 30"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[9], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Omicron_" else "", "", i*7, "_days.RData"))
    p9 <- plot + labs(title = expression("January 2"^"nd")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[10], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Omicron_" else "", "", i*7, "_days.RData"))
    p10 <- plot + labs(title = expression("January 5"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[11], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Omicron_" else "", "", i*7, "_days.RData"))
    p11 <- plot + labs(title = expression("January 8"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[12], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Omicron_" else "", "", i*7, "_days.RData"))
    p12 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    
    p <- (p1 + p2) / (p3 + p4) / (p5 + p6) / (p7 + p8) / (p9 + p10) / (p11 + p12) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(country, "/V4/ThirdScenario/", if(variants) "Omicron_" else "", "infection_rates_evolution_", i*7, "days", type_variants, ".png"), units="in", width=34, height=22, res=300)
    print(p)
    dev.off()
  }
}

plots_second_ascending_scenario <- function(external_dir_names, country, variants){
  type_variants <- ""
  if(variants)
    type_variants <- "_Variants"
  
  for(i in 1:4){
    load(paste0(external_dir_names[1], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p1 <- plot + labs(title = expression("February 15"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[2], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p2 <- plot + labs(title = expression("February 18"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[3], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p3 <- plot + labs(title = expression("February 21"^"st")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[4], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p4 <- plot + labs(title = expression("February 24"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[5], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p5 <- plot + labs(title = expression("February 27"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[6], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p6 <- plot + labs(title = expression("March 2"^"nd")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[7], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p7 <- plot + labs(title = expression("March 5"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[8], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p8 <- plot + labs(title = expression("March 8"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[9], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p9 <- plot + labs(title = expression("March 11"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[10], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p10 <- plot + labs(title = expression("March 14"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[11], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p11 <- plot + labs(title = expression("March 17"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[12], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p12 <- plot + labs(title = expression("March 20"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    
    p <- (p1 + p2) / (p3 + p4) / (p5 + p6) / (p7 + p8) / (p9 + p10) / (p11 + p12) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
  
    png(paste0(country, "/V4/SecondAscendingScenario/SecondAscendingScenario_", i*7, "days", type_variants, ".png"), units="in", width=34, height=22, res=300)
    print(p)
    dev.off()
  }
  
  for(i in 1:4){
    load(paste0(external_dir_names[1], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Alpha_" else "", "", i*7, "_days.RData"))
    p1 <- plot + labs(title = expression("February 15"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[2], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Alpha_" else "", "", i*7, "_days.RData"))
    p2 <- plot + labs(title = expression("February 18"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[3], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Alpha_" else "", "", i*7, "_days.RData"))
    p3 <- plot + labs(title = expression("February 21"^"st")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[4], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Alpha_" else "", "", i*7, "_days.RData"))
    p4 <- plot + labs(title = expression("February 24"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[5], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Alpha_" else "", "", i*7, "_days.RData"))
    p5 <- plot + labs(title = expression("February 27"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[6], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Alpha_" else "", "", i*7, "_days.RData"))
    p6 <- plot + labs(title = expression("March 2"^"nd")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[7], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Alpha_" else "", "", i*7, "_days.RData"))
    p7 <- plot + labs(title = expression("March 5"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[8], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Alpha_" else "", "", i*7, "_days.RData"))
    p8 <- plot + labs(title = expression("March 8"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[9], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Alpha_" else "", "", i*7, "_days.RData"))
    p9 <- plot + labs(title = expression("March 11"^"nd")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[10], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Alpha_" else "", "", i*7, "_days.RData"))
    p10 <- plot + labs(title = expression("March 14"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[11], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Alpha_" else "", "", i*7, "_days.RData"))
    p11 <- plot + labs(title = expression("March 17"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[12], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "Alpha_" else "", "", i*7, "_days.RData"))
    p12 <- plot + labs(title = expression("March 20"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    
    p <- (p1 + p2) / (p3 + p4) / (p5 + p6) / (p7 + p8) / (p9 + p10) / (p11 + p12) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(country, "/V4/SecondAscendingScenario/", if(variants) "Alpha_" else "", "infection_rates_evolution_", i*7, "days", type_variants, ".png"), units="in", width=34, height=22, res=300)
    print(p)
    dev.off()
  }
}

plots_v11_scenario <- function(external_dir_names, country, variants){
  type_variants <- ""
  if(variants)
    type_variants <- "_Variants"
  
  for(i in 1:4){
    load(paste0(external_dir_names[1], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p1 <- plot + labs(title = expression("January 10"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[2], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p2 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[3], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p3 <- plot + labs(title = expression("January 12"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[4], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p4 <- plot + labs(title = expression("January 13"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[5], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p5 <- plot + labs(title = expression("January 14"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[6], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
    p6 <- plot + labs(title = expression("January 15"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    
    p <- (p1 + p2) / (p3 + p4) / (p5 + p6) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(country, "/V11/SIRD_forecast_evolution_", i*7, "days", type_variants, ".png"), units="in", width=34, height=15, res=300)
    print(p)
    dev.off()
  }
  
  for(i in 1:4){
    load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
    p1 <- plot + labs(title = expression("January 10"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
    p2 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
    p3 <- plot + labs(title = expression("January 12"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
    p4 <- plot + labs(title = expression("January 13"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
    p5 <- plot + labs(title = expression("January 14"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
    p6 <- plot + labs(title = expression("January 15"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    
    p <- (p1 + p2) / (p3 + p4) / (p5 + p6) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(country, "/V11/BA.1_infection_rates_evolution_", i*7, "_days.png"), units="in", width=28, height=26, res=300)
    print(p)
    dev.off()
  }
  
  for(i in 1:4){
    load(paste0(external_dir_names[1], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "BA.1_" else "", "", i*7, "_days.RData"))
    p1 <- plot + labs(title = expression("January 10"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[2], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "BA.1_" else "", "", i*7, "_days.RData"))
    p2 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[3], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "BA.1_" else "", "", i*7, "_days.RData"))
    p3 <- plot + labs(title = expression("January 12"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[4], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "BA.1_" else "", "", i*7, "_days.RData"))
    p4 <- plot + labs(title = expression("January 13"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[5], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "BA.1_" else "", "", i*7, "_days.RData"))
    p5 <- plot + labs(title = expression("January 14"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    load(paste0(external_dir_names[6], "SIRD_", if(variants) "Variants_" else "", "Reinfection/forecast_plot/RData/forecast_infection_rates_", if(variants) "BA.1_" else "", "", i*7, "_days.RData"))
    p6 <- plot + labs(title = expression("January 15"^"th")) + theme(plot.title=element_text(size=34), axis.title.x = element_blank(), axis.title.y = element_blank())
    
    p <- (p1 + p2) / (p3 + p4) / (p5 + p6) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(country, "/V11/", if(variants) "BA.2_" else "", "infection_rates_evolution_", i*7, "days", type_variants, ".png"), units="in", width=34, height=15, res=300)
    print(p)
    dev.off()
  }
}