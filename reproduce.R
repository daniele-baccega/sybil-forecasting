# Prophet with Covid-19 data (paper's results)
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


# Include the forecast function and the common functions
source("forecast.R")
source("plots.R")
source("common_functions.R")

Sys.setlocale("LC_TIME", "en_US.UTF-8")


# Select the configuration (with or without variants)
# To reproduce the results set the variable to TRUE
variants <- TRUE

# Global final date
global_final_date <- Sys.Date()

# Reproduce the results in the paper set the variable to TRUE
reproduce <- TRUE

if(reproduce){
  global_final_date <- "2023-06-04"
}


# Scenarios with Italy
country <- "Italy"

if(!file.exists(country)){
  system(paste0("mkdir -p ", country))
  system(paste0("mkdir -p ", country, "/V4"))
  system(paste0("mkdir -p ", country, "/V4/FirstAscendingScenario"))
  system(paste0("mkdir -p ", country, "/V4/SecondAscendingScenario"))
  system(paste0("mkdir -p ", country, "/V11"))
}

variants_to_disregard <- c("AY.4.2", "B.1.1.529", "B.1.1.7+E484K", "B.1.617.3", "BA.2+L452X", "BA.3", "BA.4/BA.5", "SGTF", "UNK")
variants_aggregated <- list(Alpha = c("B.1.1.7"),
                            Delta = c("B.1.617.2"),
                            Omicron = c("BA.1", "BA.2", "BA.2.75", "BA.4", "BA.5", "BQ.1", "XBB", "XBB.1.5", "XBB.1.5+F456L"),
                            Other = c("B.1.351", "B.1.427/B.1.429", "B.1.525", "B.1.616", "B.1.617.1", "B.1.620", "B.1.621", "C.37", "P.1", "P.3", "B.1.427-B.1.429", "Other"))
variants_aggregated_names <- list("Alpha", "Delta", "Omicron", "Other")

# Five scenarios on Italy
external_dir_names <- c(paste0(country, "/V4/FirstScenario/"),
                        paste0(country, "/V4/SecondScenario/"),
                        paste0(country, "/V4/ThirdScenario/"),
                        paste0(country, "/V4/FourthScenario/"),
                        paste0(country, "/V4/FifthScenario/"))

initial_dates <- c(as.Date("2020-03-14"),
                   as.Date("2021-12-13"),
                   as.Date("2022-06-14"),
                   as.Date("2020-05-30"),
                   as.Date("2023-01-07"))

final_dates <- c(as.Date("2020-04-14"),
                 as.Date("2022-01-13"),
                 as.Date("2022-07-14"),
                 as.Date("2020-06-30"),
                 as.Date("2023-02-07"))

forecast(variants, global_final_date, country, external_dir_names, initial_dates, final_dates, variants_to_disregard, variants_aggregated, variants_aggregated_names, reproduce)


# First ascending scenario
external_dir_names <- c(paste0(country, "/V4/FirstAscendingScenario/February15-2021/"),
                        paste0(country, "/V4/FirstAscendingScenario/February18-2021/"),
                        paste0(country, "/V4/FirstAscendingScenario/February21-2021/"),
                        paste0(country, "/V4/FirstAscendingScenario/February24-2021/"),
                        paste0(country, "/V4/FirstAscendingScenario/February27-2021/"),
                        paste0(country, "/V4/FirstAscendingScenario/March2-2021/"),
                        paste0(country, "/V4/FirstAscendingScenario/March5-2021/"),
                        paste0(country, "/V4/FirstAscendingScenario/March8-2021/"),
                        paste0(country, "/V4/FirstAscendingScenario/March11-2021/"),
                        paste0(country, "/V4/FirstAscendingScenario/March14-2021/"),
                        paste0(country, "/V4/FirstAscendingScenario/March17-2021/"),
                        paste0(country, "/V4/FirstAscendingScenario/March20-2021/"))

initial_dates <- c(as.Date("2021-01-15"),
                   as.Date("2021-01-18"),
                   as.Date("2021-01-21"),
                   as.Date("2021-01-24"),
                   as.Date("2021-01-27"),
                   as.Date("2021-01-30"),
                   as.Date("2021-02-02"),
                   as.Date("2021-02-05"),
                   as.Date("2021-02-08"),
                   as.Date("2021-02-11"),
                   as.Date("2021-02-14"),
                   as.Date("2021-02-17"))

final_dates <- c(as.Date("2021-02-15"),
                 as.Date("2021-02-18"),
                 as.Date("2021-02-21"),
                 as.Date("2021-02-24"),
                 as.Date("2021-02-27"),
                 as.Date("2021-03-02"),
                 as.Date("2021-03-05"),
                 as.Date("2021-03-08"),
                 as.Date("2021-03-11"),
                 as.Date("2021-03-14"),
                 as.Date("2021-03-17"),
                 as.Date("2021-03-20"))

forecast(variants, global_final_date, country, external_dir_names, initial_dates, final_dates, variants_to_disregard, variants_aggregated, variants_aggregated_names, reproduce)

for(i in 1:4){
  load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p1 <- plot + labs(title = expression("February 15"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p2 <- plot + labs(title = expression("February 18"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p3 <- plot + labs(title = expression("February 21"^"st")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p4 <- plot + labs(title = expression("February 24"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p5 <- plot + labs(title = expression("February 27"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p6 <- plot + labs(title = expression("March 2"^"nd")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[7], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p7 <- plot + labs(title = expression("March 5"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[8], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p8 <- plot + labs(title = expression("March 8"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[9], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p9 <- plot + labs(title = expression("March 11"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[10], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p10 <- plot + labs(title = expression("March 14"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[11], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p11 <- plot + labs(title = expression("March 17"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[12], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p12 <- plot + labs(title = expression("March 20"^"th")) + theme(plot.title=element_text(size=34))
  
  p <- (p1 + p2 + p3) / (p4 + p5 + p6) / (p7 + p8 + p9) / (p10 + p11 + p12) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png(paste0(country, "/V4/FirstAscendingScenario/FirstAscendingScenario_", i*7, "days.png"), units="in", width=34, height=22, res=300)
  print(p)
  dev.off()
}

for(i in 1:4){
  load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
  p1 <- plot + labs(title = expression("February 15"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
  p2 <- plot + labs(title = expression("February 18"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
  p3 <- plot + labs(title = expression("February 21"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
  p4 <- plot + labs(title = expression("February 24"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
  p5 <- plot + labs(title = expression("February 27"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
  p6 <- plot + labs(title = expression("March 2"^"nd")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[7], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
  p7 <- plot + labs(title = expression("March 5"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[8], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
  p8 <- plot + labs(title = expression("March 8"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[9], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
  p9 <- plot + labs(title = expression("March 11"^"nd")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[10], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
  p10 <- plot + labs(title = expression("March 14"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[11], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
  p11 <- plot + labs(title = expression("March 17"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[12], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Alpha_", i*7, "_days.RData"))
  p12 <- plot + labs(title = expression("March 20"^"th")) + theme(plot.title=element_text(size=34))
  
  p <- (p1 + p2 + p3) / (p4 + p5 + p6) / (p7 + p8 + p9) / (p10 + p11 + p12) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png(paste0(country, "/V4/FirstAscendingScenario/Alpha_infection_rates_evolution_", i*7, "days.png"), units="in", width=34, height=22, res=300)
  print(p)
  dev.off()
}


# Second ascending scenario
external_dir_names <- c(paste0(country, "/V4/SecondAscendingScenario/December9-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/December12-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/December15-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/December18-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/December21-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/December24-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/December27-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/December30-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/January2-2022/"),
                        paste0(country, "/V4/SecondAscendingScenario/January5-2022/"),
                        paste0(country, "/V4/SecondAscendingScenario/January8-2022/"),
                        paste0(country, "/V4/SecondAscendingScenario/January11-2022/"))

initial_dates <- c(as.Date("2021-11-09"),
                   as.Date("2021-11-12"),
                   as.Date("2021-11-15"),
                   as.Date("2021-11-18"),
                   as.Date("2021-11-21"),
                   as.Date("2021-11-24"),
                   as.Date("2021-11-27"),
                   as.Date("2021-11-30"),
                   as.Date("2021-12-02"),
                   as.Date("2021-12-05"),
                   as.Date("2021-12-08"),
                   as.Date("2021-12-11"))

final_dates <- c(as.Date("2021-12-09"),
                 as.Date("2021-12-12"),
                 as.Date("2021-12-15"),
                 as.Date("2021-12-18"),
                 as.Date("2021-12-21"),
                 as.Date("2021-12-24"),
                 as.Date("2021-12-27"),
                 as.Date("2021-12-30"),
                 as.Date("2022-01-02"),
                 as.Date("2022-01-05"),
                 as.Date("2022-01-08"),
                 as.Date("2022-01-11"))

forecast(variants, global_final_date, country, external_dir_names, initial_dates, final_dates, variants_to_disregard, variants_aggregated, variants_aggregated_names, reproduce)

for(i in 1:4){
  load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p1 <- plot + labs(title = expression("December 9"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p2 <- plot + labs(title = expression("December 12"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p3 <- plot + labs(title = expression("December 15"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p4 <- plot + labs(title = expression("December 18"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p5 <- plot + labs(title = expression("December 21"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p6 <- plot + labs(title = expression("December 24"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[7], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p7 <- plot + labs(title = expression("December 27"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[8], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p8 <- plot + labs(title = expression("December 30"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[9], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p9 <- plot + labs(title = expression("January 2"^"nd")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[10], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p10 <- plot + labs(title = expression("January 5"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[11], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p11 <- plot + labs(title = expression("January 8"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[12], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p12 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34))
  
  p <- (p1 + p2 + p3) / (p4 + p5 + p6) / (p7 + p8 + p9) / (p10 + p11 + p12) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png(paste0(country, "/V4/SecondAscendingScenario/SecondAscendingScenario_", i*7, "days.png"), units="in", width=34, height=22, res=300)
  print(p)
  dev.off()
}

for(i in 1:4){
  load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
  p1 <- plot + labs(title = expression("December 9"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
  p2 <- plot + labs(title = expression("December 12"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
  p3 <- plot + labs(title = expression("December 15"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
  p4 <- plot + labs(title = expression("December 18"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
  p5 <- plot + labs(title = expression("December 21"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
  p6 <- plot + labs(title = expression("December 24"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[7], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
  p7 <- plot + labs(title = expression("December 27"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[8], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
  p8 <- plot + labs(title = expression("December 30"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[9], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
  p9 <- plot + labs(title = expression("January 2"^"nd")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[10], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
  p10 <- plot + labs(title = expression("January 5"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[11], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
  p11 <- plot + labs(title = expression("January 8"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[12], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_Omicron_", i*7, "_days.RData"))
  p12 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34))
  
  p <- (p1 + p2 + p3) / (p4 + p5 + p6) / (p7 + p8 + p9) / (p10 + p11 + p12) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png(paste0(country, "/V4/SecondAscendingScenario/Omicron_infection_rates_evolution_", i*7, "days.png"), units="in", width=34, height=22, res=300)
  print(p)
  dev.off()
}


# Select the variants to disregard and to aggregate
variants_to_disregard <- c("AY.4.2", "B.1.1.529", "B.1.1.7+E484K", "B.1.427/B.1.429", "B.1.616", "B.1.617.3", "BA.2+L452X", "BA.3", "BA.4/BA.5", "C.37", "P.3", "SGTF", "UNK")
variants_aggregated <- list(B_1 = c("B.1.1.7", "B.1.351", "B.1.525"),
                            B_1_6xx = c("B.1.617.1", "B.1.617.2", "B.1.620", "B.1.621"),
                            XBB = c("XBB", "XBB.1.5", "XBB.1.5+F456L"))
variants_aggregated_names <- list("B1", "B.1.6xx", "XBB")

# Scenarios with the movements of the training window
external_dir_names <- c(paste0(country, "/V11/ProphetJanuary-10-2022/"),
                        paste0(country, "/V11/ProphetJanuary-11-2022/"),
                        paste0(country, "/V11/ProphetJanuary-12-2022/"),
                        paste0(country, "/V11/ProphetJanuary-13-2022/"),
                        paste0(country, "/V11/ProphetJanuary-14-2022/"),
                        paste0(country, "/V11/ProphetJanuary-15-2022/"),
                        paste0(country, "/V11/ProphetJanuary-22-2022/"))

initial_dates <- c(as.Date("2021-12-10"),
                   as.Date("2021-12-11"),
                   as.Date("2021-12-12"),
                   as.Date("2021-12-13"),
                   as.Date("2021-12-14"),
                   as.Date("2021-12-15"),
                   as.Date("2021-12-22"))

final_dates <- c(as.Date("2022-01-10"),
                 as.Date("2022-01-11"),
                 as.Date("2022-01-12"),
                 as.Date("2022-01-13"),
                 as.Date("2022-01-14"),
                 as.Date("2022-01-15"),
                 as.Date("2022-01-22"))

forecast(variants, global_final_date, country, external_dir_names, initial_dates, final_dates, variants_to_disregard, variants_aggregated, variants_aggregated_names, reproduce)

for(i in 1:4){
  load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p1 <- plot + labs(title = expression("January 10"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p2 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p3 <- plot + labs(title = expression("January 12"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p4 <- plot + labs(title = expression("January 13"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p5 <- plot + labs(title = expression("January 14"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_", i*7, "_days.RData"))
  p6 <- plot + labs(title = expression("January 15"^"th")) + theme(plot.title=element_text(size=34))
  
  p <- (p1 + p2 + p3) / (p4 + p5 + p6) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png(paste0(country, "/V11/SIRD_forecast_evolution_", i*7, "_days.png"), units="in", width=34, height=15, res=300)
  print(p)
  dev.off()
}

for(i in 1:4){
  load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
  p1 <- plot + labs(title = expression("January 10"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
  p2 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
  p3 <- plot + labs(title = expression("January 12"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
  p4 <- plot + labs(title = expression("January 13"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
  p5 <- plot + labs(title = expression("January 14"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.1_", i*7, "_days.RData"))
  p6 <- plot + labs(title = expression("January 15"^"th")) + theme(plot.title=element_text(size=34))
  
  p <- (p1 + p2 + p3) / (p4 + p5 + p6) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png(paste0(country, "/V11/BA.1_infection_rates_evolution_", i*7, "_.png"), units="in", width=34, height=15, res=300)
  print(p)
  dev.off()
}

for(i in 1:4){
  load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.2_", i*7, "_days.RData"))
  p1 <- plot + labs(title = expression("January 10"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.2_", i*7, "_days.RData"))
  p2 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.2_", i*7, "_days.RData"))
  p3 <- plot + labs(title = expression("January 12"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.2_", i*7, "_days.RData"))
  p4 <- plot + labs(title = expression("January 13"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.2_", i*7, "_days.RData"))
  p5 <- plot + labs(title = expression("January 14"^"th")) + theme(plot.title=element_text(size=34))
  load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.2_", i*7, "_days.RData"))
  p6 <- plot + labs(title = expression("January 15"^"th")) + theme(plot.title=element_text(size=34))
  
  p <- (p1 + p2 + p3) / (p4 + p5 + p6) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png(paste0(country, "/V11/BA.2_infection_rates_evolution_", i*7, "_.png"), units="in", width=34, height=15, res=300)
  print(p)
  dev.off()
}



# Scenarios with Austria
country <- "Austria"

if(!file.exists(country)){
  system(paste0("mkdir -p ", country))
  system(paste0("mkdir -p ", country, "/V4"))
}

variants_to_disregard <- c("AY.4.2", "B.1.1.529", "B.1.1.7+E484K", "B.1.617.3", "BA.2+L452X", "BA.3", "BA.4/BA.5", "SGTF", "UNK")
variants_aggregated <- list(Alpha = c("B.1.1.7"),
                            Delta = c("B.1.617.2"),
                            Omicron = c("BA.1", "BA.2", "BA.2.75", "BA.4", "BA.5", "BQ.1", "XBB", "XBB.1.5", "XBB.1.5+F456L"),
                            Other = c("B.1.351", "B.1.427/B.1.429", "B.1.525", "B.1.616", "B.1.617.1", "B.1.620", "B.1.621", "C.37", "P.1", "P.3", "B.1.427-B.1.429", "Other"))
variants_aggregated_names <- list("Alpha", "Delta", "Omicron", "Other")

external_dir_names <- c(paste0(country, "/V4/FirstScenario/"),
                        paste0(country, "/V4/SecondScenario/"),
                        paste0(country, "/V4/ThirdScenario/"),
                        paste0(country, "/V4/FourthScenario/"))

initial_dates <- c(as.Date("2022-01-01"),
                   as.Date("2020-03-03"),
                   as.Date("2022-06-14"),
                   as.Date("2020-05-10"))

final_dates <- c(as.Date("2022-02-01"),
                 as.Date("2020-04-03"),
                 as.Date("2022-07-14"),
                 as.Date("2020-06-10"))

forecast(variants, global_final_date, country, external_dir_names, initial_dates, final_dates, variants_to_disregard, variants_aggregated, variants_aggregated_names, reproduce)