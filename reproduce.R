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
  system(paste0("mkdir ", country))
}

variants_to_disregard <- c("AY.4.2", "B.1.1.529", "B.1.1.7+E484K", "B.1.617.3", "BA.2+L452X", "BA.3", "BA.4/BA.5", "SGTF", "UNK")
variants_aggregated <- list(Alpha = c("B.1.1.7"),
                            Delta = c("B.1.617.2"),
                            Omicron = c("BA.1", "BA.2", "BA.2.75", "BA.4", "BA.5", "BQ.1", "XBB", "XBB.1.5", "XBB.1.5+F456L"),
                            Other = c("B.1.351", "B.1.427/B.1.429", "B.1.525", "B.1.616", "B.1.617.1", "B.1.620", "B.1.621", "C.37", "P.1", "P.3", "B.1.427-B.1.429", "Other"))
variants_aggregated_names <- list("Alpha", "Delta", "Omicron", "Other")

# Five scenarios on Italy
external_dir_names <- c(paste0(country, "/FirstScenario/"),
                        paste0(country, "/SecondScenario/"),
                        paste0(country, "/ThirdScenario/"),
                        paste0(country, "/FourthScenario/"),
                        paste0(country, "/FifthScenario/"))

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




# Select the variants to disregard and to aggregate
variants_to_disregard <- c("AY.4.2", "B.1.1.529", "B.1.1.7+E484K", "B.1.427/B.1.429", "B.1.616", "B.1.617.3", "BA.2+L452X", "BA.3", "BA.4/BA.5", "C.37", "P.3", "SGTF", "UNK")
variants_aggregated <- list(B_1 = c("B.1.1.7", "B.1.351", "B.1.525"),
                            B_1_6xx = c("B.1.617.1", "B.1.617.2", "B.1.620", "B.1.621"),
                            XBB = c("XBB", "XBB.1.5", "XBB.1.5+F456L"))
variants_aggregated_names <- list("B1", "B.1.6xx", "XBB")

# Scenarios with the movements of the training window
external_dir_names <- c(paste0(country, "/ProphetJanuary-10-2022/"),
                        paste0(country, "/ProphetJanuary-11-2022/"),
                        paste0(country, "/ProphetJanuary-12-2022/"),
                        paste0(country, "/ProphetJanuary-13-2022/"),
                        paste0(country, "/ProphetJanuary-14-2022/"),
                        paste0(country, "/ProphetJanuary-15-2022/"),
                        paste0(country, "/ProphetJanuary-22-2022/"))

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

load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_28_days.RData"))
p1 <- plot + labs(title = expression("January 10"^"th")) + theme(plot.title=element_text(size=34))
load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_28_days.RData"))
p2 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34))
load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_28_days.RData"))
p3 <- plot + labs(title = expression("January 12"^"th")) + theme(plot.title=element_text(size=34))
load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_28_days.RData"))
p4 <- plot + labs(title = expression("January 13"^"th")) + theme(plot.title=element_text(size=34))
load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_28_days.RData"))
p5 <- plot + labs(title = expression("January 14"^"th")) + theme(plot.title=element_text(size=34))
load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/SIRD_forecast_28_days.RData"))
p6 <- plot + labs(title = expression("January 15"^"th")) + theme(plot.title=element_text(size=34))

p <- (p1 + p2 + p3) / (p4 + p5 + p6) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.box = "vertical")

png(paste0(country, "/SIRD_forecast_evolution.png"), units="in", width=34, height=15, res=300)
print(p)
dev.off()


load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.1_28_days.RData"))
p1 <- plot + labs(title = expression("January 10"^"th")) + theme(plot.title=element_text(size=34))
load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.1_28_days.RData"))
p2 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34))
load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.1_28_days.RData"))
p3 <- plot + labs(title = expression("January 12"^"th")) + theme(plot.title=element_text(size=34))
load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.1_28_days.RData"))
p4 <- plot + labs(title = expression("January 13"^"th")) + theme(plot.title=element_text(size=34))
load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.1_28_days.RData"))
p5 <- plot + labs(title = expression("January 14"^"th")) + theme(plot.title=element_text(size=34))
load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.1_28_days.RData"))
p6 <- plot + labs(title = expression("January 15"^"th")) + theme(plot.title=element_text(size=34))

p <- (p1 + p2 + p3) / (p4 + p5 + p6) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.box = "vertical")

png(paste0(country, "/BA.1_infection_rates_evolution.png"), units="in", width=34, height=15, res=300)
print(p)
dev.off()


load(paste0(external_dir_names[1], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.2_28_days.RData"))
p1 <- plot + labs(title = expression("January 10"^"th")) + theme(plot.title=element_text(size=34))
load(paste0(external_dir_names[2], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.2_28_days.RData"))
p2 <- plot + labs(title = expression("January 11"^"th")) + theme(plot.title=element_text(size=34))
load(paste0(external_dir_names[3], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.2_28_days.RData"))
p3 <- plot + labs(title = expression("January 12"^"th")) + theme(plot.title=element_text(size=34))
load(paste0(external_dir_names[4], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.2_28_days.RData"))
p4 <- plot + labs(title = expression("January 13"^"th")) + theme(plot.title=element_text(size=34))
load(paste0(external_dir_names[5], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.2_28_days.RData"))
p5 <- plot + labs(title = expression("January 14"^"th")) + theme(plot.title=element_text(size=34))
load(paste0(external_dir_names[6], "SIRD_Variants_Reinfection_MCMC1000/OneMonth/forecast_plot/RData/forecast_infection_rates_BA.2_28_days.RData"))
p6 <- plot + labs(title = expression("January 15"^"th")) + theme(plot.title=element_text(size=34))

p <- (p1 + p2 + p3) / (p4 + p5 + p6) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.box = "vertical")

png(paste0(country, "/BA.2_infection_rates_evolution.png"), units="in", width=34, height=15, res=300)
print(p)
dev.off()



# Scenarios with Austria
country <- "Austria"

if(!file.exists(country)){
  system(paste0("mkdir ", country))
}

variants_to_disregard <- c("AY.4.2", "B.1.1.529", "B.1.1.7+E484K", "B.1.617.3", "BA.2+L452X", "BA.3", "BA.4/BA.5", "SGTF", "UNK")
variants_aggregated <- list(Alpha = c("B.1.1.7"),
                            Delta = c("B.1.617.2"),
                            Omicron = c("BA.1", "BA.2", "BA.2.75", "BA.4", "BA.5", "BQ.1", "XBB", "XBB.1.5", "XBB.1.5+F456L"),
                            Other = c("B.1.351", "B.1.427/B.1.429", "B.1.525", "B.1.616", "B.1.617.1", "B.1.620", "B.1.621", "C.37", "P.1", "P.3", "B.1.427-B.1.429", "Other"))
variants_aggregated_names <- list("Alpha", "Delta", "Omicron", "Other")

external_dir_names <- c(paste0(country, "/FirstScenario/"),
                        paste0(country, "/SecondScenario/"),
                        paste0(country, "/ThirdScenario/"),
                        paste0(country, "/FourthScenario/"))

initial_dates <- c(as.Date("2022-01-01"),
                   as.Date("2020-03-03"),
                   as.Date("2022-06-14"),
                   as.Date("2020-05-10"))

final_dates <- c(as.Date("2022-02-01"),
                 as.Date("2020-04-03"),
                 as.Date("2022-07-14"),
                 as.Date("2020-06-10"))

forecast(variants, global_final_date, country, external_dir_names, initial_dates, final_dates, variants_to_disregard, variants_aggregated, variants_aggregated_names, reproduce)
