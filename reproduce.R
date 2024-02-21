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

# Include the Sybil function and the common functions
source("Sybil.R")
source("common_functions.R")
source("plots.R")
source("plots_ascending_scenarios.R")

Sys.setlocale("LC_TIME", "en_US.UTF-8")


# Select the configuration (with or without variants, daily or daily-spline data)
# If in a particular country data are weekly you have to use daily spline data.
daily_spline <- FALSE
variants <- TRUE

# Forecast or only extract the rates?
forecast <- TRUE

# Initialize some variables
immunization_end_rate <- 1 / 180
recovery_rate <- 1 / 14

# Global final date
global_final_date <- Sys.Date()

# Reproduce the results in the paper set the variable to TRUE
reproduce <- TRUE

if(reproduce){
  global_final_date <- as.Date("2023-06-04")
}


# Scenarios with Italy (V=4)
country <- "Italy"

if(!file.exists(country)){
  system(paste0("mkdir -p ", country))
  system(paste0("mkdir -p ", country, "/V4"))
  system(paste0("mkdir -p ", country, "/V4/SecondAscendingScenario"))
  system(paste0("mkdir -p ", country, "/V11"))
}

# Aggregate variants
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
                        paste0(country, "/V4/FifthScenario/"),
                        paste0(country, "/V4/SixthScenario/"))

initial_dates <- c(as.Date("2020-03-14"),
                   as.Date("2021-12-13"),
                   as.Date("2021-11-09"),
                   as.Date("2022-06-14"),
                   as.Date("2020-05-30"),
                   as.Date("2023-01-07"))

final_dates <- c(as.Date("2020-04-14"),
                 as.Date("2022-01-13"),
                 as.Date("2021-12-09"),
                 as.Date("2022-07-14"),
                 as.Date("2020-06-30"),
                 as.Date("2023-02-07"))

Sybil(variants, daily_spline, global_final_date, country, external_dir_names, immunization_end_rate, recovery_rate, reproduce, forecast, initial_dates, final_dates, variants_to_disregard, variants_aggregated, variants_aggregated_names)


# First ascending scenario on Italy
external_dir_names <- c(paste0(country, "/V4/ThirdScenario/December9-2021/"),
                        paste0(country, "/V4/ThirdScenario/December12-2021/"),
                        paste0(country, "/V4/ThirdScenario/December15-2021/"),
                        paste0(country, "/V4/ThirdScenario/December18-2021/"),
                        paste0(country, "/V4/ThirdScenario/December21-2021/"),
                        paste0(country, "/V4/ThirdScenario/December24-2021/"),
                        paste0(country, "/V4/ThirdScenario/December27-2021/"),
                        paste0(country, "/V4/ThirdScenario/December30-2021/"),
                        paste0(country, "/V4/ThirdScenario/January2-2022/"),
                        paste0(country, "/V4/ThirdScenario/January5-2022/"),
                        paste0(country, "/V4/ThirdScenario/January8-2022/"),
                        paste0(country, "/V4/ThirdScenario/January11-2022/"))

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

Sybil(variants, daily_spline, global_final_date, country, external_dir_names, immunization_end_rate, recovery_rate, reproduce, forecast, initial_dates, final_dates, variants_to_disregard, variants_aggregated, variants_aggregated_names)

plots_first_ascending_scenario(external_dir_names, country, daily_spline, variants)


# Second ascending scenario on Italy
external_dir_names <- c(paste0(country, "/V4/SecondAscendingScenario/February15-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/February18-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/February21-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/February24-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/February27-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/March2-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/March5-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/March8-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/March11-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/March14-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/March17-2021/"),
                        paste0(country, "/V4/SecondAscendingScenario/March20-2021/"))

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

Sybil(variants, daily_spline, global_final_date, country, external_dir_names, immunization_end_rate, recovery_rate, reproduce, forecast, initial_dates, final_dates, variants_to_disregard, variants_aggregated, variants_aggregated_names)

plots_second_ascending_scenario(external_dir_names, country, daily_spline, variants)


# Scenarios with Austria (V=11)
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

Sybil(variants, daily_spline, global_final_date, country, external_dir_names, immunization_end_rate, recovery_rate, reproduce, forecast, initial_dates, final_dates, variants_to_disregard, variants_aggregated, variants_aggregated_names)

plots_v11_scenario(external_dir_names, country, daily_spline, variants)


# Scenarios with Austria (V=4)
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

Sybil(variants, daily_spline, global_final_date, country, external_dir_names, immunization_end_rate, recovery_rate, reproduce, forecast, initial_dates, final_dates, variants_to_disregard, variants_aggregated, variants_aggregated_names)