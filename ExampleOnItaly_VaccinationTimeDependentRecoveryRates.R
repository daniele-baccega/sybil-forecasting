# Sybil
#
# Author: Daniele Baccega
# Data: COVID-19 R library
# Prophet: https://facebook.github.io/prophet/docs/quick_start.html


# Import the necessary libraries
library(dplyr)
library(ggplot2)
library(zoo)
library(forecast)
library(COVID19)
library(prophet)
library(scales)
library(patchwork)
library(countrycode)

# Include the Sybil function and the common functions
source("Sybil.R")
source("prepare_data_VaccinationTimeDependentRecoveryRates.R")
source("common_functions.R")
source("plots.R")

Sys.setlocale("LC_TIME", "en_US.UTF-8")


# Select the configuration (with or without variants, daily or daily-spline data)
# If in a particular country data are weekly you have to use daily-spline data.
daily_spline <- FALSE
variants <- TRUE

# Set this flag to true if data related to variants are daily, false if they are weekly
daily_variants_data <- FALSE

# Forecast or only extract the rates and the SIRD evolution?
forecast <- TRUE

# Initialize some variables
immunization_end_rate <- 1 / 180
recovery_rate <- 1 / 14

# Global initial and final dates
global_initial_date <- as.Date("2020-02-24")
global_final_date <- Sys.Date()


# Scenarios with Italy (V=4)
country <- "Italy"

if(!file.exists(country)){
  system(paste0("mkdir -p ", country))
  system(paste0("mkdir -p ", country, "/V4"))
}

# Aggregate variants
variants_to_disregard <- c("AY.4.2", "B.1.1.529", "B.1.1.7+E484K", "B.1.617.3", "BA.2+L452X", "BA.3", "BA.4/BA.5", "SGTF", "UNK")
variants_aggregated <- list(Alpha = c("B.1.1.7"),
                            Delta = c("B.1.617.2"),
                            Omicron = c("BA.2.86", "XBB.1.5-like", "XBB.1.5-like+F456L", "BA.1", "BA.2", "BA.2.75", "BA.4", "BA.5", "BQ.1", "XBB", "XBB.1.5", "XBB.1.5+F456L"),
                            Other = c("B.1.351", "B.1.427/B.1.429", "B.1.525", "B.1.616", "B.1.617.1", "B.1.620", "B.1.621", "C.37", "P.1", "P.3", "B.1.427-B.1.429", "Other"))
variants_aggregated_names <- list("Alpha", "Delta", "Omicron", "Other")

# First scenario on Italy
external_dir_names <- c(paste0(country, "/V4/Example/"))

initial_dates <- c(as.Date("2021-11-20"))

final_dates <- c(as.Date("2022-01-20"))

data <- prepare_data(country, global_initial_date, global_final_date, immunization_end_rate, recovery_rate, variants, variants_to_disregard, variants_aggregated, variants_aggregated_names, daily_spline)
df_variants_all <- data[[1]]
df_disease_all <- data[[2]]
SIRDS_initial_marking <- data[[3]]

Sybil(df_disease_all, df_variants_all, SIRDS_initial_marking, variants, daily_variants_data, daily_spline, external_dir_names, immunization_end_rate, recovery_rate, forecast, initial_dates, final_dates)
