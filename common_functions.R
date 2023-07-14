# Prophet with Covid-19 data
#
# Author: Daniele Baccega
# Data: COVID19 R library
# Prophet: https://facebook.github.io/prophet/docs/quick_start.html

# Download files and load data
download_files_and_load_data <- function(docker_path, country_long, who_labels){
  if(!file.exists(paste0(docker_path, "datasets"))){
    system(paste0("mkdir ", docker_path, "datasets"))
  }
  
  # Check if the files are updated
  updated_file <- file.exists(paste0(docker_path, "datasets/", country_long, "_", Sys.Date(), ".csv")) &&
    file.exists(paste0(docker_path, "datasets/variants_data_", Sys.Date(), ".csv"))
  
  if(!updated_file){
    # Download the updated data
    system(paste0("rm ", docker_path, "datasets/*.csv"))

    covid19(country = country_long, level = 3, start = "2020-01-01", dir = docker_path)
    system(paste0("rm ", docker_path,  Sys.Date(), "/country/index.csv"))
    if(length(list.files(paste0(docker_path, Sys.Date(), "/country/"))) == 0)
      stop(paste0("Country ", country_long, " not found in COVID19 library data"))
    system(paste0("mv ", docker_path, Sys.Date(), "/country/*.csv ", docker_path, "datasets/", country_long, "_", Sys.Date(), ".csv"))
    system(paste0("rm -r ", docker_path, Sys.Date()))
    
    download.file("https://opendata.ecdc.europa.eu/covid19/virusvariant/csv/data.csv", paste0(docker_path, "datasets/variants_data_", Sys.Date(), ".csv"))
  }
  
  # Read and preprocess the data
  df_COVID19_ref_init <- read.csv(paste0(docker_path, "datasets/", country_long, "_", Sys.Date(), ".csv"))
  df_COVID19_ref_init <- df_COVID19_ref_init %>%
    filter(administrative_area_level_2 == "", administrative_area_level_3 == "", !is.na(confirmed))
  
  
  df_variants_init <- read.csv(paste0(docker_path, "datasets/variants_data_", Sys.Date(), ".csv"))
  variants_countries <- unique(df_variants_init$country)
  
  if(!country_long %in% variants_countries)
    stop(paste0("Country ", country_long, " not found in variants data"))
  
  if(who_labels){
    df_variants_init <- df_variants_init %>%
      filter(country == country_long, !is.na(new_cases), source == "GISAID", ! variant %in% c("B.1.1.7+E484K", "BA.3", "BA.2+L452X", "B.1.617.3", "UNK", "B.1.1.529", "AY.4.2", "SGTF")) %>%
      mutate(year = as.integer(substr(year_week, 1, 4)),
             week = as.integer(substr(year_week, 6, 7)),
             number_detections_variant = as.integer(number_detections_variant),
             number_sequenced_known_variant = as.integer(number_sequenced_known_variant),
             percent_variant = number_detections_variant / number_sequenced_known_variant) %>%
      filter(!(year == 2020 & week %in% c(1, 2, 3, 4, 5, 6, 7, 8)))

    df_variants_init <- df_variants_init %>%
      mutate(variant = str_replace(variant, "/", "-"))
    df_variants_init$percent_variant[is.na(df_variants_init$percent_variant)] <- 0
    
    df_variants_init$variant[which(df_variants_init$variant %in% c("B.1.1.7"))] <- "Alpha"
    df_variants_init$variant[which(df_variants_init$variant %in% c("B.1.617.2"))] <- "Delta"
    df_variants_init$variant[which(df_variants_init$variant %in% c("BA.1", "BA.2", "BA.2.75", "BA.4", "BA.5", "BQ.1", "XBB", "XBB.1.5"))] <- "Omicron"
    df_variants_init$variant[which(df_variants_init$variant %in% c("B.1.351", "P.1", "B.1.525", "B.1.621", "B.1.620", "B.1.617.1", "B.1.621", "B.1.616", "P.3", "B.1.427-B.1.429", "C.37", "Other"))] <- "Other"
  }
  else{
    df_variants_init <- df_variants_init %>%
      filter(country == country_long, !is.na(new_cases), source == "GISAID", ! variant %in% c("B.1.616", "P.3", "B.1.427/B.1.429", "C.37", "B.1.1.7+E484K", "BA.3", "BA.2+L452X", "B.1.617.3", "UNK", "B.1.1.529", "AY.4.2", "SGTF")) %>%
      mutate(year = as.integer(substr(year_week, 1, 4)),
             week = as.integer(substr(year_week, 6, 7)),
             number_detections_variant = as.integer(number_detections_variant),
             number_sequenced_known_variant = as.integer(number_sequenced_known_variant),
             percent_variant = number_detections_variant / number_sequenced_known_variant) %>%
      filter(!(year == 2020 & week %in% c(1, 2, 3, 4, 5, 6, 7, 8)))

    df_variants_init <- df_variants_init %>%
      mutate(variant = str_replace(variant, "/", "-"))
    df_variants_init$percent_variant[is.na(df_variants_init$percent_variant)] <- 0
    
    df_variants_init$variant[which(df_variants_init$variant %in% c("B.1.1.7", "B.1.351", "B.1.525"))] <- "B.1.1.7 - B.1.351 - B.1.525"
    df_variants_init$variant[which(df_variants_init$variant %in% c("B.1.617.2", "B.1.617.1", "B.1.620", "B.1.621"))] <- "B.1.617.2 - B.1.617.1 - B.1.620 - B.1.621"
    df_variants_init$variant[which(df_variants_init$variant %in% c("XBB.1.5", "XBB"))] <- "XBB - XBB.1.5"
  }
  
  df_variants_init <- df_variants_init %>%
    group_by(year_week) %>%
    aggregate(percent_variant ~ year_week + variant + week + year, FUN=sum) %>%
    arrange(year_week)
  
  df_variants_init <- filter_variants(df_variants_init)
  
  df_variants_init$percent_variant[which(df_variants_init$percent_variant > 1.0)] <- 1.0
  
  return(list(df_COVID19_ref_init, df_variants_init, updated_file))
}

# Filter out small values in the variants proportion
filter_variants <- function(df_variants_init){
  variants_names <- unique(df_variants_init$variant)
  
  other_proportion <- c()
  df_variants <- data.frame()
  for(v in variants_names){
    df_variants_local <- df_variants_init %>%
      filter(variant == v)
    
    n <- nrow(df_variants_local)
    
    peak <- first(which(df_variants_local$percent_variant == max(df_variants_local$percent_variant)))
    
    right_peak <- match(0, df_variants_local$percent_variant[seq(peak, n)]) + peak - 1
    left_peak <- peak - match(0, rev(df_variants_local$percent_variant[seq(1, peak)])) + 1
    
    if(is.na(right_peak)){
      right_peak <- n
    }
    
    if(is.na(left_peak)){
      left_peak <- 1
    }
    
    variants_local_other <- df_variants_local$percent_variant
    variants_local_other[left_peak:right_peak] <- 0
    if(length(other_proportion) == 0){
      other_proportion <- variants_local_other
    }
    else{
      other_proportion <- other_proportion + variants_local_other
    }
    
    df_variants_local$percent_variant[-c(left_peak:right_peak)] <- 0
    df_variants <- rbind(df_variants, df_variants_local)
  }
  
  df_variants$percent_variant[which(df_variants$variant == "Other")] <- df_variants$percent_variant[which(df_variants$variant == "Other")] + other_proportion
  df_variants$percent_variant[which(df_variants$variant == "Other" & df_variants$year == 2020 & df_variants$week < 45)] <- 1.0
  
  return(df_variants)
}

# Compute and save the necessary data.
compute_data <- function(dir_name, df_COVID19_ref, df_variants_ref, immunization_end_rate, new_data=FALSE){
  if(file.exists(paste0(dir_name, "/data/date.RData")) && !new_data){
    load(paste0(dir_name, "/data/date.RData"))
    new_data <- !(today == Sys.Date())
  }
  else{
    new_data <- TRUE
  }
  
  if(new_data){
    N <- df_COVID19_ref$population[1]
     
    df_COVID19_ref <- df_COVID19_ref %>%
      mutate(total_cases = confirmed, total_deaths = deaths, total_recovered = recovered) %>%
      select(date, total_cases, total_deaths, total_recovered, population) %>%
      mutate(new_cases = diff(c(0, total_cases)), new_deaths = diff(c(0, total_deaths)), new_recovered = diff(c(0, total_recovered))) %>%
      filter(!is.na(new_cases), date >= "2020-02-24")
    
    df_COVID19_ref$date <- as.Date(df_COVID19_ref$date)
    
    df_COVID19_ref$week <- as.integer(substr(format(df_COVID19_ref$date, format="%Y-%V"), 6, 7))
    df_COVID19_ref$year <- as.integer(substr(format(df_COVID19_ref$date, format="%Y"), 1, 4))
    
    df_COVID19_ref$year[which(df_COVID19_ref$year == 2021 & df_COVID19_ref$week == 53)] <- 2020
    df_COVID19_ref$year[which(df_COVID19_ref$year == 2022 & df_COVID19_ref$week == 52 & df_COVID19_ref$date <= as.Date("2022-02-01"))] <- 2021
    df_COVID19_ref$year[which(df_COVID19_ref$year == 2023 & df_COVID19_ref$week == 52 & df_COVID19_ref$date <= as.Date("2023-02-01"))] <- 2022
    
    df_COVID19_ref_weekly <- df_COVID19_ref %>%
      group_by(week, year) %>%
      summarise(date = first(date))
    
    df_COVID19_ref_weekly <- df_COVID19_ref_weekly[order(df_COVID19_ref_weekly$date),]
    
    df_COVID19_ref_weekly_variants <- df_COVID19_ref_weekly %>%
      filter(!(year == df_variants_ref$year[nrow(df_variants_ref)] & week > df_variants_ref$week[nrow(df_variants_ref)]))
    
    n <- nrow(df_COVID19_ref)
    
    S_local <- I_local <- R_local <- D_local <- rep(0, n)
    
    S_local[1] <- N - df_COVID19_ref$total_cases[1]
    I_local[1] <- df_COVID19_ref$total_cases[1]
    R_local[1] <- df_COVID19_ref$total_recovered[1]
    D_local[1] <- df_COVID19_ref$total_deaths[1]
    
    for(t in 2:n){
      S_local[t] <- S_local[t-1] - df_COVID19_ref$new_cases[t] + R_local[t-1] * immunization_end_rate
      I_local[t] <- I_local[t-1] + df_COVID19_ref$new_cases[t] - (df_COVID19_ref$new_deaths[t] + df_COVID19_ref$new_recovered[t])
      R_local[t] <- R_local[t-1] + df_COVID19_ref$new_recovered[t] - R_local[t-1] * immunization_end_rate
      D_local[t] <- D_local[t-1] + df_COVID19_ref$new_deaths[t]
    }
   
    SIRD_all <- data.frame(date=df_COVID19_ref$date, S=S_local, I=I_local, R=R_local, D=D_local)
   
    results_all <- get_rates(SIRD_all[-nrow(SIRD_all),], SIRD_all[nrow(SIRD_all),], immunization_end_rate, rep(N, nrow(SIRD_all)-1))
    
    df_variants_ref <- df_variants_ref %>%
      filter(!(year == df_COVID19_ref_weekly_variants$year[nrow(df_COVID19_ref_weekly_variants)] & week > df_COVID19_ref_weekly_variants$week[nrow(df_COVID19_ref_weekly_variants)])) %>%
      arrange(variant)
    
    df_variants_ref$date <- rep(df_COVID19_ref_weekly_variants$date, length(unique(df_variants_ref$variant)))
    
    today <- Sys.Date()
    save(df_variants_ref, df_COVID19_ref, S_local, I_local, R_local, D_local, SIRD_all, results_all, file=paste0(dir_name, "/data/data.RData"))
    save(today, file=paste0(dir_name, "/data/date.RData"))
  }
  else{
    load(paste0(dir_name, "/data/data.RData"))
  }
  
  return(list(df_variants_ref, df_COVID19_ref, SIRD_all, results_all))
}

# Computes the infection, the recovery and the fatality rates starting
# from the system of ODEs of the SIRD model.
get_rates <- function(SIRD, after_date_SIRD, immunization_end_rate, N){
  fat_rates <- c(diff(SIRD$D), after_date_SIRD$D - SIRD$D[nrow(SIRD)]) / SIRD$I
  rec_rates <- (c(diff(SIRD$R), after_date_SIRD$R - SIRD$R[nrow(SIRD)]) + SIRD$R * immunization_end_rate) / SIRD$I
  infection_rates <- (c(diff(SIRD$I), after_date_SIRD$I - SIRD$I[nrow(SIRD)]) + SIRD$I * (rec_rates + fat_rates)) * (N / (SIRD$S * SIRD$I))
  
  return(data.frame(date=SIRD$date, infection_rates, rec_rates, fat_rates))
}

# Checks if we can re-obtain the real data starting from the previously computed
# rates; in particular, we used a deterministic SIRD model.
SIRD_check <- function(dir_name, SIRD, infection_rates, rec_rates, fat_rates, immunization_end_rate, N){
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
    scale_colour_manual(values=c("black", "red"))
  print(plot)
  dev.off()
}

# Plot SIRD$I
plot_I <- function(dir_name, SIRD_plot){
  png(paste0(dir_name, "/I.png"), units="in", width=34, height=15, res=300)
  plot <- ggplot(SIRD_plot, aes(x=date, y=I)) +
    geom_line(linewidth=1.5) +
    theme(legend.position = "bottom", legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=35), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
    labs(x="date", y="infected")
  print(plot)
  dev.off()
  
  png(paste0(dir_name, "/I_scenarios.png"), units="in", width=34, height=15, res=300)
  plot <- ggplot(SIRD_plot, aes(x=date, y=I)) +
    geom_line(linewidth=1.5) +
    geom_vline(aes(xintercept = as.Date("2020-04-14")), color="red", linetype="dashed", linewidth=1.5) +
    geom_vline(aes(xintercept = as.Date("2020-06-30")), color="red", linetype="dashed", linewidth=1.5) +
    geom_vline(aes(xintercept = as.Date("2022-01-13")), color="red", linetype="dashed", linewidth=1.5) +
    geom_vline(aes(xintercept = as.Date("2023-02-07")), color="red", linetype="dashed", linewidth=1.5) +
    geom_vline(aes(xintercept = as.Date("2022-07-14")), color="red", linetype="dashed", linewidth=1.5) +
    geom_text(aes(x = as.Date("2020-04-14")-30, label="\nFirst forecast", y=max(I) - max(I)/9 ), size = 13, colour="red", angle=90) +
    geom_text(aes(x = as.Date("2020-06-30")-30, label="\nSecond forecast", y=max(I) - max(I)/9), size = 13, colour="red", angle=90) +
    geom_text(aes(x = as.Date("2022-01-13")-30, label="\nThird forecast", y=max(I) - max(I)/9), size = 13, colour="red", angle=90) +
    geom_text(aes(x = as.Date("2023-02-07")-30, label="\nFifth forecast", y=max(I) - max(I)/9), size = 13, colour="red", angle=90) +
    geom_text(aes(x = as.Date("2022-07-14")-30, label="\nFourth forecast", y=max(I) - max(I)/9), size = 13, colour="red", angle=90) +
    theme(legend.position = "bottom", legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=35), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
    labs(x="date", y="infected")
  print(plot)
  dev.off()
}

generate_and_plot_variants_info <- function(dir_name, df_variants, df_COVID19_all, SIRD_all, results_all){
  variants_name <- unique(df_variants$variant)
  
  variants_global_df <- data.frame()
  i <- 1
  for(v in variants_name){
    df_variants_local <- df_variants %>%
      filter(variant == v)
    
    n <- nrow(df_variants_local)
    
    df_variants_local$percent_variant <- rollapplyr(df_variants_local$percent_variant, 3, (mean), fill = if(v == "Other") 1 else 0)
    
    variants_spline <- splinefun(seq(1, n*7, 7), df_variants_local$percent_variant, method = "monoH.FC")
    variants_data_spline <- variants_spline(seq(1, length(seq.Date(df_variants_local$date[1], df_variants_local$date[nrow(df_variants_local)], 1))))
    variants_data_spline[variants_data_spline < 0] <- 0
    
    variants_spline_df <- data.frame(date=seq.Date(df_variants_local$date[1], df_variants_local$date[nrow(df_variants_local)], 1), y=variants_data_spline, variant=rep(v, length(variants_data_spline)))
    
    variants_global_df <- rbind(variants_global_df, variants_spline_df)
    
    png(paste0(dir_name, "variants_", v, "_spline_check.png"), units="in", width=34, height=15, res=300)
    plot <- ggplot(variants_spline_df) +
      geom_line(aes(x=date, y=y, col=variant), linewidth=1.5) +
      scale_colour_manual(values=hue_pal()(length(variants_name))[i]) +
      theme(legend.position = "bottom", legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=35), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
      labs(x="date", y="Proportion", col="Variant")
    print(plot)
    dev.off()
    
    i <- i + 1
  }
  
  png(paste0(dir_name, "variants_check.png"), units="in", width=34, height=15, res=300)
  plot <- ggplot(variants_global_df) +
    geom_line(aes(x=date, y=y, col=variant), linewidth=1.5) +
    scale_colour_manual(values=hue_pal()(length(variants_name))) +
    theme(legend.position = "bottom", legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=35), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
    labs(x="date", y="proportion", col="Variants")
  print(plot)
  dev.off()
  
  df_COVID19_all <- df_COVID19_all %>%
    filter(date <= df_variants$date[nrow(df_variants)])
  
  SIRD_all <- SIRD_all %>%
    filter(date <= df_variants$date[nrow(df_variants)])
  
  results_all <- results_all %>%
    filter(date <= df_variants$date[nrow(df_variants)])
      
  return(list(variants_global_df, df_COVID19_all, SIRD_all, results_all))
}

SIRD_variants <- function(dir_name, df_variants, SIRD_all, results_all, immunization_end_rate, N){
  variants_name <- unique(df_variants$variant)
  
  SIRD_all_variants_plot <- data.frame()
  infection_rates_all_variants <- c()
  for(i in 1:length(variants_name)){
    df_variants_local <- df_variants %>%
      filter(variant == variants_name[i])
    
    I_variant <- SIRD_all$I * df_variants_local$y
    
    SIRD_variant <- data.frame(date=SIRD_all$date, S=SIRD_all$S, I=I_variant, R=SIRD_all$R, D=SIRD_all$D)
    infection_rates_variant <- (diff(SIRD_variant$I) + SIRD_variant$I[-nrow(SIRD_variant)] * (results_all$rec_rates[-nrow(SIRD_variant)] + results_all$fat_rates[-nrow(SIRD_variant)])) * (rep(N, nrow(SIRD_variant)-1) / (SIRD_variant$S[-nrow(SIRD_variant)] * SIRD_all$I[-nrow(SIRD_variant)]))
    infection_rates_variant[is.na(infection_rates_variant) | is.infinite(infection_rates_variant) | infection_rates_variant < 0] <- 0
    
    infection_rates_all_variants <- c(infection_rates_all_variants, infection_rates_variant)
    
    SIRD_all_variants_plot <- rbind(SIRD_all_variants_plot, data.frame(date=SIRD_variant$date, S=SIRD_variant$S, I=SIRD_variant$I, R=SIRD_variant$R, D=SIRD_variant$D, variant=variants_name[i]))
  }

  df_variants_names <- df_variants %>%
    filter(date != SIRD_all$date[nrow(SIRD_all)])
  
  df_all_variants <- data.frame(date=rep(SIRD_all$date[-nrow(SIRD_all)], length(variants_name)), infection_rates=infection_rates_all_variants, rec_rates=rep(results_all$rec_rates[-nrow(SIRD_variant)], length(variants_name)), fat_rates=rep(results_all$fat_rates[-nrow(SIRD_variant)], length(variants_name)), variant=df_variants_names$variant)

  png(paste0(dir_name, "/I_variants_scenarios.png"), units="in", width=34, height=15, res=300)
  plot <- ggplot(SIRD_all_variants_plot, aes(date, I, color=variant))
  for(i in 1:length(variants_name)){
    plot <- plot + geom_line(linewidth=1.5)
  }
  plot <- plot +
    geom_vline(aes(xintercept = as.Date("2020-04-14")), color="red", linetype="dashed", linewidth=1.5) +
    geom_vline(aes(xintercept = as.Date("2020-06-30")), color="red", linetype="dashed", linewidth=1.5) +
    geom_vline(aes(xintercept = as.Date("2022-01-13")), color="red", linetype="dashed", linewidth=1.5) +
    geom_vline(aes(xintercept = as.Date("2023-02-07")), color="red", linetype="dashed", linewidth=1.5) +
    geom_vline(aes(xintercept = as.Date("2022-07-14")), color="red", linetype="dashed", linewidth=1.5) +
    geom_text(aes(x = as.Date("2020-04-14")-40, label="\nFirst forecast", y=max(I) - max(I)/9), size = 13, colour="red", angle=90) +
    geom_text(aes(x = as.Date("2020-06-30")-40, label="\nSecond forecast", y=max(I) - max(I)/9), size = 13, colour="red", angle=90) +
    geom_text(aes(x = as.Date("2022-01-13")-40, label="\nThird forecast", y=max(I) - max(I)/9), size = 13, colour="red", angle=90) +
    geom_text(aes(x = as.Date("2023-02-07")-40, label="\nFifth forecast", y=max(I) - max(I)/9), size = 13, colour="red", angle=90) +
    geom_text(aes(x = as.Date("2022-07-14")-40, label="\nFourth forecast", y=max(I) - max(I)/9), size = 13, colour="red", angle=90) +
    scale_colour_manual(values=hue_pal()(length(variants_name))) +
    theme(legend.position = "bottom", legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=35), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
  labs(x="date", y="infected", col="Variants")
  print(plot)
  dev.off()
  
  png(paste0(dir_name, "/I_variants.png"), units="in", width=34, height=15, res=300)
  plot <- ggplot(SIRD_all_variants_plot, aes(date, I, color=variant))
  for(i in 1:length(variants_name)){
    plot <- plot + geom_line(linewidth=1.5)
  }
  plot <- plot +
    scale_colour_manual(values=hue_pal()(length(variants_name))) +
    theme(legend.position = "bottom", legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=35), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
    labs(x="date", y="infected", col="Variants")
  print(plot)
  dev.off()
  
  return(list(SIRD_all_variants_plot, df_all_variants))
}

# Gets the data frames.
filter_data <- function(df_COVID19_ref, SIRD_all, SIRD_all_variants, results_all, results_all_variants, initial_date, final_date, final_date_ref, variants){
  # Reference dataframes
  df_COVID19_ref <- df_COVID19_ref %>%
    filter(date >= initial_date, date <= final_date_ref)
  
  SIRD_all <- SIRD_all %>%
    filter(date >= initial_date, date <= final_date_ref)
  
  SIRD_all_variants <- SIRD_all_variants %>%
    filter(date >= initial_date, date <= final_date_ref)
  
  results_all <- results_all %>%
    filter(date >= initial_date, date <= final_date_ref)
  
  results_all_variants <- results_all_variants %>%
    filter(date >= initial_date, date <= final_date_ref)
  
  
  # Dataframes
  df_COVID19 <- df_COVID19_ref %>%
    filter(date <= final_date)

  SIRD <- SIRD_all %>%
    filter(date <= final_date)
  
  SIRD_variants <- SIRD_all_variants %>%
    filter(date <= final_date)
  
  results <- results_all %>%
    filter(date <= final_date)
  
  results_variants <- results_all_variants %>%
    filter(date <= final_date)
  
  
  # Select the correct dataframes
  df_COVID19_ref_used <- df_COVID19_ref
  SIRD_ref_used <- SIRD_all
  results_ref_used <- results_all
  df_COVID19_used <-df_COVID19
  SIRD_used <- SIRD
  results_used <- results
  
  if(variants){
    SIRD_ref_used <- SIRD_all_variants
    results_ref_used <- results_all_variants
    SIRD_used <- SIRD_variants
    results_used <- results_variants
  }
  
  return(list(df_COVID19_ref_used, df_COVID19_used, SIRD_ref_used, SIRD_used, results_ref_used, results_used))
}

# Applies Prophet.
apply_Prophet <- function(dir_name, date, values, time_step, file_name, plot_dir, mcmc_samples=0, changepoint_prior_scale=0.05){
  profet_df <- data.frame(ds=date, y=values)
  
  path <- paste0(dir_name, "/prophet_models/model_", file_name, ".RData")
  if(file.exists(path)){
    load(path)
  }
  else{
    m <- prophet(profet_df, interval.width=0.8, changepoint.prior.scale = changepoint_prior_scale, mcmc.samples=mcmc_samples)
    save(m, file=path)
  }
  
  future <- make_future_dataframe(m, periods=time_step, freq="day", include_history = FALSE)
  forecast <- predict(m, future)
  
  return(forecast)
}

# Generates the forecast plot.
forecast_plot <- function(dir_name, method, ref_data_flag, final_date, n, n_ref, dates, dates_ref, data_ref, data_fc, i, title, p=0, d=0, q=0){
  if(ref_data_flag){
    type <- rep(NA, n_ref + (n_ref - n) + 2)
    
    type[1:n] <- rep("ground truth", n)
    type[(n+1):(n_ref+1)] <- rep("ground truth (new)", n_ref-n+1)
    type[(n_ref+2):(n_ref+(n_ref-n)+2)] <- rep("forecast", n_ref-n+1)
    
    
    date <- c(dates_ref[1:n], dates_ref[n:n_ref], dates_ref[n:n_ref])
    
    
    value <- c(data_ref[1:n], data_ref[n:n_ref], data_ref[n], data_fc$yhat)
    low <- c(rep(NA, n_ref+1), data_ref[n], data_fc$yhat_lower)
    up <- c(rep(NA, n_ref+1), data_ref[n], data_fc$yhat_upper)
    
    df_plot <- data.frame(date, value, low, up, type)
  }
  else{
    type <- rep(NA, n_ref + 1)
    
    type[1:n] <- rep("ground truth", n)
    type[(n+1):(n_ref+1)] <- rep("forecast", n_ref-n+1)
    
    date <- c(dates, seq(as.Date(dates[n]), final_date, q))
    
    value <- c(data_ref[1:n], data_ref[n], data_fc$yhat)
    low <- c(rep(NA, n), data_ref[n], data_fc$yhat_lower)
    up <- c(rep(NA, n), data_ref[n], data_fc$yhat_upper)
    
    df_plot <- data.frame(date, value, low, up, type)
  }
  
  plot <- ggplot(df_plot, aes(x=date, col=type)) +
    geom_line(aes(y=value), linewidth=1.5) +
    labs(title=paste0(i, " days"), x="date", y=if(title[1] == "I") "I" else "infection rates", color="Variants", linetype="Type") +
    scale_colour_manual(values=c("red", "black", "blue")) +
    theme(legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=35), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
    guides(color=guide_legend(override.aes=list(fill=NA)))
  save(plot, file = paste0(sub("/I/", "/", sub("/infection_rates/", "/", paste0(dir_name, "/"))), "/RData/forecast_", title, "_", i, "_days.RData"))
}

# Deterministic SIRD model.
SIRD_det <- function(n, n_ref, N, SIRD, infection_rates, global_infection_rates, global_recovery_rates, global_fatality_rates, immunization_end_rate, variants, time_step){
  if(variants){
    SIRD_all_variants <- SIRD %>%
      group_by(date) %>%
      summarize(S=first(S), I=sum(I), R=first(R), D=first(D))
    
    n <- nrow(SIRD_all_variants)
    n_ref <- n + time_step
    
    S_local_all_variants <- I_local_all_variants <- R_local_all_variants <- D_local_all_variants <- rep(NA, n_ref)
    
    S_local_all_variants[1:n] <- SIRD_all_variants$S
    I_local_all_variants[1:n] <- SIRD_all_variants$I
    R_local_all_variants[1:n] <- SIRD_all_variants$R
    D_local_all_variants[1:n] <- SIRD_all_variants$D
    
    for(t in n:(n_ref-1)){
      S_local_all_variants[t+1] <- S_local_all_variants[t] - global_infection_rates$mean[(t-n)+1] * I_local_all_variants[t] * S_local_all_variants[t] / N + R_local_all_variants[t] * immunization_end_rate
      I_local_all_variants[t+1] <- I_local_all_variants[t] + global_infection_rates$mean[(t-n)+1] * I_local_all_variants[t] * S_local_all_variants[t] / N - I_local_all_variants[t] * (global_recovery_rates$mean[(t-n)+1] + global_fatality_rates$mean[(t-n)+1])
      R_local_all_variants[t+1] <- R_local_all_variants[t] + I_local_all_variants[t] * global_recovery_rates$mean[(t-n)+1] - R_local_all_variants[t] * immunization_end_rate
      D_local_all_variants[t+1] <- D_local_all_variants[t] + I_local_all_variants[t] * global_fatality_rates$mean[(t-n)+1]
    }
    
    variants_name <- unique(infection_rates$variant)
    
    I_local <- data.frame()
    for(i in 1:length(variants_name)){
      infection_rates_variant <- infection_rates %>%
        filter(variant == variants_name[i])
      
      SIRD_variant <- SIRD %>%
        filter(variant == variants_name[i])
      
      I_local_variant <- rep(NA, n_ref)
      I_local_variant[1:n] <- SIRD_variant$I
      
      for(t in n:(n_ref-1)){
        I_local_variant[t+1] <- I_local_variant[t] + infection_rates_variant$mean[(t-n)+1] * I_local_all_variants[t] * S_local_all_variants[t] / N - I_local_variant[t] * (global_recovery_rates$mean[(t-n)+1] + global_fatality_rates$mean[(t-n)+1])
      }
      
      I_local <- rbind(I_local, data.frame(I=I_local_variant, variant=rep(variants_name[i], n_ref)))
    }
    
    SIRD_ev <- data.frame(date=seq.Date(SIRD_all_variants$date[1], SIRD_all_variants$date[n]+time_step, 1), S=rep(S_local_all_variants, length(variants_name)), I=I_local$I, R=rep(R_local_all_variants, length(variants_name)), D=rep(D_local_all_variants, length(variants_name)), variant=I_local$variant)
  }
  else{
    S_local <- I_local <- R_local <- D_local <- rep(NA, n_ref)
    
    S_local[1:n] <- SIRD$S
    I_local[1:n] <- SIRD$I
    R_local[1:n] <- SIRD$R
    D_local[1:n] <- SIRD$D

    for(t in n:(n_ref-1)){
      S_local[t+1] <- S_local[t] - infection_rates$mean[(t-n)+1] * I_local[t] * S_local[t] / N + R_local[t] * immunization_end_rate
      I_local[t+1] <- I_local[t] + infection_rates$mean[(t-n)+1] * I_local[t] * S_local[t] / N - I_local[t] * (global_recovery_rates$mean[(t-n)+1] + global_fatality_rates$mean[(t-n)+1])
      R_local[t+1] <- R_local[t] + I_local[t] * global_recovery_rates$mean[(t-n)+1] - R_local[t] * immunization_end_rate
      D_local[t+1] <- D_local[t] + I_local[t] * global_fatality_rates$mean[(t-n)+1]
    }
    
    SIRD_ev <- data.frame(date=seq.Date(SIRD$date[1], SIRD$date[n] + time_step, 1), S=S_local, I=I_local, R=R_local, D=D_local)
  }
  
  return(SIRD_ev)
}

# Evolution of the deterministic SIRD model.
SIRD_evolution <- function(dir_name, method, time_step, ref_data_flag, final_date, infection_rates, global_infection_rates, global_recovery_rates, global_fatality_rates, immunization_end_rate, fc_I, SIRD, SIRD_ref, N, variants){
  n <- length(unique(SIRD$date))
  n_ref <- n + time_step
  
  df_local <- SIRD_det(n, n_ref, N, SIRD, infection_rates, global_infection_rates, global_recovery_rates, global_fatality_rates, immunization_end_rate, variants, time_step)
  
  if(variants){
    variants_name <- unique(infection_rates$variant)
    
    if(ref_data_flag){
      type <- rep(NA, n_ref + (n_ref - n) + 2)
      
      type[1:n] <- rep("ground truth", n)
      type[(n+1):(n_ref+1)] <- rep("forecast", n_ref-n+1)
      type[(n_ref+2):(n_ref+2+(n_ref-n))] <- rep("ground truth (new)", n_ref-n+1)
      type <- rep(type, 3 + length(variants_name))
      
      date <- rep(c(unique(SIRD_ref$date), SIRD_ref$date[n], SIRD_ref$date[n:n_ref]), 3 + length(variants_name))
      
      value <- c()
      place <- c()
      variant <- c()
      for(i in 1:length(variants_name)){
        df_local_I <- df_local %>%
          filter(variant == variants_name[i])
        
        SIRD_ref_local <- SIRD_ref %>%
          filter(variant == variants_name[i])
        
        value <- c(value, c(df_local_I$I, SIRD_ref_local$I[n], SIRD_ref_local$I[n:n_ref]))
        place <- c(place, c(rep("I", n_ref+2+(n_ref-n))))
        variant <- c(variant, rep(variants_name[i], n_ref+2+(n_ref-n)))
      }
      
      value <- c(value, c(unique(df_local$S), SIRD_ref$S[n], SIRD_ref$S[n:n_ref]), c(unique(df_local$R), SIRD_ref$R[n], SIRD_ref$R[n:n_ref]), c(unique(df_local$D), SIRD_ref$D[n], SIRD_ref$D[n:n_ref]))
      place <- c(place, rep("S", n_ref+2+(n_ref-n)), rep("R", n_ref+2+(n_ref-n)), rep("D", n_ref+2+(n_ref-n)))
      variant <- c(variant, rep("-", n_ref+2+(n_ref-n)), rep("-", n_ref+2+(n_ref-n)), rep("-", n_ref+2+(n_ref-n)))
    }
    else{
      type <- rep(NA, n_ref + 1)
      
      type[1:n] <- rep("ground truth", n)
      type[(n+1):(n_ref+1)] <- rep("forecast", n_ref-n+1)
      type <- rep(type, 3 + length(variants_name))
      
      date <- rep(c(unique(SIRD$date), seq(SIRD$date[n], final_date, 1)), 3 + length(variants_name))
      
      value <- c()
      place <- c()
      variant <- c()
      for(i in 1:length(variants_name)){
        df_local_I <- df_local %>%
          filter(variant == variants_name[i])
        
        SIRD_ref_local <- SIRD_ref %>%
          filter(variant == variants_name[i])
        
        value <- c(value, c(df_local_I$I[1:n], df_local_I$I[n:n_ref]))
        place <- c(place, c(rep("I", n_ref+1)))
        variant <- c(variant, rep(variants_name[i], n_ref+1))
      }
      
      value <- c(value, c(unique(df_local$S[1:n]), df_local$S[n:n_ref]), c(unique(df_local$R[1:n]), df_local$R[n:n_ref]), c(unique(df_local$D[1:n]), df_local$D[n:n_ref]))
      place <- c(place, rep("S", n_ref+1), rep("R", n_ref+1), rep("D", n_ref+1))
      variant <- c(variant, rep("-", n_ref+1), rep("-", n_ref+1), rep("-", n_ref+1))
    }
    
    df_plot <- data.frame(date, value, place, type, variant)
    df_plot$place = factor(df_plot$place, levels = c("S", "I", "R", "D"))
    df_plot$variant = factor(df_plot$variant, levels = c(variants_name, "-"))
    
    df_plot <- df_plot %>%
      filter(place == "I")
  
    plot <- ggplot() +
      geom_line(data=df_plot, aes(date, value, col=variant, linetype=type), linewidth=1.5) +
      facet_wrap(~place, scales="free_y") +
      scale_colour_manual(values=c(hue_pal()(length(variants_name)), "#808080")) +
      theme(legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=35), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
      labs(title=paste0(time_step, " days"), x="date", y="population", color="Variants", linetype="Type")
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
      
      comparison(dir_name, time_step, "I", paste0("I (", variants_name[i], ")"), ref_data_flag, final_date, SIRD_local$date, SIRD_ref_local$date, df_local_I$I, fc_I_local$mean, SIRD_ref_local$I) 
    }
  }
  else{
    if(ref_data_flag){
      type <- rep(NA, n_ref + (n_ref - n) + 2)
      
      type[1:n] <- rep("ground truth", n)
      type[(n+1):(n_ref+1)] <- rep("forecast", n_ref-n+1)
      type[(n_ref+2):(n_ref+2+(n_ref-n))] <- rep("ground truth (new)", n_ref-n+1)
      
      date <- rep(c(SIRD_ref$date, SIRD_ref$date[n], SIRD_ref$date[n:n_ref]), 4)
      value <- c(c(df_local$S, SIRD_ref$S[n], SIRD_ref$S[n:n_ref]), c(df_local$I, SIRD_ref$I[n], SIRD_ref$I[n:n_ref]), c(df_local$R, SIRD_ref$R[n], SIRD_ref$R[n:n_ref]), c(df_local$D, SIRD_ref$D[n], SIRD_ref$D[n:n_ref]))
      place <- c(rep("S", n_ref+2+(n_ref-n)), rep("I", n_ref+2+(n_ref-n)), rep("R", n_ref+2+(n_ref-n)), rep("D", n_ref+2+(n_ref-n)))
      type <- rep(type, 4)
    }
    else{
      type <- rep(NA, n_ref + 1)
      
      type[1:n] <- rep("ground truth", n)
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
  
    plot <- ggplot() +
      geom_line(data=df_plot, aes(date, value, col=type), linewidth=1.5) +
      facet_wrap(~place, scales="free_y") +
      scale_colour_manual(values=c("red", "black", "blue")) +
      theme(legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=35), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
      labs(title=paste0(time_step, " days"), x="date", y="population", color="Variants", linetype="Type")
    save(plot, file = paste0(dir_name, "/RData/SIRD_forecast_", time_step, "_days.RData"))

    comparison(dir_name, time_step, "I", "I", ref_data_flag, final_date, SIRD$date, SIRD_ref$date, df_local$I, fc_I, SIRD_ref$I)
  }
  
  return(df_local)
}

# Generates a comparison plot.
comparison <- function(dir_name, time_step, variable_name, variable_name_variants, ref_data_flag, final_date_local, dates, dates_ref, variable_local, variable_fc, variable_ref){
  n <- length(dates)
  n_ref <- n + time_step
  
  if(ref_data_flag){
    type <- rep(NA, n_ref + (n_ref - n) * 2 + 3)
    
    type[1:n] <- rep("ground truth", n)
    type[(n+1):(n_ref+1)] <- rep("forecast on the rates", n_ref-n+1)
    type[(n_ref+2):(n_ref+2+(n_ref-n))] <- rep("ground truth (new)", n_ref-n+1)
    type[(n_ref+3+(n_ref-n)):(n_ref+3+(n_ref-n)*2)] <- rep(paste0("forecast on ", variable_name), n_ref-n+1)
    
    date <- c(dates_ref, dates_ref[n], dates_ref[n:n_ref], dates_ref[n:n_ref])
    value <- c(variable_local, variable_ref[n], variable_ref[n:n_ref], variable_ref[n], variable_fc)
    place <- rep("I", n_ref + (n_ref - n) * 2 + 3)
  }
  else{
    type <- rep(NA, n_ref + (n_ref - n) + 2)
    
    type[1:n] <- rep("ground truth", n)
    type[(n+1):(n_ref+1)] <- rep("forecast on the rates", n_ref-n+1)
    type[(n_ref+2):(n_ref+(n_ref-n)+2)] <- rep("forecast on I", n_ref-n+1)
    
    date <- c(dates, seq(dates[n], final_date_local, 1), seq(dates[n], final_date_local, 1))
    value <- c(variable_local[1:n], variable_local[n], variable_local[(n+1):length(variable_local)], variable_local[n], variable_fc)
    place <- rep("I", n_ref + (n_ref - n) + 2)
  }
  
  df_plot <- data.frame(date, value, place, type)
  
  plot <- ggplot(df_plot, aes(date, value, col=type)) +
    geom_line(linewidth=1.5) +
    scale_colour_manual(values=c("green4", "red", "black", "blue")) +
    theme(legend.key.size = unit(1.5, 'cm'), axis.text=element_text(size=35), axis.title=element_text(size=30, face="bold"), plot.title = element_text(size=40, face="bold"), legend.title=element_text(size=40, face="bold"), legend.text=element_text(size=38)) +
    labs(title=paste0(time_step, " days"), x="date", y="I", color="Variants", linetype="Type")
  save(plot, file=paste0(dir_name, "/RData/", variable_name_variants, "_compare_", time_step, "_days.RData"))
}

final_plots <- function(dir_name){
  load(paste0(dir_name, "/forecast_plot/RData/SIRD_forecast_7_days.RData"))
  p1 <- plot
  load(paste0(dir_name, "/forecast_plot/RData/SIRD_forecast_14_days.RData"))
  p2 <- plot
  load(paste0(dir_name, "/forecast_plot/RData/SIRD_forecast_21_days.RData"))
  p3 <- plot
  load(paste0(dir_name, "/forecast_plot/RData/SIRD_forecast_28_days.RData"))
  p4 <- plot
  
  p <- (p1 + p2) / (p3 + p4) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png(paste0(dir_name, "/forecast_plot/SIRD_forecast.png"), units="in", width=34, height=15, res=300)
  print(p)
  dev.off()
  
  
  load(paste0(dir_name, "/forecast_plot/RData/forecast_recovery_rates_7_days.RData"))
  p1 <- plot
  load(paste0(dir_name, "/forecast_plot/RData/forecast_recovery_rates_14_days.RData"))
  p2 <- plot 
  load(paste0(dir_name, "/forecast_plot/RData/forecast_recovery_rates_21_days.RData"))
  p3 <- plot 
  load(paste0(dir_name, "/forecast_plot/RData/forecast_recovery_rates_28_days.RData"))
  p4 <- plot 
  
  p <- (p1 + p2) / (p3 + p4) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png(paste0(dir_name, "/forecast_plot/forecast_recovery_rates.png"), units="in", width=34, height=15, res=300)
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
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.box = "vertical")
  
  png(paste0(dir_name, "/forecast_plot/forecast_fatality_rates.png"), units="in", width=34, height=15, res=300)
  print(p)
  dev.off()
  
  
  if(variants){
    load(paste0(dir_name, "/forecast_plot/RData/forecast_global_infection_rates_7_days.RData"))
    p1 <- plot 
    load(paste0(dir_name, "/forecast_plot/RData/forecast_global_infection_rates_14_days.RData"))
    p2 <- plot 
    load(paste0(dir_name, "/forecast_plot/RData/forecast_global_infection_rates_21_days.RData"))
    p3 <- plot 
    load(paste0(dir_name, "/forecast_plot/RData/forecast_global_infection_rates_28_days.RData"))
    p4 <- plot
    
    p <- (p1 + p2) / (p3 + p4) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(dir_name, "/forecast_plot/infection_rates/forecast_global_infection_rates.png"), units="in", width=34, height=15, res=300)
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
        plot_layout(guides = "collect") &
        theme(legend.position = "bottom", legend.box = "vertical")
      
      png(paste0(dir_name, "/forecast_plot/I/forecast_I_", variants_name[k], ".png"), units="in", width=34, height=15, res=300)
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
        plot_layout(guides = "collect") &
        theme(legend.position = "bottom", legend.box = "vertical")
      
      png(paste0(dir_name, "/forecast_plot/infection_rates/forecast_infection_rates_", variants_name[k], ".png"), units="in", width=34, height=15, res=300)
      print(p)
      dev.off()
      
      
      load(paste0(dir_name, "/forecast_plot/RData/I (", variants_name[k], ")_compare_7_days.RData"))
      p1 <- plot 
      load(paste0(dir_name, "/forecast_plot/RData/I (", variants_name[k], ")_compare_14_days.RData"))
      p2 <- plot 
      load(paste0(dir_name, "/forecast_plot/RData/I (", variants_name[k], ")_compare_21_days.RData"))
      p3 <- plot 
      load(paste0(dir_name, "/forecast_plot/RData/I (", variants_name[k], ")_compare_28_days.RData"))
      p4 <- plot 
      
      p <- (p1 + p2) / (p3 + p4) +
        plot_layout(guides = "collect") &
        theme(legend.position = "bottom", legend.box = "vertical")
      
      png(paste0(dir_name, "/forecast_plot/comparison/I (", variants_name[k], ")_compare.png"), units="in", width=34, height=15, res=300)
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
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(dir_name, "/forecast_plot/infection_rates/forecast_infection_rates.png"), units="in", width=34, height=15, res=300)
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
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(dir_name, "/forecast_plot/I/forecast_I.png"), units="in", width=34, height=15, res=300)
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
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom", legend.box = "vertical")
    
    png(paste0(dir_name, "/forecast_plot/comparison/I_compare.png"), units="in", width=34, height=15, res=300)
    print(p)
    dev.off()
  }
}