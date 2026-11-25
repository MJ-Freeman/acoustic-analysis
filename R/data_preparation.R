

# Acoustic Monitoring on Railways

# Data preparation

# Purpose: To develop a consistent and reusable workflow for analysing
# bird and bat acoustic monitoring data across the UK, Spain and Portugal.

# prepare the source data for country, location and device-level analyses.



#------------------------------------------------------------------------------
# Libraraies
#------------------------------------------------------------------------------

library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(tidyr)



#------------------------------------------------------------------------------
# Parameters
#------------------------------------------------------------------------------

countries <- c("UK", "ES", "PT")



#------------------------------------------------------------------------------
# File directories
#------------------------------------------------------------------------------

# Bird data directories

UK_bird_dir <- "data/Bird/UK"
ES_bird_dir <- "data/Bird/ES"
PT_bird_dir <- "data/Bird/PT"


# Bat data directories

UK_bat_dir <- "data/Bat/UK"
ES_bat_dir <- "data/Bat/ES"
PT_bat_dir <- "data/Bat/PT"


#------------------------------------------------------------------------------
# Source data
#------------------------------------------------------------------------------

# function to import csv files from the country folders

import_detections <- function(folder){
  files <- list.files(
    folder,
    pattern = "*.csv",
    recursive = TRUE,
    full.names = TRUE)
  files %>%
    lapply(read_csv) %>%
    bind_rows()
}


# Bird data

UKdata <- import_detections(UK_bird_dir)
ESdata <- import_detections(ES_bird_dir)
PTdata <- import_detections(PT_bird_dir)



# Bat data

UKbat <- import_detections(UK_bat_dir)
ESbat <- import_detections(ES_bat_dir)
PTbat <- import_detections(PT_bat_dir)





#------------------------------------------------------------------------------
# standardise/clean datasets where required
#------------------------------------------------------------------------------

# UK bat data in different format from alternate model output in ES and PT
# Create a common event timestamp and standardise species names
# while retaining the original columns.

UKbat_clean <- UKbat %>%
  dplyr::mutate(
    Country = "UK", 
    event_datetime = call_datetime,
    species_standardised = dplyr::case_when(
      species == "Barbastellus barbastellus" ~ "Barbastella barbastellus",
      TRUE ~ species)
  )


ESbat_clean <- ESbat %>%
  dplyr::mutate(
    Country = "Spain",
    event_datetime = window_datetime,
    species_standardised = species
  )


PTbat_clean <- PTbat %>%
  dplyr::mutate(
    Country = "Portugal",
    event_datetime = window_datetime,
    species_standardised = species
  )





#------------------------------------------------------------------------------
# Combining datasets
#------------------------------------------------------------------------------

# Combined bird datasets

bird_data <- bind_rows(
  UK = UKdata,
  Spain = ESdata,
  Portugal = PTdata,
  .id = "Country"
)


# Combined bat datasets

bat_data <- dplyr::bind_rows(
  UKbat_clean,
  ESbat_clean,
  PTbat_clean
)



#------------------------------------------------------------------------------
# Saving prepared datasets
#------------------------------------------------------------------------------

# Combined bird dataset

saveRDS(bird_data, "data/prepared/bird.data.rds")


# Combined bat dataset

saveRDS(bat_data, "data/prepared/bat_data.rds")






