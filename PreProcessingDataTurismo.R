#############################################
# Script: Preprocessing of Tourist Presence Data
# Author: Leonardo Marchesin, Ilenia Di Battista
# Date: 2025
# Description:
#   This script preprocesses tourist presence data
#   for Lombardy (Italy). It includes:
#     - Filtering the observation period 
#       (December 13, 2021 – January 13, 2022),
#     - Aggregating visitor classes into residents vs. tourists,
#     - Geocoding areas and clipping to Lombardy municipalities,
#     - Exporting cleaned datasets,
#     - Preparing pilot subsets for analysis:
#         * First weekday (Dec 13, 2021)
#         * Christmas (Dec 25, 2021)
#         * January 6, 2022
#
# Dependencies:
#   - dplyr
#   - tidygeocoder
#   - sf
#############################################

#### Load Libraries ####
library(dplyr)
library(tidygeocoder)
library(sf)

#### Import Data ####
presence_raw <- read.csv("Turismo_Presenza.csv", sep = ",", dec = ",", header = TRUE)

#### Filter Observation Period ####
presence_raw$day <- as.Date(presence_raw$giorno)

presence_filtered <- presence_raw %>%
  filter(day >= as.Date("2021-12-13") & day <= as.Date("2022-01-13")) %>%
  select(-c(2, 3, 6, 8))  # remove unused columns

#### Aggregate by Visitor Class ####
# First aggregation: remove "classe_cliente"
presence_agg <- presence_filtered %>%
  select(-classe_cliente) %>%
  group_by(across(-utenti_unici)) %>%
  summarise(unique_users = sum(utenti_unici), .groups = "drop")

# Reclassify visitor classes: group into "Turisti" and "Abitanti"
presence_agg <- presence_agg %>%
  mutate(visitor_class = case_when(
    classe_visitatore %in% c("Escursionisti", "Turisti", "Visitatori") ~ "Turisti",
    TRUE ~ classe_visitatore
  )) %>%
  group_by(across(-unique_users)) %>%
  summarise(unique_users = sum(unique_users), .groups = "drop")

#### Remove Duplicates (average unique users) ####
presence_clean <- presence_agg %>%
  group_by(day, area, area_id, visitor_class) %>%
  summarise(unique_users = mean(unique_users), .groups = "drop")

#### Geocoding: Latitude & Longitude ####
presence_geo <- presence_clean %>%
  geocode(area, method = "osm", lat = latitude, long = longitude) %>%
  na.omit()

#### Spatial Clipping to Lombardy Municipalities ####
municipalities <- st_read("Geo Data/Compuni_Lombardia.shp")

presence_sf <- st_as_sf(presence_geo, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(municipalities)) %>%
  st_intersection(municipalities)

# Extract coordinates back into dataframe
presence_final <- presence_sf %>%
  mutate(
    longitude = st_coordinates(geometry)[, 1],
    latitude = st_coordinates(geometry)[, 2]
  ) %>%
  st_set_geometry(NULL) %>%
  select(day, area, area_id, visitor_class, unique_users, longitude, latitude)

#### Export Preprocessed Dataset ####
write.csv(presence_final, "Data_Presenze.csv", row.names = FALSE)

#### Aggregate by Residents (unique users per area/day) ####
residents_day <- presence_final %>%
  group_by(day, area, area_id, latitude, longitude) %>%
  summarise(unique_users = sum(unique_users), .groups = "drop")

#### Export Pilot Datasets ####
# First day: Dec 13, 2021
residents_day_first <- residents_day %>%
  filter(day == "2021-12-13")
write.csv(residents_day_first, "Geo Data/Data_First_Day.csv", row.names = FALSE)

# Christmas: Dec 25, 2021
residents_day_christmas <- residents_day %>%
  filter(day == "2021-12-25")
write.csv(residents_day_christmas, "Geo Data/Data_Christmas.csv", row.names = FALSE)

# January 6, 2022
residents_day_jan6 <- residents_day %>%
  filter(day == "2022-01-06")
write.csv(residents_day_jan6, "Geo Data/Data_Jan06.csv", row.names = FALSE)
#### End of Script ####