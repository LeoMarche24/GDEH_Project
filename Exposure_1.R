#############################################
# Script: Pilot Exposure Calculation (Four Days)
# Author: Leonardo Marchesin, Ilenia Di Battista
# Date: 2025
# Description:
#   This script calculates exposure values by combining
#   smoothed fields with user presence data.
#   The analysis is performed for two pilot days:
#       - December 13, 2021 (weekday reference)
#       - December 25, 2021 (Christmas holiday)
#
#   Results are aggregated at the municipal (comune) and 
#   provincial levels, and visualized with choropleth maps.
#
#   The purpose is to evaluate aggregation procedures 
#   and compare patterns between an ordinary day and a holiday.
#
# Dependencies:
#   - sf
#   - dplyr
#   - ggplot2
#   - viridis
#############################################

#### Load Libraries ####
library(sf)
library(dplyr)
library(ggplot2)

#### Import Data ####
presence_data <- read.csv("Data_Presenze.csv", sep = ",", dec = ",", header = TRUE)
presence_data$unique_users <- as.numeric(gsub(",", ".", presence_data$unique_users))

municipalities <- st_read("Geo Data/Compuni_Lombardia.shp")
provinces <- st_read("Geo Data/Provincie.shp")

smoothed_field <- read.csv("results_smoothing/f_eval_fine2.csv", header = TRUE)[, -1]
coords <- read.csv("results_smoothing/nodes_eval_fine2.csv", header = TRUE)[, -1]

areas <- read.csv("aree_con_comuni.csv") %>%
  filter(comune != "")

time <- unique(presence_data$day)

unique_comuni <- unique(areas$PRO_COM)

#### Function: Calculate Exposure for a Given Day ####
calculate_exposure <- function(date_string, presence_data, smoothed_field, 
                               coords, areas, municipalities, provinces)
{
  # Filter residents for the chosen day
  residents_day <- presence_data %>%
    group_by(day, area, area_id, latitude, longitude) %>%
    summarise(unique_users = sum(unique_users), .groups = "drop") %>%
    filter(day == as.Date(date_string))
  
  
  smoothed_day <- smoothed_field[, which(time == date_string)]  # assuming same smoothing field applies
  
  # Initialize dataset
  exposure_df <- data.frame(
    comune = unique_comuni,
    smooth_value = rep(0, length(unique_comuni)),
    users = rep(0, length(unique_comuni)),
    exposure = rep(0, length(unique_comuni))
  )
  
  # Spatial mapping of nodes to municipalities
  points_sf <- st_as_sf(
    data.frame(x = coords[, 1], y = coords[, 2]),
    coords = c("x", "y"), crs = 4326
  ) %>%
    st_transform(st_crs(municipalities))
  
  points_joined <- st_join(points_sf, municipalities["PRO_COM"])
  comuni_per_point <- points_joined$PRO_COM
  
  # Aggregate smoothing values
  exposure_df$smooth_value <- sapply(unique_comuni, function(comune) {
    nodes <- which(comuni_per_point == comune)
    if (length(nodes) == 0) return(NA)
    mean(smoothed_day[nodes], na.rm = TRUE)
  })
  
  # Aggregate users
  exposure_df$users <- sapply(unique_comuni, function(comune) {
    area_id <- areas[areas$PRO_COM == comune, "area_id"]
    nodes <- which(residents_day$area_id %in% area_id)
    if (length(nodes) == 0) return(NA)
    mean(residents_day$unique_users[nodes], na.rm = TRUE)
  })
  
  # Compute exposure
  exposure_df <- exposure_df %>%
    mutate(
      smooth_value = as.numeric(smooth_value),
      users = as.numeric(users),
      exposure = users * smooth_value
    )
  
  # Municipal level dataset
  municipalities_plot <- municipalities %>%
    select(PRO_COM, COD_PROV) %>%
    filter(PRO_COM %in% exposure_df$comune) %>%
    left_join(exposure_df, by = c("PRO_COM" = "comune"))
  
  # Provincial aggregation
  province_agg <- exposure_df %>%
    left_join(municipalities_plot[, c("PRO_COM", "COD_PROV")], by = c("comune" = "PRO_COM")) %>%
    group_by(COD_PROV) %>%
    summarise(
      exp_prov = sum(exposure, na.rm = TRUE),
      users_prov = sum(users, na.rm = TRUE),
      smooth_prov = mean(smooth_value, na.rm = TRUE)
    ) %>%
    mutate(exp_prov = exp_prov / users_prov)
  
  provinces_plot <- provinces %>%
    select(COD_PROV) %>%
    filter(COD_PROV %in% province_agg$COD_PROV) %>%
    left_join(province_agg, by = "COD_PROV")
  
  list(
    exposure_df = exposure_df,
    municipalities_plot = municipalities_plot,
    provinces_plot = provinces_plot
  )
}

#### Run Analysis for Pilot Days ####
# Day 1: December 13, 2021
results_day1 <- calculate_exposure(
  date_string = "2021-12-13",
  presence_data, smoothed_field, coords, areas, municipalities, provinces
)

# Day 2: December 25, 2021 (Christmas)
results_christmas <- calculate_exposure(
  date_string = "2021-12-25",
  presence_data, smoothed_field, coords, areas, municipalities, provinces
)

# Day 3: January 1, 2022 (Christmas)
results_jan1 <- calculate_exposure(
  date_string = "2022-01-01",
  presence_data, smoothed_field, coords, areas, municipalities, provinces
)

# Day 4: January 6, 2022 (Christmas)
results_jan6 <- calculate_exposure(
  date_string = "2022-01-06",
  presence_data, smoothed_field, coords, areas, municipalities, provinces
)

#### Plot Results ####

# Municipal level - Day 1
ggplot(data = results_day1$municipalities_plot) +
  geom_sf(aes(fill = smooth_value)) +
  scale_fill_viridis_c(limits = c(min(smoothed_field), max(smoothed_field))) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    panel.grid.major = element_line(color = "transparent")
  ) +
  geom_sf(data = provinces, fill = NA, color = "black", size = 0.5)

# Municipal level - Christmas

ggplot(data = results_christmas$municipalities_plot) +
  geom_sf(aes(fill = smooth_value)) +
  scale_fill_viridis_c(limits = c(min(smoothed_field), max(smoothed_field))) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    panel.grid.major = element_line(color = "transparent")
  ) +
  geom_sf(data = provinces, fill = NA, color = "black", size = 0.5)

# Municipal level - Jan 1

ggplot(data = results_jan1$municipalities_plot) +
  geom_sf(aes(fill = smooth_value)) +
  scale_fill_viridis_c(limits = c(min(smoothed_field), max(smoothed_field))) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    panel.grid.major = element_line(color = "transparent")
  ) +
  geom_sf(data = provinces, fill = NA, color = "black", size = 0.5)

# Municipal level - Jan 6

ggplot(data = results_jan6$municipalities_plot) +
  geom_sf(aes(fill = smooth_value)) +
  scale_fill_viridis_c(limits = c(min(smoothed_field), max(smoothed_field))) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    panel.grid.major = element_line(color = "transparent")
  ) +
  geom_sf(data = provinces, fill = NA, color = "black", size = 0.5)

## Unique users

min <- log(min(results_day1$municipalities_plot$users, 
           results_christmas$municipalities_plot$users,
           results_jan1$municipalities_plot$users,
           results_jan6$municipalities_plot$users, na.rm = TRUE))
max <- log(max(results_day1$municipalities_plot$users, 
           results_christmas$municipalities_plot$users,
           results_jan1$municipalities_plot$users,
           results_jan6$municipalities_plot$users, na.rm = TRUE))

# Municipal level - Day 1

ggplot(data = results_day1$municipalities_plot) +
  geom_sf(aes(fill = log(users))) +
  scale_fill_viridis_c(limits = c(min, 
                                  max)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    panel.grid.major = element_line(color = "transparent")
  ) +
  geom_sf(data = provinces, fill = NA, color = "black", size = 0.5)

# Municipal level - Christmas

ggplot(data = results_christmas$municipalities_plot) +
  geom_sf(aes(fill = log(users))) +
  scale_fill_viridis_c(limits = c(min, 
                                  max)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    panel.grid.major = element_line(color = "transparent")
  ) +
  geom_sf(data = provinces, fill = NA, color = "black", size = 0.5)

# Municipal level - Jan 1

ggplot(data = results_jan1$municipalities_plot) +
  geom_sf(aes(fill = log(users))) +
  scale_fill_viridis_c(limits = c(min, 
                                  max)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    panel.grid.major = element_line(color = "transparent")
  ) +
  geom_sf(data = provinces, fill = NA, color = "black", size = 0.5)

# Municipal level - Jan 6

ggplot(data = results_jan6$municipalities_plot) +
  geom_sf(aes(fill = log(users))) +
  scale_fill_viridis_c(limits = c(min, 
                                  max)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    panel.grid.major = element_line(color = "transparent")
  ) +
  geom_sf(data = provinces, fill = NA, color = "black", size = 0.5)

# Province level

# Search for max and min exposure province

max_exp_prov <- max(c(
  results_day1$provinces_plot$exp_prov,
  results_christmas$provinces_plot$exp_prov,
  results_jan1$provinces_plot$exp_prov,
  results_jan6$provinces_plot$exp_prov
), na.rm = TRUE)
min_exp_prov <- min(c(
  results_day1$provinces_plot$exp_prov,
  results_christmas$provinces_plot$exp_prov,
  results_jan1$provinces_plot$exp_prov,
  results_jan6$provinces_plot$exp_prov
), na.rm = TRUE)

# Provincial level - Day 1

ggplot(data = results_day1$provinces_plot) +
  geom_sf(aes(fill = exp_prov)) +
  scale_fill_viridis_c(limits = c(min_exp_prov, max_exp_prov)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    panel.grid.major = element_line(color = "transparent")
  ) +
  geom_sf(data = provinces, fill = NA, color = "black", size = 0.5)

# Provincial level - Christmas

ggplot(data = results_christmas$provinces_plot) +
  geom_sf(aes(fill = exp_prov)) +
  scale_fill_viridis_c(limits = c(min_exp_prov, max_exp_prov)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    panel.grid.major = element_line(color = "transparent")
  ) +
  geom_sf(data = provinces, fill = NA, color = "black", size = 0.5)

# Provincial level - Jan 1

ggplot(data = results_jan1$provinces_plot) +
  geom_sf(aes(fill = exp_prov)) +
  scale_fill_viridis_c(limits = c(min_exp_prov, max_exp_prov)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    panel.grid.major = element_line(color = "transparent")
  ) +
  geom_sf(data = provinces, fill = NA, color = "black", size = 0.5)

# Provincial level - Jan 6

ggplot(data = results_jan6$provinces_plot) +
  geom_sf(aes(fill = exp_prov)) +
  scale_fill_viridis_c(limits = c(min_exp_prov, max_exp_prov)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    panel.grid.major = element_line(color = "transparent")
  ) +
  geom_sf(data = provinces, fill = NA, color = "black", size = 0.5)

# End of Script