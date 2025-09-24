#############################################
# Script: Exposure Time Series by Province
# Author: Leonardo Marchesin, Ilenia Di Battista
# Date: 2025
# Description:
#   This script calculates daily exposure values
#   aggregated at the provincial level in Lombardy.
#   Steps include:
#     - Importing preprocessed presence data,
#     - Matching smoothed fields to municipalities,
#     - Aggregating exposures by province,
#     - Generating time series plots for:
#         * Exposure,
#         * Unique users,
#         * Pollution proxy (smoothed field average).
#
# Dependencies:
#   - sf
#   - dplyr
#############################################

#### Load Libraries ####

library(sf)
library(dplyr)
library(fda)
library(ggplot2)
library(gridExtra)
library(tidyr)

#### Import Data ####
presence_data <- read.csv("Data_Presenze.csv", sep = ",", dec = ",", header = TRUE)
presence_data$unique_users <- as.numeric(gsub(",", ".", presence_data$unique_users))

municipalities <- st_read("Geo Data/Compuni_Lombardia.shp")
provinces <- st_read("Geo Data/Provincie.shp")
provinces$DEN_PROV[which(provinces$DEN_PROV == '-')] <- "Milano"

smoothed_field <- read.csv("results_smoothing/f_eval_fine2.csv", header = TRUE)[, -1]
coords <- read.csv("results_smoothing/nodes_eval_fine2.csv", header = TRUE)[, -1]

areas <- read.csv("aree_con_comuni.csv") %>%
  filter(comune != "")

#### Aggregate Users (all visitors) ####
presence_all <- presence_data %>%
  group_by(day, area, area_id, latitude, longitude) %>%
  summarise(unique_users = sum(unique_users), .groups = "drop")

#### Prepare Spatial Join ####
points_sf <- st_as_sf(
  data.frame(x = coords[, 1], y = coords[, 2]),
  coords = c("x", "y"), crs = 4326
)
points_sf <- st_transform(points_sf, st_crs(municipalities))
joined <- st_join(points_sf, municipalities["PRO_COM"])
municipality_per_point <- joined$PRO_COM

#### Initialize Results ####
days <- unique(presence_all$day)
n_prov <- nrow(provinces)

exposure <- matrix(0, nrow = n_prov, ncol = length(days))
users <- matrix(0, nrow = n_prov, ncol = length(days))
pollution <- matrix(0, nrow = n_prov, ncol = length(days))

prov_aggregation <- unlist(lapply(unique(areas$PRO_COM), function(x) {
  which(provinces$COD_PROV ==
          municipalities$COD_PROV[which(municipalities$PRO_COM == x)][1])
}))

#### Daily Loop ####
for (t in seq_along(days)) {
  message(paste("Processing day", t, "of", length(days)))
  
  day_data <- presence_all %>%
    filter(day == days[t])
  
  smoothing_agg <- data.frame(comune = unique(areas$PRO_COM))
  
  smoothing_agg$smooth <- sapply(unique(areas$PRO_COM), function(comune) {
    nodes <- which(municipality_per_point == comune)
    if (length(nodes) == 0) return(0)
    mean(smoothed_field[nodes, t])
  })
  
  smoothing_agg$users <- sapply(unique(areas$PRO_COM), function(comune) {
    rows <- which(day_data$area_id %in% areas$area_id[areas$PRO_COM == comune])
    if (length(rows) == 0) return(0)
    sum(day_data$unique_users[rows], na.rm = TRUE)
  })
  
  smoothing_agg$exp <- smoothing_agg$users * smoothing_agg$smooth
  
  exposure[, t] <- sapply(1:n_prov, function(p) {
    idx <- which(prov_aggregation == p)
    sum(smoothing_agg$exp[idx], na.rm = TRUE) /
      sum(smoothing_agg$users[idx], na.rm = TRUE)
  })
  
  users[, t] <- sapply(1:n_prov, function(p) {
    idx <- which(prov_aggregation == p)
    log(sum(smoothing_agg$users[idx], na.rm = TRUE) + 1)
  })
  
  pollution[, t] <- sapply(1:n_prov, function(p) {
    idx <- which(prov_aggregation == p)
    mean(smoothing_agg$smooth[idx], na.rm = TRUE)
  })
}

#### Custom Colors ####
colors <- c(
  "#FF8C00",  # Bergamo
  "#1E90FF",  # Brescia
  "#FFD700",  # Como
  "#2E8B57",  # Cremona
  "#FF69B4",  # Lecco
  "#228B22",  # Lodi
  "#006400",  # Mantova
  "#6A0DAD",  # Milano
  "#800020",  # Monza
  "#32CD32",  # Pavia
  "#87CEEB",  # Sondrio
  "#A52A2A"
)

#### Key Dates for X-axis ####
key_dates <- as.Date(c(min(days), "2021-12-25", "2022-01-06", max(days)))
key_idx   <- match(key_dates, days)

add_date_axis <- function() {
  axis(1, at = key_idx, labels = format(key_dates, "%d-%b"), las = 2)
}

#### Plot Exposure by Province ####
df_exposure <- as.data.frame(exposure)
colnames(df_exposure) <- as.character(days)
df_exposure$province <- provinces$DEN_PRO

df_exposure_long <- df_exposure %>%
  tidyr::pivot_longer(
    cols = -province,
    names_to = "day",
    values_to = "exposure"
  ) %>%
  mutate(day = as.Date(day))

#### Plot with ggplot ####
ggplot(df_exposure_long, aes(x = day, y = exposure, color = province, group = province)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(breaks = key_dates,
               labels = format(key_dates, "%d-%b")) +
  scale_color_manual(values = colors) +
  theme_minimal() +
  #titolo della legenda da cambiare, metti Province
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right", legend.title = element_blank()
  ) +
  labs(y = "Exposure", x = NULL, title = "Exposure by Province")

#### Plot Users by Province ####
x11()
plot(1:length(days), users[1, ], type = "b", pch = 16, col = colors[1],
     ylim = c(min(users, na.rm = TRUE), max(users, na.rm = TRUE)),
     ylab = "Log(Unique Users)", xaxt = "n")
for (p in 2:n_prov) {
  lines(1:length(days), users[p, ], type = "b", pch = 16, col = colors[p])
}
add_date_axis()
legend("topright", legend = provinces$DEN_PRO, col = colors, pch = 16, cex = 0.7)

#### Plot Pollution Proxy by Province ####
x11()
plot(1:length(days), pollution[1, ], type = "b", pch = 16, col = colors[1],
     ylim = c(min(pollution, na.rm = TRUE), max(pollution, na.rm = TRUE)),
     ylab = "Smoothed Field Average", xaxt = "n")
for (p in 2:n_prov) {
  lines(1:length(days), pollution[p, ], type = "b", pch = 16, col = colors[p])
}
add_date_axis()
legend("topright", legend = provinces$DEN_PRO, col = colors, pch = 16, cex = 0.7)

#############################################
# Functional PCA on Exposure Time Series
#############################################

#### Define Functional Basis ####
day_range <- c(1, length(days))
n_basis <- 15
basis <- create.fourier.basis(day_range, n_basis)

#### Convert Exposure Data to Functional Object ####
exposure_fd <- Data2fd(
  argvals = 1:length(days),
  y = t(exposure),
  basisobj = basis
)

#### Functional PCA ####
pca_exposure <- pca.fd(exposure_fd, nharm = 4)

# Flip PC2 for interpretability
pca_exposure$scores[, 2] <- -pca_exposure$scores[, 2]

#### Plot Scores (PC1 vs PC2) ####
x11()
plot(pca_exposure$scores[, 1], pca_exposure$scores[, 2],
     xlab = "PC1", ylab = "PC2", pch = 16, col = colors,
     main = "Scores Plot (PC1 vs PC2)")
text(pca_exposure$scores[, 1], pca_exposure$scores[, 2],
     labels = provinces$DEN_PRO, pos = 3, cex = 0.7)

#### Evaluate mean and harmonics ####
time_grid <- 1:length(days)
mean_eval  <- eval.fd(time_grid, pca_exposure$meanfd)
harm1_eval <- eval.fd(time_grid, pca_exposure$harmonics[1])
harm2_eval <- -eval.fd(time_grid, pca_exposure$harmonics[2])

#### Build dataframe for PC1 ####
df_pc1 <- data.frame(
  day = as.Date(time_grid),
  mean = mean_eval,
  plus2sd = mean_eval + 2 * sqrt(pca_exposure$values[1]) * harm1_eval,
  minus2sd = mean_eval - 2 * sqrt(pca_exposure$values[1]) * harm1_eval
)

df_pc1_long <- df_pc1 %>%
  pivot_longer(cols = -day, names_to = "curve", values_to = "value")

#### Plot PC1 ####
p_pc1 <- ggplot(df_pc1_long, aes(x = day, y = value, color = curve, linetype = curve)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("mean" = "black",
                                "mean.1" = "darkblue",
                                "mean.2" = "darkred"),
                     labels = c("Mean", "+2SD (PC1)", "-2SD (PC1)")) +
  scale_linetype_manual(values = c("mean" = "solid",
                                   "mean.1" = "dashed",
                                   "mean.2" = "dashed"),
                        labels = c("Mean", "+2SD (PC1)", "-2SD (PC1)")) +
  labs(title = "Functional Mean ± 2SD (PC1)",
       y = "Exposure", x = NULL) +
  theme_minimal() +
  theme(legend.title = element_blank())

#### Build dataframe for PC2 ####
df_pc2 <- data.frame(
  day = as.Date(time_grid),
  mean = mean_eval,
  plus2sd = mean_eval + 2 * sqrt(pca_exposure$values[2]) * harm2_eval,
  minus2sd = mean_eval - 2 * sqrt(pca_exposure$values[2]) * harm2_eval
)

df_pc2_long <- df_pc2 %>%
  pivot_longer(cols = -day, names_to = "curve", values_to = "value")

#### Plot PC2 ####
p_pc2 <- ggplot(df_pc2_long, aes(x = day, y = value, color = curve, linetype = curve)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("mean" = "black",
                                "mean.1" = "darkblue",
                                "mean.2" = "darkred"),
                     labels = c("Mean", "+2SD (PC2)", "-2SD (PC2)")) +
  scale_linetype_manual(values = c("mean" = "solid",
                                   "mean.1" = "dashed",
                                   "mean.2" = "dashed"),
                        labels = c("Mean", "+2SD (PC2)", "-2SD (PC2)")) +
  labs(title = "Functional Mean ± 2SD (PC2)",
       y = "Exposure", x = NULL) +
  theme_minimal() +
  theme(legend.title = element_blank())

#### Print the plots ####

print(p_pc1)
print(p_pc2)

#### Plot Cumulative Variance Explained ####
x11()
plot(cumsum(pca_exposure$varprop), type = "b", pch = 16,
     ylab = "Cumulative Variance Explained", ylim = c(0, 1),
     main = "Variance Explained by Functional PCs", xaxt = "n")
add_date_axis()
abline(h = 0.9, col = "red", lty = 2)

#### Maps of PC1 and PC2 Scores ####
provinces_pca <- provinces %>%
  mutate(PC1 = pca_exposure$scores[, 1],
         PC2 = pca_exposure$scores[, 2])

x11()
p1 <- ggplot(provinces_pca) +
  geom_sf(aes(fill = PC1), color = "black", size = 0.3) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Functional PCA - PC1 Scores", fill = "PC1")

p2 <- ggplot(provinces_pca) +
  geom_sf(aes(fill = PC2), color = "black", size = 0.3) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Functional PCA - PC2 Scores", fill = "PC2")

grid.arrange(p1, p2, ncol = 2)

p1

p2

#############################################