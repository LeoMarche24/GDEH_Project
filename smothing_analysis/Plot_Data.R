## Plot Data PM10

locs = read.csv(paste0(getwd(), "/smoothing/space_locs.csv"), header=TRUE)[, -1]
                       
n = dim(locs)[1]
m = 32

y_all = as.matrix(read.csv(paste0(getwd(), "/smoothing/y.csv"), header=TRUE))[, -1]  
y_mat = matrix(as.numeric(y_all), nrow=n, ncol=m)
range(as.numeric(y_all), na.rm=TRUE)

# choose day
days_chosen = c(1, 13, 20, 25)
range(y_mat[, days_chosen], na.rm=TRUE)
clim_sup = 135

day = 1
y = y_mat[, day]
na_idxs = which(is.na(y))

lombardy_boundary_nodes = read.table(paste0(getwd(), "/boundary_nodes_lombardy.txt"), header=TRUE)
library(ggmap)
register_stadiamaps("...", write = FALSE)
# Insert your key
bbox <- c(left = 8.3, bottom = 44.5, right = 11.5, top = 46.8)
lombardy_map <- get_stadiamap(bbox, zoom = 8, maptype = "stamen_terrain")
data <- data.frame(
  lon = locs$lon_vec[-na_idxs],
  lat = locs$lat_vec[-na_idxs],
  y_log = y[-na_idxs]
)



# Plot Data with common colour scale 
estimate_plot <- ggmap(lombardy_map, darken = c(.356,"white")) +
  geom_point(
    data = data,
    aes(x = lon, y = lat, color = y_log),
    size = 4
  ) +
  scale_color_viridis_c(
    option = "viridis",
    limits = c(0, clim_sup),      
    oob = scales::squish    
  ) +
  geom_path(
    data = rbind(lombardy_boundary_nodes, lombardy_boundary_nodes[1,]),
    aes(x = lon, y = lat),
    color = "black",
    linewidth = 0.75
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(0.7, "cm"),
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),   
    axis.text.y = element_blank(), 
    axis.text = element_text(size = 20),
    panel.background = element_rect(fill = 'transparent')
  ) + guides(color = "none")

estimate_plot

# ggsave(paste0(getwd(), "/plots/data_day", day, ".pdf"), plot = estimate_plot, width = 10, height = 8, dpi = 300)

