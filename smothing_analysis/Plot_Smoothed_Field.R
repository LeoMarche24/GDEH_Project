# Plot field 

# time domain
tf=32
m = tf
M = 13
time_mesh = seq(1, tf, length.out=M)
time_locs = seq(1, tf)


library(ggmap)
register_stadiamaps("...", write = FALSE)
# Insert your own API key above
bbox <- c(left = 8.3, bottom = 44.5, right = 11.5, top = 46.8)
lombardy_map <- get_stadiamap(bbox, zoom = 8, maptype = "stamen_terrain")


## Read results from fdaPDE
library(fdaPDE)
mesh_canotto =  readRDS(paste0(getwd(), "/mesh_geospatial/mesh.RData"))  #mesh of the convex hull with margins
new_nodes_canotto = cbind(mesh_canotto$nodes[,1]*(7/5), mesh_canotto$nodes[,2])
range(new_nodes_canotto[,1])
nnodes_canotto = dim(new_nodes_canotto)[1]
FEMbasis = create.FEM.basis(mesh_canotto)

FEMbasis$mesh$nodes[,1] <- FEMbasis$mesh$nodes[,1]*(7/5)

boundary_nodes <- data.frame(new_nodes_canotto[which(mesh_canotto$nodesmarkers==1) ,])
names(boundary_nodes) <- c("lon", "lat")
mesh_lombardy = readRDS(paste0(getwd(), "/mesh_lombardy/mesh.RData"))  # mesh only Lombardy

FEMbasis_lombardy = create.FEM.basis(mesh_lombardy)
lombardy_nodes = cbind(mesh_lombardy$nodes[,1]*(7/5), mesh_lombardy$nodes[,2])
lombardy_segments = mesh_lombardy$segments

lombardy_boundary_nodes = read.table(paste0(getwd(), "/boundary_nodes_lombardy.txt"), header=TRUE)

f = read.csv(paste0(getwd(), "/smoothing/results/f.csv"), header=FALSE)$V1


# Fix a time
Nx = Ny = 1000
X <- seq(from = min(lombardy_boundary_nodes$lon), to = max(lombardy_boundary_nodes$lon), length.out = Nx)
Y <- seq(from = min(lombardy_boundary_nodes$lat), to = max(lombardy_boundary_nodes$lat), length.out = Ny)
grid <- expand.grid(X, Y)


# choose day
days_chosen = c(1, 13, 20, 25)
day = 1
spacetime_locs_plot = cbind(rep(time_locs[day], each= dim(grid)[1]),
                       grid[,1], grid[,2])


evaluation = eval.FEM.time(FEM.time(f, time_mesh, FEMbasis), 
              space.time.locations = cbind(spacetime_locs_plot[,1], spacetime_locs_plot[,2], spacetime_locs_plot[,3]))

range(evaluation, na.rm=TRUE)


# Plot field with common colour scale
clim_smoothing_days_chosen = c(0, 85)

estimate <- data.frame(lon = grid[,1], lat = grid[,2], solution = evaluation)

# Settings
library(sf)
poly_mat <- as.matrix(
  rbind(
    lombardy_boundary_nodes,
    lombardy_boundary_nodes[1, ]
  )
)

# 2) Create object POLYGON and set CRS
poly_geom <- st_sfc(
  st_polygon(list(poly_mat)),
  crs = 4326
)

# 3) Build data.frame
lomb_poly <- st_sf(
  id       = 1,
  geometry = poly_geom
)

lomb_poly <- 
  list(poly_mat) %>% 
  st_polygon() %>% 
  st_sfc(crs = 4326) %>% 
  st_sf(id = 1, geometry = .)

# 2) Filter only inside Lombardy
estimate_sf <- st_as_sf(estimate, coords = c("lon", "lat"), crs = 4326)
estimate_inside <- estimate_sf[st_within(estimate_sf, lomb_poly, sparse = FALSE), ]

estimate_df <- data.frame(
  lon      = st_coordinates(estimate_inside)[,1],
  lat      = st_coordinates(estimate_inside)[,2],
  solution = estimate_inside$solution
)

estimate_plot <- ggmap(lombardy_map, darken = c(.356,"white")) +
  geom_contour_filled(
    data = estimate_df,
    aes(
      x    = lon,
      y    = lat,
      z    = as.numeric(solution),
      fill = after_stat(level_mid)         
    ),
    # bins       = 100,
    breaks = seq(0, clim_smoothing_days_chosen[2], by = 1),
    alpha      = 0.75,
    show.legend = TRUE
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    name   = "Turists",
    limits = c(0, clim_smoothing_days_chosen[2]),       # fix colour scale
    oob    = scales::squish  
  ) +
  geom_path(
    data = rbind(lombardy_boundary_nodes, lombardy_boundary_nodes[1,]),
    aes(x = lon, y = lat),
    color     = "black",
    linewidth = 0.75
  ) +
  coord_quickmap() +                        
  labs(
    x    = "Longitude",
    y    = "Latitude"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(0.7, "cm"),
    legend.text = element_text(size = 20)
  ) +
  theme(
    legend.position = 'right',
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),   
    axis.text.y = element_blank(), 
    # Make the text bigger
    axis.text = element_text(size = 20),
    panel.background = element_rect(fill = 'transparent')
  ) +
  guides(fill = "none")   

estimate_plot

ggsave(paste0(getwd(), "/plots/smooth_day", day, ".pdf"), plot = estimate_plot, width = 10, height = 8, dpi = 300)


