library("rayshader")
library("sf")
library("tidyverse")
library(osmdata)


# Read extension and EP
aoi <- st_read(dsn = here::here("data/spatial/dem/extent_ep1km.shp"),
              quiet = TRUE)
ep <- st_read(dsn = here::here("data/spatial/01_EP_ANDALUCIA/EP_Andalucía.shp"),
              quiet = TRUE)

# Digital Elevation Model
demraw <- raster::raster(here::here("data/spatial/dem/dem.tif"))
dem <- raster::crop(demraw, aoi)
m <- raster_to_matrix(dem)

# Plot the DEM
base_map <- m %>%
  sphere_shade(texture = "desert", colorintensity = 2) %>%
  add_shadow(ambient_shade(heightmap = m), max_darken = 0.1)

plot_map(base_map)

# Get Streams layer
## Get data from OSM
library("osmdata")
aoi4326 <- st_transform(aoi, 4326)
b <- lapply(st_geometry(aoi4326), st_bbox)

water <- opq(b) %>%
  add_osm_feature("waterway") %>%
  osmdata_sf()

water$osm_lines
sf::sf_use_s2(FALSE)
rios <- st_transform(water$osm_lines, crs=25830)

rios_layer <- generate_line_overlay(
  rios, extent = raster::extent(aoi),
  color = "skyblue2", linewidth = 4,
  heightmap = m
)


# Plot to map
base_map %>%
  add_overlay(rios_layer, alphalayer = 0.8) %>%
  plot_map()




# raw_carreteras = opq(b) %>%
#   add_osm_feature("highway") %>%
#   osmdata_sf()
# raw_carreteras
#
# carreteras <- st_transform(raw_carreteras$osm_lines, crs=25830)
#
# cc <- generate_line_overlay(
#   carreteras, extent = raster::extent(aoi),
#   color = "white", linewidth = 3,
#   heightmap = m
# )




# %>%
alcontar_paleta <- c("NP" = "orange", "P" = "darkred", "PR" = "darkgreen")

zonas <- generate_polygon_overlay(
  ep,
  extent = raster::extent(aoi),
  heightmap = m,
  data_column_fill = "ABREVIA",
  palette = alcontar_paleta
)


base_map %>%
  add_overlay(rios_layer, alphalayer = 0.8) %>%
  # add_overlay(cc, alphalayer = 0.8) %>%
  add_overlay(zonas, alphalayer = .8) %>%
  plot_map()



# zonas_label <- generate_label_overlay(
#   ep_label,
#   extent = raster::extent(aoi),
#   heightmap = m,
#   text_size = 1.5, point_size = 1,
#   data_label_column = "name")


compass <- generate_compass_overlay(heightmap = m,
                         x = 0.15,
                         y=0.15,
                         text_color="white")

base_map %>%
  add_overlay(rios_layer, alphalayer = 0.8) %>%
  add_overlay(zonas, alphalayer = .8) %>%
  add_overlay(compass) %>%
  plot_map()


base_map %>%
  add_overlay(rios_layer, alphalayer = 0.8) %>%
  add_overlay(zonas, alphalayer = .8) %>%
  # add_overlay(compass) %>%
  plot_3d(m, zscale = 10, fov = 0,
        theta = 135, zoom = .8, phi = 45,
        windowsize = c(1000, 800))

# Add text
ep_label <- st_centroid(ep)
la <- ep_label %>% st_drop_geometry()

render_label(m, lat = la[1,"Y"], long = la[1,"X"], text = la[1,"name"],
             extent = raster::extent(aoi),zscale = 20)
render_label(m, lat = la[2,"Y"], long = la[2,"X"], text = la[2,"name"],
             extent = raster::extent(aoi),zscale = 20)
render_label(m, lat = la[3,"Y"], long = la[3,"X"], text = la[3,"name"],
            extent = raster::extent(aoi),zscale = 20)
render_compass(position = "N")
render_scalebar(limits=c(0, 5), label_unit = "km")




render_snapshot()

render_camera(fov = 0, theta =135, zoom = 0.75, phi = 45)
render_scalebar(limits=c(0, 5, 10),label_unit = "km",position = "E", y=50,
                scale_length = c(0.2,1))





