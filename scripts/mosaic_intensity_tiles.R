
library(terra)
library(tidyverse)
library(rnaturalearth)
library(sf)

# Get world data
world <- ne_countries(scale = "medium", returnclass = "sf")
# world <- filter(world, !continent %in% c("Oceania Seven seas (open ocean)", "Antarctica"))

# Transform world data to Mollweide equal area projection
world_mollweide <- st_transform(world, crs = "ESRI:54009")
world_mollweide$id <- 1

# Define raster extent and create an empty raster
ext_raster <- rast(ext(world_mollweide), resolution = 2400, crs = "ESRI:54009")
# Rasterize directly to disk
r_mask <- rasterize(world_mollweide, ext_raster, field = "id")

# MERGE TILES TOGETHER
rlist <- list.files(
  file.path("E:/data_sharing_sparing/land_use_change/agric_intensity/2000"),
  pattern = "\\.tif$", full.names = TRUE, recursive = TRUE
)
agric_intensity <- mosaic(sprc(lapply(rlist, rast)), fun = "max")
names(agric_intensity) <- "agric_intensity"

agric_intensity_mollweide <- project(agric_intensity, r_mask, method = "bilinear") %>% mask(., r_mask)
plot(agric_intensity_mollweide)

writeRaster(
  agric_intensity_mollweide,
  "E:/data_sharing_sparing/land_use_change/agric_intensity/global_intensity_2000_2400m_mollweide.tif", 
  overwrite = TRUE
  )




