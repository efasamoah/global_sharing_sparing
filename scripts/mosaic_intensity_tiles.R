
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
# Rasterize directly to disk
ext_raster <- rast(ext(world_mollweide), resolution = 2400, crs = "ESRI:54009")
r_mask <- rasterize(world_mollweide, ext_raster, field = "id")

main_dir <- "E:/QUT_SHARING_SPARING"
# change when using RDSS
# main_dir <- U:\ULVCSK5231\Analyses_2026

# MERGE TILES TOGETHER
target_res_m = 2400

lapply(c(2000, 2005, 2010, 2015, 2020), function(year){
  out <- file.path(main_dir, 
            "land_use_change/agric_intensity",
            paste0("global_intensity_", year, "_", target_res_m, "_mollweide.tif"))
  if (file.exists(out)){
    message("Global agricuture intensity for ", year," - already processed!")
    NULL
  }
  
  rlist <- list.files(
    file.path(main_dir, "land_use_change/agric_intensity", year),
    pattern = "\\.tif$", full.names = TRUE, recursive = TRUE
  )
  agric_intensity <- mosaic(sprc(lapply(rlist, rast)), fun = "sum")
  agric_intensity <- clamp(agric_intensity, lower = 0, upper = 1 )
  names(agric_intensity) <- paste0("agric_intensity_", year)
  
  # Project 
  agric_intensity_mollweide <- project(
    agric_intensity, r_mask, method = "bilinear") %>%
    mask(., r_mask)
  
  plot(agric_intensity_mollweide, main = names(agric_intensity_mollweide))
  writeRaster(agric_intensity_mollweide, out, overwrite = TRUE)
  
  message("Mosaicking agric intensity complete for ", year, " !!!")
  return(agric_intensity_mollweide)
})



