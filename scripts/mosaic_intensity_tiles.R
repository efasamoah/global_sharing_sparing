
library(terra)
library(tidyverse)
library(rnaturalearth)
library(sf)


virtualMachine = FALSE

if (virtualMachine) {
  main_dir <- "U:/Research/Projects/ULVCSK5231/Analyses_2026"
  n_workers <- ceiling(availableCores()-2)
  
} else {
  main_dir <- "E:/QUT_SHARING_SPARING"
  n_workers <- ceiling(availableCores()/2)
}


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


# MERGE TILES TOGETHER
target_res_m = 1200

lapply(c(2000,2020), function(year){
  
  for(target_res_m in c(1200, 2400)){
    
    out <- file.path(main_dir, 
                     "land_use_change/agric_intensity", target_res_m,
                     paste0("global_intensity_", year, "_", target_res_m, "_mollweide.tif"))
    if (file.exists(out)){
      message("Global agricuture intensity for ", year, "_", target_res_m," - already processed!")
      next
    }
    
    rlist <- list.files(
      file.path(main_dir, "land_use_change/agric_intensity", target_res_m, year),
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
  }
  
})



