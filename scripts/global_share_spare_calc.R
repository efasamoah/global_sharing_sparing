# === GLOBAL LAND SHARING AND SPARING INDICES ===

# Dr. Ernest Frimpong Asamoah
# Last updated 14 February 2026

rm(list = ls())

library(terra)
library(fitdistrplus)
library(sf)
library(future.apply)

virtualMachine = FALSE

if (virtualMachine) {
  main_dir <- "U:/Research/Projects/ULVCSK5231/Analyses_2026"
  n_workers <- ceiling(availableCores()-2)
  
} else {
  main_dir <- "E:/QUT_SHARING_SPARING"
  n_workers <- ceiling(availableCores()/2)
}

testSite <- "global"

# Focal Grid Sizes
planning_grids <- expand.grid(
  x = c(1200, 2400), # grid size (600m, 1200m, or 2400m)
  y = c(60) # planning grids (30km or 60km)
)

# import helper functions
source("./scripts/helper_functions.R")

# Make sure the output folder exist
outfolder <- file.path(main_dir, "share_spare_results", testSite)
if(!dir.exists(outfolder)){
  dir.create(outfolder, recursive = TRUE)
}

# Process years sequentially, tiles in parallel
plan(multisession, workers = n_workers)
years <- c(2000, 2020)

print("=== GLOBAL LAND SHARING AND SPARING INDICES ===")
print(paste("Started at:", Sys.time()))
strttime <- Sys.time()

for (id in 1:nrow(planning_grids)){
  
  planning_unit_size <- planning_grids[id,]$y
  grid_size <- planning_grids[id,]$x
  
  FishnetPath <- file.path(
    main_dir, "fishnet", 
    glue::glue("{testSite}_{planning_unit_size}km_fishnet.shp")
  )
  fishnet_polygon <- st_read(FishnetPath, quiet = TRUE)
  gridID <- unique(fishnet_polygon$PageName)
  
  globalIntensityDataPath <- list.files(
    file.path(main_dir, "land_use_change/agric_intensity", grid_size),
    pattern = "\\.tif$", full.names = TRUE
  )
  
  for(year in years){
    
    out <- file.path(
      outfolder, 
      paste0(testSite,"_share_spare_",year,"_",grid_size,"_", planning_unit_size, "km_results.csv")
    )
    
    if(file.exists(out)){
      message(paste0(out," already exists. Skipping."))
      next
    }
    # Run the main function
    global_share_spare_pipeline(
      year = year, 
      output_file = out, 
      IntensityPath = globalIntensityDataPath,
      globalFishnetPath = FishnetPath
    )
    # Clear the temp file
    terra::tmpFiles(current = TRUE, remove = TRUE)
    gc()
  }
}

# Close parallel workers AFTER all loops complete
plan(sequential)

# FIX: Correct time calculation
elapsed_time <- Sys.time() - strttime
print(paste("\nFinished at:", Sys.time()))
print(paste("Total elapsed time:", round(elapsed_time, 2), units(elapsed_time)))
