# === GLOBAL LAND SHARING AND SPARING INDICES ===

# Dr. Ernest Frimpong Asamoah
# Last updated 5 February 2026

library(terra)
library(fitdistrplus)
library(sf)
library(future.apply)


rm(list = ls())

main_dir <- "E:/QUT_SHARING_SPARING"
# main_dir <- "U:/Research/Projects/ULVCSK5231/Analyses_2026"

testSite <- "test"
planning_grid = 60
subgrid = 2400

# import helper functions
source("./scripts/helper_functions.R")

# Make sure the output folder exist
outfolder <- file.path(main_dir, "share_spare_results", testSite)
if(!dir.exists(outfolder)){
  dir.create(outfolder, recursive = TRUE)
}

# Process years sequentially, tiles in parallel
plan(multisession, workers = ceiling(availableCores()/2))

print("=== GLOBAL LAND SHARING AND SPARING INDICES ===")
print(paste("Started at:", Sys.time()))
strttime <- Sys.time()

globalIntensityDataPath <- list.files(
  file.path(main_dir, "land_use_change/agric_intensity", testSite),
  pattern = "\\.tif$", full.names = TRUE, recursive = TRUE
)
globalIntensityDataPath <- grep(subgrid, globalIntensityDataPath, value = TRUE)
print(globalIntensityDataPath)

for(Path in globalIntensityDataPath){
  
  out <- file.path(
    outfolder, 
    paste0("share_spare_", paste0(tools::file_path_sans_ext(basename(Path))),
           paste0(planning_grid, "km"), "_results.csv")
  )
  
  if(file.exists(out)){
    message(paste0(out," already exists. Skipping."))
    next
  }
  
  # Run the main function
  test_tile_share_spare(
    output_file = out, 
    IntensityPath = Path,
    gen_fishnet = TRUE, 
    projectFolder = outfolder,
    planning_unit_size = planning_grid,
    subgrid = 2400
  )
  # Clear the temp file
  terra::tmpFiles(current = TRUE, remove = TRUE)
  gc()
}

# Close parallel workers
plan(sequential)

# FIX: Correct time calculation
elapsed_time <- Sys.time() - strttime
print(paste("\nFinished at:", Sys.time()))
print(paste("Total elapsed time:", round(elapsed_time, 2), units(elapsed_time)))

