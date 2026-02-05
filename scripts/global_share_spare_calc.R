# === GLOBAL LAND SHARING AND SPARING INDICES ===

# Dr. Ernest Frimpong Asamoah
# Last updated 5 February 2026

library(terra)
library(fitdistrplus)
library(sf)
library(future.apply)

# main_dir <- "E:/QUT_SHARING_SPARING"
main_dir <- "U:/Research/Projects/ULVCSK5231/Analyses_2026"

globalFishnetPath <- file.path(main_dir, "fishnet/global_fishnet_60km.shp")
fishnet_polygon <- st_read(globalFishnetPath, quiet = TRUE)
gridID <- unique(fishnet_polygon$PageName)
length(gridID)

# gridID <- sample(gridID, 1000)

# import helper functions
source("./scripts/helper_functions.R")

# Focal Grid Size
grid_size <- 1200

globalIntensityDataPath <- list.files(
  file.path(main_dir, "land_use_change/agric_intensity", grid_size),
  pattern = "\\.tif$", full.names = TRUE
)

# Make sure the output folder exist
outfolder <- file.path(main_dir, "share_spare_results")
if(!dir.exists(outfolder)){
  dir.create(outfolder, recursive = TRUE)
}


# Process years sequentially, tiles in parallel
plan(multisession, workers = ceiling(availableCores()-2))

# Run
years <- c(2000, 2005, 2010, 2015, 2020)

print("=== GLOBAL LAND SHARING AND SPARING INDICES ===")
print(paste("Started at:", Sys.time()))
strttime <- Sys.time()

for(year in years){
  
  out <- file.path(
    outfolder, 
    paste0("global_share_spare_",year,"_",grid_size,"_60km_results.csv")
    )
  
  if(file.exists(out)){
    message(paste0(out," already exists. Skipping."))
    next
  }
  
  global_share_spare_pipeline(
    year = year, 
    output_file = out, 
    IntensityPath = globalIntensityDataPath
  )
  
  terra::tmpFiles(current = TRUE, remove = TRUE)
  gc()
}

plan(sequential)
print(paste("\n\nFinished at:", strttime - Sys.time()))
