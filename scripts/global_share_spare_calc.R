
print("=== GLOBAL LAND SHARING AND SPARING INDICES ===")

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

globalIntensityDataPath <- list.files(
  file.path(main_dir, "land_use_change/agric_intensity"),
  pattern = "\\.tif$", full.names = TRUE
  )

# import helper functions
source("./scripts/helper_functions.R")

years <- c(2000, 2005, 2010, 2015, 2020)
grid_size <- 2400

# Make sure the output folder exist
outfolder <- file.path(main_dir, "share_spare_results")
if(!dir.exists(outfolder)){
  dir.create(outfolder, recursive = TRUE)
}

# Start 
print(paste("Started at:", Sys.time()))

# Process years sequentially, tiles in parallel
plan(multisession, workers = ceiling(availableCores()-2))

# Run
for(year in years){
  
  output_file <- file.path(outfolder, paste0("global_share_spare_", year, "_60km_results.csv"))
  
  if(file.exists(output_file)){
    message(paste0("global_share_spare_", year, "_60km_results.csv already exists. Skipping."))
    next
  }
  
  global_share_spare_pipeline(year, output_file)
  terra::tmpFiles(current = TRUE, remove = TRUE)
  gc()
}

plan(sequential)
print(paste("\n\nFinished at:", Sys.time()))
