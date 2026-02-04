# === GLOBAL AGRICULTURE INTENSITY BY TILES ===

# Dr. Ernest Frimpong Asamoah
# Last updated 28 January 2026

# (1) This code classifies Global Forest Change's land-use and land cover maps by isolating removing built up, permanent forest, pure deserts and water bodies. 
# (2) For any remaining areas not classified as cropland (natural areas), we assigned either cultivated grassland based on Global Pasture Watch's 30m resolution data
# (3) We then calculate agriculture intensity for fraction of each 2400m x 2400m grid covered by agriculture (crop + pasture) 

print("=== GLOBAL AGRICULTURE INTENSITY BY TILES ===")

library(terra)
library(glue)
library(future.apply)
library(future)

main_dir <- "E:/QUT_SHARING_SPARING"
# change when using RDSS
# main_dir <- "U:/Research/Projects/ULVCSK5231/Analyses_2026"

# import helper functions
source("./scripts/helper_functions.R")

years <- c(2000, 2005, 2010, 2015, 2020)
target_res_m <- 1200

cultivated_files <- list.files(
  file.path(main_dir, "land_use_change/cultivated"), 
  pattern = "\\.tif$", 
  full.names = TRUE
)
print(cultivated_files)


# Process years sequentially, tiles in parallel
plan(multisession, workers = ceiling(availableCores()/2))

# Clear the custom temp folder
file.remove(list.files(file.path(main_dir, "terra_tmp"), full.names = T))

for(year in years){
  
  OutPutFolder <- file.path(main_dir, "land_use_change/agric_intensity", target_res_m, year)
  if(!dir.exists(OutPutFolder)){
    dir.create(OutPutFolder, recursive = TRUE)
  }
  #  Run main function
  agriculture_intensity_process(year, OutPutFolder)
  terra::tmpFiles(current = TRUE, remove = TRUE)
  gc()
}
plan(sequential)



