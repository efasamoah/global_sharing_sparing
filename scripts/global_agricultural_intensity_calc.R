# === GLOBAL AGRICULTURE INTENSITY BY TILES ===

# Dr. Ernest Frimpong Asamoah
# Last updated 14 February 2026

# (1) This code classifies Global Forest Change's land-use and land cover maps by isolating removing built up, permanent forest, pure deserts and water bodies. 
# (2) For any remaining areas not classified as cropland (natural areas), we assigned either cultivated grassland based on Global Pasture Watch's 30m resolution data
# (3) We then calculate agriculture intensity for fraction of each 2400m x 2400m grid covered by agriculture (crop + pasture) 


rm(list = ls())

library(terra)
library(glue)
library(future.apply)
library(future)

virtualMachine = FALSE

if (virtualMachine) {
  main_dir <- "U:/Research/Projects/ULVCSK5231/Analyses_2026"
  n_workers <- ceiling(availableCores()-2) 
  # the VM has only 16 cores. use 14
  
} else {
  main_dir <- "E:/QUT_SHARING_SPARING"
  n_workers <- ceiling(availableCores()/2) 
  # Personal laptop has 32 cores but RAM can support only half
}

# import helper functions
source("./scripts/helper_functions.R")

years <- c(2000, 2020)
planning_units <- c(1200, 2400)

grassland_paths <- list.files(
  file.path(main_dir, "land_use_change/cultivated"), 
  pattern = "\\.tif$", 
  full.names = TRUE
)
print(grassland_paths)

testSite <- "global"

# Process years sequentially, tiles in parallel
plan(multisession, workers = n_workers)

# Clear the custom temp folder
file.remove(list.files(file.path(main_dir, "terra_tmp"), full.names = T))

lapply(planning_units, function(target_res_m) {
  
  for(year in years){
    
    site_subdir <- switch(testSite,
           "global" = "global",
           "test" = "test"
    )
    
    output_directory <- file.path(main_dir, 
                                  "land_use_change/agric_intensity", 
                                  site_subdir, 
                                  target_res_m, 
                                  year
    )
    if(!dir.exists(output_directory)){
      dir.create(output_directory, recursive = TRUE)
    }
    
    landcoverPath <- list.files(
      file.path(main_dir, "land_use_change/landcover", 
                site_subdir, year), 
      pattern = "\\.tif$", 
      full.names = TRUE
    )
    #  Run main function
    agriculture_intensity_process(
      year, 
      OutPutFolder = output_directory, 
      landcover_files = landcoverPath,
      cell_size = target_res_m, 
      cultivated_files = grassland_paths)
  }
  terra::tmpFiles(current = TRUE, remove = TRUE)
  gc()
})

plan(sequential)

