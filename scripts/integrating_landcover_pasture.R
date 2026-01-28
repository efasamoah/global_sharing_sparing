
# (1) This code classifies Global Forest Change's land-use and land cover maps by isolating removing built up, permanent forest, pure deserts and water bodies. 
# (2) For any remaining areas not classified as cropland (natural areas), we assigned either cultivated grassland based on Global Pasture Watch's 30m resolution data
# (3) We then calculate agriculture intensity for fraction of each 2400m x 2400m grid covered by agriculture (crop + pasture) 

# Dr. Ernest Frimpong Asamoah
# Last updated 28 January 2026

# Function to process one year
process_year <- function(year){
  
  message(glue("Starting year {year}"))
  
  glclu_files <- list.files(
    file.path(main_dir, "land_use_change/gfc_lulc", year), 
    pattern = "\\.tif$", 
    full.names = TRUE
  )
  if(length(glclu_files) == 0){
    warning(glue("No files found for year {year}"))
    return(NULL)
  }
  
  OutPutFolder <- file.path(main_dir, "land_use_change/agric_intensity", year)
  if(!dir.exists(OutPutFolder)){
    dir.create(OutPutFolder, recursive = TRUE)
  }

  future_lapply(glclu_files, function(k){
    
    tryCatch({
      # Set terra options
      terraOptions(todisk = TRUE, tempdir = "E:/terra_tmp")
      
      # Set path
      OutputFilePath <- file.path(
        OutPutFolder, 
        paste0(tools::file_path_sans_ext(basename(k)), "_", target_res_m, "m.tif")
      )
      # Skip if already exists
      if(file.exists(OutputFilePath)){
        message(glue("Skipping {basename(k)} - already processed"))
        return(OutputFilePath)
      }
      
      message(glue("Processing {basename(k)}..."))
      # Read cultivated grassland raster
      cultivated_grassland_file <- grep(paste0("p_30m_", year), cultivated_files, value = TRUE)
      if(length(cultivated_grassland_file) == 0){
        stop(glue("No cultivated grassland file found for year {year}"))
      }
      cultivated_grassland <- rast(cultivated_grassland_file)
      
      lulcc <- rast(k)
      # Set built-up, water bodies, pure deserts (0, 1), permanent ice () values to NA
      # Classify: 244 = grassland (becomes 1), everything else becomes 0
      NoData <- c(0, 1, 200:207, 241, 250, 254)
      lulcc[lulcc %in% NoData] <- NA
      glclu_tile <- classify(lulcc, cbind(244, 1), others = 0)
      
      # Crop to extent of lulcc
      cult_grass <- crop(cultivated_grassland, lulcc, snap = "out")
      cult_grass <- cult_grass / 100
      
      # Align raster - use resample instead of project if same CRS
      cultivated_resampled <- resample(cult_grass, glclu_tile, method = "near")
      
      # Merge: where glclu_tile is 0 and cultivated has data, use cultivated value
      output_tile <- ifel(glclu_tile == 0 & !is.na(cultivated_resampled), 
                          cultivated_resampled, 
                          glclu_tile)
      
      # Calculate aggregation factor
      original_res <- res(glclu_tile)[1]  # Get current resolution in degrees
      
      # Convert 2400m to degrees (approximate at equator: 1 degree ≈ 111,320 m)
      target_res_deg <- target_res_m / 111320
      
      # Calculate aggregation factor
      agg_factor <- round(target_res_deg / original_res)
      
      message(glue("Aggregation factor: {agg_factor}"))
      
      # Aggregate using sum
      output_2400m <- aggregate(output_tile, 
                                fact = agg_factor, 
                                fun = "sum", 
                                na.rm = TRUE)
      
      # Convert to proportion (0-1 range)
      output_2400m_fnal <- output_2400m / (agg_factor^2)
      names(output_2400m_fnal) <- "agric_intensity"
      
      # Write with compression
      writeRaster(output_2400m_fnal, 
                  OutputFilePath, 
                  overwrite = TRUE,
                  gdal = c("COMPRESS=LZW", "TILED=YES"))
      
      message(glue("Completed {basename(k)}"))
      
      return(OutputFilePath)
      
    }, error = function(e){
      message(glue("Error processing {basename(k)}: {e$message}"))
      return(NULL)
    })
    
    # Clean up terra temp files inside the worker
    terra::tmpFiles(current = TRUE, orphan = FALSE, remove = TRUE)
    
  }, future.seed = TRUE, 
  future.scheduling = 1, 
  future.packages = c("terra", "glue"))
  
  return(year)
}

print("=== GLOBAL AGRICULTURE INTENSITY BY TILES ===")

library(terra)
library(glue)
library(future.apply)

main_dir <- "E:/QUT_SHARING_SPARING"
# change when using RDSS
# main_dir <- U:\ULVCSK5231\Analyses_2026

years <- c(2000, 2005, 2010, 2015, 2020)
target_res_m <- 2400

cultivated_files <- list.files(
  file.path(main_dir, "cultivated"), 
  pattern = "\\.tif$", 
  full.names = TRUE
)

# Process years sequentially, tiles in parallel
plan(multisession, workers = 8)

# Clear the custom temp folder
file.remove(list.files("E:/terra_tmp", full.names = TRUE))

for(year in years){
  process_year(year)
  terra::tmpFiles(current = TRUE, remove = TRUE)
  gc()
}
plan(sequential)



