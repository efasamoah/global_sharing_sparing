
# Function to process one year
agriculture_intensity_process <- function(year){
  
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

# Jonathan's classify function
classify_spare_share <- function(x, n_boot = 1000, conf_level = 0.95, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  
  x <- ifelse(x == 0, 0.001, x)
  x <- ifelse(x == 1, 0.999, x)			
  
  fit <- fitdist(x, "beta", method = "mle")
  alpha_hat <- fit$estimate["shape1"]
  beta_hat  <- fit$estimate["shape2"]
  
  boot <- bootdist(fit, niter = n_boot)
  alpha_boot <- boot$estim[, "shape1"]
  beta_boot <- boot$estim[, "shape2"]
  
  p_sparing <- mean(alpha_boot < 1 & beta_boot < 1)
  test_sparing <- p_sparing > conf_level
  
  p_sharing <- mean(alpha_boot > 1 & beta_boot > 1)
  test_sharing <- p_sharing > conf_level
  
  classification <- if (test_sparing) {
    "sparing"
  } else if (test_sharing) {
    "sharing"
  } else {
    "neither"
  }
  
  return(classification)
}

# Function to get full details for histogram
get_beta_details <- function(x, n_boot = 500, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  
  x <- ifelse(x == 0, 0.001, x)
  x <- ifelse(x == 1, 0.999, x)
  
  fit <- fitdist(x, "beta", method = "mle")
  alpha_hat <- fit$estimate["shape1"]
  beta_hat <- fit$estimate["shape2"]
  
  boot <- bootdist(fit, niter = n_boot)
  alpha_boot <- boot$estim[, "shape1"]
  beta_boot <- boot$estim[, "shape2"]
  
  p_sparing <- mean(alpha_boot < 1 & beta_boot < 1)
  p_sharing <- mean(alpha_boot > 1 & beta_boot > 1)
  
  return(list(
    alpha = alpha_hat,
    beta = beta_hat,
    p_sparing = p_sparing,
    p_sharing = p_sharing
  ))
}

# Main function
global_share_spare_pipeline <- function(year) {
  
  # Initialize empty results list
  results_list <- lapply(seq_along(gridID), function(i){
    
    id <- gridID[i]
    
    # Print progress every 10 grids (adjust frequency as needed)
    if(i %% 10 == 0) {
      cat(sprintf("  [%s] Progress: %d/%d (%.1f%%) - Current ID: %s\n", 
                  Sys.time(), i, length(gridID), (i/length(gridID))*100, id))
    }
    
    planning_units <- st_read(globalFishnetPath, quiet = TRUE)
    focalGrid <- subset(planning_units, PageName == id)
    focalGridBuffered <- st_buffer(focalGrid, grid_size)
    
    # Extract values
    agric_intensity <- rast(grep(year, globalIntensityDataPath, value = TRUE))
    intensity_per_grid <- crop(agric_intensity, focalGridBuffered)
    grid_values <- values(intensity_per_grid)
    grid_values <- grid_values[!is.na(grid_values)]
    
    n_valid <- length(grid_values)
    mean_int <- if(n_valid > 0) mean(grid_values) else NA
    
    # NOW filter at grid level: mean >10% AND enough data
    if(n_valid > 20 && !is.na(mean_int) && mean_int > 0.1) {
      
      # Classify using Jonathan's function
      tryCatch({
        classification <- classify_spare_share(grid_values, n_boot=500, seed=123)
        # Get details for histogram
        beta_details <- get_beta_details(grid_values, n_boot=500, seed=123)
        
        # Return single row data.frame
        data.frame(
          grid_id = id,
          n_pixels = n_valid,
          mean_intensity = mean_int,
          classification = classification,
          alpha = beta_details$alpha,
          beta = beta_details$beta,
          p_sparing = beta_details$p_sparing,
          p_sharing = beta_details$p_sharing,
          stringsAsFactors = FALSE
        )
        
      }, error = function(e) {
        cat(sprintf("  [ERROR] Grid %s failed: %s\n", id, e$message))
        data.frame(
          grid_id = id, 
          n_pixels = n_valid, 
          mean_intensity = mean_int,
          classification = "error",
          alpha = NA, 
          beta = NA,
          p_sparing = NA, 
          p_sharing = NA,
          stringsAsFactors = FALSE
        )
      })
      
    } else {
      # Excluded - determine reason
      if(n_valid <= 20) {
        reason <- "insufficient_data"
      } else if(is.na(mean_int)) {
        reason <- "insufficient_data"
      } else {
        reason <- "below_10%_threshold"
      }
      
      data.frame(
        grid_id = id,
        n_pixels = n_valid,
        mean_intensity = ifelse(is.na(mean_int), NA, mean_int),
        classification = reason,
        alpha = NA, 
        beta = NA, 
        p_sparing = NA, 
        p_sharing = NA,
        stringsAsFactors = FALSE
      )
    }
  })
  
  # Combine all results
  cat(sprintf("  [%s] Combining results...\n", Sys.time()))
  all_Results <- do.call(rbind, results_list)
  
  # Write results
  output_file <- file.path(main_dir, paste0("global_share_spare_", year, "_60km_results.csv"))
  write.csv(all_Results, output_file, row.names = FALSE)
  
  return(all_Results)
}

