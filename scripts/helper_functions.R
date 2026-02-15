
# Function to process one year
agriculture_intensity_process <- function(
    year, 
    OutPutFolder, 
    landcover_files, 
    cell_size, 
    cultivated_files){
  
  message(glue("Starting year {year}"))
  
  if(length(landcover_files) == 0){
    warning(glue("No files found for year {year}"))
    return(NULL)
  }
  
  future_lapply(landcover_files, function(k){
    
    tryCatch({
      # Set terra options
      terraOptions(todisk = TRUE, tempdir = file.path(main_dir, "terra_tmp"))
      
      # Set path
      OutputFilePath <- file.path(
        OutPutFolder, 
        paste0(tools::file_path_sans_ext(basename(k)),"_",cell_size,"m.tif")
      )
      # Skip if already exists
      if(file.exists(OutputFilePath)){
        message(glue("Skipping {basename(k)} - already processed"))
        return(OutputFilePath)
      }
      
      message(glue("Processing {basename(k)}..."))
      # Read cultivated grassland raster
      cultivated_grassland_file <- grep(paste0("c_30m_", year), cultivated_files, value = TRUE)
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
      cult_grass_binned <- ifel(cult_grass == 1, 1, NA)
      cultivated_resampled <- resample(cult_grass_binned, glclu_tile, method = "near")
      
      # Merge: where glclu_tile is 0 and cultivated has data, use cultivated value
      output_tile <- ifel(glclu_tile == 0 & cultivated_resampled == 1, 1, glclu_tile)
      
      landcover_proj <- project(output_tile, "ESRI:54009", res = 30, method = "near")
      agg_factor <- round(cell_size / res(landcover_proj)[1])
      
      # Convert to proportion (0-1 range)
      message(glue("Aggregation factor: {agg_factor}"))
      intensity_proportions <- aggregate(landcover_proj, fact = agg_factor, fun = "mean", na.rm = TRUE)
      names(intensity_proportions) <- "agric_intensity"
      
      # Write to disk
      writeRaster(intensity_proportions, 
                  OutputFilePath, 
                  overwrite = TRUE,
                  gdal = c("COMPRESS=LZW", "TILED=YES")
      )
      message(glue("Completed {basename(k)}"))
      
      return(OutputFilePath)
    }, error = function(e){
      message(glue("Error processing {basename(k)}: {e$message}"))
      return(NULL)
    })
    
    # Clean up terra temp files inside the worker
    terra::tmpFiles(current = TRUE, orphan = FALSE, remove = TRUE)
    
  }, future.seed = TRUE, 
  future.scheduling = 2, 
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
  
  return(list(
    classification = classification,
    alpha = alpha_hat,
    beta = beta_hat,
    p_sparing = p_sparing,
    p_sharing = p_sharing
  ))
}


# Main function
global_share_spare_pipeline <- function(year,
                                        id_var = "PageName",
                                        output_file, 
                                        IntensityPath, 
                                        FishnetPath) {
  
  
  fishnet_polygon <- st_read(FishnetPath, quiet = TRUE)
  gridID <- unique(fishnet_polygon[, id_var])
  
  # Initialize empty results list
  results_list <- future_lapply(seq_along(gridID), function(i){
    
    id <- gridID[i]
    
    # Print progress every 20 grids
    if(i %% 20 == 0) {
      cat(sprintf("  [%s] Progress: %d/%d (%.1f%%) - Current ID: %s\n", 
                  Sys.time(), i, length(gridID), (i/length(gridID))*100, id))
    }
    
    # *** WRAP ENTIRE GRID PROCESSING IN tryCatch ***
    result <- tryCatch({
      
      planning_units <- st_read(FishnetPath, quiet = TRUE)
      focalGrid <- subset(planning_units, PageName == id)
      focalGridBuffered <- st_buffer(focalGrid, grid_size)
      
      # Extract values
      agric_intensity <- rast(grep(year, IntensityPath, value = TRUE))
      
      # Ensure CRS match
      if(!same.crs(agric_intensity, focalGridBuffered)) {
        focalGridBuffered <- st_transform(focalGridBuffered, crs(agric_intensity))
      }
      
      # Catch if extents don't overlap
      intensity_per_grid <- tryCatch({
        crop(agric_intensity, focalGridBuffered)
      }, error = function(e) {
        # If crop fails (e.g., no overlap), return NULL
        if(grepl("do not overlap", e$message)) {
          return(NULL)
        } else {
          stop(e)  # Re-throw if it's a different error
        }
      })
      
      # Check if crop succeeded
      if(is.null(intensity_per_grid)) {
        return(data.frame(
          grid_id = id,
          n_pixels = 0,
          mean_intensity = NA,
          classification = "no_raster_overlap",
          alpha = NA, beta = NA, p_sparing = NA, p_sharing = NA,
          stringsAsFactors = FALSE
        ))
      }
      
      grid_values <- values(intensity_per_grid)
      grid_values <- grid_values[!is.na(grid_values)]
      n_valid <- length(grid_values)
      mean_int <- if(n_valid > 0) mean(grid_values) else NA
      
      # NOW filter at grid level: mean >10% AND enough data
      # (n_valid > 20 && !is.na(mean_int) && mean_int > 0.1) # n_valid threshold has been removed after discussions with Jonathan
      if(!is.na(mean_int) && mean_int > 0.1) {
        
        # Classify using Jonathan's function
        tryCatch({
          beta_details <- classify_spare_share(grid_values, n_boot=500, seed=123)
          
          # Return single row data.frame
          data.frame(
            grid_id = id,
            n_pixels = n_valid,
            mean_intensity = mean_int,
            classification = beta_details$classification,
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
      
    }, error = function(e) {
      # *** OUTER CATCH: Catches ANY error not already handled ***
      cat(sprintf("  [FATAL ERROR] Grid %s: %s\n", id, e$message))
      data.frame(
        grid_id = id,
        n_pixels = NA,
        mean_intensity = NA,
        classification = "fatal_error",
        alpha = NA, 
        beta = NA,
        p_sparing = NA, 
        p_sharing = NA,
        stringsAsFactors = FALSE
      )
    })
    
    # Clean up terra temp files inside the worker
    terra::tmpFiles(current = TRUE, orphan = FALSE, remove = TRUE)
    
    # *** ALWAYS return the result (guaranteed to be a data.frame) ***
    return(result)
    
  }, future.seed = TRUE, 
  future.scheduling = 1, 
  future.packages = c("terra", "fitdistrplus", "sf", "glue"))
  
  # Combine all results
  cat(sprintf("  [%s] Combining results...\n", Sys.time()))
  all_Results <- do.call(rbind, results_list)
  
  # Print summary statistics
  cat(sprintf("\n[%s] RESULTS SUMMARY:\n", Sys.time()))
  cat(sprintf("  Total grids: %d\n", nrow(all_Results)))
  cat(sprintf("  Classification breakdown:\n"))
  print(table(all_Results$classification))
  
  # Write results
  write.csv(all_Results, output_file, row.names = FALSE)
  cat(sprintf("  [%s] Results written to: %s\n", Sys.time(), output_file))
  
  return(all_Results)
}


generate_tiles <- function(aoi = NULL, tile_size = 10000, lonlat = TRUE) {
  # Determine input type and extract bbox/crs
  if (is.null(aoi)) {
    if (!lonlat) {
      stop("When lonlat = FALSE, an AOI must be provided with a projected CRS")
    }
    aoi_ext <- c(xmin = -180, ymin = -90, xmax = 180, ymax = 90)
    target_crs <- 4326
    
  } else if (inherits(aoi, "SpatRaster")) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("Package 'terra' is required for SpatRaster input")
    }
    
    aoi_extent <- terra::ext(aoi)
    
    # Extract as unnamed numeric vector to avoid indexing issues
    aoi_ext <- c(
      xmin = aoi_extent[1],  # xmin
      ymin = aoi_extent[3],  # ymin
      xmax = aoi_extent[2],  # xmax
      ymax = aoi_extent[4]   # ymax
    )
    aoi_ext <- unname(aoi_ext)
    names(aoi_ext) <- c("xmin", "ymin", "xmax", "ymax")
    
    # Get CRS as WKT string for sf compatibility
    target_crs <- terra::crs(aoi)
    
    # Validate CRS matches lonlat parameter
    if (lonlat && !terra::is.lonlat(aoi)) {
      warning("lonlat = TRUE but AOI is not in geographic coordinates")
    }
    if (!lonlat && terra::is.lonlat(aoi)) {
      warning("lonlat = FALSE but AOI is in geographic coordinates")
    }
    
  } else if (inherits(aoi, "sf") || inherits(aoi, "sfc")) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package 'sf' is required for sf input")
    }
    
    aoi_ext <- sf::st_bbox(aoi)
    if (anyNA(aoi_ext)) stop("AOI bounding box contains NA values")
    target_crs <- sf::st_crs(aoi)
    
    # Validate CRS matches lonlat parameter
    if (lonlat && !sf::st_is_longlat(aoi)) {
      warning("lonlat = TRUE but AOI is not in geographic coordinates")
    }
    if (!lonlat && sf::st_is_longlat(aoi)) {
      warning("lonlat = FALSE but AOI is in geographic coordinates")
    }
    
  } else {
    stop("aoi must be NULL, an sf/sfc object, or a SpatRaster object")
  }
  
  # Calculate tile sizes
  if (lonlat) {
    lat_tile_size <- tile_size
    lon_tile_size <- tile_size
  } else {
    lat_tile_size <- tile_size
    lon_tile_size <- tile_size
  }
  
  # Generate sequences using numeric indices to avoid named vector issues
  lon_seq <- seq(aoi_ext[1], aoi_ext[3], by = lon_tile_size)  # xmin to xmax
  lat_seq <- seq(aoi_ext[2], aoi_ext[4], by = lat_tile_size)  # ymin to ymax
  
  # Ensure full coverage
  if (tail(lon_seq, 1) < aoi_ext[3]) lon_seq <- c(lon_seq, aoi_ext[3])  # xmax
  if (tail(lat_seq, 1) < aoi_ext[4]) lat_seq <- c(lat_seq, aoi_ext[4])  # ymax
  
  # Pre-allocate list
  n_tiles <- (length(lat_seq) - 1) * (length(lon_seq) - 1)
  tiles <- vector("list", n_tiles)
  tile_idx <- 1
  
  # Create tiles
  for (i in 1:(length(lat_seq) - 1)) {
    for (j in 1:(length(lon_seq) - 1)) {
      coords <- matrix(c(
        lon_seq[j], lat_seq[i],
        lon_seq[j + 1], lat_seq[i],
        lon_seq[j + 1], lat_seq[i + 1],
        lon_seq[j], lat_seq[i + 1],
        lon_seq[j], lat_seq[i]
      ), ncol = 2, byrow = TRUE)
      
      tiles[[tile_idx]] <- sf::st_polygon(list(coords))
      tile_idx <- tile_idx + 1
    }
  }
  
  # Combine into sf object
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required to create output")
  }
  
  tiles_sf <- sf::st_sf(geometry = sf::st_sfc(tiles), crs = target_crs)
  tiles_sf$ID <- paste0("T", seq_len(nrow(tiles_sf)))
  st_crs(tiles_sf) <- target_crs
  
  return(tiles_sf)
}





test_tile_share_spare <- function(output_file = "", 
                                  IntensityPath = "",
                                  gen_fishnet = TRUE, 
                                  projectFolder = "",
                                  planning_unit_size = "", 
                                  subgrid = "") {
  
  # ===== STEP 1: GENERATE FISHNET ONCE (MAIN SESSION) =====
  cat(sprintf("[%s] Setting up fishnet...\n", Sys.time()))
  
  FishnetPath <- file.path(
    projectFolder, 
    paste0(tools::file_path_sans_ext(basename(IntensityPath)), "_", 
           paste0(planning_unit_size, "km"), "_fishnet.shp")
  )
  
  if(gen_fishnet) {
    roi <- rast(IntensityPath)
    Fishnet <- generate_tiles(aoi = roi, tile_size = planning_unit_size*1000, lonlat = FALSE)
    
    st_write(st_transform(Fishnet, st_crs(Fishnet)), FishnetPath, delete_dsn = TRUE, quiet = TRUE)
    cat(sprintf("[%s] Fishnet created: %d grids\n", Sys.time(), nrow(Fishnet)))
    
    gridID <- Fishnet$ID
    rm(Fishnet, roi)  # Clean up
    gc()
  } else {
    # Read just to get IDs
    Fishnet_temp <- st_read(FishnetPath, quiet = TRUE)
    gridID <- Fishnet_temp$ID
    rm(Fishnet_temp)
    gc()
  }
  
  cat(sprintf("[%s] Processing %d grid cells in parallel...\n", Sys.time(), length(gridID)))
  
  # ===== STEP 2: PARALLEL PROCESSING (EACH WORKER READS FILES) =====
  results_list <- future_lapply(seq_along(gridID), function(i){
    
    id <- gridID[i]
    
    # Print progress every 20 grids
    if(i %% 20 == 0) {
      cat(sprintf("  [%s] Progress: %d/%d (%.1f%%) - Current ID: %s\n", 
                  Sys.time(), i, length(gridID), (i/length(gridID))*100, id))
    }
    
    result <- tryCatch({
      
      # Read FILES FRESH IN EACH WORKER
      planning_units <- st_read(FishnetPath, quiet = TRUE)
      focalGrid <- planning_units[planning_units$ID == id, ]
      
      # Clean up immediately
      rm(planning_units)
      
      # Apply buffer
      focalGridBuffered <- st_buffer(focalGrid, subgrid)
      
      # EAD RASTER FRESH IN EACH WORKER
      agric_intensity <- rast(IntensityPath)
      
      # Ensure CRS match
      if(!st_crs(focalGridBuffered) == st_crs(agric_intensity)) {
        focalGridBuffered <- st_transform(focalGridBuffered, crs(agric_intensity))
      }
      
      # Crop raster to buffered grid
      intensity_per_grid <- tryCatch({
        crop(agric_intensity, focalGridBuffered, snap = "out")
      }, error = function(e) {
        if(grepl("do not overlap|outside|extent", e$message, ignore.case = TRUE)) {
          return(NULL)
        } else {
          stop(e)
        }
      })
      
      # Clean up raster object
      rm(agric_intensity, focalGrid, focalGridBuffered)
      
      # Check if crop succeeded
      if(is.null(intensity_per_grid) || ncell(intensity_per_grid) == 0) {
        return(data.frame(
          grid_id = id,
          n_pixels = 0,
          mean_intensity = NA,
          classification = "no_raster_overlap",
          alpha = NA, beta = NA, p_sparing = NA, p_sharing = NA,
          stringsAsFactors = FALSE
        ))
      }
      
      # Extract values
      grid_values <- values(intensity_per_grid, mat = FALSE)
      rm(intensity_per_grid)  # Clean up immediately
      
      grid_values <- grid_values[!is.na(grid_values)]
      n_valid <- length(grid_values)
      mean_int <- if(n_valid > 0) mean(grid_values) else NA
      
      # Filter: mean >10% AND enough data
      if(n_valid > 20 && !is.na(mean_int) && mean_int > 0.1) {
        
        tryCatch({
          beta_details <- classify_spare_share(grid_values, n_boot=500, seed=123)
          
          data.frame(
            grid_id = id,
            n_pixels = n_valid,
            mean_intensity = mean_int,
            classification = beta_details$classification,
            alpha = beta_details$alpha,
            beta = beta_details$beta,
            p_sparing = beta_details$p_sparing,
            p_sharing = beta_details$p_sharing,
            stringsAsFactors = FALSE
          )
          
        }, error = function(e) {
          cat(sprintf("  [ERROR] Grid %s classification failed: %s\n", id, e$message))
          data.frame(
            grid_id = id, 
            n_pixels = n_valid, 
            mean_intensity = mean_int,
            classification = "error",
            alpha = NA, beta = NA,
            p_sparing = NA, p_sharing = NA,
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
          alpha = NA, beta = NA, p_sparing = NA, p_sharing = NA,
          stringsAsFactors = FALSE
        )
      }
      
    }, error = function(e) {
      cat(sprintf("  [FATAL ERROR] Grid %s: %s\n", id, e$message))
      data.frame(
        grid_id = id,
        n_pixels = NA,
        mean_intensity = NA,
        classification = "fatal_error",
        alpha = NA, beta = NA,
        p_sparing = NA, p_sharing = NA,
        stringsAsFactors = FALSE
      )
    })
    
    # clean up terra temp files
    terra::tmpFiles(current = TRUE, remove = TRUE)
    gc(verbose = FALSE, full = TRUE)
    
    return(result)
    
  }, future.seed = TRUE, 
  future.scheduling = 1,
  future.packages = c("terra", "fitdistrplus", "sf"))
  
  # ===== STEP 3: COMBINE AND SAVE RESULTS =====
  cat(sprintf("  [%s] Combining results...\n", Sys.time()))
  all_Results <- do.call(rbind, results_list)
  
  # Print summary
  cat(sprintf("\n[%s] RESULTS SUMMARY:\n", Sys.time()))
  cat(sprintf("  Total grids: %d\n", nrow(all_Results)))
  cat(sprintf("  Classification breakdown:\n"))
  print(table(all_Results$classification))
  
  # Write results
  write.csv(all_Results, output_file, row.names = FALSE)
  cat(sprintf("  [%s] Results written to: %s\n", Sys.time(), output_file))
  
  return(all_Results)
}