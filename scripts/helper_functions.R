
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

