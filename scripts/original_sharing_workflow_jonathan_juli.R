## 25/11/2025 
## get the fishnet so can see the patterns underneath

library(terra)
library(fitdistrplus)
library(sf)

print("=== QLD ANALYSIS - EXACT ORIGINAL WORKFLOW ===")
print(paste("Started at:", Sys.time()))

# Main function - EXACT original workflow
process_qld_original_workflow <- function(tile_path, output_dir = "qld_correct") {
  
  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  hist_dir <- file.path(output_dir, "histograms")
  if(!dir.exists(hist_dir)) dir.create(hist_dir, recursive = TRUE)
  
  print("\n=== STEP 1: LOAD DATA ===")
  print("Loading 300m data...")
  raster_300m <- rast(tile_path)
  print(paste("300m raster:", nrow(raster_300m), "x", ncol(raster_300m)))
  
  # Get spatial info
  raster_crs <- crs(raster_300m)
  
  print("\n=== STEP 2: AGGREGATE TO 600m ===")
  print("NO FILTERING YET - aggregate first!")
  raster_600m <- aggregate(raster_300m, fact=2, fun=mean, na.rm=TRUE)
  
  rows_600m <- nrow(raster_600m)
  cols_600m <- ncol(raster_600m)
  res_600m <- res(raster_600m)[1]
  ext_600m <- ext(raster_600m)
  
  print(paste("600m raster:", rows_600m, "x", cols_600m))
  print(paste("Resolution:", res_600m, "meters"))
  
  # Save 600m raster
  writeRaster(raster_600m, 
              file.path(output_dir, "qld_600m_base.tif"),
              overwrite=TRUE)
  
  # Convert to matrix
  raster_600m_matrix <- matrix(values(raster_600m), nrow=rows_600m, ncol=cols_600m)
  
  print("\n=== STEP 3: DIVIDE INTO 60km GRIDS ===")
  print("Using floor() like original...")
  
  grid_size_cells <- 100
  grid_size_meters <- 60000
  
  n_grids_y <- floor(rows_600m / grid_size_cells)
  n_grids_x <- floor(cols_600m / grid_size_cells)
  
  print(paste("Grid matrix:", n_grids_y, "rows x", n_grids_x, "columns"))
  print(paste("Total grids:", n_grids_y * n_grids_x))
  
  print("\n=== STEP 4: PROCESS EACH GRID ===")
  print("Filter at GRID level (mean >10%), then classify")
  print("This will take 15-30 minutes...")
  
  results <- data.frame()
  grid_count <- 0
  histogram_count <- 0
  
  for(i in 1:n_grids_y) {
    for(j in 1:n_grids_x) {
      
      grid_count <- grid_count + 1
      grid_id <- paste0("grid_", i, "_", j)
      
      # Calculate cell indices
      row_start <- (i-1) * grid_size_cells + 1
      row_end <- i * grid_size_cells
      col_start <- (j-1) * grid_size_cells + 1
      col_end <- j * grid_size_cells
      
      # Calculate geographic coordinates
      xmin <- ext_600m[1] + (col_start - 1) * res_600m
      xmax <- ext_600m[1] + col_end * res_600m
      ymax <- ext_600m[4] - (row_start - 1) * res_600m
      ymin <- ext_600m[4] - row_end * res_600m
      
      # Extract ALL values (no filtering yet!)
      grid_values <- raster_600m_matrix[row_start:row_end, col_start:col_end]
      grid_values <- as.vector(grid_values)
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
          
          # Create histogram
          png_file <- file.path(hist_dir, paste0(grid_id, "_histogram.png"))
          png(png_file, width=800, height=600)
          
          hist_color <- switch(classification,
                               "sparing" = "lightcoral",
                               "sharing" = "lightblue",
                               "neither" = "lightgray",
                               "lightgray")
          border_color <- switch(classification,
                                 "sparing" = "darkred",
                                 "sharing" = "darkblue",
                                 "neither" = "black",
                                 "black")
          
          hist(grid_values, breaks=30, probability=TRUE,
               main=paste0(grid_id, " (Row ", i, ", Col ", j, "): ",
                           toupper(classification),
                           "\nα=", round(beta_details$alpha, 3),
                           ", β=", round(beta_details$beta, 3),
                           ", P(sparing)=", round(beta_details$p_sparing, 3)),
               xlab="Agricultural Intensity (600m cells, 0-1)",
               col=hist_color, border=border_color, xlim=c(0, 1))
          
          # Add beta curve
          x_seq <- seq(0.001, 0.999, length.out=500)
          y_seq <- dbeta(x_seq, beta_details$alpha, beta_details$beta)
          hist_data <- hist(grid_values, breaks=30, plot=FALSE)
          max_density <- max(hist_data$density)
          y_seq_scaled <- y_seq * (max_density / max(y_seq)) * 0.95
          lines(x_seq, y_seq_scaled, col=border_color, lwd=3)
          
          text(0.02, max_density*0.95, 
               paste0("n = ", n_valid, " pixels\nmean = ", round(mean_int, 3)),
               adj=c(0, 1), cex=1.1)
          
          dev.off()
          histogram_count <- histogram_count + 1
          
          # Store result
          results <- rbind(results, data.frame(
            grid_id = grid_id,
            grid_row = i,
            grid_col = j,
            row_start = row_start,
            row_end = row_end,
            col_start = col_start,
            col_end = col_end,
            xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax,
            n_pixels = n_valid,
            mean_intensity = mean_int,
            classification = classification,
            alpha = beta_details$alpha,
            beta = beta_details$beta,
            p_sparing = beta_details$p_sparing,
            p_sharing = beta_details$p_sharing,
            histogram_file = paste0(grid_id, "_histogram.png"),
            stringsAsFactors = FALSE
          ))
          
        }, error = function(e) {
          results <<- rbind(results, data.frame(
            grid_id = grid_id, grid_row = i, grid_col = j,
            row_start = row_start, row_end = row_end,
            col_start = col_start, col_end = col_end,
            xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax,
            n_pixels = n_valid, mean_intensity = mean_int,
            classification = "error",
            alpha = NA, beta = NA, p_sparing = NA, p_sharing = NA,
            histogram_file = NA, stringsAsFactors = FALSE
          ))
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
        
        results <- rbind(results, data.frame(
          grid_id = grid_id, grid_row = i, grid_col = j,
          row_start = row_start, row_end = row_end,
          col_start = col_start, col_end = col_end,
          xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax,
          n_pixels = n_valid,
          mean_intensity = ifelse(is.na(mean_int), NA, mean_int),
          classification = reason,
          alpha = NA, beta = NA, p_sparing = NA, p_sharing = NA,
          histogram_file = NA, stringsAsFactors = FALSE
        ))
      }
      
      if(grid_count %% 50 == 0) {
        print(paste("  Processed", grid_count, "/", n_grids_y * n_grids_x, 
                    "grids...", histogram_count, "histograms"))
      }
    }
  }
  
  print("\n=== STEP 5: CREATE FISHNET ===")
  
  fishnet_polygons <- list()
  for(k in 1:nrow(results)) {
    coords <- matrix(c(
      results$xmin[k], results$ymin[k],
      results$xmax[k], results$ymin[k],
      results$xmax[k], results$ymax[k],
      results$xmin[k], results$ymax[k],
      results$xmin[k], results$ymin[k]
    ), ncol=2, byrow=TRUE)
    fishnet_polygons[[k]] <- st_polygon(list(coords))
  }
  
  fishnet_sf <- st_sf(results, geometry=st_sfc(fishnet_polygons, crs=raster_crs))
  
  st_write(fishnet_sf, file.path(output_dir, "qld_60km_fishnet.shp"),
           delete_dsn=TRUE, quiet=TRUE)
  st_write(fishnet_sf, file.path(output_dir, "qld_60km_fishnet.gpkg"),
           delete_dsn=TRUE, quiet=TRUE)
  write.csv(results, file.path(output_dir, "qld_60km_results.csv"), row.names=FALSE)
  
  print("\n=== COMPLETE! ===")
  analyzed <- results[results$classification %in% c("sparing", "sharing", "neither"), ]
  print(paste("Total grids:", nrow(results)))
  print(paste("Analyzed:", nrow(analyzed)))
  print(paste("Histograms:", histogram_count))
  
  if(nrow(analyzed) > 0) {
    print("\nClassifications:")
    print(table(analyzed$classification))
  }
  
  print("\nFiles:")
  print(paste("  -", file.path(output_dir, "qld_60km_fishnet.shp")))
  print(paste("  -", file.path(output_dir, "qld_600m_base.tif")))
  print(paste("  -", file.path(output_dir, "histograms/ (", histogram_count, "files)")))
  
  return(list(results = results, fishnet = fishnet_sf))
}

# RUN IT
print("\nExpected: Mostly SPARING (like original)")

qld_correct <- process_qld_original_workflow(
  tile_path = "Data/with_cultivated_intensity_300m/bi_20S_150E_m30m_small_grid.tif",
  output_dir = "qld_correct"
)

print(paste("\n\nFinished at:", Sys.time()))
print("\n🎯 This should match your original results!")
