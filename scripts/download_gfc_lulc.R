download_glucc <- function (tiles, 
                            output_folder, 
                            year = c("2000", "2005", "2010", "2015", "2020"), 
                            dataset = "GLCLU2000-2020", 
                            version = "v2") 
{
  stopifnot(all(year %in% c("2000", "2005", "2010", "2015", "2020")))
  
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  message(paste(nrow(tiles), "tiles to download/check."))
  
  successes <- 0
  failures <- 0
  skips <- 0
  
  for (n in 1:nrow(tiles)) {
    gfc_tile <- tiles[n, ]
    min_x <- st_bbox(gfc_tile)[1]
    max_y <- st_bbox(gfc_tile)[4]
    
    if (min_x < 0) {
      min_x <- paste0(sprintf("%03i", abs(min_x)), "W")
    } else {
      min_x <- paste0(sprintf("%03i", min_x), "E")
    }
    
    if (max_y < 0) {
      max_y <- paste0(sprintf("%02i", abs(max_y)), "S")
    } else {
      max_y <- paste0(sprintf("%02i", max_y), "N")
    }
    
    file_suffix <- paste0(max_y, "_", min_x, ".tif")
    
    # Correct URL structure: .../v2/2000/40N_080W.tif
    tile_url <- paste0("https://storage.googleapis.com/earthenginepartners-hansen/", 
                       dataset, "/", version, "/", year, "/", file_suffix)
    
    local_path <- file.path(output_folder, file_suffix)
    
    if (file.exists(local_path)) {
      message(paste(basename(local_path), "already exists - skipping"))
      skips <- skips + 1
      next
    }
    
    message(paste("Downloading:", tile_url))
    
    tryCatch({
      download.file(tile_url, local_path, mode = "wb", quiet = FALSE)
      successes <- successes + 1
      message(paste(basename(local_path), "downloaded successfully"))
    }, error = function(e) {
      failures <- failures + 1
      message(paste("Failed to download", basename(local_path), ":", e$message))
    })
  }
  
  message(paste(successes, "file(s) succeeded,", skips, "file(s) skipped,", 
                failures, "file(s) failed."))
}

library(tidyverse)
library(terra)
library(sf)
library(glue)

main_dir <- "U:/ULVCSK5231/Analyses_2026"

for (focal_year in c(2000, 2005, 2010, 2015, 2020)) {
  output_dir <- file.path(getwd(), glue("land_use_change/gfc_lulc/yr{focal_year}"))
  
  download_glucc(gfc_tiles,
                 output_folder = output_dir, 
                 year = focal_year, 
                 dataset = "GLCLU2000-2020", 
                 version = "v2")
}
