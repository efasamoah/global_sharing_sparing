library(terra)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(magick)

# Get world data
world <- ne_countries(scale = 50, returnclass = "sf")
world <- filter(world, !continent %in% c("Oceania Seven seas (open ocean)", "Antarctica"))

# Transform world data to Robinson projection
world_eckert_iv <- st_transform(world, crs = "ESRI:54009")

# Create a bounding box in Robinson projection
bbox <- st_bbox(world_eckert_iv)
bbox_poly <- st_as_sfc(bbox)

# bounding boxes
bbox_ne <- ne_download(
  scale = 10,
  type = "wgs84_bounding_box",
  category = "physical",
  returnclass = "sf"
)
# Transform world data to Robinson projection
bbox_eckert_iv <- st_transform(bbox_ne, crs = "ESRI:54009")
bbox_eckert_iv <- st_intersection(bbox_eckert_iv, bbox_poly)

r_template <- rast(world_eckert_iv, res = 900)

alt_spectral <- colorRampPalette(c(
  "#FFFFCC",  # Light yellow
  "#FFEDA0",  # Yellow
  "#FEB24C",  # Orange
  "#FC4E2A",  # Red-orange
  "#E31A1C",  # Red
  "#BD0026",  # Dark red
  "#800026",  # Very dark red
  "#54278F",  # Purple
  "#3F007D",  # Dark purple
  "#08519C",  # Blue
  "#08306B"   # Dark blue
))(100)
show_gradient(alt_spectral, "8. Alternative Spectral")


main_dir <- "E:/QUT_SHARING_SPARING"
# change when using RDSS
# main_dir <- "U:/Research/Projects/ULVCSK5231/Analyses_2026"

grid_size = 2400
agric_intensity <- rast(list.files(glue::glue("{main_dir}/land_use_change/agric_intensity/{grid_size}"), pattern = "\\.tif$", full.names = TRUE))
df <- as.data.frame(agric_intensity, xy = TRUE, na.rm = TRUE)
head(df)


results <- lapply(c(2000, 2005, 2010, 2015, 2020), function(k){
  col_name <- paste0("agric_intensity_", k)
  data <- cbind.data.frame(df[,c("x","y")], df[, col_name])
  colnames(data) <- c("x","y","indicator")
  
  p <- ggplot() + 
    geom_sf(data = bbox_eckert_iv, fill = NA, color = "black", linewidth = 0.5) +
    geom_sf(data = world_eckert_iv, fill = "gray90", color = NA) + 
    geom_raster(data = data, aes(x, y, fill = (indicator)), interpolate = FALSE) + 
    scale_fill_gradientn(
      name = "Agriculture intensity (%)",
      colors = alt_spectral,
      na.value = "transparent",
      limits = c(0, 1),
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(20, "lines"),
        barheight = unit(1, "lines"),
        frame.colour = "black",
        frame.linewidth = 0.3,
        ticks.colour = "black",
        ticks.linewidth = 0.5
      )
    ) + labs(title = k) + 
    geom_sf(data = world_eckert_iv, fill = NA, color = "gray40", linewidth = 0.1) +
    theme_void(base_size = 18) + 
    coord_sf(expand = FALSE) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "center",
      legend.title = element_text(hjust = 0.5, face = "bold", size = 18),
      legend.text = element_text(size = 18),
      legend.margin = margin(t = 12, b = 5, l = 0, r = 0),
      plot.margin = margin(2, 2, 2, 2),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  return(p)
})


library(patchwork)

combined_plot <- wrap_plots(results, 
                            ncol = 2, 
                            widths = c(1, 1),
                            heights = rep(1, ceiling(length(results)/2))) +
  plot_layout(guides='collect') & theme(legend.position='bottom')

# Save combined plot
ggsave(plot = combined_plot,
       filename = glue::glue(
         "{main_dir}/share_spare_results/figures/global_agric_intensity_{grid_size}m.png"
       ),
       dpi = 300,
       width = 14,
       height = 14,
       units = "in",
       bg = "white"
)





main_dir <- "E:/QUT_SHARING_SPARING"
# main_dir <- "U:/Research/Projects/ULVCSK5231/Analyses_2026"

testSite <- "global"
# takes "australia" or "global" only

fishnet_polygon <- st_read(
  file.path(main_dir, "fishnet", paste0(testSite, "_fishnet_60x60km.shp")), 
  quiet = TRUE
)
head(fishnet_polygon)

years = c(2000, 2005, 2010, 2015)
grid_size = 1200

results <- lapply(years, function(year){
  
  data <- read.csv(
    file.path(
      main_dir, 
      glue::glue("share_spare_results/{testSite}/{testSite}_share_spare_{year}_{grid_size}_60km_results.csv")
    )
  )
  
  df <- summary(factor(data$classification))[c("neither", "sharing", "sparing")]
  100 * df/sum(df)
  
  fishnet_polygon_results <- merge(fishnet_polygon, data, by.x = "PageName", by.y = "grid_id")
  
  # Dissolve fishnet by classification to remove internal borders
  fishnet_dissolved <- fishnet_polygon_results %>%
    filter(classification %in% c("sharing", "sparing", "neither")) %>%
    group_by(classification) %>%
    summarise(geometry = st_union(geometry))
  
  if(testSite == "global"){
    admin_bound <- world_eckert_iv
  } else {
    admin_bound <- subset(world_eckert_iv, admin %in% stringr::str_to_sentence(testSite))
  }
  
  plotData_sf <- st_intersection(fishnet_dissolved, admin_bound)
  
  # Plot the dissolved version
  xx <- ggplot() +
    # geom_sf(data = bbox_eckert_iv, fill = "aliceblue", color = "black", linewidth = 0.5) +
    geom_sf(data = admin_bound, fill = "gray90", color = NA) + 
    geom_sf(data = plotData_sf, aes(fill = factor(classification)), 
            colour = NA, linewidth = 0) +
    geom_sf(data = admin_bound, fill = NA, color = "gray40", linewidth = 0.1) +
    scale_fill_manual(name = "", 
                      values = c("neither" = "darkred", 
                                 "sharing" = "dodgerblue4",
                                 "sparing" = "yellow"
                                 )
                      ) + 
    theme_void(base_size = 18) + 
    coord_sf(expand = FALSE, crs = sf::st_crs(4326)) + labs(title = year) + 
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "center",
      legend.title = element_text(hjust = 0.5, face = "bold", size = 18),
      legend.text = element_text(size = 18),
      legend.margin = margin(t = 12, b = 5, l = 0, r = 0),
      plot.margin = margin(2, 2, 2, 2),
      panel.grid = element_blank(),
      panel.border = element_blank()
    )
  return(xx)
})


library(patchwork)

combined_plot <- wrap_plots(results, 
                            ncol = 2, 
                            widths = c(1, 1),
                            heights = rep(1, ceiling(length(results)/2))) +
  plot_layout(guides='collect') & theme(legend.position='bottom')

# Save combined plot
ggsave(plot = combined_plot,
       filename = glue::glue(
         "{main_dir}/share_spare_results/figures/{testSite}_share_spare_{grid_size}.png"
       ),
       dpi = 300,
       width = 14,
       height = 8,
       units = "in",
       bg = "white"
       )


# summaries 

years = c(2000, 2005, 2010, 2015, 2020)
grid_size = 2400

xx <- lapply(years, function(year){
  
  data <- read.csv(
    glue::glue(
      "{main_dir}/share_spare_results/{testSite}/{testSite}_share_spare_{year}_{grid_size}_60km_results.csv"
    ))
  # df <- summary(factor(data$classification))[c("neither", "sharing", "sparing")]
  df <- summary(factor(data$classification))
  xx <- 100 * df/sum(df)
  
  return(xx)
})

names(xx) <- years
print(xx)



