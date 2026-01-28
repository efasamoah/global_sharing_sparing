library(terra)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(magick)

# Get world data
world <- ne_countries(scale = "medium", returnclass = "sf")
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



agric_intensity <- rast(list.files("E:/QUT_SHARING_SPARING/land_use_change/agric_intensity", pattern = "\\.tif$", full.names = TRUE))
df <- as.data.frame(agric_intensity, xy = TRUE, na.rm = TRUE)
head(df)


results <- lapply(c(2000, 2005, 2010, 2015), function(k){
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

combined_plot <- wrap_plots(results, ncol = 2, 
                            widths = c(1, 1),
                            heights = rep(1, ceiling(length(results)/2))) +
  plot_layout(guides='collect') & theme(legend.position='bottom')

# Save combined plot
ggsave(plot = combined_plot,
       filename = "E:/QUT_SHARING_SPARING/share_spare_results/figures/global_agric_intensity.png",
       dpi = 1200,
       width = 14,
       height = 8,
       units = "in",
       bg = "white")



