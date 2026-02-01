# Estimate Beta Distribution Parameters using Maximum Likelihood Estimation (MLE)
# This is the most accurate method for parameter estimation

# Install and load required package
if (!require("fitdistrplus")) {
  install.packages("fitdistrplus")
}
library(fitdistrplus)
library(terra)
library(sf)
library(patchwork)

main_dir <- "E:/QUT_SHARING_SPARING"
data <- read.csv(file.path(main_dir, glue::glue("share_spare_results/global_share_spare_2000_60km_results.csv")))

classifications = c("neither", "sharing", "sparing")

lapply(classifications, function(class){
  
  grid_id <- subset(data, classification == class)$grid_id
  grid_id <- sample(grid_id, 5)
  
  out <- lapply(grid_id, function(id){
    
    fishnet_polygon <- st_read(file.path(main_dir, "fishnet/global_fishnet_60km.shp"), quiet = TRUE)
    focalGrid <- subset(fishnet_polygon, PageName == id)
    focalGridBuffered <- st_buffer(focalGrid, 2400)
    
    # Extract values
    intensity_per_grid <- rast(
      file.path(main_dir,"land_use_change/agric_intensity/global_intensity_2000_2400_mollweide.tif")
    ) %>% crop(., focalGridBuffered)
    
    df <- values(intensity_per_grid)
    df <- df[!is.na(df)]
    
    # Fit beta distribution using MLE
    fit <- fitdist(df, "beta", method = "mle")
    
    # Extract parameters
    alpha <- fit$estimate["shape1"]
    beta <- fit$estimate["shape2"]
    sample_mean <- mean(df)
    sample_var <- var(df)
    
    x_seq <- seq(0, 1, length.out = 1000)
    
    # Prepare data for plotting
    hist_data <- data.frame(x = df)
    x_seq <- seq(0, 1, length.out = 500)
    fitted_curve <- data.frame(
      x = x_seq,
      density = dbeta(x_seq, alpha, beta)
    )
    
    # Create the plot
    p <- ggplot() +
      # Histogram
      geom_histogram(data = hist_data, aes(x = x, y = after_stat(density)),
                     bins = 30, fill = "lightblue", color = "white", alpha = 0.7) +
      # Fitted beta curve
      geom_line(data = fitted_curve, aes(x = x, y = density),
                color = "red", size = 1.5) +
      # Labels
      labs(
        title = glue::glue("GRID: {id} ({class})"),
        subtitle = sprintf(
          "α = %.4f, β = %.4f | n = %d | Mean = %.4f, Var = %.4f",
          alpha, beta, length(data), sample_mean, sample_var
        ),
        x = "Value",
        y = "Density"
      ) +
      # Theme
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")
      ) +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
      # Add parameter annotation
      annotate("label", x = 0.8, y = max(fitted_curve$density) * 0.9,
               label = sprintf("Beta(α=%.3f, β=%.3f)", alpha, beta),
               size = 5, color = "red", fontface = "bold",
               fill = "white", label.padding = unit(0.5, "lines"))
    
    return(p)
  })
  
  combined_plot <- wrap_plots(out, 
                              ncol = 2, 
                              widths = c(1, 1),
                              heights = rep(1, ceiling(length(out)/2))) +
    plot_layout(guides='collect') & theme(legend.position='bottom')
  
  # Save combined plot
  ggsave(plot = combined_plot,
         filename = file.path(main_dir, glue::glue("share_spare_results/figures/test_dist_{class}.png")),
         dpi = 300,
         width = 14,
         height = 8,
         units = "in",
         bg = "white")
  print(combined_plot)
})
