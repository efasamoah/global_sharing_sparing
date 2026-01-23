
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

