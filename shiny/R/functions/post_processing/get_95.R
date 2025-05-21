get_95CI <- function(x, method = c("normal", "t.test", "bootstrap"), conf_level = 0.95, n_boot = 1000, type) {
  method <- match.arg(method)
  
  if (!is.numeric(x)) stop("Input must be a numeric vector.")
  x <- x[!is.na(x)]
  if (length(x) < 2) stop("Need at least two non-missing values.")
  
  alpha <- 1 - conf_level
  mean_x <- mean(x)
  
  if (method == "normal") {
    se_x <- sd(x) / sqrt(length(x))
    z <- qnorm(1 - alpha / 2)
    ci <- c(mean_x - z * se_x, mean_x + z * se_x)
    
  } else if (method == "t.test") {
    ci <- t.test(x, conf.level = conf_level)$conf.int
    
  } else if (method == "bootstrap") {
    boot_means <- replicate(n_boot, mean(sample(x, replace = TRUE)))
    ci <- quantile(boot_means, probs = c(alpha / 2, 1 - alpha / 2))
  }
  
  if(type == "mean"){
    mean_x
  } else if(type == "low"){
    ci[1]
  } else if(type == "high"){
    ci[2]
  }
}