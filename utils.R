get_ooci_frac <- function(sd, n_sim, n_sample) {
  seeds <- round(runif(n_sim, 0, 1000))
  dfs <- data.frame()
  for (i in 1:n_sim) {
    set.seed(seeds[i])
    x_log <- rnorm(n_sample, 10)
    y_log <- 3 + x_log + rnorm(n_sample, sd=sd)
    x <- exp(x_log)
    y <- exp(y_log)
    
    model <- lm(y~x)
    df <- tidy(model, conf.int=TRUE, conf.level=0.95)
    df$iter <- i
    dfs <- rbind(dfs, df)
  }
  slopes <- filter(dfs, term=='x')
  slopes <- mutate(slopes, not_in_ci=(true_val < conf.low) | (true_val > conf.high)) 
  frac_not_in_ci <- mean(slopes$not_in_ci)
  return(frac_not_in_ci)
}

plot_resid_qq <- function(model) {
  ggplot() + 
    geom_qq(aes(sample=model$residuals), color='dodgerblue') + 
    geom_qq_line(aes(sample=model$residuals), color='dodgerblue') +
    labs(title='Normal Q-Q', x='Theoretical Quantiles', y='Standardized Residuals')
}

plot_resids <- function(model, span=5.0) {
  ggplot() + 
    geom_point(aes(x=model$fitted, y=model$residuals)) + 
    geom_smooth(aes(x=model$fitted, y=model$residuals), span=span) + 
    labs(title='Residuals vs. Predictor', x='Fitted Values', y='Residuals')
}