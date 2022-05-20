get_weights <- function(model) {
  abs_resid <- abs(model$residuals)
  abs_resid_lm <- lm(abs_resid ~ model$fitted.values)
  est_var <- abs_resid_lm$fitted.values^2
  weights <- 1 / est_var
  return(weights)
}

get_model_difference <- function(new_model, old_model) {
  params <- tidy(old_model)
  new_params <- tidy(new_model)
  abs_coef_diff = abs(new_params$estimate - params$estimate)
  abs_std_err_dff = abs(new_params$std.error - params$std.error)
  tot_abs_diff <- sum(abs_coef_diff) + sum(abs_std_err_dff)
  
  return(tot_abs_diff)
}

iwls <- function(best_formula, data, verbose=TRUE) {
  # Define our old model as our OLS model
  old_model <- lm(best_formula, data=data)
  
  count <- 0 # Setup a count so we don't get stuck in the while loop
  # dummy values to get into the while loop
  old_weights <- 1
  model_diff <- 1000
  
  while(count < 50 & model_diff > 1e-12) {
    count <- count + 1 # Increment count by one so we don't get stuck
    
    # Calculate the weights and fit the model with them
    new_weights <- get_weights(old_model)
    new_model <- lm(best_formula, data=data, weights=new_weights)  
    
    # Calculate the differences between models and the weights
    model_diff <- get_model_difference(new_model, old_model)
    weight_diff <- sum(abs(new_weights - old_weights))
    
    # Set the old model as the current "new_model" so we can compare next time
    old_model <- new_model
    old_weights <- new_weights
    
    if (verbose) {
      cat('Count:\t', count, '\nModel Params Diff:\t', model_diff, '\nWeight Diff:\t', weight_diff, '\n\n')
    }
  }
  
  return(new_model)
}