# Load packages
pacman::p_load(loo, ggplot2, tidyverse)


# Load in weighted and simple model
simple_bayes <- readRDS("simmodels/simple_bayes_realdata.rds")
weighted_bayes <- readRDS("simmodels/weighted_bayes_realdata.rds")


# Function to extract log-likelihood and compute LOO
compute_loo <- function(model_fit) {
  # Extract log-likelihood values
  log_lik <- model_fit$draws("log_lik", format = "matrix")
  
  # Compute LOO-Cross Validation using PSIS
  loo_result <- loo(log_lik)
  return(loo_result)
}

# Extract log-likelihood from models
loo_simple <- compute_loo(simple_bayes)
loo_weighted <- compute_loo(weighted_bayes)

# Optional!
# Check dimensions of loo matrix, should match our iterations and observations 
dim(loo_simple)
dim(loo_weighted)

# Plot PSIS diagnostics - 
plot(loo_simple) # gives error!
plot(loo_weighted)

# Print the LOO values
print(loo_simple)
print(loo_weighted)

# Do a pareto-k-diagnostic to see what is happening to our LOO estimates.
# The Pareto K diagnostics tells us how reliable our LOO estimates are.

# Function to check Pareto k diagnostics
check_pareto_k <- function(loo_result, model_name) {
  # Extract Pareto k values
  pareto_k <- loo_result$diagnostics$pareto_k
  
  # Count problematic k values
  n_k_high <- sum(pareto_k > 0.7)
  n_k_medium <- sum(pareto_k > 0.5 & pareto_k <= 0.7)
  
  # Proportion of problematic observations
  prop_problematic <- (n_k_high + n_k_medium) / length(pareto_k)
  
  # Create diagnostic summary
  summary_df <- tibble(
    model = model_name,
    total_obs = length(pareto_k),
    k_high = n_k_high,
    k_medium = n_k_medium,
    prop_problematic = prop_problematic,
    reliability = case_when(
      prop_problematic == 0 ~ "Excellent",
      prop_problematic < 0.05 ~ "Good",
      prop_problematic < 0.1 ~ "Fair",
      TRUE ~ "Poor"
    )
  )
  
  return(summary_df)
}

# Check diagnostics for all models
diagnostics <- bind_rows(
  check_pareto_k(loo_simple,"Simple"),
  check_pareto_k(loo_weighted, "weighted"))

# Display diagnostics table
knitr::kable(diagnostics, 
             digits = 3,
             caption = "PSIS-LOO Reliability Diagnostics")


# Compare models - First input = model1, second = model 2 etc. Just so no one gets confused.
loo_comparison <- loo_compare(loo_simple, loo_weighted)

# Plot model comparison with LOO CV (cross-validation).
model_weights <- loo_model_weights(list( "Simple Model" = loo_simple, "Weighted Model" = loo_weighted))
model_comp_data <- tibble(  model = names(model_weights), weight = as.numeric(model_weights))
p_model_comp <- ggplot(model_comp_data, aes(x = model, y = weight, fill = model)) +  geom_col() +  geom_text(aes(label = scales::percent(weight, accuracy = 0.1)), 
                vjust = -0.5, size = 5) + labs( title = "Model Comparison Using LOO-CV",   subtitle = "Higher weights indicate better predictive performance",  x = NULL,  y = "Model Weight"  ) + scale_fill_brewer(palette = "Set1") +  theme_minimal() +  theme(legend.position = "none")







