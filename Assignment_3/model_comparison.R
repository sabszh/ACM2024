# Load packages
pacman::p_load(loo, ggplot2)



# weighted and simple model
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

loo_simple <- compute_loo(simple_bayes)
loo_weighted <- compute_loo(weighted_bayes)

# Check dimensions of loo matrix, should match our iterations and observations 
dim(loo_simple)
dim(loo_weighted)


# Plot PSIS diagnostics
plot(loo_simple) # gives error!
plot(loo_weighted)

# Print the LOO-IC values
print(loo_simple)
print(loo_weighted)

# Compare models - First input = model1, second = model 2 etc. Just so noone gets confused.
loo_comparison <- loo_compare(loo_simple, loo_weighted)

# Plot comparison
model_weights <- loo_model_weights(list( "Simple Integration" = loo_simple, "Weighted Integration" = loo_weighted))
model_comp_data <- tibble(  model = names(model_weights), weight = as.numeric(model_weights))
p_model_comp <- ggplot(model_comp_data, aes(x = model, y = weight, fill = model)) +  geom_col() +  geom_text(aes(label = scales::percent(weight, accuracy = 0.1)), 
                vjust = -0.5, size = 5) + labs( title = "Model Comparison Using LOO-CV",   subtitle = "Higher weights indicate better predictive performance",  x = NULL,  y = "Model Weight"  ) + scale_fill_brewer(palette = "Set1") +  theme_minimal() +  theme(legend.position = "none")
p_model_comp




