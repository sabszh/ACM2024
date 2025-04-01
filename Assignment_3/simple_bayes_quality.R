# Load packages
pacman::p_load(tidyverse, ggplot2, tidybayes, posterior, bayesplot)

# Summarizing the model
samples_simple <- readRDS("simmodels/simple_bayes.rds")
samples_weighted <- readRDS("simmodels/weighted_bayes.rds")

# Assess model quality
draws_df_simple <- as_draws_df(samples_simple$draws())
draws_df_weighted <- as_draws_df(samples_weighted$draws())

# Import the simulated data
d <- read.csv("data/Simonsen_clean.csv")

df <- d %>% subset(ID == 229)

# Turn it into a list with n
data <- list(N = 153, trial = df$FaceID, FirstRating_og = df$FirstRating, GroupRating_og = df$GroupRating, SecondRating_og = df$SecondRating)

# Prior predictions in long format
prior_pred_simple <- draws_df_simple %>%
  select(.draw, starts_with("prior_pred_SecondRating[")) %>%
  pivot_longer(
    cols = starts_with("prior_pred_SecondRating["),
    names_to = "trial",
    values_to = "pred_prior"
  ) %>%
  mutate(
    trial = as.integer(gsub("prior_pred_SecondRating\\[|\\]", "", trial))
  )

prior_pred_weighted <- draws_df_weighted %>%
  select(.draw, starts_with("prior_pred_SecondRating[")) %>%
  pivot_longer(
    cols = starts_with("prior_pred_SecondRating["),
    names_to = "trial",
    values_to = "pred_prior"
  ) %>%
  mutate(
    trial = as.integer(gsub("prior_pred_SecondRating\\[|\\]", "", trial))
  )

# Posterior predictions in long format
posterior_pred_simple <- draws_df_simple %>%
  select(.draw, starts_with("posterior_pred_SecondRating[")) %>%
  pivot_longer(
    cols = starts_with("posterior_pred_SecondRating["),
    names_to = "trial",
    values_to = "pred_posterior"
  ) %>%
  mutate(
    trial = as.integer(gsub("posterior_pred_SecondRating\\[|\\]", "", trial))
  )

posterior_pred_weighted <- draws_df_weighted %>%
  select(.draw, starts_with("posterior_pred_SecondRating[")) %>%
  pivot_longer(
    cols = starts_with("posterior_pred_SecondRating["),
    names_to = "trial",
    values_to = "pred_posterior"
  ) %>%
  mutate(
    trial = as.integer(gsub("posterior_pred_SecondRating\\[|\\]", "", trial))
  )

preds_simple <- left_join(prior_pred_simple, posterior_pred_simple, by = c(".draw", "trial"))
preds_weight <- left_join(prior_pred_weighted, posterior_pred_weighted, by = c(".draw", "trial"))


# Prior predictive check
gridExtra::grid.arrange(
  (ggplot(preds_simple) +
    geom_histogram(aes(pred_prior), color = "red", fill = "red", alpha = 0.3, bins = 8) +
    labs(title = "Prior Predictive Checks - simple") +
    xlab("Predicted rating") +
    ylab("Posterior Density") +
    theme_classic()),
  
  (ggplot(preds_weight) +
    geom_histogram(aes(pred_prior), color = "red", fill = "red", alpha = 0.3, bins = 8) +
    labs(title = "Prior Predictive Checks - weighted") +
    xlab("Predicted rating") +
    ylab("Posterior Density") +
    theme_classic())
)

# Posterior Predictive Checks 
gridExtra::grid.arrange(
  (ggplot(preds_simple) +
     geom_histogram(aes(pred_prior), color = "red", fill = "red", alpha = 0.3, bins = 8) +
     geom_histogram(aes(pred_posterior), color = "blue", fill = "blue", alpha = 0.1, bins = 8) +
     labs(title = "Posterior Predictive Checks - simple") +
     xlab("Predicted rating") +
     ylab("Posterior Density") +
     theme_classic()),
  
  (ggplot(preds_weight) +
     geom_histogram(aes(pred_prior), color = "red", fill = "red", alpha = 0.3, bins = 8) +
     geom_histogram(aes(pred_posterior), color = "blue", fill = "blue", alpha = 0.1, bins = 8) +
     labs(title = "Posterior Predictive Checks - weighted") +
     xlab("Predicted rating") +
     ylab("Posterior Density") +
     theme_classic())
)

ggplot(posterior_pred_weighted, aes(pred_posterior)) +
  geom_histogram(bins = 8, fill = "blue", alpha = 0.5) +
  labs(title = "Sanity check: Weighted model posterior")

ggplot(prior_pred_weighted, aes(pred_prior)) +
  geom_histogram(bins = 8, fill = "blue", alpha = 0.5) +
  labs(title = "Sanity check: Weighted model posterior")

