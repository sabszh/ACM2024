# Load packages
pacman::p_load(tidyverse, ggplot2, tidybayes, posterior, bayesplot)

# Summarizing the model
samples_simple_sims <- readRDS("simmodels/simple_bayes_sims.rds")
samples_simple_real <- readRDS("simmodels/simple_bayes_realdata.rds")

samples_weighted_sims <- readRDS("simmodels/weighted_bayes_sims.rds")
samples_weighted_real <- readRDS("simmodels/weighted_bayes_realdata.rds")

# Assess model quality
draws_df_simple_sims <- as_draws_df(samples_simple_sims$draws())
draws_df_simple_real <- as_draws_df(samples_simple_real$draws())

draws_df_weighted_sims <- as_draws_df(samples_weighted_sims$draws())
draws_df_weighted_real <- as_draws_df(samples_weighted_real$draws())

# Import the simulated data
d_real <- read.csv("data/Simonsen_clean.csv")
d_simple_sims <- read.csv("data/simulated_simple_bayes.csv")
d_weighted_sims <- read.csv("data/simulated_weighted_bayes.csv")

df_real <- d_real %>% subset(ID == 229)
df_simple_sims <- d_simple_sims %>% subset(ID == 3)
df_weighted_sims <- d_weighted_sims %>% subset(ID == 3)

# Turn it into a list with n
data_real <- list(N = 153, trial = df_real$FaceID, FirstRating_og = df_real$FirstRating, GroupRating_og = df_real$GroupRating, SecondRating_og = df_real$SecondRating)
data_simple_sims <- list(N = 153, trial = df_simple_sims$trial, FirstRating_og = df_simple_sims$FirstRating, GroupRating_og = df_simple_sims$GroupRating, SecondRating_og = df_simple_sims$SecondRating)
data_weighted_sims <- list(N = 153, trial = df_weighted_sims$trial, FirstRating_og = df_weighted_sims$FirstRating, GroupRating_og = df_weighted_sims$GroupRating, SecondRating_og = df_weighted_sims$SecondRating)
  
# Prior predictions in long format
prior_pred_simple_real <- draws_df_simple_real %>%
  select(.draw, starts_with("prior_pred_SecondRating[")) %>%
  pivot_longer(
    cols = starts_with("prior_pred_SecondRating["),
    names_to = "trial",
    values_to = "pred_prior"
  ) %>%
  mutate(
    trial = as.integer(gsub("prior_pred_SecondRating\\[|\\]", "", trial))
  )

prior_pred_simple_sims <- draws_df_simple_sims %>%
  select(.draw, starts_with("prior_pred_SecondRating[")) %>%
  pivot_longer(
    cols = starts_with("prior_pred_SecondRating["),
    names_to = "trial",
    values_to = "pred_prior"
  ) %>%
  mutate(
    trial = as.integer(gsub("prior_pred_SecondRating\\[|\\]", "", trial))
  )

prior_pred_weight_real <- draws_df_weighted_real %>%
  select(.draw, starts_with("prior_pred_SecondRating[")) %>%
  pivot_longer(
    cols = starts_with("prior_pred_SecondRating["),
    names_to = "trial",
    values_to = "pred_prior"
  ) %>%
  mutate(
    trial = as.integer(gsub("prior_pred_SecondRating\\[|\\]", "", trial))
  )

prior_pred_weight_sims <- draws_df_weighted_sims %>%
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
posterior_pred_simple_real <- draws_df_simple_real %>%
  select(.draw, starts_with("posterior_pred_SecondRating[")) %>%
  pivot_longer(
    cols = starts_with("posterior_pred_SecondRating["),
    names_to = "trial",
    values_to = "pred_posterior"
  ) %>%
  mutate(
    trial = as.integer(gsub("posterior_pred_SecondRating\\[|\\]", "", trial))
  )

posterior_pred_simple_sims <- draws_df_simple_sims %>%
  select(.draw, starts_with("posterior_pred_SecondRating[")) %>%
  pivot_longer(
    cols = starts_with("posterior_pred_SecondRating["),
    names_to = "trial",
    values_to = "pred_posterior"
  ) %>%
  mutate(
    trial = as.integer(gsub("posterior_pred_SecondRating\\[|\\]", "", trial))
  )

posterior_pred_weight_real <- draws_df_weighted_real %>%
  select(.draw, starts_with("posterior_pred_SecondRating[")) %>%
  pivot_longer(
    cols = starts_with("posterior_pred_SecondRating["),
    names_to = "trial",
    values_to = "pred_posterior"
  ) %>%
  mutate(
    trial = as.integer(gsub("posterior_pred_SecondRating\\[|\\]", "", trial))
  )

posterior_pred_weight_sims <- draws_df_weighted_sims %>%
  select(.draw, starts_with("posterior_pred_SecondRating[")) %>%
  pivot_longer(
    cols = starts_with("posterior_pred_SecondRating["),
    names_to = "trial",
    values_to = "pred_posterior"
  ) %>%
  mutate(
    trial = as.integer(gsub("posterior_pred_SecondRating\\[|\\]", "", trial))
  )


preds_simple_real <- left_join(prior_pred_simple_real, posterior_pred_simple_real, by = c(".draw", "trial"))
preds_simple_sims <- left_join(prior_pred_simple_sims, posterior_pred_simple_sims, by = c(".draw", "trial"))
  
preds_weight_real <- left_join(prior_pred_weight_real, posterior_pred_weight_real, by = c(".draw", "trial"))
preds_weight_sims <- left_join(prior_pred_weight_sims, posterior_pred_weight_sims, by = c(".draw", "trial"))

# Prior predictive check
gridExtra::grid.arrange(
  (ggplot(preds_simple_real) +
    geom_histogram(aes(pred_prior), color = "red", fill = "red", alpha = 0.3, bins = 8) +
    labs(title = "Prior Predictive Checks - simple") +
    xlab("Predicted rating") +
    ylab("Posterior Density") +
    theme_classic()),
  
  (ggplot(preds_weight_real) +
    geom_histogram(aes(pred_prior), color = "red", fill = "red", alpha = 0.3, bins = 8) +
    labs(title = "Prior Predictive Checks - weighted") +
    xlab("Predicted rating") +
    ylab("Posterior Density") +
    theme_classic())
)

# Posterior Predictive Checks ¨
gridExtra::grid.arrange(
  (ggplot(preds_simple_sims) +
    geom_histogram(aes(pred_prior), color = "red", fill = "red", alpha = 0.3, bins = 8) +
    geom_histogram(aes(pred_posterior), color = "blue", fill = "blue", alpha = 0.1, bins = 8) +
    labs(title = "Posterior Predictive Checks - simple") +
    xlab("Predicted rating") +
    ylab("Posterior Density") +
    theme_classic()),
  (ggplot(preds_weight_sims) +
    geom_histogram(aes(pred_prior), color = "red", fill = "red", alpha = 0.3, bins = 8) +
    geom_histogram(aes(pred_posterior), color = "blue", fill = "blue", alpha = 0.1, bins = 8) +
    labs(title = "Posterior Predictive Checks - weighted") +
    xlab("Predicted rating") +
    ylab("Posterior Density") +
    theme_classic())
)

# Prior-posterior update checks
gridExtra::grid.arrange(
  (ggplot(draws_df_weighted_sims) +
    geom_histogram(aes(weight_direct_prior), fill = "red", alpha = 0.3) +
    geom_histogram(aes(weight_direct), fill = "blue", alpha = 0.3) +
    geom_vline(xintercept = 0.3, linetype = "dashed", color = "black", size = 1.5) +
    xlim(0, 1) +
    labs(title = "Prior-posterior Update Checks (simulated)", subtitle = "Weight Direct") +
    theme_classic()),
  (ggplot(draws_df_weighted_sims) +
    geom_histogram(aes((weight_social_prior)), fill = "red", alpha = 0.3) +
    geom_histogram(aes(weight_social), fill = "blue", alpha = 0.3) +
    geom_vline(xintercept = 0.7, linetype = "dashed", color = "black", size = 1.5) +
    xlim(0, 1) +
    labs(subtitle = "Weight Social") +
    theme_classic())
)

