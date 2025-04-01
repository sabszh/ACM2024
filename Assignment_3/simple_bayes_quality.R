# Load packages
pacman::p_load(tidyverse, ggplot2, tidybayes, posterior, bayesplot)

# Summarizing the model
samples <- readRDS("simmodels/simple_bayes.rds")

samples$summary()

# Assess model quality
draws_df <- as_draws_df(samples$draws())

# Import the simulated data
d <- read.csv("data/Simonsen_clean.csv")

df <- d %>% subset(ID == 229)

# Turn it into a list with n
data <- list(N = 153, trial = df$FaceID, FirstRating_og = df$FirstRating, GroupRating_og = df$GroupRating, SecondRating_og = df$SecondRating)

# Prior predictions in long format
prior_pred_df <- draws_df %>%
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
posterior_pred_df <- draws_df %>%
  select(.draw, starts_with("posterior_pred_SecondRating[")) %>%
  pivot_longer(
    cols = starts_with("posterior_pred_SecondRating["),
    names_to = "trial",
    values_to = "pred_posterior"
  ) %>%
  mutate(
    trial = as.integer(gsub("posterior_pred_SecondRating\\[|\\]", "", trial))
  )

preds <- left_join(prior_pred_df, posterior_pred_df, by = c(".draw", "trial"))


# Prior predictive check
ggplot(preds) +
  geom_histogram(aes(pred_prior), color = "red", fill = "red", alpha = 0.3, bins = 90) +
  labs(title = "Prior Predictive Checks") +
  xlab("Predicted heads out of 120 trials") +
  ylab("Posterior Density") +
  theme_classic()

# Posterior Predictive Checks 
ggplot(preds) +
  geom_histogram(aes(pred_prior), color = "red", fill = "red", alpha = 0.3, bins = 90) +
  geom_histogram(aes(pred_posterior), color = "blue", fill = "blue", alpha = 0.3, bins = 90) +
  #geom_point(x = sum(data$selfchoice), y = 0, color = "black", shape = 17, size = 5) +
  labs(title = "Posterior Predictive Checks") +
  xlab("Predicted heads out of 120 trials") +
  ylab("Posterior Density") +
  theme_classic()

# Prior-posterior update checks
gridExtra::grid.arrange(
  ggplot(draws_df) +
    geom_histogram(aes(theta_w_prior), fill = "red", alpha = 0.3) +
    geom_histogram(aes(theta_w_posterior), fill = "blue", alpha = 0.3) +
    geom_vline(xintercept = (0.8), linetype = "dashed", color = "black", size = 1.5) +
    labs(title = "Prior-posterior update checks") +
    theme_classic(),
  ggplot(draws_df) +
    geom_histogram(aes(theta_l_prior), fill = "red", alpha = 0.3) +
    geom_histogram(aes(theta_l_posterior), fill = "blue", alpha = 0.3) +
    geom_vline(xintercept = (0.9), linetype = "dashed", color = "black", size = 1.5) +
    labs(title = " ") +
    theme_classic(),
  nrow = 1
  
)
