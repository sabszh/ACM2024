# Load packages
pacman::p_load(tidyverse, ggplot2)

# Summarizing the model
samples <- readRDS("simmodels/AWSLS.rds")

samples$summary()

# Assess model quality
draws_df <- as_draws_df(samples$draws())

# Checking the model's chains
ggplot(draws_df, aes(.iteration, logit_scaled(theta_w), group = .chain, color = .chain)) + geom_line() + labs(title = "Theta w")+ theme_classic()
ggplot(draws_df, aes(.iteration, logit_scaled(theta_l), group = .chain, color = .chain)) + geom_line() + labs(title = "Theta l") + theme_classic()

# prior predictive check
gridExtra::grid.arrange(
  ggplot(draws_df) +
    geom_density(aes((theta_w_prior)), fill = "red", alpha = 0.3) +
    xlab("Rate") +
    ylab("Prior Density") +
    labs(title = "Prior-Predictive Checks (probability space)", subtitle = "Theta w") +
    theme_classic(),
  ggplot(draws_df) +
    geom_density(aes((theta_l_prior)), fill = "red", alpha = 0.3) +
    xlab("Rate") +
    labs(title = "", subtitle = "Theta l") +
    theme_classic(),
  ggplot(draws_df) +
    geom_density(aes(logit_scaled(theta_w_prior)), fill = "pink", alpha = 0.3) +
    xlab("Rate") +
    ylab("Prior Density") +
    labs(title = "Prior-Predictive Checks (logit-scaled)", subtitle = "Theta w") +
    theme_classic(),
  ggplot(draws_df) +
    geom_density(aes(logit_scaled(theta_l_prior)), fill = "pink", alpha = 0.3) +
    xlab("Rate") +
    labs(title = "", subtitle = "Theta l") +
    theme_classic(),
  nrow = 2
)


# prior-posterior update checks
gridExtra::grid.arrange(
  ggplot(draws_df) +
    #ylim(0, 5) +
    #xlim(0, 1) +
    geom_density(aes(logit_scaled(theta_w)), fill = "blue", alpha = 0.3) +
    geom_density(aes(logit_scaled(theta_w_prior)), fill = "red", alpha = 0.3) +
    geom_vline(xintercept = logit_scaled(0.8), linetype = "dashed", color = "black", size = 1.5) +
    xlab("Rate") +
    ylab("Prior Density") +
    labs(title = "Prior-Posterior Update Checks", subtitle = "Theta w") +
    theme_classic(),
  ggplot(draws_df) +
    #ylim(0, 5) +
    #xlim(0, 1) +
    geom_density(aes(logit_scaled(theta_l)), fill = "blue", alpha = 0.3) +
    geom_density(aes(logit_scaled(theta_l_prior)), fill = "red", alpha = 0.3) +
    geom_vline(xintercept = logit_scaled(0.9), linetype = "dashed", color = "black", size = 1.5) +
    xlab("Rate") +
    ylab("Prior-Posterior Update Checks") +
    labs(title = "", subtitle = "Theta l") +
    theme_classic(),
  nrow = 1
)



