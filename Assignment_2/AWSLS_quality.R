# Load packages
pacman::p_load(tidyverse, ggplot2, tidybayes, posterior, bayesplot)

# Summarizing the model
samples <- readRDS("simmodels/AWSLS.rds")

samples$summary()

# Assess model quality
draws_df <- as_draws_df(samples$draws())

# Import the simulated data
d <- read.csv("data/simulated_WSLSvsMEM7.csv")

df <- d %>% subset(agent == 100)

# Turn it into a list with n
data <- list(N = 120, selfchoice = df$WSLS, trial = df$trial, oppchoice = df$MEM)

# Getting the prior and posterior predictions from the draws
pred_prior <- rowSums(draws_df[,8:128]) # Getting the prior predictions from the draws
pred_posterior <- rowSums(draws_df[,128:248]) # Getting the posterior predictions from the draws

preds <- data.frame(pred_prior, pred_posterior) # Combining the predictions into a dataframe

# Checking the model's chains
gridExtra::grid.arrange(
  ggplot(draws_df, aes(.iteration, theta_w, group = .chain, color = .chain)) +
    geom_line() +
    labs(title = "Model chains", subtitle = "Theta w") + theme_classic(),
  ggplot(draws_df, aes(.iteration, theta_l, group = .chain, color = .chain)) + geom_line() + labs(subtitle = "Theta l") + theme_classic(),
  nrow = 2
)

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
  geom_point(x = sum(data$selfchoice), y = 0, color = "black", shape = 17, size = 5) +
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
