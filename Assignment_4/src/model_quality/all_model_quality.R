# Load necessary packages
pacman::p_load(tidyverse, posterior, bayesplot,loo)

# Load the posterior samples
samples_gcm_real <- readRDS("src/stan/empirical_fit.rds")
samples_gcm_sims <- readRDS("src/stan/simulated_fit.rds")

# Convert draws to data frames
draws_gcm_real <- as_draws_df(samples_gcm_real$draws())
draws_gcm_sims <- as_draws_df(samples_gcm_sims$draws())

# Import the data
d_sim <- read.csv("data/simulated_data.csv")
d_real <- read.csv("data/Aliendata.txt")

# Filter to one subject and one condition, and remove practice trials
d_real <- d_real %>% filter(subject == 3, condition==1, session == 1) %>% filter(!grepl("pt", stimulus))

# stimulus column should be named stimuli and content should be without jpg, and named stimuli
d_real <- d_real %>%
  mutate(stimuli = gsub(".jpg", "", stimulus))

# load empirical data
d_real <- read.csv("data/AlienData.txt")

# make subset for subject 1 and session 1 and condition 1 
d_real <- d_real[d_real$subject == 1 & d_real$session == 1 & d_real$condition == 1, ]

# create a column "danger" with 1 for "danger" and 0 for "safe". If category is 3 or 4, it is "danger", otherwise "safe"
d_real$danger <- ifelse(d_real$category %in% c(3, 4), 1, 0)

# convert stimulus into features
d_real <- d_real %>%
  mutate(
    stim_str = gsub(".jpg", "", stimulus)
  ) %>%
  mutate(
    spots   = as.integer(substr(stim_str, 1, 1)),
    legs    = as.integer(substr(stim_str, 2, 2)),
    eyes    = as.integer(substr(stim_str, 3, 3)),
    arms_up = as.integer(substr(stim_str, 4, 4)),
    color   = as.integer(substr(stim_str, 5, 5))
  ) %>%
  select(-stim_str)

# remove rows with test = 1
d_real <- d_real[d_real$test == 0, ]

# get all w's, r and rr
extract_param_matrix <- function(draws, prefix) {
  draws %>%
    select(starts_with(prefix)) %>%
    as.matrix()
}

# Extract from real data
w_real         <- extract_param_matrix(draws_gcm_real, "w[")
w_prior_real   <- extract_param_matrix(draws_gcm_real, "w_prior[")
r_real         <- extract_param_matrix(draws_gcm_real, "r[")
r_prior_real   <- extract_param_matrix(draws_gcm_real, "r_prior[")
rr_real        <- extract_param_matrix(draws_gcm_real, "rr[")
rr_prior_real  <- extract_param_matrix(draws_gcm_real, "rr_prior[")
c_real         <- extract_param_matrix(draws_gcm_real, "c[")

# Extract from simulated data
w_sim          <- extract_param_matrix(draws_gcm_sims, "w[")
w_prior_sim    <- extract_param_matrix(draws_gcm_sims, "w_prior[")
r_sim          <- extract_param_matrix(draws_gcm_sims, "r[")
r_prior_sim    <- extract_param_matrix(draws_gcm_sims, "r_prior[")
rr_sims        <- extract_param_matrix(draws_gcm_sims, "rr[")
rr_prior_sims  <- extract_param_matrix(draws_gcm_sims, "rr_prior[")
c_sims         <- extract_param_matrix(draws_gcm_sims, "c[")

# turn the rr into a long format and 
rrs_real <- draws_gcm_real %>%
  select(starts_with("rr[")) %>%
  pivot_longer(cols = everything(), names_to = "rr", values_to = "value") %>%
  mutate(rr = as.integer(gsub("rr\\[|\\]", "", rr))) %>% 
  filter(!is.na(value))

rrs_sims <- draws_gcm_sims %>%
  select(starts_with("rr[")) %>%
  pivot_longer(cols = everything(), names_to = "rr", values_to = "value") %>%
  mutate(rr = as.integer(gsub("rr\\[|\\]", "", rr))) %>% 
  filter(!is.na(value))


# Prior and posterior predictions (binary responses)
prior_preds_real <- draws_gcm_real %>%
  select(.draw, starts_with("prior_preds[")) %>%
  pivot_longer(cols = starts_with("prior_preds["), names_to = "trial", values_to = "prior_pred") %>%
  mutate(trial = as.integer(gsub("prior_preds\\[|\\]", "", trial)))

prior_preds_sim <- draws_gcm_sims %>%
  select(.draw, starts_with("prior_preds[")) %>%
  pivot_longer(cols = starts_with("prior_preds["), names_to = "trial", values_to = "prior_pred") %>%
  mutate(trial = as.integer(gsub("prior_preds\\[|\\]", "", trial)))

posterior_preds_real <- draws_gcm_real %>%
  select(.draw, starts_with("posterior_preds[")) %>%
  pivot_longer(cols = starts_with("posterior_preds["), names_to = "trial", values_to = "posterior_pred") %>%
  mutate(trial = as.integer(gsub("posterior_preds\\[|\\]", "", trial)))

posterior_preds_sim <- draws_gcm_sims %>%
  select(.draw, starts_with("posterior_preds[")) %>%
  pivot_longer(cols = starts_with("posterior_preds["), names_to = "trial", values_to = "posterior_pred") %>%
  mutate(trial = as.integer(gsub("posterior_preds\\[|\\]", "", trial)))

# Posterior accuracy
posterior_accuracy_real <- draws_gcm_real %>%
  select(.draw, starts_with("posterior_true[")) %>%
  pivot_longer(cols = starts_with("posterior_true["), names_to = "trial", values_to = "correct") %>%
  mutate(trial = as.integer(gsub("posterior_true\\[|\\]", "", trial)))

posterior_accuracy_sim <- draws_gcm_sims %>%
  select(.draw, starts_with("posterior_true[")) %>%
  pivot_longer(cols = starts_with("posterior_true["), names_to = "trial", values_to = "correct") %>%
  mutate(trial = as.integer(gsub("posterior_true\\[|\\]", "", trial)))

log_lik_real <- draws_gcm_real %>%
  select(starts_with("log_lik[")) %>%
  as.matrix()

log_lik_sim <- draws_gcm_sims %>%
  select(starts_with("log_lik[")) %>%
  as.matrix()


# Combine for visualization
preds_real <- prior_preds_real %>%
  left_join(posterior_preds_real, by = c(".draw", "trial")) %>%
  left_join(posterior_accuracy_real, by = c(".draw", "trial"))

preds_sim <- prior_preds_sim %>%
  left_join(posterior_preds_sim, by = c(".draw", "trial")) %>%
  left_join(posterior_accuracy_sim, by = c(".draw", "trial"))


# Prior predictive check 
gridExtra::grid.arrange(
  ggplot(preds_sim) +
    geom_histogram(aes(prior_pred), color = "red", fill = "red", alpha = 0.3, bins = 2) +
    labs(title = "Prior Predictive Checks (Simulated data)") +
    xlab("Predicted rating") +
    ylab("Posterior Density") +
    theme_classic(),

  ggplot(preds_real) +
    geom_histogram(aes(prior_pred), color = "red", fill = "red", alpha = 0.3, bins = 2) +
    labs(title = "Prior Predictive Checks (Real data)") +
    xlab("Predicted rating") +
    ylab("Posterior Density") +
    theme_classic()
)

# Posterior predictive checks
# plotting the rr's

gridExtra::grid.arrange(
  ggplot(rrs_sims) +
    geom_density(aes(x = value), fill = "blue", alpha = 0.5) +
    labs(title = "Posterior Predictive Check (Simulated data)", x = "rr", y = "Density") +
    xlim(0,1) +
    theme_minimal(),
  ggplot(rrs_real) +
    geom_density(aes(x = value), fill = "blue", alpha = 0.5) +
    labs(title = "Posterior Predictive Check (Real data)", x = "rr", y = "Density") +
    xlim(0,1) +
    theme_minimal()
)

gridExtra::grid.arrange(
  ggplot(preds_sim) +
    geom_histogram(aes(posterior_pred), color = "blue", fill = "blue", alpha = 0.3, bins=2) +
    labs(title = "Posterior Predictive Checks (Simulated data)") +
    xlab("Predicted rating") +
    ylab("Posterior Density") +
    theme_classic(),

  ggplot(preds_real) +
    geom_histogram(aes(posterior_pred), color = "blue", fill = "blue", alpha = 0.3, bins = 2) +
    labs(title = "Posterior Predictive Checks (Real data)") +
    xlab("Predicted rating") +
    ylab("Posterior Density") +
    theme_classic()
)

# Posterior predictive accuracy
accuracy_summary_real <- posterior_accuracy_real %>%
  group_by(.draw) %>%
  summarise(accuracy = mean(correct))

accuracy_summary_sim <- posterior_accuracy_sim %>%
  group_by(.draw) %>%
  summarise(accuracy = mean(correct))

gridExtra::grid.arrange(
  ggplot(accuracy_summary_sim, aes(x = accuracy,fill="red")) +
    geom_histogram(bins = 30) +
    labs(title = "Posterior Predictive Accuracy (Simulated data)", x = "Accuracy", y = "Draw Frequency") +
    xlim(0,1) +
    theme_classic() + theme(legend.position = "none"),
  ggplot(accuracy_summary_real, aes(x = accuracy, ,fill="red")) +
    geom_histogram(bins = 30) +
    labs(title = "Posterior Predictive Accuracy (Real data)", x = "Accuracy", y = "Draw Frequency") +
    xlim(0,1) + 
    theme_classic() + theme(legend.position = "none")
)


# Function to plot prior/posterior
# Combine prior and posterior into one dataframe
plot_w_real <- bind_rows(
  as_tibble(w_prior_real) %>% 
    pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
    mutate(feature_n = as.integer(gsub(".*\\[(\\d+)\\]", "\\1", parameter))) %>% 
    mutate(type = "prior"),
  as_tibble(w_real) %>% 
    pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
    mutate(feature_n = as.integer(gsub(".*\\[(\\d+)\\]", "\\1", parameter))) %>% 
    mutate(type = "posterior")
)

plot_w_sims <- bind_rows(
  as_tibble(w_prior_sim) %>% 
    pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
    mutate(feature_n = as.integer(gsub(".*\\[(\\d+)\\]", "\\1", parameter))) %>% 
    mutate(type = "prior"),
  as_tibble(w_sim) %>% 
    pivot_longer(everything(), names_to = "parameter", values_to = "value") %>% 
    mutate(feature_n = as.integer(gsub(".*\\[(\\d+)\\]", "\\1", parameter))) %>% 
    mutate(type = "posterior")
)

# Feature weights
gridExtra::grid.arrange(
  ggplot(plot_w_sims, aes(x = value, fill = type)) +
    geom_density(alpha = 0.6) +
    facet_wrap(~ feature_n) + 
    labs(
      title = "Feature Weights (w) Prior vs. Posterior (Simulated data)",
      x = "Value", y = "Density"
    ) +
    theme_minimal() + theme(legend.position = "none"),
  
  ggplot(plot_w_real, aes(x = value, fill = type)) +
    geom_density(alpha = 0.6) +
    facet_wrap(~ feature_n) + 
    labs(
      title = "Feature Weights (w) Prior vs. Posterior (Real data)",
      x = "Value", y = "Density"
    ) +
    theme_minimal() + theme(legend.position = "bottom")
)



# Attention weights
gridExtra::grid.arrange(
  draws_gcm_sims %>%
    select(starts_with("w[")) %>%
    pivot_longer(cols = everything(), names_to = "weight", values_to = "value") %>%
    ggplot(aes(x = value, fill = weight)) +
    geom_density(alpha = 0.5) +
    labs(title = "Posterior Distributions of Attention Weights (Simulated data)", x = "Weight", y = "Density") +
    theme_minimal() + theme(legend.position = "none") + xlim(0,1),
  draws_gcm_real %>%
    select(starts_with("w[")) %>%
    pivot_longer(cols = everything(), names_to = "weight", values_to = "value") %>%
    ggplot(aes(x = value, fill = weight)) +
    geom_density(alpha = 0.5) +
    labs(title = "Posterior Distributions of Attention Weights (Real data)", x = "Weight", y = "Density") +
  theme_minimal() + theme(legend.position = "bottom") + xlim(0,1)
)

# Sensitivity parameter c
gridExtra::grid.arrange(
  ggplot(draws_gcm_sims, aes(x = c)) +
    geom_density(fill = "purple", alpha = 0.5) +
    labs(title = "Posterior of Sensitivity, c (Simulated data)", x = "c", y = "Density") +
    theme_classic() + xlim(0,5),
  ggplot(draws_gcm_real, aes(x = c)) +
    geom_density(fill = "purple", alpha = 0.5) +
    labs(title = "Posterior of Sensitivity, c (Real data)", x = "c", y = "Density") +
    theme_classic() + xlim(0,5)
)


# Calculate cumulative accuracy
cumulative_accuracy_real <- preds_real %>%
  arrange(trial) %>%
  group_by(trial) %>%
  summarize(
    posterior_acc = mean(posterior_pred == correct),
    prior_acc = mean(prior_pred == correct)
  ) %>%
  mutate(
    cumul_posterior = cummean(posterior_acc),
    cumul_prior = cummean(prior_acc)
  )

cumulative_accuracy_sim <- preds_sim %>%
  arrange(trial) %>%
  group_by(trial) %>%
  summarize(
    posterior_acc = mean(posterior_pred == correct),
    prior_acc = mean(prior_pred == correct)
  ) %>%
  mutate(
    cumul_posterior = cummean(posterior_acc),
    cumul_prior = cummean(prior_acc)
  )


# Plot cumulative accuracy
# First plot: no legend
plot_no_legend <- ggplot(cumulative_accuracy_sim, aes(x = trial)) +
  geom_line(aes(y = cumul_posterior, color = "Posterior"), linewidth = 1) +
  geom_line(aes(y = cumul_prior, color = "Prior"), linewidth = 1, linetype = "dashed") +
  scale_color_manual(values = c("Posterior" = "blue", "Prior" = "darkgreen")) +
  labs(title = "Cumulative Prediction Accuracy (Simulated data)",
       y = "Cumulative Accuracy", x = "Trial") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "none")

# Second plot: with legend at bottom
plot_with_legend <- ggplot(cumulative_accuracy_real, aes(x = trial)) +
  geom_line(aes(y = cumul_posterior, color = "Posterior"), linewidth = 1) +
  geom_line(aes(y = cumul_prior, color = "Prior"), linewidth = 1, linetype = "dashed") +
  scale_color_manual(values = c("Posterior" = "blue", "Prior" = "darkgreen")) +
  labs(title = "Cumulative Prediction Accuracy (Real data)",
       y = "Cumulative Accuracy", x = "Trial", color = "Model") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Arrange side-by-side
gridExtra::grid.arrange(plot_no_legend, plot_with_legend, nrow = 2)

# Compare models (sim vs real, or simple vs weighted)
loo_real <- loo(log_lik_real)
loo_sim  <- loo(log_lik_sim)

print(loo_real)
print(loo_compare(loo_real, loo_sim))
