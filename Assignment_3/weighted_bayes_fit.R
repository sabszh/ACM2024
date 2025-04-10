# Package importing
pacman::p_load(tidyverse,
               here,
               posterior,
               cmdstanr,
               brms, tidybayes)


# Import the data
d <- read.csv("data/Simonsen_clean.csv")
d2 <- read.csv("data/simulated_weighted_bayes.csv")

df <- d %>% subset(ID == 201)
df2 <- d2 %>% subset(ID == 3)

# Turn it into a list with n
data <- list(N = 153, trial = df$FaceID, FirstRating_og = df$FirstRating, GroupRating_og = df$GroupRating, SecondRating_og = df$SecondRating)
data2 <- list(N = 153, trial = df2$trial, FirstRating_og = df2$FirstRating, GroupRating_og = df2$GroupRating, SecondRating_og = df2$SecondRating)

# Specify where the model is
file <- file.path("stan/weighted_bayes.stan")

# Compiling the model
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)


# Commands to call Stan with specific options
samples <- mod$sample(
  data = data,
  seed = 0901,
  chains = 2,
  parallel_chains = 2,
  threads_per_chain = 2,
  iter_warmup = 1000,
  refresh = 100,
  output_dir = "simmodels",
  max_treedepth = 20,
  adapt_delta = 0.99
)

samples2 <- mod$sample(
  data = data2,
  seed = 0901,
  chains = 2,
  parallel_chains = 2,
  threads_per_chain = 2,
  iter_warmup = 1000,
  refresh = 100,
  output_dir = "simmodels",
  max_treedepth = 20,
  adapt_delta = 0.99
)

# Save the fitted model
samples$save_object("simmodels/weighted_bayes_realdata.rds")
samples2$save_object("simmodels/weighted_bayes_sims.rds")