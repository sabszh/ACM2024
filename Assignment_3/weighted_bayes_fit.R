# Package importing
pacman::p_load(tidyverse,
               here,
               posterior,
               cmdstanr,
               brms, tidybayes)


# Import the data
d <- read.csv("data/Simonsen_clean.csv")

df <- d %>% subset(ID == 229)

# Turn it into a list with n
data <- list(N = 153, trial = df$FaceID, FirstRating_og = df$FirstRating, GroupRating_og = df$GroupRating, SecondRating_og = df$SecondRating)

# Specify where the model is
file <- file.path("stan/weighted_bayes.stan")

# Compiling the model
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))

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

# Save the fitted model
samples$save_object("simmodels/weighted_bayes.rds")