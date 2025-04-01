data {
  int<lower=0> N; // Number of trials
  array[N] int<lower=0, upper=8> SecondRating_og; // Ratings we're trying to predict
  array[N] int<lower=0, upper=8> FirstRating_og;  // Direct evidence
  array[N] int<lower=0, upper=8> GroupRating_og;  // Social evidence
}

transformed data {
  array[N] int<lower=0, upper=7> SecondRating;
  array[N] int<lower=0, upper=7> FirstRating;
  array[N] int<lower=0, upper=7> GroupRating;

  for (i in 1:N) {
    SecondRating[i] = SecondRating_og[i] - 1;
    FirstRating[i]  = FirstRating_og[i]  - 1;
    GroupRating[i]  = GroupRating_og[i]  - 1;
  }
}

parameters {
  real<lower=0> total_weight;         // Total influence of all evidence
  real<lower=0, upper=1> weight_prop; // Proportion of weight for direct evidence
}

transformed parameters {
  real<lower=0> weight_direct = total_weight * weight_prop;
  real<lower=0> weight_social = total_weight * (1 - weight_prop);
}

model {
  // Priors
  target += lognormal_lpdf(total_weight | .8, .4);
  target += beta_lpdf(weight_prop | 1, 1);

  // Likelihood
  for (i in 1:N) {
    real weighted_first = FirstRating[i] * weight_direct;
    real weighted_first_comp = (7 - FirstRating[i]) * weight_direct;
    real weighted_group = GroupRating[i] * weight_social;
    real weighted_group_comp = (7 - GroupRating[i]) * weight_social;

    real alpha_post = 1 + weighted_first + weighted_group;
    real beta_post = 1 + weighted_first_comp + weighted_group_comp;

    target += beta_binomial_lpmf(SecondRating[i] | 7, alpha_post, beta_post);
  }
}

generated quantities {
  vector[N] log_lik;
  array[N] int posterior_pred_SecondRating;
  array[N] int prior_pred_SecondRating;

  // Prior samples for prior predictive checks
  real total_weight_prior = lognormal_rng(.8, .4);
  real weight_prop_prior = beta_rng(1, 1);
  real weight_direct_prior = total_weight_prior * weight_prop_prior;
  real weight_social_prior = total_weight_prior * (1 - weight_prop_prior);

  for (i in 1:N) {
    // Posterior predictions
    real weighted_first = FirstRating[i] * weight_direct;
    real weighted_first_comp = (7 - FirstRating[i]) * weight_direct;
    real weighted_group = GroupRating[i] * weight_social;
    real weighted_group_comp = (7 - GroupRating[i]) * weight_social;

    real alpha_post = 1 + weighted_first + weighted_group;
    real beta_post = 1 + weighted_first_comp + weighted_group_comp;

    log_lik[i] = beta_binomial_lpmf(SecondRating[i] | 7, alpha_post, beta_post);
    posterior_pred_SecondRating[i] = beta_binomial_rng(7, alpha_post, beta_post);

    // Prior predictions
    real prior_weighted_first = FirstRating[i] * weight_direct_prior;
    real prior_weighted_first_comp = (7 - FirstRating[i]) * weight_direct_prior;
    real prior_weighted_group = GroupRating[i] * weight_social_prior;
    real prior_weighted_group_comp = (7 - GroupRating[i]) * weight_social_prior;

    real alpha_prior = 1 + prior_weighted_first + prior_weighted_group;
    real beta_prior = 1 + prior_weighted_first_comp + prior_weighted_group_comp;

    prior_pred_SecondRating[i] = beta_binomial_rng(7, alpha_prior, beta_prior);
  }
}
