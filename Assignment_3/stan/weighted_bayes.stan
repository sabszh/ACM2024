// weighted = different levels of influence that FirstRating and GroupRating have on the SecondRating

// Conceptionally we are using 1-8, but in beta_binomial space
// 0 is how we give "zero trust", so we shift it by 1

data {
    int<lower=0> N; // trials
    array[N] int<lower=0, upper=8> SecondRating_og; // what we try to predict
    array[N] int<lower=0, upper=8> FirstRating_og;
    array[N] int<lower=0, upper=8> GroupRating_og;
    }
    
transformed data {
  array[N] int<lower=0, upper=7> SecondRating;
  array[N] int<lower=0, upper=7> FirstRating;
  array[N] int<lower=0, upper=7> GroupRating;
  
  
  for (i in 1:N) {
    SecondRating[i] = SecondRating_og[i] -1;
    FirstRating[i]  = FirstRating_og[i] -1;
    GroupRating[i]  = GroupRating_og[i] -1;
    }

}

parameters{
    real<lower=0, upper=1> weight_direct; // Weight for FirstRating
    real<lower=0, upper=1> weight_social; // Weight GroupRating
    }

model{
    // priors
    target += normal_lpdf(weight_direct | 0, 0.3);
    target += normal_lpdf(weight_social | 0, 0.3);
    
    // each observation is a separate decision
    for (i in 1:N){
      // for this specific decision:
      real weighted_first_rating = FirstRating[i] * weight_direct;
      real weighted_first_rating2 = (7 - FirstRating[i]) * weight_direct;
      real weighted_group_rating = GroupRating[i] * weight_social;
      real weighted_group_rating2 = (7 - GroupRating[i]) *  weight_social;
      // calculate beta parameters for this decision
      real alpha_post = 1 + weighted_first_rating + weighted_group_rating;
      real beta_post = 1 + weighted_first_rating2 + weighted_group_rating2;
    
      // use beta_binomial distribution to integrate over the full posterior
      target += beta_binomial_lpmf(SecondRating[i] | 7, alpha_post, beta_post);
    }

}

generated quantities{
    // log likelihood and predictions
    vector[N] log_lik;
    array[N] int <lower=0, upper=7> posterior_pred_SecondRating;
    
    // generate samples from prior for prior predictive checks
    real weight_direct_prior = inv_logit(normal_rng(0, 0.3));
    real weight_social_prior = inv_logit(normal_rng(0, 0.3));
    
    for (i in 1:N){
      real weighted_first_rating = FirstRating[i] * weight_direct;
      real weighted_first_rating2 = (7 - FirstRating[i]) * weight_direct;
      real weighted_group_rating = GroupRating[i] * weight_social;
      real weighted_group_rating2 = (7 - GroupRating[i]) *  weight_social;
    
      real alpha_post = 1 + weighted_first_rating + weighted_group_rating;
      real beta_post = 1 + weighted_first_rating2 + weighted_group_rating2;
    
      // log likelihood using beta_binomial
      log_lik[i] = beta_binomial_lpmf(SecondRating[i] | 7, alpha_post, beta_post);
    
      // generate predictions from the full distribution
      posterior_pred_SecondRating[i] = beta_binomial_rng(7, alpha_post, beta_post); 
    }
    
    
    // generate prior predictions for model checking
    array[N] int prior_pred_SecondRating;
    
    for (i in 1:N){
      real prior_weighted_first_rating = FirstRating[i] * weight_direct_prior;
      real prior_weighted_first_rating2 = (7 - FirstRating[i]) * weight_direct_prior;
      real prior_weighted_group_rating = GroupRating[i] * weight_social_prior;
      real prior_weighted_group_rating2 = (7 - GroupRating[i]) * weight_social_prior;
      
      real prior_alpha_post = 1 + prior_weighted_first_rating + prior_weighted_group_rating;
      real prior_beta_post = 1 + prior_weighted_first_rating2 + prior_weighted_group_rating2;
      // generate prior predictions
      prior_pred_SecondRating[i] = beta_binomial_rng(7, prior_alpha_post, prior_beta_post);
      }
}


