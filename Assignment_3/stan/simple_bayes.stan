// simple = same levels of influence that FirstRating and GroupRating have on the SecondRating

// Conceptionally we are using 1-8, but in beta_binomial space
// 0 is how we give "zero trust", so we shift it by 1

data {
    int<lower=0> N; // trials
    array[N] int<lower=1, upper=8> SecondRating_og; // what we try to predict
    array[N] int<lower=1, upper=8> FirstRating_og;
    array[N] int<lower=1, upper=8> GroupRating_og;
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


model{
    for (i in 1:N) {
        real alpha_post = 1 + FirstRating[i] + GroupRating[i]; // the first 1 is conceptually equal to a prior, for a beta distribution priors of (1,1) is the same as a uniform distribution
        real beta_post = 1 + (7 - FirstRating[i] )+ (7 - GroupRating[i]);
    
        target+= beta_binomial_lpmf(SecondRating[i] | 7, alpha_post, beta_post);
    }
    
}


generated quantities{
    vector[N] log_lik; // Log likelihood for model comparison
    
   // Prior and posterior predictive checks?
   array[N] int prior_pred_SecondRating;
   array[N] int posterior_pred_SecondRating;
   
   for (i in 1:N) {
     // For prior predictions, use uniform prior (Beta(1,1))
     prior_pred_SecondRating[i] = beta_binomial_rng(7,1,1);
     
     real alpha_post = 1 + FirstRating[i] + GroupRating[i];
     real beta_post = 1 + (7 - FirstRating[i] )+ (7 - GroupRating[i]);
     
     // Generate predictions using the complete beta-binomial model
     posterior_pred_SecondRating[i] = beta_binomial_rng(7, alpha_post, beta_post);
     
     // Log likelihood calculation using beta-binomial
     log_lik[i] = beta_binomial_lpmf(SecondRating[i] | 7, alpha_post, beta_post);
     } 
     }

