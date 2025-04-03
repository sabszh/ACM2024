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
        SecondRating[i] = SecondRating_og[i] - 1;
        FirstRating[i]  = FirstRating_og[i] - 1;
        GroupRating[i]  = GroupRating_og[i] - 1;
    }
}

parameters {
    real<lower=0> total_weight; 
    real<lower=0, upper=1> weight_prop; // Proportion of weight assigned to direct influence
}

transformed parameters {
    real<lower=0> weight_direct = total_weight * weight_prop;
    real<lower=0> weight_social = total_weight * (1 - weight_prop);
}

model {
    // Priors
    target+= normal_lpdf(total_weight | 1, 0.1); 
    target+= beta_lpdf(weight_prop | 1, 1); 

    for (i in 1:N) {
        real weighted_first_rating = FirstRating[i] * weight_direct;
        real weighted_first_rating2 = (7 - FirstRating[i]) * weight_direct;
        real weighted_group_rating = GroupRating[i] * weight_social;
        real weighted_group_rating2 = (7 - GroupRating[i]) * weight_social;

        // Calculate beta parameters
        real alpha_post = 1 + weighted_first_rating + weighted_group_rating;
        real beta_post = 1 + weighted_first_rating2 + weighted_group_rating2;

        // Likelihood
        target += beta_binomial_lpmf(SecondRating[i] | 7, alpha_post, beta_post);
    }
}


generated quantities {
    // Log likelihood and posterior predictions
    vector[N] log_lik;
    array[N] int<lower=0, upper=7> posterior_pred_SecondRating;

    for (i in 1:N) {
        real weighted_first_rating = FirstRating[i] * weight_direct;
        real weighted_first_rating2 = (7 - FirstRating[i]) * weight_direct;
        real weighted_group_rating = GroupRating[i] * weight_social;
        real weighted_group_rating2 = (7 - GroupRating[i]) * weight_social;

        real alpha_post = 1 + weighted_first_rating + weighted_group_rating;
        real beta_post = 1 + weighted_first_rating2 + weighted_group_rating2;

        // Log likelihood
        log_lik[i] = beta_binomial_lpmf(SecondRating[i] | 7, alpha_post, beta_post);

        // Posterior predictive check
        posterior_pred_SecondRating[i] = beta_binomial_rng(7, alpha_post, beta_post);
    }

    // ---- Prior Predictive Checks ----
    // Generate prior samples in generated quantities
    real total_weight_prior;
    real weight_prop_prior;
    real weight_direct_prior;
    real weight_social_prior;

    // Sample prior values
    total_weight_prior = normal_rng(1, 0.1);
    weight_prop_prior = beta_rng(1, 1);
    
    weight_direct_prior = total_weight_prior * weight_prop_prior;
    weight_social_prior = total_weight_prior * (1 - weight_prop_prior);

    array[N] int prior_pred_SecondRating;
    
    for (i in 1:N) {
        real prior_weighted_first_rating = FirstRating[i] * weight_direct_prior;
        real prior_weighted_first_rating2 = (7 - FirstRating[i]) * weight_direct_prior;
        real prior_weighted_group_rating = GroupRating[i] * weight_social_prior;
        real prior_weighted_group_rating2 = (7 - GroupRating[i]) * weight_social_prior;

        real prior_alpha_post = 1 + prior_weighted_first_rating + prior_weighted_group_rating;
        real prior_beta_post = 1 + prior_weighted_first_rating2 + prior_weighted_group_rating2;

        // Generate prior predictions
        prior_pred_SecondRating[i] = beta_binomial_rng(7, prior_alpha_post, prior_beta_post);
    }
}