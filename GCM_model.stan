// GCM model
data {
  int<lower=1> trials;
  int nfeatures = 5;
  array[trials] int<lower=0, upper=1> danger; // if the stimuli is actually dangerous
  array[trials] int<lower=0, upper=1> response_danger; // the participant's response of danger
  array[trials, nfeatures] int stimuli;
  
  // Priors
  vector[nfeatures] w_prior_values; // prior for feature weight
  array[2] real c_prior_values; // prior for scaling parameter
}

transformed data {
    array[ntrials] int<lower=0, upper=1> non_danger; // dummy variable
    array[sum(danger)] int<lower=1, upper=ntrials> danger_idx; // stimuli that are dangerous
    array[ntrials-sum(danger)] int<lower=1, upper=ntrials> non_danger_idx; // stimuli that are not dangerous


    int count_idx_danger = 1; 
    int count_idx_non_danger = 1;

    for (i in 1:trials){
        non_danger[i] = abs(danger[i] - 1);

        if (danger[i] == 1){
            danger_idx[count_idx_danger] = i;
            count_idx_danger += 1;

        } else {
            non_danger_idx[count_idx_non_danger] = i;
            count_idx_non_danger += 1;
        }

    }

}

parameters {
  simplex[nfeatures] w;  // simplex means sum(w)=1
  real logit_c; // scaling parameter
}

transformed parameters {
  
}

model{
    // priors 
    target += dirichlet_lpdf(w | w_prior_values);
    target += normal_lpdf(logit_c | c_prior_values[1], c_prior_values[2]);

    // model response
    target += bernoulli_lpmf(response | learning_rate);
}
