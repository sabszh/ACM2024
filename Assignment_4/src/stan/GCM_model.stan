// GCM model: Categorization based on exemplar similarity
data {
  int<lower=1> trials;                      // number of trials
  int nfeatures;                            // number of binary features
  array[trials] int<lower=0, upper=1> danger;     // true label: is the stimulus dangerous?
  array[trials] int<lower=0, upper=1> response;   // participant's response (1 = dangerous)
  array[trials, nfeatures] int stimuli;     // binary feature values of each stimulus

  // Priors (these are specified when running the model)
  vector[nfeatures] w_prior_values;         // prior for feature attention weights
  array[2] real c_prior_values;             // prior for sensitivity parameter (normal on logit scale)
}

transformed data {
    array[trials] int<lower=0, upper=1> non_danger; // dummy variable
    array[sum(danger)] int<lower=1, upper=trials> danger_idx; // stimuli that are dangerous
    array[trials-sum(danger)] int<lower=1, upper=trials> non_danger_idx; // stimuli that are not dangerous


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
  real logit_c; // sensitivity parameter on logit scale
}

transformed parameters {
  // parameter c
  real<lower=0, upper=5> c = inv_logit(logit_c)*5; // map logit_c to [0, 5]
  
  // parameter r (probability of response = category 1)
  array[trials] real<lower=0.0001, upper=0.9999> r; // the real rate but bounded between 0.0001-0.9999
  array[trials] real rr; //real rate
  
  
  for (i in 1:trials) {
    
    // calculate distance from stimuli to all exemplars
    array[(i-1)] real exemplar_sim; // store exemplar similarity
    for (exemplar in 1:(i-1)){
        array[nfeatures] real dist;
        for (feature in 1:nfeatures){
            dist[feature] = w[feature]*abs(stimuli[exemplar,feature] - stimuli[i,feature]); // manhatten distance
        }
        exemplar_sim[exemplar] = exp(-c * sum(dist)); // scaling factor impacts sensitivity in distances
    }
  
    if (sum(danger[:(i-1)])==0 || sum(non_danger[:(i-1)])==0){ // if there are no exemplars in one of the categories, probability is random
        r[i] = 0.5;
    } else {
      // calculate similarity
      array[2] real similarities;
      
      array[sum(danger[:(i-1)])] int tmp_danger_idx = danger_idx[:sum(danger[:(i-1)])];
      array[sum(non_danger[:(i-1)])] int tmp_non_danger_idx = non_danger_idx[:sum(non_danger[:(i-1)])];
      
      similarities[1] = mean(exemplar_sim[tmp_danger_idx]);
      similarities[2] = mean(exemplar_sim[tmp_non_danger_idx]);
      
      // calculate rr[i]
      rr[i] = similarities[1] / (similarities[1] + similarities[2]); // Luce-choice rule - turns relative similarities into probabilities.
      
      // to make the sampling work
      if (rr[i] > 0.9999){
         r[i] = 0.9999;
      } else if (rr[i] < 0.0001) {
          r[i] = 0.0001;
      } else if (rr[i] > 0.0001 && rr[i] < 0.9999){
          r[i] = rr[i];
      } else {
          r[i] = 0.5;
      }
    }
  }
}

model {
  // Priors
  target += dirichlet_lpdf(w | w_prior_values);
  target += normal_lpdf(logit_c | c_prior_values[1], c_prior_values[2]); // normal prior

  // Likelihood
  target += bernoulli_lpmf(response | r);
}

generated quantities {
  // Prior predictive samples
  vector[nfeatures] w_prior = dirichlet_rng(w_prior_values); // sampling from the prior
  real<lower=0, upper=5> c_prior = inv_logit(normal_rng(c_prior_values[1], c_prior_values[2])) * 5; // sampling from the prior
  
  // Generating prior-predicted responses
  array[trials] real r_prior;
  array[trials] real rr_prior; // real rate
  
  for (i in 1:trials) {
      array[i-1] real exemplar_sim;
      
      for (exemplar in 1:(i-1)){
          array[nfeatures] real dist;
          for (feature in 1:nfeatures){
              dist[feature] = w[feature]*abs(stimuli[exemplar,feature] - stimuli[i,feature]);
          }
          exemplar_sim[exemplar] = exp(-c * sum(dist)); // scaling factor impacts sensitivity in distances
    }
    // if there are no exemplars in one of the categories, probability is random
    if (sum(danger[:(i-1)])==0 || sum(non_danger[:(i-1)])==0){ 
        r_prior[i] = 0.5;
    } else {
      // calculate similarity
      array[2] real similarities;
      
      array[sum(danger[:(i-1)])] int tmp_danger_idx = danger_idx[:sum(danger[:(i-1)])];
      array[sum(non_danger[:(i-1)])] int tmp_non_danger_idx = non_danger_idx[:sum(non_danger[:(i-1)])];
      
      similarities[1] = mean(exemplar_sim[tmp_danger_idx]);
      similarities[2] = mean(exemplar_sim[tmp_non_danger_idx]);
      
      // calculate r[i]
      rr_prior[i] = similarities[1] / (similarities[1] + similarities[2]); // Luce-choice rule - turns relative similarities into probabilities.
      
      // to make the sampling work
      if (rr_prior[i] > 0.9999){
         r_prior[i] = 0.9999;
      } else if (rr_prior[i] < 0.0001) {
          r_prior[i] = 0.0001;
      } else if (rr_prior[i] > 0.0001 && rr_prior[i] < 0.9999){
          r_prior[i] = rr_prior[i];
      } else {
          r_prior[i] = 0.5;
      }
    }
  }
    // the prior predictions
    array[trials] int<lower=0, upper=1> prior_preds = bernoulli_rng(r_prior);
    
    // Generate predictions using posterior samples
    array[trials] int<lower=0, upper=1> posterior_preds = bernoulli_rng(r);
    // Compare posterior predictions to true labels
    array[trials] int<lower=0, upper=1> posterior_true;
    
    for (i in 1:trials) {
        if (posterior_preds[i] == danger[i]) {
            posterior_true[i] = 1;
        } else {
            posterior_true[i] = 0;
        }
    }
    
    // log likelihood for each trial (needed for model fit metrics)
    array[trials] real log_lik; 

    for (i in 1:trials){
        log_lik[i] = bernoulli_lpmf(response[i] | r[i]);
}
}
