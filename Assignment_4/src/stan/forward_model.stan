// forward model
data {
  int<lower=1> trials;
  int nfeatures = 5;
  array[ntrials] int<lower=0, upper=4> category;
  array[ntrials] int<lower=0, upper=4> response;
  array[ntrials, nfeatures] int obs;
  
  // Priors
  vector[nfeatures] w_prior_values;
  array[2] real c_prior_values;
}

parameters {
  simplex[nfeatures] w;  // simplex means sum(w)=1
  real logit_c;

}

transformed parameters {
  
  
  
}