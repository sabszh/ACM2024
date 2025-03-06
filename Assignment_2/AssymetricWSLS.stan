//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//


// Assignment 2
// This stan program defines a probabilistic win stay lose shift model

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> N;
  array[N] int trial; //vector for trials
  
  array[N] int<lower=0, upper=1> MEM; //vector for  opponent's choice
  
  array[N] int<lower=0, upper=1> WSLS; //vector for your own choice
}


// The parameters accepted by the model are theta_w and theta_l

parameters {
  real<lower=0, upper=1> theta_w; //0.8 in generating model

  real<lower=0, upper=1> theta_l; //0.9 in generating model

}


// The model to be estimated. 

model {
  target += normal_lpdf(theta_w | 0, 1); // prior for theta in wins
  target += normal_lpdf(theta_l | 0, 1); // prior for theta in losses

  for (i in 2:N) {
    if (MEM[i-1] == WSLS[i-1] && WSLS[i-1] == 1) {
      target += bernoulli_logit_lpmf(WSLS[i] | theta_w);
    }
    if (MEM[i-1] == WSLS[i-1] && WSLS[i-1] == 0) {
      target += bernoulli_logit_lpmf(WSLS[i] | -theta_w);
    }
    if (MEM[i-1] != WSLS[i-1] && WSLS[i-1] == 1) {
      target += bernoulli_logit_lpmf(WSLS[i] | -theta_l);
    }
    if (MEM[i-1] != WSLS[i-1] && WSLS[i-1] == 0) {
      target += bernoulli_logit_lpmf(WSLS[i] | theta_l);
    }
  }
}

generated quantities {
  real<lower=0, upper=1> theta_w_prior;
  real<lower=0, upper=1> theta_w_posterior;
  real<lower=0, upper=1> theta_l_prior;
  real<lower=0, upper=1> theta_l_posterior;
  
  int prior_w_preds;
  int posterior_w_preds;
  int prior_l_preds;
  int posterior_l_preds;

  theta_w_prior = inv_logit(normal_rng(0, 1));
  theta_l_prior = inv_logit(normal_rng(0, 1));
  theta_w_posterior = inv_logit(theta_w);
  theta_l_posterior = inv_logit(theta_l);

  prior_w_preds = binomial_rng(N, theta_w_prior);
  prior_l_preds = binomial_rng(N, theta_l_prior);
  posterior_w_preds = binomial_rng(N, theta_w_posterior);
  posterior_l_preds = binomial_rng(N, theta_l_posterior);
}



