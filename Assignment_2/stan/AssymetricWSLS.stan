// Assignment 2
// This stan program defines a probabilistic win stay lose shift model

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> N;
  array[N] int trial; //vector for trials
  
  array[N] int<lower=0, upper=1> oppchoice; //vector for  opponent's choice
  
  array[N] int<lower=0, upper=1> selfchoice; //vector for your own choice
}


// The parameters accepted by the model are theta_w and theta_l

parameters {
  real theta_w; //0.8 in generating model

  real theta_l; //0.9 in generating model

}


// The model to be estimated. 

model {
  
  target += normal_lpdf(theta_w|0, 1); // prior for theta in wins (theta_w is centered around log odds of 0 = 0.5 in probability space and has a standard deviation of 1 - 0.73 in probability space)
  
  target += normal_lpdf(theta_l|0, 1); //prior for theta in losses
  


for (n in 2:N ){
    if (oppchoice[n-1]==selfchoice[n-1] && selfchoice[n-1] == 1){
        target += bernoulli_logit_lpmf(selfchoice[n] | theta_w); }
    if (oppchoice[n-1]==selfchoice[n-1] && selfchoice[n-1] == 0){
        target += bernoulli_logit_lpmf(selfchoice[n] | -theta_w); }
    if (oppchoice[n-1]!=selfchoice[n-1] && selfchoice[n-1] == 1){
        target += bernoulli_logit_lpmf(selfchoice[n] | -theta_l); }
    if (oppchoice[n-1]!=selfchoice[n-1] && selfchoice[n-1] == 0){
        target += bernoulli_logit_lpmf(selfchoice[n] | theta_l); }
    }
}

generated quantities {
  real theta_w_prior;
  real theta_w_posterior;
  real theta_l_prior;
  real theta_l_posterior;
    
  array[N] int<lower=0, upper=N> prior_preds;
  array[N] int<lower=0, upper=N> posterior_preds;
  
  theta_w_prior = inv_logit(normal_rng(0,1));
  theta_l_prior = inv_logit(normal_rng(0,1));
  theta_w_posterior = inv_logit(theta_w);
  theta_l_posterior = inv_logit(theta_l);
  
  prior_preds[1] = bernoulli_rng(0.5);
  posterior_preds[1] = bernoulli_rng(0.5);
  
  for (n in 2:N ){
    if (oppchoice[n-1]==selfchoice[n-1] && selfchoice[n-1] == 1){
        prior_preds[n] = bernoulli_rng(theta_w_prior); }
    if (oppchoice[n-1]==selfchoice[n-1] && selfchoice[n-1] == 0){
        prior_preds[n] = bernoulli_rng(1-theta_w_prior); }
    if (oppchoice[n-1]!=selfchoice[n-1] && selfchoice[n-1] == 1){
        prior_preds[n] = bernoulli_rng(1-theta_l_prior); }
    if (oppchoice[n-1]!=selfchoice[n-1] && selfchoice[n-1] == 0){
        prior_preds[n] = bernoulli_rng(theta_l_prior); }
    }
  
  for (n in 2:N ){
    if (oppchoice[n-1]==selfchoice[n-1] && selfchoice[n-1] == 1){
        posterior_preds[n] = bernoulli_rng(theta_w_posterior); }
    if (oppchoice[n-1]==selfchoice[n-1] && selfchoice[n-1] == 0){
        posterior_preds[n] = bernoulli_rng(1-theta_w_posterior); }
    if (oppchoice[n-1]!=selfchoice[n-1] && selfchoice[n-1] == 1){
        posterior_preds[n] = bernoulli_rng(1-theta_l_posterior); }
    if (oppchoice[n-1]!=selfchoice[n-1] && selfchoice[n-1] == 0){
        posterior_preds[n] = bernoulli_rng(theta_l_posterior); }
    }
}