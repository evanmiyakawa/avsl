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

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  int<lower=0> N_coeffs;
  int<lower=0> goals[N];
  int<lower=0> off_id[N];
  int<lower=0> def_id[N];
  real<lower=0> beta_sd;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real log_alpha;
  real log_beta[N_coeffs];
  
  // real beta_off[N_coeffs];
  // real beta_def[N_coeffs];
}

transformed parameters{
  real alpha = exp(log_alpha);
  vector[N] log_game_mean;
  
  
  for (i in 1:N) {
    // log_game_mean[i] = alpha + beta[off_id[i]] - beta[def_id[i]];
    log_game_mean[i] = log_alpha + log_beta[off_id[i]] - log_beta[def_id[i]];
    // game_mean[i] = alpha;
  }

  
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // vector[N] log_game_mean;
  // alpha ~ normal(mean(goals), 5);
  // log_alpha ~ normal(0, 100);
  log_beta ~ normal(0, beta_sd);
  // beta_off ~ normal(0, beta_sd);
  // beta_def ~ normal(0, beta_sd);
  
  goals ~ poisson(exp(log_game_mean));
}

generated quantities {
  // real beta[N_coeffs] = exp(log_beta);
  real off_xg[N_coeffs];
  real def_xg[N_coeffs];
  real beta[N_coeffs];
  
  for (i in 1:N_coeffs) {
    off_xg[i] = exp(log_alpha + log_beta[i] - mean(log_beta));
    def_xg[i] = exp(log_alpha - log_beta[i] + mean(log_beta));
  }
  
  vector[N] game_mean = exp(log_game_mean);

  
}
