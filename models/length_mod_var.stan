// Scott equation 2

// length vs age with breakpoint

data {
  int<lower=0> N;  // number of observations
  vector[N] age;  // response variable
  vector[N] length;  // predictor variable
}

parameters {
  real theta0;
  real<lower=0> theta1;
  real<upper=0> theta2;
  real<lower=0, upper=63> delta;
  real gamma;
}

transformed parameters{
  // linear predictor
  vector[N] mu;
  for (n in 1:N) {
    mu[n] = theta0 + theta1 * (age[n] - delta) + theta2 * sqrt(square(age[n] - delta) + gamma);
  }

  // increasing variance with increasing length
  vector<lower=0>[N] sigma;
  for(n in 1:N) {
    sigma[n] = mu[n] * tau
  }
}

model {
  length ~ normal(mu, sigma);

  // priors
  theta0 ~ normal(0, 30);
  theta1 ~ normal(0, 30);
  theta2 ~ normal(0, 30);
  delta ~ uniform(0, 63);
  gamma ~ normal(0, 30);
  tau ~ normal(0, 30);
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(length[n] | theta0 + theta1 * (age[n] - delta) + theta2 * sqrt(square(age[n] - delta) + gamma), sigma);
  }
}
