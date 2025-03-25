// Scott equation 2

// length vs age with breakpoint

data {
  int<lower=1> N; // number of data points
  vector[N] age;
  vector[N] length;
}

parameters {
  real theta0;
  real theta1;
  real theta2;
  real<lower=2, upper=62> delta; // breakpoint
  real<lower=0> gamma;
  real<lower=0> sigma;
}

model {
  // priors
  theta0 ~ normal(0, 30);
  theta1 ~ normal(0, 30);
  theta2 ~ normal(0, 30);
  delta ~ uniform(2, 62);
  gamma ~ normal(0, 30);
  sigma ~ gamma(0.01, 0.01);

  // likelihood
  vector[N] mu;
  for (n in 1:N) {
    mu[n] = theta0 + theta1 * (age[n] - delta) + theta2 * sqrt(square(age[n] - delta) + gamma);
    length[n] ~ normal(mu[n], sigma);
  }
}

generated quantities {
  // generate log likelihood
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(length[n] | theta0 + theta1 * (age[n] - delta) + theta2 * sqrt(square(age[n] - delta) + gamma), sigma);
  }
}

