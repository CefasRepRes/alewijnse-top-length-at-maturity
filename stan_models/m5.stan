// m5

// no change-point in length, with tag year RE
// "m5" = bf(
  //   growth_m5 ~ alpha +
  //   b1a * length,
  //   alpha ~ 1 + (1 | tag_year),
  //   b1a ~ 1

  data {
    int<lower=0> N;  // number of observations
    int<lower=0> T; // number of tag years
    vector[N] growth;  // response variable
    vector[N] length;  // predictor variable
    array[N] int<lower=1, upper=T> tag_year;  // group-level predictor
  }

  parameters {
    real alpha;  // overall intercept
    real<lower=0> sigma;  // residual standard deviation
    real<lower=0> alpha_iid_sd; // sd of group level intercepts
    vector[T] alpha_base;  // random intercepts
    real b1a;  // slope
  }

  transformed parameters{
    vector[N] mu;

    vector[T] alpha_iid = alpha_iid_sd * alpha_base; // non-centred parameterisation

    for (n in 1:N) {
      // linear predictor
      mu[n] = alpha + alpha_iid[tag_year[n]] + b1a * length[n];
    }
  }

  model {
    growth ~ normal(mu, sigma);  // likelihood
    alpha ~ normal(0, 3);  // prior
    alpha_iid_sd ~ cauchy(0, 1);  // prior
    alpha_base ~ normal(0, 1); // prior
    b1a ~ normal(0, 3);  // prior
    sigma ~ cauchy(0, 1); // prior
  }

  generated quantities {
    vector[N] log_lik;
    for (n in 1:N) {
      log_lik[n] = normal_lpdf(growth[n] | alpha + alpha_iid[tag_year[n]] + b1a * length[n], sigma);
    }
  }

