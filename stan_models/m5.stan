// m5

// no change-point in length, with tag year RE
// "m5" = bf(
  //   growth_m5 ~ alpha +
  //   b1a * length,
  //   alpha ~ 1 + (1 | tag_year),
  //   b1a ~ 1,
  //   nl = TRUE
  // )

  data {
    int<lower=0> N;  // number of observations
    vector[N] growth;  // response variable
    vector[N] length;  // predictor variable
    array[N] int<lower=1> tag_year;  // group-level predictor
  }

  parameters {
    real alpha;  // overall intercept
    real<lower=0> sigma;  // residual standard deviation
    vector[N] alpha_iid;  // group-level intercepts
    real b1a;  // slope
  }

  transformed parameters{
    vector[N] mu;
    for (n in 1:N) {
      // linear predictor
      mu[n] = alpha + alpha_iid[tag_year[n]] + b1a * length[n];
    }
  }

  model {
    growth ~ normal(mu, sigma);  // likelihood
    alpha ~ normal(0, 3);  // prior
    alpha_iid ~ normal(0, 3);  // prior
    b1a ~ normal(0, 3);  // prior
    sigma ~ cauchy(0, 1); // prior
  }

  generated quantities {
    vector[N] log_lik;
    for (n in 1:N) {
      log_lik[n] = normal_lpdf(growth[n] | alpha + alpha_iid[tag_year[n]] + b1a * length[n], sigma);
    }
  }

