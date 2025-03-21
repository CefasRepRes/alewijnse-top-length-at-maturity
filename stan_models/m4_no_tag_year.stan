// m4

// single change-point in length
//  "m4" = bf(
  //    growth_m4 ~ alpha +
  //      (b1a * length * step(omega - length)) +
  //      ((b1a * omega + b1b * (length - omega)) * step(length - omega)),
  //    omega ~ 1,
  //    b1a + b1b ~ 1

  data {
    int<lower=0> N;  // number of observations
    vector[N] growth;  // response variable
    vector[N] length;  // predictor variable
  }

  parameters {
    real alpha;  // overall intercept
    real<lower=0> sigma;  // residual standard deviation
    real omega_logit; // unconstrained omega
    real b1a;  // slope before omega
    real b1b;  // slope after omega
  }

  transformed parameters {
    vector[N] mu;
    real omega;

    omega = inv_logit(omega_logit) * (7.46 - (-3.07)) + (-3.07); // logit bounding of omega

    for (n in 1:N) {
      // linear predictor
      if (length[n] <= omega) {
        mu[n] = alpha + b1a * length[n];
      } else {
        mu[n] = alpha + b1a * omega + b1b * (length[n] - omega);
      }
    }
  }


  model {
    growth ~ normal(mu, sigma);  // likelihood
    alpha ~ normal(0, 3);  // prior
    b1a ~ normal(0, 1);  // prior
    b1b ~ normal(0, 1); // prior
    omega_logit ~ normal(0, 1); // prior
    sigma ~ normal(0, 1); // prior
  }

  generated quantities {
    vector[N] log_lik;
    for (n in 1:N) {
      if (length[n] <= omega) {
        log_lik[n] = normal_lpdf(growth[n] | alpha + b1a * length[n], sigma);
      } else {
        log_lik[n] = normal_lpdf(growth[n] | alpha + b1a * omega + b1b * (length[n] - omega), sigma);
      }
    }
  }
