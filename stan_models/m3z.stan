// m3z

// "m3z" = bf(
//    growth ~ alpha +
//      (b1a * length * step(omega - length)) +
//      ((b1a * omega + b1b * (length - omega)) * step(length - omega)),
//    alpha ~ 1 + (1 | tag_year),
//    omega ~ 1 + sex + b3 * temp
//    b1a + b1b ~ 1,


  data {
    int<lower=0> N;  // number of observations
    int<lower=0> T; // number of tag years
    array[N] int<lower=1, upper=T> tag_year;  // group-level predictor
    array[N] int<lower=0, upper=1> sex; // binary
    vector[N] growth;  // response variable
    vector[N] length;  // predictor variable
    vector[N] temperature; // temperature
  }

  parameters {
    real alpha;  // overall intercept
    real<lower=0> sigma;  // residual standard deviation
    real<lower=0> alpha_iid_sd; // sd of group level intercepts
    vector[T] alpha_base;  // random intercepts
    real omega_0;  //  omega base
    real omega_sex; // effect of sex
    real<lower=0> omega_temp_sd; // omega temp sd
    real omega_temp_base; // omega with temp
    real b1a;  // slope before omega
    real b1b;  // slope after omega
  }

  transformed parameters {
    vector[N] mu;
    vector[N] omega;
    real omega_logit;

    vector[T] alpha_iid = alpha_iid_sd * alpha_base; // non-centred parameterisation

    real omega_temp = omega_temp_sd * omega_temp_base; // non-centred parameterisation

    for (n in 1:N) {
      // compute omega
      omega_logit = omega_sex * sex[n] + omega_temp * temperature[n];

      omega[n] = inv_logit(omega_logit) * (7.46 - (-3.07)) + (-3.07); // logit bounding of omega

      // linear predictor
      if (length[n] <= omega[n]) {
        mu[n] = alpha + alpha_iid[tag_year[n]] + b1a * length[n];
      } else {
        mu[n] = alpha + alpha_iid[tag_year[n]] + b1a * omega[n] + b1b * (length[n] - omega[n]);
      }
    }
  }


  model {
    growth ~ normal(mu, sigma);  // likelihood
    alpha ~ normal(0, 3);  // prior
    alpha_iid_sd ~ normal(0, 3);  // prior
    alpha_base ~ normal(0, 1); // prior
    b1a ~ normal(0, 3);  // prior
    b1b ~ normal(0, 3); // prior
    omega_temp_sd ~ normal(0, 3); // prior
    omega_temp_base ~ normal(0, 1); // prior
    omega_sex ~ normal(0, 3); // prior
    omega_0 ~ normal(0, 3); // prior
    sigma ~ normal(0, 3); // prior
  }

  generated quantities {
    vector[N] log_lik;
    for (n in 1:N) {
      if (length[n] <= omega[n]) {
        log_lik[n] = normal_lpdf(growth[n] | alpha + alpha_iid[tag_year[n]] + b1a * length[n], sigma);
      } else {
        log_lik[n] = normal_lpdf(growth[n] | alpha + alpha_iid[tag_year[n]] + b1a * omega[n] + b1b * (length[n] - omega[n]), sigma);
      }
    }
  }
