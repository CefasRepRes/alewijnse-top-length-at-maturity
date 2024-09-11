// m3a

// year- & sex-specific change-points in length, with tag year RE
//  "m3a" = bf(
//    growth ~ alpha +
//      (b1a * length * step(omega - length)) +
//      ((b1a * omega + b1b * (length - omega)) * step(length - omega)),
//    alpha ~ 1 + (1 | tag_year),
//    omega ~ 1 + sex + (1 | recap_year),
//    b1a + b1b ~ 1,
//    nl = TRUE
//  ),

  data {
    int<lower=0> N;  // number of observations
    int<lower=0> R; // number of recapture years
    int<lower=0> T; // number of tag years
    array[N] int<lower=1, upper=T> tag_year;  // group-level predictor
    array[N] int<lower=1, upper=R> recap_year; // random effect
    array[N] int<lower=0, upper=1> sex; // binary
    vector[N] growth;  // response variable
    vector[N] length;  // predictor variable
  }

  parameters {
    real alpha;  // overall intercept
    real<lower=0> sigma;  // residual standard deviation
    real<lower=0> alpha_iid_sd; // sd of group level intercepts
    vector[T] alpha_base;  // random intercepts
    real omega_logit;  // unconstrained omega base
    real<lower=0> omega_recap_sd; // omega recap sd
    vector[R] omega_recap_base; // omega with recap year
    real omega_sex; // effect of sex
    real b1a;  // slope before omega
    real b1b;  // slope after omega
  }

  transformed parameters {
    vector[N] mu;
    real omega;
    real omega_0;

    vector[T] alpha_iid = alpha_iid_sd * alpha_base; // non-centred parameterisation

    vector[R] omega_recap = omega_recap_sd * omega_recap_base; // non-centred parameterisation

    omega_0 = inv_logit(omega_logit) * (7.46 - (-3.07)) + (-3.07); // logit bounding of omega

    for (n in 1:N) {
      // compute omega
      omega = omega_0 + omega_sex * sex[n] + omega_recap[recap_year[n]];

      // linear predictor
      if (length[n] <= omega) {
        mu[n] = alpha + alpha_iid[tag_year[n]] + b1a * length[n];
      } else {
        mu[n] = alpha + alpha_iid[tag_year[n]] + b1a * omega + b1b * (length[n] - omega);
      }
    }
  }


  model {
    growth ~ normal(mu, sigma);  // likelihood
    alpha ~ normal(0, 3);  // prior
    alpha_iid_sd ~ cauchy(0, 1);  // prior
    alpha_base ~ normal(0, 3); // prior
    b1a ~ normal(0, 3);  // prior
    b1b ~ normal(0, 3); // prior
    omega_recap_sd ~ cauchy(0, 1); // prior
    omega_recap_base ~ normal(0, 3); // prior
    omega_sex ~ normal(0, 3); // prior
    omega_logit ~ normal(0, 3); // prior
    sigma ~ cauchy(0, 1); // prior
  }

  generated quantities {
    vector[N] log_lik;
    for (n in 1:N) {
      if (length[n] <= omega) {
        log_lik[n] = normal_lpdf(growth[n] | alpha + alpha_iid[tag_year[n]] + b1a * length[n], sigma);
      } else {
        log_lik[n] = normal_lpdf(growth[n] | alpha + alpha_iid[tag_year[n]] + b1a * omega + b1b * (length[n] - omega), sigma);
      }
    }
  }
