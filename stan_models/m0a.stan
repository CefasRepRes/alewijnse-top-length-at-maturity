// m0a

// "m0a" = bf(
//    growth ~ alpha +
//      (b1a * length * step(omega - length)) +
//      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
//      (b2 * sex) + (b3 * temperature),
//    alpha ~ 1 + (1 | tag_year),
//    omega ~ 1 + sex + (1 | recap_year),
//    b1a + b1b + b2 + b3 ~ 1,

  data {
    int<lower=0> N;  // number of observations
    int<lower=0> R; // number of recapture years
    int<lower=0> T; // number of tag years
    array[N] int<lower=1, upper=T> tag_year;  // group-level predictor
    array[N] int<lower=1, upper=R> recap_year; // random effect
    array[N] int<lower=0, upper=1> sex; // binary
    vector[N] growth;  // response variable
    vector[N] length;  // predictor variable
    vector[N] temperature; // temperature
  }

  parameters {
    real alpha;  // overall intercept
    real<lower=0> sigma;  // residual standard deviation
    real<lower=0> alpha_iid_sd; // sd of group level intercepts
    vector[T] alpha_iid;  // random intercepts
    real<lower=-3.07, upper=7.46> omega_0;  // breakpoint base
    real omega_sex; // effect of sex
    real<lower=0> omega_recap_sd; // omega recap sd
    vector[R] omega_recap; // omega with recap year
    real b1a;  // slope before omega
    real b1b;  // slope after omega
    real b2;  // effect of sex on growth
    real b3;  // effect of temperature on growth
  }

  transformed parameters {
    vector[N] mu;
    real omega;

    for (n in 1:N) {
      // compute omega
      omega = omega_0 + omega_sex * sex[n] + omega_recap[recap_year[n]];

      // linear predictor
      if (length[n] <= omega) {
        mu[n] = alpha + alpha_iid[tag_year[n]] + b1a * length[n] + b2 * sex[n] + b3 * temperature[n];
      } else {
        mu[n] = alpha + alpha_iid[tag_year[n]] + b1a * omega + b1b * (length[n] - omega) + b2 * sex[n] + b3 * temperature[n];
      }
    }
  }


  model {
    growth ~ normal(mu, sigma);  // likelihood
    alpha ~ normal(0, 3);  // prior
    alpha_iid_sd ~ cauchy(0, 1);  // prior
    alpha_iid ~ normal(0, alpha_iid_sd); // prior
    b1a ~ normal(0, 3);  // prior
    b1b ~ normal(0, 3); // prior
    b2 ~ normal(0, 3); // prior
    b3 ~ normal(0, 3); // prior
    omega_recap_sd ~ cauchy(0, 1); // prior
    omega_recap ~ normal(0, omega_recap_sd); // prior
    omega_sex ~ normal(0, 3); // prior
    omega_0 ~ normal(0, 3); // prior
    sigma ~ cauchy(0, 1); // prior
  }

  generated quantities {
    vector[N] log_lik;
    for (n in 1:N) {
      if (length[n] <= omega) {
        log_lik[n] = normal_lpdf(growth[n] | alpha + alpha_iid[tag_year[n]] + b1a * length[n] + b2 * sex[n] + b3 * temperature[n], sigma);
      } else {
        log_lik[n] = normal_lpdf(growth[n] | alpha + alpha_iid[tag_year[n]] + b1a * omega + b1b * (length[n] - omega) + b2 * sex[n] + b3 * temperature[n], sigma);
      }
    }
  }
