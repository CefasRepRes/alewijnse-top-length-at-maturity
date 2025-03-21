rm(list = ls())

library(here)
library(data.table)
library(ggplot2)
library(brms)
library(bayesplot)
library(truncnorm)
library(patchwork)
library(loo)
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# set seed for reproducibility
set.seed(1408)

# load data
load(here::here("data", "top-maturity-modeldata.RData"))

## choose outlier removal to use
dat <- dat_lst$iqr_3yr_data

# numbers
n_total <- 3408
n_years <- 16
n_per_year <- rep(n_total / n_years, n_years)
p_female <- 0.58
n_female_per_year <- rbinom(n_years, n_per_year, p_female)
n_male_per_year <- n_per_year - n_female_per_year

# fixed and linear coefficients
alpha <- 0.8 # intercept
b_length_a <- -0.005 # -0.35 # effect of length before breakpoint (based on model data estimates)
b_length_b <- - 0.0025 # -0.5 # effect of length after breakpoint (based on model data estimates)

# covariates
length_sex_means <- c("female" = 83, "male" = 79) # mean length f/m
length_sex_sds <- c("female" = 10, "male" = 8.5)
temperature_means <- rnorm(n_years, mean = 1.4, sd = 0.01) # seq(1:n_years) # test
df <- data.table(recap_year = rep(1:n_years, n_per_year),
                 length = c(unlist(sapply(n_female_per_year,
                                          rnorm,
                                          length_sex_means["female"],
                                          length_sex_sds["female"])),
                            unlist(sapply(n_male_per_year,
                                          rnorm,
                                          length_sex_means["male"],
                                          length_sex_sds["male"]))),
                 sex = factor(rep(c(1, 2),
                                  c(sum(n_female_per_year),
                                    sum(n_male_per_year))),
                              labels = c("female", "male")),
                 temperature = rnorm(n_total,
                                     temperature_means[rep(1:n_years,
                                                           n_per_year)],
                                     0.3))
df[, "tag_year" := (recap_year + 3) - sample(x = 1:3, size = n_total,
                                             prob = c(0.33, 0.33, 0.33), replace = TRUE)]
df[, "year" := as.numeric(recap_year)]

# random intercept with tag year
alpha_sd <- 0.03
alpha_iid <- rnorm(n = length(unique(df$tag_year)), mean = 0, sd = alpha_sd) # effect of each year

# random breakpoints
brk_0 <- 70

# response variables
min_growth <- min(dat$growth_raised)
eps <- 2

# set sex as binary - female default
df[, sex := ifelse(sex == "female", 0, 1)]

# single change-point in length, with tag year RE
df[, "m4" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0,
            b_length_a * length,
            b_length_a * brk_0 + b_length_b * (length - brk_0)),
   by = .I]

# sample response from normal distribution
# df[, "growth" := rtruncnorm(.N, mean = m4, sd = eps, a = min_growth)]
df[, "growth" := rlnorm(.N, meanlog = log(m4), sdlog = log(eps))]

# brms formula
# bform_switch <- brms::bf(growth | trunc(lb = 0) ~ alpha + b1a * inv_logit((omega - length) * 5) +
#                            b1b * inv_logit((length - omega) * 5),
#                          # keep omega within the range of predictor
#                          brms::nlf(omega ~ inv_logit(omegalogit) * 120),
#                          alpha ~ 1 + (1 | tag_year),
#                          b1a + b1b + omegalogit ~ 1,
#                          nl = TRUE)
bform_switch <- brms::bf(growth ~ alpha + b1a * inv_logit((omega - length) * 5) +
                           b1b * inv_logit((length - omega) * 5),
                         # keep omega within the range of predictor
                         brms::nlf(omega ~ inv_logit(omegalogit) * 120),
                         alpha ~ 1 + (1 | tag_year),
                         b1a + b1b + omegalogit ~ 1,
                         nl = TRUE)

# priors
bprior <- prior(normal(0, 2), nlpar = "alpha") +
  prior(normal(0, 2), nlpar = "b1a") +
  prior(normal(0, 2), nlpar = "b1b") +
  prior(normal(0, 1), nlpar = "omegalogit")

# fit
# fit_m4 <- brms::brm(bform_switch, data = df, prior = bprior)
fit_m4 <- brms::brm(bform_switch, data = df, prior = bprior, family = lognormal)

# plot
plot(brms::conditional_effects(fit_m4, method = "posterior_predict"), points = TRUE)


stop()

# likelihood tests
loo(fit_m4)
log_lik(fit_m4)

# model
cat("

// m4

// single change-point in length, with tag year RE
//  bf(
//    growth_m4 ~ alpha + b1a * inv_logit((omega - length) * 5) +
//      b1b * inv_logit((length - omega) * 5),
//    alpha ~ 1 + (1 | tag_year),
//    omega ~ 1,
//    b1a + b1b ~ 1

data {
  int<lower=0> N;  // number of observations
  int<lower=1> T; // number of tag years
  array[N] int<lower=1, upper=T> tag_year;  // group-level predictor
  vector[N] growth;  // response variable
  vector[N] length;  // predictor variable
}

parameters {
  real alpha; // overall intercept
  real<lower=0> sigma;  // residual standard deviation
  real<lower=0> alpha_iid_sd;  // SD of group-level intercepts
  vector[T] alpha_iid; // random intercepts
  real omega; // breakpoint
  real b1a;  // slope before omega
  real b1b;  // slope after omega
}

transformed parameters {
  vector[N] mu;
  // linear predictor
  for (n in 1:N) {
    mu[n] = alpha + alpha_iid[tag_year[n]] + b1a * inv_logit((omega - length[n]) * 8.6) + b1b * inv_logit((length[n] - omega) * 8.6);
  }
}

model {
  growth ~ normal(mu, sigma);  // likelihood
  alpha ~ normal(0, 3);  // prior
  alpha_iid_sd ~ normal(0, 3); // prior
  // alpha_base ~ normal(0, 1);  // prior
  alpha_iid ~ normal(0, alpha_iid_sd);  // prior
  b1a ~ normal(0, 3);  // prior
  b1b ~ normal(0, 3); // prior
  // omega_logit ~ normal(0, 3); // prior
  omega ~ normal(0, 3); // prior
  sigma ~ normal(0, 3); // prior
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(growth[n] | alpha + alpha_iid[tag_year[n]] + b1a * inv_logit((omega - length[n]) * 8.6) + b1b * inv_logit((length[n] - omega) * 8.6), sigma);
  }
}

", file = here("stan_models", "m4_logit_example.stan"))


