#### Simulation testing ####

library(rstan)

# Load actual data -------------------------------------------------------------

# load data
source("C:/Users/sa20/OneDrive - CEFAS/Projects/southern_ocean/r-projects/master-data-wrangling/code/data-prep-483-TOP-age.R")

summary(TOP_all_age_dat)

# Generate simulated data ------------------------------------------------------

# constants
n <- 1000
x <- 2:63
age <- sample(x = x, size = n, replace = TRUE)

# fit function
fit_function <- function(x, t) {
  sapply(1:ncol(t), function(v) t["theta0", v] +
           t["theta1", v] * (x - t["delta", v]) +
           t["theta2", v] * sqrt((x - t["delta", v])^2 + t["gamma", v]))
}

# coefs
coefs <- c("theta0" = 50,
           "theta1" = 2.5,
           "theta2" = -1.5,
           "delta" = 10,
           "gamma" = 0.1)

# model error
sd <- 3

# data
length <- fit_function(age, as.matrix(coefs))
length <- rnorm(n, length, sd)

# plot
plot(length ~ age)

# Fit stan model ---------------------------------------------------------------

# data
mod_dat <- list(age = age,
                length = length,
                N = n)

# run model
stan_fit <- rstan::stan(file = here::here("models", "length_mod.stan"),
                        model_name = "length_mod",
                        data = mod_dat,
                        chains = 3,
                        iter = 5000,
                        init = 0,
                        cores = 4,
                        seed = 1408,
                        control = list(adapt_delta = 0.95,
                                       stepsize = 0.01))

# save
save(stan_fit,
     file = here::here("outputs", "fits", "simtest.Rdata"))

# get summary
summary(stan_fit, pars = names(coefs))$summary

# bayesplots
bayesplot::mcmc_trace(stan_fit,
                      pars = names(coefs))
bayesplot::mcmc_dens_overlay(stan_fit,
                             pars = names(coefs))
bayesplot::mcmc_acf_bar(stan_fit,
                        pars = names(coefs))

# check LOO and WAIC work
loo::loo(stan_fit)
log_lik <- loo::extract_log_lik(stan_fit)
loo::waic(log_lik)

# Fit JAGS model ---------------------------------------------------------------

library(dclone)

jags_fit <- jags.fit(data = mod_dat,
                     params = names(coefs),
                     model = here::here("models", "length-mod.jags"),
                     n.chains = 3,
                     n.adapt = 1000,
                     n.update = 2000,
                     thin = 100,
                     n.iter = 5000)

# get summary
summary(jags_fit)

# bayesplots
bayesplot::mcmc_trace(jags_fit,
                      pars = names(coefs))

# check LOO and WAIC work
loo::loo(jags_fit)
log_lik <- loo::extract_log_lik(stan_fit)
loo::waic(log_lik)
