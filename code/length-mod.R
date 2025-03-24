#### Implementation of Scott equation 2 ####

# libraries
library(dclone)

# load data
source("C:/Users/sa20/OneDrive - CEFAS/Projects/southern_ocean/r-projects/master-data-wrangling/code/data-prep-483-TOP-age.R")

# constants
n <- 1000
x <- 0:30
xs <- sample(x = x, size = n, replace = TRUE)

# fit function
f <- function(x, t) {
  sapply(1:ncol(t), function(v) t["theta0", v] +
           t["theta1", v] * (x - t["delta", v]) +
           t["theta2", v] * sqrt((x - t["delta", v])^2 + t["gamma", v]))
}

# coefs
coefs <- c("theta0" = 50, "theta1" = 2.5, "theta2" = -1.7,
           "delta" = 10, "gamma" = 0.1)

# model error
sd <- 3

# data
lp <- f(xs, as.matrix(coefs))
y <- rnorm(n, lp, sd)

# plot
plot(TOP_all_age_dat$Length_mm ~ TOP_all_age_dat$Age)

# fit
fit <- jags.fit(data = list(y = TOP_all_age_dat$Length_mm,
                            x = TOP_all_age_dat$Age,
                            n = nrow(TOP_all_age_dat)),
                params = names(coefs),
                model = here::here("stan_models", "length-mod.jags"),
                n.chains = 3, n.adapt = 1000, n.update = 2000, thin = 100, n.iter = 5000)

# print
print(summary(fit))
plot(fit)

# ages for fit
age <- seq(from = min(TOP_all_age_dat$Age), to = max(TOP_all_age_dat$Age), l = n)

# add fit
th <- summary(fit)$statistics[, 1, drop = FALSE]
length <- f(age, th)
points(length ~ age,
       type = "l", col = "blue", lwd = 3)

# add confidence bands
quants <- summary(fit)$quantiles[, c(1, 5), drop = FALSE]
ci <- f(age, quants)
sapply(1:2, function(v) points(ci[, v] ~ age,
                               type = "l", col = "red", lwd = 3, lty = "dashed"))

# stan -------------------------------------------------------------------------

# data
mod_dat <- list(age = TOP_all_age_dat$Age,
                length = TOP_all_age_dat$Length_mm,
                N = nrow(TOP_all_age_dat))

stan_fit <- rstan::stan(file = here::here("stan_models", "length_mod.stan"),
                        model_name = "length_mod",
                        data = mod_dat,
                        chains = 3,
                        iter = 1000,
                        init = 1,
                        cores = 4,
                        seed = 1408,
                        control = list(adapt_delta = 0.95,
                                       stepsize = 0.01))
plot(stan_fit)
summary(stan_fit)
loo::loo(stan_fit)
log_lik <- loo::extract_log_lik(stan_fit)
loo::waic(log_lik)
