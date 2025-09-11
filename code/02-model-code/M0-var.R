### M0

# libraries
library(R2jags)
library(data.table)
library(ggplot2)
library(magrittr)

# load data
dd_dat <- data.table::fread(here::here("data", 'age_dat_w_dd_base_0_subset.csv'))

# fit with JAGS ----------------------------------------------------------------

# recode sex
dd_dat[, Sex := ifelse(Sex == "Female", 1, 2)]

# list params
params <- c("intercept", "beta_sex",
            "beta_age", "sigma")
pars <- c("intercept", "beta_sex[1]", "beta_sex[2]",
          "beta_age", "sigma")

mod_dat <- with(dd_dat, list(age = age_months,
                             length = Length,
                             sex = Sex,
                             n = nrow(dd_dat)))
str(mod_dat)

# run model
# jags_fit <- R2jags::jags.parallel(model.file = here::here("models",
#                                                           "M0-var-2.jags"),
#                                   parameters.to.save = c(params, "loglik"),
#                                   data = mod_dat,
#                                   n.chains = 3,
#                                   n.iter = 100000,
#                                   n.burnin = 50000,
#                                   jags.seed = 1408,
#                                   n.thin = 100);beepr::beep()
#
# saveRDS(jags_fit, here::here("outputs", "fits", "M0-jags-var.Rds"))
jags_fit <- readRDS(here::here("outputs", "fits", "M0-jags-var.Rds"))

# check output
print(jags_fit)

# extract samples
jags_fit_samples <- coda::as.mcmc(jags_fit)

# traceplot
traceplot <- bayesplot::mcmc_trace(jags_fit_samples, pars = pars)
traceplot
png(here::here("outputs", "plots", "M0-var",
               "traceplot.png"),
    width = 8, height = 8, units = "in", res = 250)
traceplot
dev.off()

# density
density <- bayesplot::mcmc_dens_overlay(jags_fit_samples, pars = pars)
density
png(here::here("outputs", "plots", "M0-var",
               "density.png"),
    width = 8, height = 6, units = "in", res = 250)
density
dev.off()

# acf
acf <- bayesplot::mcmc_acf_bar(jags_fit_samples, pars = pars)
acf
png(here::here("outputs", "plots", "M0-var",
               "acf.png"),
    width = 8, height = 6, units = "in", res = 250)
acf
dev.off()

# pairs
pairs <- bayesplot::mcmc_pairs(jags_fit_samples, pars = pars)
pairs
png(here::here("outputs", "plots", "M0-var",
               "pairs.png"),
    width = 8, height = 6, units = "in", res = 250)
pairs
dev.off()

# get loo and waic
# extract loglik samples
loglik_array <- jags_fit$BUGSoutput$sims.list$loglik

# convert to matrix: rows = posterior draws, columns = data points
loglik_matrix <- as.matrix(loglik_array)

# loo and WAIC
loo::waic(loglik_matrix)
loo::loo(loglik_matrix)

# plot model -------------------------------------------------------------------

# extract summary, samples and coefficients
jags_fit_summary <- summary(jags_fit_samples)
coefs <- jags_fit_summary$statistics %>% as.data.frame()
jags_fit_samples <- as.matrix(jags_fit_samples)

fit_func <- function(dat, coefs){
  coefs["intercept"] + coefs[paste0("beta_sex[", dat$Sex, "]")] + dat$Age * coefs["beta_age"]
}

# data for prediction
n <- 1000
pred_dat <- data.frame(Sex = rep(c(1, 2), n / 2),
                       Age = seq(from = min(dd_dat$age_months), to = max(dd_dat$age_months), l = n)) %>%
  data.table()

# predict using entire posterior
n_sims <- nrow(jags_fit_samples)
pred_matrix <- matrix(NA, nrow = nrow(pred_dat), ncol = n_sims)

for (i in 1:n_sims) {
  coefs_i <- as.numeric(jags_fit_samples[i, ])
  names(coefs_i) <- colnames(jags_fit_samples)
  st_dev <- coefs_i["sigma"] * sqrt(pred_dat$Age)
  lp <- fit_func(dat = pred_dat, coefs = coefs_i)
  pred_matrix[, i] <- rnorm(n = length(lp), mean = lp, sd = st_dev)
}

# extract
pred_dat$pred_mean <- rowMeans(pred_matrix)
pred_dat$pred_lower <- apply(pred_matrix, 1, quantile, probs = 0.025)
pred_dat$pred_upper <- apply(pred_matrix, 1, quantile, probs = 0.975)

# get coefs
coefs_mean <- coefs$Mean
names(coefs_mean) <- rownames(coefs)
coefs <- jags_fit_summary$quantiles %>% as.data.frame()
coefs_low <- coefs$`2.5%`
names(coefs_low) <- rownames(coefs)
coefs_up <- coefs$`97.5%`
names(coefs_up) <- rownames(coefs)

# set sex
Sex <- c("1" = "Female",
         "2" = "Male")

pred_plot <- ggplot() +
  geom_point(data = dd_dat, aes(x = age_months, y = Length, col = as.factor(Sex)), pch = 1) +
  geom_line(data = pred_dat, aes(x = Age, y = pred_mean, group = Sex)) +
  geom_ribbon(data = pred_dat, aes(x = Age, ymin = pred_lower, ymax = pred_upper),
              alpha = 0.2) +
  scale_colour_manual(values = c("#BB5566", "#4477AA")) +
  xlab("Age (months)") +
  ylab("Length (cm)") +
  facet_wrap(.~ Sex, labeller = as_labeller(Sex),
             ncol = 1) +
  theme_bw() +
  theme(legend.position = "none")
pred_plot

png(here::here("outputs", "plots", "M0-var",
               "pred.png"),
    width = 6, height = 6, units = "in", res = 250)
pred_plot
dev.off()

# residuals vs fitted ----------------------------------------------------------

# modify fit function
fit_func <- function(dat, coefs){
  coefs["intercept"] + coefs[paste0("beta_sex[", dat$Sex, "]")] + dat$age_months * coefs["beta_age"]
}

# get fitted values
dd_dat[, fit_vals := fit_func(dat = dd_dat, coefs = coefs_mean)]

# get residuals
dd_dat[, resid_vals := Length - fit_vals]

# standardise residuals
dd_dat[, stan_res := resid_vals / (coefs_i["sigma"] * sqrt(age_months))]

ggplot(data = dd_dat, aes(x = fit_vals, y = resid_vals)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess") +
  theme_bw()

ggplot(data = dd_dat, aes(x = fit_vals, y = stan_res)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess") +
  theme_bw()
