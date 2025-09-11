### M3

# libraries
library(R2jags)
library(data.table)
library(ggplot2)
library(beepr)
library(magrittr)

# load data
dm_dat <- data.table::fread(here::here("data", 'age_dat_w_dm_base_0_subset.csv'))

# fit with JAGS ----------------------------------------------------------------

# recode sex
dm_dat[, Sex := ifelse(Sex == "Female", 1, 2)]

# list params
params <- c("intercept", "beta_sex", "beta_age1",
            "beta_age2", "beta_dm", "delta", "gamma", "sigma")
pars <- c("intercept", "beta_sex[1]", "beta_sex[2]", "beta_age1",
          "beta_age2", "beta_dm", "delta[1]", "delta[2]", "gamma", "sigma")

mod_dat <- with(dm_dat, list(age = age_months,
                             length = Length,
                             sex = Sex,
                             dm = dm_scaled,
                             min_age = min(age_months),
                             max_age = max(age_months),
                             n = nrow(dm_dat)))
str(mod_dat)

# run model
# jags_fit <- R2jags::jags.parallel(model.file = here::here("models",
#                                                           "M3.jags"),
#                                   parameters.to.save = c(params, "loglik"),
#                                   data = mod_dat,
#                                   n.chains = 3,
#                                   n.iter = 100000,
#                                   n.burnin = 50000,
#                                   jags.seed = 1408,
#                                   n.thin = 100);beepr::beep()
#
# saveRDS(jags_fit, here::here("outputs", "fits", "M3-jags.Rds"))
jags_fit <- readRDS(here::here("outputs", "fits", "M3-jags.Rds"))

# check output
print(jags_fit)

# extract samples
jags_fit_samples <- coda::as.mcmc(jags_fit)

# traceplot
traceplot <- bayesplot::mcmc_trace(jags_fit_samples, pars = pars)
traceplot
png(here::here("outputs", "plots", "M3",
               "traceplot.png"),
    width = 10, height = 8, units = "in", res = 250)
traceplot
dev.off()

# density
density <- bayesplot::mcmc_dens_overlay(jags_fit_samples, pars = pars)
density
png(here::here("outputs", "plots", "M3",
               "density.png"),
    width = 10, height = 6, units = "in", res = 250)
density
dev.off()

# acf
acf <- bayesplot::mcmc_acf_bar(jags_fit_samples, pars = pars)
acf
png(here::here("outputs", "plots", "M3",
               "acf.png"),
    width = 10, height = 6, units = "in", res = 250)
acf
dev.off()

# pairs
pairs <- bayesplot::mcmc_pairs(jags_fit_samples, pars = pars)
pairs
png(here::here("outputs", "plots", "M3",
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
  coefs["intercept"] + coefs[paste0("beta_sex[", dat$Sex, "]")] + coefs["beta_dm"] * dat$dm +
    coefs["beta_age1"] * (dat$Age - coefs[paste0("delta[", dat$Sex, "]")]) +
    coefs["beta_age2"] * sqrt((dat$Age - coefs[paste0("delta[", dat$Sex, "]")])^2 + coefs["gamma"])
}

# data for prediction
n <- 1000
pred_dat <- data.frame(dm = seq(from = min(dm_dat$dm_scaled), to = max(dm_dat$dm_scaled), l = n),
                       Sex = rep(c(1, 2), n / 2),
                       Age = seq(from = min(dm_dat$age_months), to = max(dm_dat$age_months), l = n)) %>%
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
pred_dat$test <- pred_dat$pred_upper - pred_dat$pred_lower

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

# get deltas
deltas <- data.frame(Sex = c("1", "2"),
                     delta = c(coefs_mean["delta[1]"],
                               coefs_mean["delta[2]"]),
                     delta_up = c(coefs_up["delta[1]"],
                                  coefs_up["delta[2]"]),
                     delta_low = c(coefs_low["delta[1]"],
                                   coefs_low["delta[2]"]))

# plot
pred_plot <- ggplot() +
  geom_point(data = dm_dat, aes(x = age_months, y = Length, col = dm_scaled), pch = 1) +
  geom_line(data = pred_dat, aes(x = Age, y = pred_mean, group = Sex)) +
  geom_ribbon(data = pred_dat, aes(x = Age, ymin = pred_lower, ymax = pred_upper),
              alpha = 0.2) +
  geom_vline(data = deltas, aes(xintercept = delta),
             linetype = "dashed") +
  geom_vline(data = deltas, aes(xintercept = delta_up),
             linetype = "dotted") +
  geom_vline(data = deltas, aes(xintercept = delta_low),
             linetype = "dotted") +
  scale_color_viridis_c(option = "inferno",
                        name = "Degree month \nanomalies",
                        end = 0.9) +
  xlab("Age (months)") +
  ylab("Length (cm)") +
  facet_wrap(.~ Sex) +
  facet_wrap(.~ Sex, labeller = as_labeller(Sex),
             ncol = 1) +
  theme_bw()
pred_plot

png(here::here("outputs", "plots", "M3",
               "pred.png"),
    width = 6, height = 6, units = "in", res = 250)
pred_plot
dev.off()

# predict with min, mean and max scaled dm -------------------------------------

# get values
min_s_dm <- min(mod_dat$dm)
max_s_dm <- max(mod_dat$dm)
mean_s_dm <- mean(mod_dat$dm)

# data for prediction
pred_dm <- data.frame(Sex = rep(c(1, 2), n / 2),
                      Age = seq(from = min(dm_dat$age_months), to = max(dm_dat$age_months), l = n)) %>%
  data.table()

# modify fit function
fit_func <- function(dat, coefs, dm){
  coefs["intercept"] + coefs[paste0("beta_sex[", dat$Sex, "]")] + coefs["beta_dm"] * dm +
    coefs["beta_age1"] * (dat$Age - coefs[paste0("delta[", dat$Sex, "]")]) +
    coefs["beta_age2"] * sqrt((dat$Age - coefs[paste0("delta[", dat$Sex, "]")])^2 + coefs["gamma"])
}

# min
pred_matrix <- matrix(NA, nrow = nrow(pred_dat), ncol = n_sims)
for (i in 1:n_sims) {
  coefs_i <- as.numeric(jags_fit_samples[i, ])
  names(coefs_i) <- colnames(jags_fit_samples)
  st_dev <- coefs_i["sigma"] * sqrt(pred_dm$Age)
  lp <- fit_func(dat = pred_dat, coefs = coefs_i, dm = min_s_dm)
  pred_matrix[, i] <- rnorm(n = length(lp), mean = lp, sd = st_dev)
}
pred_dm[, Min := rowMeans(pred_matrix)] # save

# mean
pred_matrix <- matrix(NA, nrow = nrow(pred_dat), ncol = n_sims)
for (i in 1:n_sims) {
  coefs_i <- as.numeric(jags_fit_samples[i, ])
  names(coefs_i) <- colnames(jags_fit_samples)
  st_dev <- coefs_i["sigma"] * sqrt(pred_dm$Age)
  lp <- fit_func(dat = pred_dat, coefs = coefs_i, dm = mean_s_dm)
  pred_matrix[, i] <- rnorm(n = length(lp), mean = lp, sd = st_dev)
}
pred_dm[, Mean := rowMeans(pred_matrix)] # save
pred_dm

# max
pred_matrix <- matrix(NA, nrow = nrow(pred_dat), ncol = n_sims)
for (i in 1:n_sims) {
  coefs_i <- as.numeric(jags_fit_samples[i, ])
  names(coefs_i) <- colnames(jags_fit_samples)
  st_dev <- coefs_i["sigma"] * sqrt(pred_dm$Age)
  lp <- fit_func(dat = pred_dat, coefs = coefs_i, dm = max_s_dm)
  pred_matrix[, i] <- rnorm(n = length(lp), mean = lp, sd = st_dev)
}
pred_dm[, Max := rowMeans(pred_matrix)] # save
pred_dm

# melt
pred_dm <- melt(pred_dm, id.vars = c("Sex", "Age"),
                measure.vars = c("Min", "Mean", "Max"),
                variable = "scaled_dm", value = "length_cm")
pred_dm

# adm values
pred_dm[scaled_dm == "Min", dm_val := min_s_dm]
pred_dm[scaled_dm == "Mean", dm_val := mean_s_dm]
pred_dm[scaled_dm == "Max", dm_val := max_s_dm]

# plot
pred_dm_plot <- ggplot(data = pred_dm, aes(x = Age, y = length_cm, col = dm_val, group = scaled_dm)) +
  geom_line() +
  xlab("Age (months)") +
  ylab("Length (cm)") +
  scale_color_viridis_c(option = "inferno",
                        name = "Degree month \nanomalies",
                        end = 0.9) +
  facet_wrap(.~ Sex, labeller = as_labeller(Sex),
             ncol = 1) +
  theme_bw()
pred_dm_plot

png(here::here("outputs", "plots", "M3",
               "pred-dm.png"),
    width = 6, height = 6, units = "in", res = 250)
pred_dm_plot
dev.off()

# residuals vs fitted ----------------------------------------------------------

# modify fit function
fit_func <- function(dat, coefs){
  coefs["intercept"] + coefs[paste0("beta_sex[", dat$Sex, "]")] + coefs["beta_dm"] * dat$dm_scaled +
    coefs["beta_age1"] * (dat$age_months - coefs[paste0("delta[", dat$Sex, "]")]) +
    coefs["beta_age2"] * sqrt((dat$age_months - coefs[paste0("delta[", dat$Sex, "]")])^2 + coefs["gamma"])
}

# get fitted values
dm_dat[, fit_vals := fit_func(dat = dm_dat, coefs = coefs_mean)]

# get residuals
dm_dat[, resid_vals := Length - fit_vals]

# standardise residuals
dm_dat[, stan_res := resid_vals / (coefs_i["sigma"] * sqrt(age_months))]

# plot residuals
ggplot(data = dm_dat, aes(x = fit_vals, y = resid_vals)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess") +
  theme_bw()

# plot standardised residuals
ggplot(data = dm_dat, aes(x = fit_vals, y = stan_res)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess") +
  theme_bw()
