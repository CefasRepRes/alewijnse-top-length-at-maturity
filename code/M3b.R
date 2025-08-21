### M3b

# libraries
library(R2jags)
library(data.table)
library(ggplot2)
library(beepr)
library(magrittr)

# load data
dd_dat <- data.table::fread(here::here("data", 'age_dat_w_dd_base_0_subset.csv'))

# fit with JAGS ----------------------------------------------------------------

# recode sex
dd_dat[, Sex := ifelse(Sex == "Female", 1, 2)]

# list params
params <- c("intercept", "beta_sex", "beta_age1",
            "beta_age2", "beta_dd", "delta_sex", "delta_dd", "gamma", "tau")
pars <- c("intercept", "beta_sex[1]", "beta_sex[2]", "beta_age1",
          "beta_age2", "beta_dd", "delta_sex[1]", "delta_sex[2]", "delta_dd",
          "gamma", "tau")

mod_dat <- with(dd_dat, list(age = age_months,
                             length = Length,
                             sex = Sex,
                             dd = dd_scaled,
                             min_age = min(age_months),
                             max_age = max(age_months),
                             n = nrow(dd_dat)))
str(mod_dat)

# run model
# jags_fit <- R2jags::jags.parallel(model.file = here::here("models",
#                                                           "M3b.jags"),
#                                   parameters.to.save = c(params, "loglik"),
#                                   data = mod_dat,
#                                   n.chains = 3,
#                                   n.iter = 100000,
#                                   n.burnin = 50000,
#                                   jags.seed = 1408,
#                                   n.thin = 100);beepr::beep()
#
# saveRDS(jags_fit, here::here("outputs", "fits", "M3b-jags.Rds"))
jags_fit <- readRDS(here::here("outputs", "fits", "M3b-jags.Rds"))

# check output
print(jags_fit)

# extract samples
jags_fit_samples <- coda::as.mcmc(jags_fit)

# traceplot
traceplot <- bayesplot::mcmc_trace(jags_fit_samples, pars = pars)
traceplot
png(here::here("outputs", "plots", "M3b",
               "traceplot.png"),
    width = 8, height = 8, units = "in", res = 250)
traceplot
dev.off()

# density
density <- bayesplot::mcmc_dens_overlay(jags_fit_samples, pars = pars)
density
png(here::here("outputs", "plots", "M3b",
               "density.png"),
    width = 8, height = 6, units = "in", res = 250)
density
dev.off()

# acf
acf <- bayesplot::mcmc_acf_bar(jags_fit_samples, pars = pars)
acf
png(here::here("outputs", "plots", "M3b",
               "acf.png"),
    width = 8, height = 6, units = "in", res = 250)
acf
dev.off()

# pairs
pairs <- bayesplot::mcmc_pairs(jags_fit_samples, pars = pars)
pairs
png(here::here("outputs", "plots", "M3b",
               "pairs.png"),
    width = 10, height = 8, units = "in", res = 250)
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

jags_fit_summary <- summary(jags_fit_samples)
coefs <- jags_fit_summary$statistics %>% as.data.frame()

fit_func <- function(dat, coefs){
  coefs["intercept"] + coefs[paste0("beta_sex[", dat$Sex, "]")] +
    coefs["beta_dd"] * dat$dd +
    coefs["beta_age1"] * (dat$Age - (coefs[paste0("delta_sex[", dat$Sex, "]")] + coefs["delta_dd"] * dat$dd)) +
    coefs["beta_age2"] * sqrt((dat$Age - (coefs[paste0("delta_sex[", dat$Sex, "]")] + coefs["delta_dd"] * dat$dd))^2 + coefs["gamma"])
}

# data for prediction
n <- 100
pred_dat <- data.frame(dd = seq(from = min(dd_dat$dd_scaled), to = max(dd_dat$dd_scaled), l = n),
                       Sex = rep(c(1, 2), n / 2),
                       Age = seq(from = min(dd_dat$age_months), to = max(dd_dat$age_months), l = n)) %>%
  data.table()

coefs_mean <- coefs$Mean
names(coefs_mean) <- rownames(coefs)
mean_pred <- fit_func(dat = pred_dat, coefs = coefs_mean)

pred_dat <- cbind(pred_dat, mean_pred)

coefs <- jags_fit_summary$quantiles %>% as.data.frame()
coefs_low <- coefs$`2.5%`
names(coefs_low) <- rownames(coefs)
low_pred <- fit_func(dat = pred_dat, coefs = coefs_low)

pred_dat <- cbind(pred_dat, low_pred)

coefs_up <- coefs$`97.5%`
names(coefs_up) <- rownames(coefs)
up_pred <- fit_func(dat = pred_dat, coefs = coefs_up)

pred_dat <- cbind(pred_dat, up_pred)

Sex <- c("1" = "Female",
         "2" = "Male")

deltas <- data.frame(Sex = c("1", "2"),
                     delta = c(coefs_mean["delta_sex[1]"],
                               coefs_mean["delta_sex[2]"]),
                     delta_up = c(coefs_up["delta_sex[1]"],
                                  coefs_up["delta_sex[2]"]),
                     delta_low = c(coefs_low["delta_sex[1]"],
                                   coefs_low["delta_sex[2]"]))

pred_plot <- ggplot() +
  geom_point(data = dd_dat, aes(x = age_months, y = Length, col = dd_scaled),
             pch = 1) +
  geom_line(data = pred_dat, aes(x = Age, y = mean_pred, group = Sex)) +
  geom_ribbon(data = pred_dat, aes(x = Age, ymin = low_pred, ymax = up_pred),
              alpha = 0.2) +
  geom_vline(data = deltas, aes(xintercept = delta),
             linetype = "dashed") +
  geom_vline(data = deltas, aes(xintercept = delta_up),
             linetype = "dotted") +
  geom_vline(data = deltas, aes(xintercept = delta_low),
             linetype = "dotted") +
  xlab("Age (months)") +
  ylab("Length (cm)") +
  scale_color_viridis_c(option = "inferno",
                        name = "Scaled \ndegree days") +
  facet_wrap(.~ Sex, labeller = as_labeller(Sex),
             ncol = 1) +
  theme_bw()
pred_plot

png(here::here("outputs", "plots", "M3b",
               "pred.png"),
    width = 6, height = 6, units = "in", res = 250)
pred_plot
dev.off()
