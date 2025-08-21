### M2

# libraries
library(dclone)
library(data.table)
library(ggplot2)
library(magrittr)

# load data
dd_dat <- data.table::fread(here::here("data", 'age_dat_w_dd_base_0_subset.csv'))

# fit with JAGS ----------------------------------------------------------------

# recode sex
dd_dat[, Sex := ifelse(Sex == "Female", 1, 2)]

# list params
params <- c("intercept", "beta_dd", "beta_sex",
            "beta_age", "tau")
pars <- c("intercept", "beta_dd", "beta_sex[1]", "beta_sex[2]",
          "beta_age", "tau")

mod_dat <- with(dd_dat, list(age = age_months,
                             length = Length,
                             dd = dd_scaled,
                             sex = Sex,
                             n = nrow(dd_dat)))
str(mod_dat)

# run model
# jags_fit <- R2jags::jags.parallel(model.file = here::here("models",
#                                                           "M2.jags"),
#                                   parameters.to.save = c(params, "loglik"),
#                                   data = mod_dat,
#                                   n.chains = 3,
#                                   n.iter = 100000,
#                                   n.burnin = 50000,
#                                   jags.seed = 1408,
#                                   n.thin = 100);beepr::beep()
#
# saveRDS(jags_fit, here::here("outputs", "fits", "M2-jags.Rds"))
jags_fit <- readRDS(here::here("outputs", "fits", "M2-jags.Rds"))

# check output
print(jags_fit)

# extract samples
jags_fit_samples <- coda::as.mcmc(jags_fit)

# traceplot
traceplot <- bayesplot::mcmc_trace(jags_fit_samples, pars = pars)
traceplot
png(here::here("outputs", "plots", "M2",
               "traceplot.png"),
    width = 8, height = 8, units = "in", res = 250)
traceplot
dev.off()

# density
density <- bayesplot::mcmc_dens_overlay(jags_fit_samples, pars = pars)
density
png(here::here("outputs", "plots", "M2",
               "density.png"),
    width = 8, height = 6, units = "in", res = 250)
density
dev.off()

# acf
acf <- bayesplot::mcmc_acf_bar(jags_fit_samples, pars = pars)
acf
png(here::here("outputs", "plots", "M2",
               "acf.png"),
    width = 8, height = 6, units = "in", res = 250)
acf
dev.off()

# pairs
pairs <- bayesplot::mcmc_pairs(jags_fit_samples, pars = pars)
pairs
png(here::here("outputs", "plots", "M2",
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

jags_fit_summary <- summary(jags_fit_samples)
coefs <- jags_fit_summary$statistics %>% as.data.frame()

fit_func <- function(dat, coefs){
  coefs["intercept"] + coefs[paste0("beta_sex[", dat$Sex, "]")] + dat$dd * coefs["beta_dd"] + dat$Age * coefs["beta_age"]
}

# data for prediction
n <- 100
pred_dat <- data.frame(dd = seq(from = min(dd_dat$dd_scaled), to = max(dd_dat$dd_scaled), l = n),
                       Sex = rep(c(1, 2), length.out = n),
                       Age = seq(from = min(dd_dat$age_months), to = max(dd_dat$age_months), l = n)) %>%
  data.table()
pred_dat <- pred_dat[order(dd), ]

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

# pred_plot <- ggplot() +
#   geom_point(data = dd_dat, aes(x = dd_scaled, y = Length, col = as.factor(Sex)), alpha = 0.2) +
#   geom_line(data = pred_dat, aes(x = dd, y = mean_pred)) +
#   geom_ribbon(data = pred_dat, aes(x = dd, ymin = low_pred, ymax = up_pred),
#               alpha = 0.2) +
#   facet_wrap(.~ Sex) +
#   scale_colour_manual(values = c("#BB5566", "#4477AA")) +
#   facet_wrap(.~ Sex, labeller = as_labeller(Sex)) +
#   xlab("Degree days") +
#   ylab("Length (cm)") +
#   theme_bw() +
#   theme(legend.position = "none")
# pred_plot
#
# png(here::here("outputs", "plots", "M2",
#                "pred_dd.png"),
#     width = 6, height = 4, units = "in", res = 250)
# pred_plot
# dev.off()

pred_plot <- ggplot() +
  geom_point(data = dd_dat, aes(x = age_months, y = Length, col = dd_scaled),
             pch = 1) +
  geom_line(data = pred_dat, aes(x = Age, y = mean_pred)) +
  geom_ribbon(data = pred_dat, aes(x = Age, ymin = low_pred, ymax = up_pred),
              alpha = 0.2) +
  facet_wrap(.~ Sex) +
  scale_color_viridis_c(option = "inferno",
                        name = "Scaled \ndegree days") +
  facet_wrap(.~ Sex, labeller = as_labeller(Sex),
             ncol = 1) +
  xlab("Age (months)") +
  ylab("Length (cm)") +
  theme_bw()
pred_plot

png(here::here("outputs", "plots", "M2",
               "pred.png"),
    width = 6, height = 6, units = "in", res = 250)
pred_plot
dev.off()
