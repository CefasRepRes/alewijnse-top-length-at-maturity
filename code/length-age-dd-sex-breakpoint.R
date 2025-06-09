### Length ~ sex + dd + age + sex-specific breakpoint

# libraries
library(R2jags)
library(data.table)
library(ggplot2)
library(beepr)
library(magrittr)

# load data
dd_dat <- data.table::fread(here::here("data", 'age_dat_w_dd_base0.csv'))
dd_dat <- dd_dat[Sex %in% c("Male", "Female")]

# subset to > 500 dd
dd_dat <- dd_dat[dd <= 500]

# fit with JAGS ----------------------------------------------------------------

# recode sex
dd_dat[, Sex := ifelse(Sex == "Female", 1, 2)]

# list params
params <- c("intercept", "beta_sex", "beta_dd1",
            "beta_dd2", "beta_age", "delta", "gamma", "tau")
pars <- c("intercept", "beta_sex[1]", "beta_sex[2]", "beta_dd1",
          "beta_dd2", "beta_age", "delta[1]", "delta[2]", "gamma", "tau")

mod_dat <- with(dd_dat, list(age = Age,
                             length = Length,
                             sex = Sex,
                             dd = dd,
                             min_dd = min(dd),
                             max_dd = max(dd),
                             n = nrow(dd_dat)))
str(mod_dat)

# run model
# jags_fit <- R2jags::jags.parallel(model.file = here::here("models",
#                                                           "length-age-dd-sex-breakpoint.jags"),
#                                   parameters.to.save = c(params, "loglik"),
#                                   data = mod_dat,
#                                   n.chains = 3,
#                                   n.iter = 10000,
#                                   n.burnin = 5000,
#                                   jags.seed = 1408,
#                                   n.thin = 10);beepr::beep()
#
# saveRDS(jags_fit, here::here("outputs", "fits", "length-age-dd-sex-breakpoint-jags.Rds"))
jags_fit <- readRDS(here::here("outputs", "fits", "length-age-dd-sex-breakpoint-jags.Rds"))

# check output
print(jags_fit)

# extract samples
jags_fit_samples <- coda::as.mcmc(jags_fit)

# traceplot
traceplot <- bayesplot::mcmc_trace(jags_fit_samples, pars = pars)
traceplot
png(here::here("outputs", "plots", "length-age-dd-sex-breakpoint",
               "traceplot.png"),
    width = 8, height = 8, units = "in", res = 250)
traceplot
dev.off()

# density
density <- bayesplot::mcmc_dens_overlay(jags_fit_samples, pars = pars)
density
png(here::here("outputs", "plots", "length-age-dd-sex-breakpoint",
               "density.png"),
    width = 8, height = 6, units = "in", res = 250)
density
dev.off()

# acf
acf <- bayesplot::mcmc_acf_bar(jags_fit_samples, pars = pars)
acf
png(here::here("outputs", "plots", "length-age-dd-sex-breakpoint",
               "acf.png"),
    width = 8, height = 6, units = "in", res = 250)
acf
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
  coefs["intercept"] + coefs[paste0("beta_sex[", dat$Sex, "]")] + coefs["beta_dd1"] * (dat$dd - coefs[paste0("delta[", dat$Sex, "]")]) + coefs["beta_dd2"] * sqrt((dat$dd - coefs[paste0("delta[", dat$Sex, "]")])^2 + coefs["gamma"]) + dat$Age * coefs["beta_age"]
}

# data for prediction
n <- 100
pred_dat <- data.frame(dd = seq(from = min(dd_dat$dd), to = max(dd_dat$dd), l = n),
                       Sex = rep(c(1, 2), n / 2),
                       Age = seq(from = min(dd_dat$Age), to = max(dd_dat$Age), l = n)) %>%
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
                     delta = c(coefs_mean["delta[1]"],
                               coefs_mean["delta[2]"]),
                     delta_up = c(coefs_up["delta[1]"],
                                  coefs_up["delta[2]"]),
                     delta_low = c(coefs_low["delta[1]"],
                                   coefs_low["delta[2]"]))

pred_plot <- ggplot() +
  geom_point(data = dd_dat, aes(x = dd, y = Length, col = as.factor(Sex)), alpha = 0.2) +
  geom_line(data = pred_dat, aes(x = dd, y = mean_pred, group = Sex)) +
  geom_ribbon(data = pred_dat, aes(x = dd, ymin = low_pred, ymax = up_pred),
              alpha = 0.2) +
  geom_vline(data = deltas, aes(xintercept = delta),
             linetype = "dashed") +
  geom_vline(data = deltas, aes(xintercept = delta_up),
             linetype = "dotted") +
  geom_vline(data = deltas, aes(xintercept = delta_low),
             linetype = "dotted") +
  facet_wrap(.~ Sex) +
  scale_colour_manual(values = c("#BB5566", "#4477AA")) +
  facet_wrap(.~ Sex, labeller = as_labeller(Sex),
             ncol = 1) +
  theme_bw() +
  theme(legend.position = "none")
pred_plot

png(here::here("outputs", "plots", "length-age-dd-sex-breakpoint",
               "pred.png"),
    width = 6, height = 6, units = "in", res = 250)
pred_plot
dev.off()
