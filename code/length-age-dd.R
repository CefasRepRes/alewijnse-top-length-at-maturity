### Age + dd + breakpoint stan model

# libraries
library(dclone)
library(data.table)
library(ggplot2)

# load data
dd_dat <- data.table::fread(here::here("data", 'age_dat_w_dd_base0.csv'))
dd_dat <- dd_dat[Sex %in% c("Male", "Female")]

# subset to > 500 dd
dd_dat <- dd_dat[dd <= 500]

# fit with JAGS ----------------------------------------------------------------

# list params
params <- c("intercept", "beta_dd", "beta_age", "tau")
pars <- params

mod_dat <- with(dd_dat, list(age = Age,
                             length = Length,
                             dd = dd,
                             n = nrow(dd_dat)))
str(mod_dat)

# # run model
# jags_fit <- R2jags::jags.parallel(model.file = here::here("models",
#                                                  "length-age-dd.jags"),
#                          parameters.to.save = c(params, "loglik"),
#                          data = mod_dat,
#                          n.chains = 3,
#                          n.iter = 10000,
#                          n.burnin = 5000,
#                          jags.seed = 1408,
#                          n.thin = 10);beepr::beep(sound = 8)

# saveRDS(jags_fit, here::here("outputs", "fits", "length-age-dd-jags.Rds"))
jags_fit <- readRDS(here::here("outputs", "fits", "length-age-dd-jags.Rds"))

# check output
print(jags_fit)

# extract samples
jags_fit_samples <- coda::as.mcmc(jags_fit)

# traceplot
traceplot <- bayesplot::mcmc_trace(jags_fit_samples, pars = pars)
traceplot
png(here::here("outputs", "plots", "length-age-dd",
               "traceplot.png"),
    width = 8, height = 8, units = "in", res = 250)
traceplot
dev.off()

# density
density <- bayesplot::mcmc_dens_overlay(jags_fit_samples, pars = pars)
density
png(here::here("outputs", "plots", "length-age-dd",
               "density.png"),
    width = 8, height = 6, units = "in", res = 250)
density
dev.off()

# acf
acf <- bayesplot::mcmc_acf_bar(jags_fit_samples, pars = pars)
acf
png(here::here("outputs", "plots", "length-age-dd",
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
  coefs["intercept"] + dat$Age * coefs["beta_age"] + dat$dd * coefs["beta_dd"]
}

# data for prediction
n <- 1000
pred_dat <- data.frame(dd = seq(from = min(dd_dat$dd), to = max(dd_dat$dd), l = 100),
                       Age = seq(from = min(dd_dat$Age), to = max(dd_dat$Age), l = 100)) %>%
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

pred_plot <- ggplot() +
  geom_point(data = dd_dat, aes(x = dd, y = Length), alpha = 0.2, col = "cornflowerblue") +
  geom_line(data = pred_dat, aes(x = dd, y = mean_pred)) +
  geom_ribbon(data = pred_dat, aes(x = dd, ymin = low_pred, ymax = up_pred),
              alpha = 0.2) +
  theme_bw()

png(here::here("outputs", "plots", "length-age-dd",
               "pred.png"),
    width = 6, height = 4, units = "in", res = 250)
pred_plot
dev.off()
