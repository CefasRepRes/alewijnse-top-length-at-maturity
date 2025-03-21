### Fitting models with Stan ####

# libraries
library(here)
library(data.table)
library(magrittr)
library(ggplot2)
library(rstan)
library(bayesplot)
library(truncnorm)
library(patchwork)
library(loo)

# set seed for reproducibility
set.seed(1408)

# Set simulation parameters ----------------------------------------------------

## inspect data ================================================================

# load data
load(here::here("data", "top-maturity-modeldata.RData"))

## choose outlier removal to use
dat <- dat_lst$iqr_3yr_data

# look at data numbers
length(unique(dat$year)) # no years
dat[, .N, by = sex] # prop sexes

dat[, sex := ifelse(sex == "Female", 0, 1)]

# Models -----------------------------------------------------------------------

## m5 ==========================================================================

# prepare data
m5_dat <- list(growth = dat$growth_raised,
               length = dat$length_std,
               tag_year = dat$tag_year,
               N = length(dat$growth_raised),
               `T` = length(unique(dat$tag_year)))
str(m5_dat)

# fit model
fit_m5 <- rstan::stan(file = here::here("stan_models", "m5.stan"),
                      model_name = "m5",
                      data = m5_dat,
                      chains = 3,
                      iter = 4000,
                      init = 0,
                      cores = 4,
                      seed = 1408,
                      control = list(adapt_delta = 0.95, stepsize = 0.01))
# save
save(fit_m5, file = here::here("results", "fits", "data", "m5_rstan.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m5)
m5_summary <- rstan::summary(fit_m5, pars = c("alpha", "alpha_iid", "b1a", "sigma"))
m5_summary <- m5_summary$summary %>% as.data.frame()
print(m5_summary)

# loo
m5_loo <- loo::loo(fit_m5)

# waic
log_lik_m5 <- loo::extract_log_lik(fit_m5)
loo::waic(log_lik_m5)

# plots
m5_post <- as.array(fit_m5)
m5_trace <- mcmc_trace(m5_post, pars = c("alpha", "alpha_iid[1]", "b1a", "sigma"))
m5_hist <- mcmc_hist(m5_post, pars = c("alpha", "alpha_iid[1]", "b1a", "sigma"))
print(m5_trace / m5_hist)
# save
png(here::here("plots", "data_fits", "m5_diagnostics.png"),
    width = 6, height = 8, units = "in", res = 200)
print(m5_trace / m5_hist)
dev.off()

# plot posteriors
alpha_iids <- sprintf("alpha_iid[%s]", seq(1:length(unique(dat$tag_year))))
m5_post <- mcmc_intervals(m5_post, pars = c("alpha", alpha_iids, "b1a", "sigma"))
print(m5_post)
png(here::here("plots", "data_fits", "m5_precis.png"),
    width = 6, height = 4, units = "in", res = 200)
print(m5_post)
dev.off()

# get predictions
m5_pred <- rstan::summary(fit_m5, pars = "mu")
m5_pred <- m5_pred$summary %>% as.data.frame()
dat$m5_pred <- m5_pred$mean
dat$m5_ci_low <- m5_pred$`2.5%`
dat$m5_ci_up <- m5_pred$`97.5%`

# plot predictions
# TODO: make pretty
ggplot(data = dat, aes(x = length_std, y = growth_raised, col = as.factor(tag_year))) +
  geom_point(alpha = 0.25) +
  geom_ribbon(aes(ymin = m5_ci_low, ymax= m5_ci_up), fill = "grey", col = NA, alpha = 0.75) +
  geom_line(aes(y = m5_pred), col = "black") +
  facet_wrap(.~as.factor(tag_year)) +
  theme_light()

## m4 ==========================================================================

# prepare data
m4_dat <- list(growth = dat$growth_raised,
               length = dat$length_std,
               tag_year = dat$tag_year,
               N = length(dat$growth_raised),
               `T` = length(unique(dat$tag_year)))
str(m4_dat)

# fit stan model
fit_m4 <- rstan::stan(file = here::here("stan_models", "m4.stan"),
                      model_name = "m4",
                      data = m4_dat,
                      chains = 3,
                      iter = 4000,
                      init = 0,
                      cores = 4,
                      seed = 1408,
                      control = list(adapt_delta = 0.95, stepsize = 0.01))
# save
save(fit_m4, file = here::here("results", "fits", "data", "m4_rstan.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m4)
m4_summary <- rstan::summary(fit_m4, pars = c("alpha", "alpha_iid", "omega", "b1a", "b1b", "sigma"))
m4_summary <- m4_summary$summary %>% as.data.frame()
print(m4_summary)

# loo
m4_loo <- loo::loo(fit_m4)

# waic
log_lik_m4 <- loo::extract_log_lik(fit_m4)
loo::waic(log_lik_m4)

# plots
m4_post <- as.array(fit_m4)
m4_trace <- mcmc_trace(m4_post, pars = c("alpha", "alpha_iid[1]", "omega", "b1a", "b1b", "sigma"))
m4_hist <- mcmc_hist(m4_post, pars = c("alpha", "alpha_iid[1]", "omega", "b1a", "b1b", "sigma"))
print(m4_trace / m4_hist)
# save
png(here::here("plots", "data_fits", "m4_diagnostics.png"),
    width = 6, height = 8, units = "in", res = 200)
print(m4_trace / m4_hist)
dev.off()

# plot posteriors
m4_post <- mcmc_intervals(m4_post, pars = c("alpha", alpha_iids, "omega", "b1a", "b1b", "sigma"))
print(m4_post)
png(here::here("plots", "data_fits", "m4_precis.png"),
    width = 6, height = 4, units = "in", res = 200)
print(m4_post)
dev.off()

# get predictions
m4_pred <- rstan::summary(fit_m4, pars = "mu")
m4_pred <- m4_pred$summary %>% as.data.frame()
dat$m4_pred <- m4_pred$mean
dat$m4_ci_low <- m4_pred$`2.5%`
dat$m4_ci_up <- m4_pred$`97.5%`
m4_omega <- m4_summary["omega", "mean"]

# plot predictions
# TODO: make pretty
ggplot(data = dat, aes(x = length_std, y = growth_raised, col = as.factor(tag_year))) +
  geom_point(alpha = 0.25) +
  geom_ribbon(aes(ymin = m4_ci_low, ymax= m4_ci_up), fill = "grey", col = NA, alpha = 0.75) +
  geom_line(aes(y = m4_pred), col = "black") +
  # geom_vline(aes(xintercept = m4_omega)) +
  facet_wrap(.~as.factor(tag_year)) +
  theme_light()

## m3c =========================================================================

# prepare data
m3c_dat <- list(growth = dat$growth_raised,
               length = dat$length_std,
               tag_year = dat$tag_year,
               sex = dat$sex,
               N = length(dat$growth_raised),
               `T` = length(unique(dat$tag_year)))
str(m3c_dat)

# fit stan model
fit_m3c <- rstan::stan(file = here::here("stan_models", "m3c.stan"),
                      model_name = "m3c",
                      data = m3c_dat,
                      chains = 3,
                      iter = 4000,
                      init = 0,
                      cores = 4,
                      seed = 1408,
                      control = list(adapt_delta = 0.95, stepsize = 0.01))
# save
save(fit_m3c, file = here::here("results", "fits", "data", "m3c_rstan.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m3c)
m3c_summary <- rstan::summary(fit_m3c, pars = c("alpha", "alpha_iid", "omega_0", "omega_sex", "b1a", "b1b", "sigma"))
m3c_summary <- m3c_summary$summary %>% as.data.frame()
print(m3c_summary)

# loo
m3c_loo <- loo::loo(fit_m3c)

# waic
log_lik_m3c <- loo::extract_log_lik(fit_m3c)
loo::waic(log_lik_m3c)

# plots
m3c_post <- as.array(fit_m3c)
m3c_trace <- mcmc_trace(m3c_post, pars = c("alpha", "alpha_iid[1]", "omega_0", "omega_sex", "b1a", "b1b", "sigma"))
m3c_hist <- mcmc_hist(m3c_post, pars = c("alpha", "alpha_iid[1]", "omega_0", "omega_sex", "b1a", "b1b", "sigma"))
print(m3c_trace / m3c_hist)
# save
png(here::here("plots", "data_fits", "m3c_diagnostics.png"),
    width = 6, height = 8, units = "in", res = 200)
print(m3c_trace / m3c_hist)
dev.off()

# plot posteriors
m3c_post <- mcmc_intervals(m3c_post, pars = c("alpha", alpha_iids, "omega_0", "omega_sex", "b1a", "b1b", "sigma"))
print(m3c_post)
png(here::here("plots", "data_fits", "m3c_precis.png"),
    width = 6, height = 4, units = "in", res = 200)
print(m3c_post)
dev.off()

# get predictions
m3c_pred <- rstan::summary(fit_m3c, pars = "mu")
m3c_pred <- m3c_pred$summary %>% as.data.frame()
dat$m3c_pred <- m3c_pred$mean
dat$m3c_ci_low <- m3c_pred$`2.5%`
dat$m3c_ci_up <- m3c_pred$`97.5%`
m3c_omega_female <- m3c_summary["omega_0", "mean"]
m3c_omega_male <- m3c_summary["omega_0", "mean"] + m3c_summary["omega_sex", "mean"]

# plot predictions
# TODO: make pretty
ggplot(data = dat, aes(x = length_std, y = growth_raised, col = as.factor(sex))) +
  geom_point(alpha = 0.25) +
  geom_ribbon(aes(ymin = m3c_ci_low, ymax= m3c_ci_up), fill = "grey", col = "black", alpha = 0.75) +
  geom_line(aes(y = m3c_pred), col = "black") +
  # geom_vline(aes(xintercept = m3c_omega)) +
  facet_wrap(.~as.factor(tag_year) + sex, ncol = 4) +
  theme_light()

## m3b =========================================================================

# prepare data
m3b_dat <- list(growth = dat$growth_raised,
                length = dat$length_std,
                tag_year = dat$tag_year,
                recap_year = dat$recap_year,
                N = length(dat$growth_raised),
                `T` = length(unique(dat$tag_year)),
                R = length(unique(dat$recap_year)))
str(m3b_dat)

# fit stan model
fit_m3b <- rstan::stan(file = here::here("stan_models", "m3b.stan"),
                       model_name = "m3b",
                       data = m3b_dat,
                       chains = 3,
                       iter = 4000,
                       init = 0,
                       cores = 4,
                       seed = 1408,
                       control = list(adapt_delta = 0.95, stepsize = 0.01))
# save
save(fit_m3b, file = here::here("results", "fits", "data", "m3b_rstan.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m3b)
m3b_summary <- rstan::summary(fit_m3b, pars = c("alpha", "alpha_iid", "omega_0", "omega_recap", "b1a", "b1b", "sigma"))
m3b_summary <- m3b_summary$summary %>% as.data.frame()
print(m3b_summary)

# loo
m3b_loo <- loo::loo(fit_m3b)

# waic
log_lik_m3b <- loo::extract_log_lik(fit_m3b)
loo::waic(log_lik_m3b)

# plots
m3b_post <- as.array(fit_m3b)
m3b_trace <- mcmc_trace(m3b_post, pars = c("alpha", "alpha_iid[1]", "omega_0", "omega_recap[1]", "b1a", "b1b", "sigma"))
m3b_hist <- mcmc_hist(m3b_post, pars = c("alpha", "alpha_iid[1]", "omega_0", "omega_recap[1]", "b1a", "b1b", "sigma"))
print(m3b_trace / m3b_hist)
# save
png(here::here("plots", "data_fits", "m3b_diagnostics.png"),
    width = 6, height = 8, units = "in", res = 200)
print(m3b_trace / m3b_hist)
dev.off()

# plot posteriors
omega_recaps <- sprintf("omega_recap[%s]", seq(1:length(unique(dat$recap_year))))
m3b_post <- mcmc_intervals(m3b_post, pars = c("alpha", alpha_iids, "omega_0", omega_recaps, "b1a", "b1b", "sigma"))
print(m3b_post)
png(here::here("plots", "data_fits", "m3b_precis.png"),
    width = 6, height = 6, units = "in", res = 200)
print(m3b_post)
dev.off()


# get predictions
m3b_pred <- rstan::summary(fit_m3b, pars = "mu")
m3b_pred <- m3b_pred$summary %>% as.data.frame()
dat$m3b_pred <- m3b_pred$mean
dat$m3b_ci_low <- m3b_pred$`2.5%`
dat$m3b_ci_up <- m3b_pred$`97.5%`

# plot predictions
# TODO: make pretty
ggplot(data = dat, aes(x = length_std, y = growth_raised, col = as.factor(sex))) +
  geom_point(alpha = 0.25) +
  geom_ribbon(aes(ymin = m3b_ci_low, ymax= m3b_ci_up), fill = "grey", col = "black", alpha = 0.75) +
  geom_line(aes(y = m3b_pred), col = "black") +
  # geom_vline(aes(xintercept = m3b_omega)) +
  facet_wrap(.~as.factor(tag_year) + as.factor(recap_year), ncol = 6) +
  theme_light() +
  theme(strip.background = element_blank(), strip.text = element_blank())

## m3a =========================================================================

# prepare data
m3a_dat <- list(growth = dat$growth_raised,
                length = dat$length_std,
                tag_year = dat$tag_year,
                recap_year = dat$recap_year,
                sex = dat$sex,
                N = length(dat$growth_raised),
                `T` = length(unique(dat$tag_year)),
                R = length(unique(dat$recap_year)))
str(m3a_dat)

# fit stan model
fit_m3a <- rstan::stan(file = here::here("stan_models", "m3a.stan"),
                       model_name = "m3a",
                       data = m3a_dat,
                       chains = 3,
                       iter = 4000,
                       init = 0,
                       cores = 4,
                       seed = 1408,
                       control = list(adapt_delta = 0.95, stepsize = 0.01))
# save
save(fit_m3a, file = here::here("results", "fits", "data", "m3a_rstan.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m3a)
m3a_summary <- rstan::summary(fit_m3a, pars = c("alpha", "alpha_iid", "omega_0", "omega_sex", "omega_recap", "b1a", "b1b", "sigma"))
m3a_summary <- m3a_summary$summary %>% as.data.frame()
print(m3a_summary)

# loo
m3a_loo <- loo::loo(fit_m3a)

# waic
log_lik_m3a <- loo::extract_log_lik(fit_m3a)
loo::waic(log_lik_m3a)

# plots
m3a_post <- as.array(fit_m3a)
m3a_trace <- mcmc_trace(m3a_post, pars = c("alpha", "alpha_iid[1]", "omega_0", "omega_sex", "omega_recap[1]", "b1a", "b1b", "sigma"))
m3a_hist <- mcmc_hist(m3a_post, pars = c("alpha", "alpha_iid[1]", "omega_0", "omega_sex", "omega_recap[1]",  "b1a", "b1b", "sigma"))
print(m3a_trace / m3a_hist)
# save
png(here::here("plots", "data_fits", "m3a_diagnostics.png"),
    width = 6, height = 8, units = "in", res = 200)
print(m3a_trace / m3a_hist)
dev.off()

# plot posteriors
m3a_post <- mcmc_intervals(m3a_post, pars = c("alpha", alpha_iids, "omega_0", "omega_sex", omega_recaps, "b1a", "b1b", "sigma"))
print(m3a_post)
png(here::here("plots", "data_fits", "m3a_precis.png"),
    width = 6, height = 6, units = "in", res = 200)
print(m3a_post)
dev.off()

# get predictions
m3a_pred <- rstan::summary(fit_m3a, pars = "mu")
m3a_pred <- m3a_pred$summary %>% as.data.frame()
dat$m3a_pred <- m3a_pred$mean
dat$m3a_ci_low <- m3a_pred$`2.5%`
dat$m3a_ci_up <- m3a_pred$`97.5%`
m3a_omega_female <- m3a_summary["omega_0", "mean"]
m3a_omega_male <- m3a_summary["omega_0", "mean"] + m3a_summary["omega_0", "mean"]

# plot predictions
# TODO: make pretty
ggplot(data = dat, aes(x = length_std, y = growth_raised, col = as.factor(sex))) +
  geom_point(alpha = 0.25) +
  geom_ribbon(aes(ymin = m3a_ci_low, ymax= m3a_ci_up), fill = "grey", col = "black", alpha = 0.75) +
  geom_line(aes(y = m3a_pred), col = "black") +
  # geom_vline(aes(xintercept = m3a_omega)) +
  facet_wrap(.~as.factor(tag_year) + as.factor(recap_year) + sex, ncol = 6) +
  theme_light() +
  theme(strip.background = element_blank(), strip.text = element_blank())

## m3z =========================================================================

# prepare data
m3z_dat <- list(growth = dat$growth_raised,
                length = dat$length_std,
                tag_year = dat$tag_year,
                temperature = dat$temperature_std,
                sex = dat$sex,
                N = length(dat$growth_raised),
                `T` = length(unique(dat$tag_year)))
str(m3z_dat)

# fit stan model
fit_m3z <- rstan::stan(file = here::here("stan_models", "m3z.stan"),
                       model_name = "m3z",
                       data = m3z_dat,
                       chains = 3,
                       iter = 4000,
                       init = 0,
                       cores = 4,
                       seed = 1408,
                       control = list(adapt_delta = 0.95, stepsize = 0.01))
# save
save(fit_m3z, file = here::here("results", "fits", "data", "m3z_rstan.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m3z)
m3z_summary <- rstan::summary(fit_m3z, pars = c("alpha", "alpha_iid", "omega_0", "omega_sex", "omega_temp", "b1a", "b1b", "sigma"))
m3z_summary <- m3z_summary$summary %>% as.data.frame()
print(m3z_summary)

# loo
m3z_loo <- loo::loo(fit_m3z)

# waic
log_lik_m3z <- loo::extract_log_lik(fit_m3z)
loo::waic(log_lik_m3z)

# plots
m3z_post <- as.array(fit_m3z)
m3z_trace <- mcmc_trace(m3z_post, pars = c("alpha", "alpha_iid[1]", "omega_0", "omega_sex", "omega_temp", "b1a", "b1b", "sigma"))
m3z_hist <- mcmc_hist(m3z_post, pars = c("alpha", "alpha_iid[1]", "omega_0", "omega_sex", "omega_temp",  "b1a", "b1b", "sigma"))
print(m3z_trace / m3z_hist)
# save
png(here::here("plots", "data_fits", "m3z_diagnostics.png"),
    width = 6, height = 8, units = "in", res = 200)
print(m3z_trace / m3z_hist)
dev.off()

# plot posteriors
m3z_post <- mcmc_intervals(m3z_post, pars = c("alpha", alpha_iids, "omega_0", "omega_sex", "omega_temp", "b1a", "b1b", "sigma"))
print(m3z_post)
png(here::here("plots", "data_fits", "m3z_precis.png"),
    width = 6, height = 6, units = "in", res = 200)
print(m3z_post)
dev.off()

# get predictions
m3z_pred <- rstan::summary(fit_m3z, pars = "mu")
m3z_pred <- m3z_pred$summary %>% as.data.frame()
dat$m3z_pred <- m3z_pred$mean
dat$m3z_ci_low <- m3z_pred$`2.5%`
dat$m3z_ci_up <- m3z_pred$`97.5%`
m3z_omega_female <- m3z_summary["omega_0", "mean"]
m3z_omega_male <- m3z_summary["omega_0", "mean"] + m3z_summary["omega_0", "mean"]

# plot predictions
# TODO: make pretty
ggplot(data = dat, aes(x = length_std, y = growth_raised, col = as.factor(sex))) +
  geom_point(alpha = 0.25) +
  geom_ribbon(aes(ymin = m3z_ci_low, ymax= m3z_ci_up), fill = "grey", col = "black", alpha = 0.75) +
  geom_line(aes(y = m3z_pred), col = "black") +
  # geom_vline(aes(xintercept = m3z_omega)) +
  facet_wrap(.~as.factor(tag_year) + as.factor(recap_year) + sex, ncol = 6) +
  theme_light() +
  theme(strip.background = element_blank(), strip.text = element_blank())

# Model comparison -------------------------------------------------------------

loo_comp <- loo_compare(m5_loo, m4_loo,
                        m3c_loo, m3b_loo, m3a_loo, m3z_loo)
loo_comp <- data.frame(model = c("m3a", "m3b", "m3z", "m3c", "m4", "m5"),
                       elpd_diff = loo_comp[, 2],
                       se_diff = loo_comp[, 3])
write.csv(loo_comp, here::here("results", "fits", "data", "loo_m5_to_m3z_comp.csv"),
          row.names = F)
