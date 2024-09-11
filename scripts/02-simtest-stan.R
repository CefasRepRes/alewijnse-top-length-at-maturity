### Testing models with Stan ####

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

## set parameters ==============================================================

# numbers
n_total <- 3408
n_years <- 16
n_per_year <- rep(n_total / n_years, n_years)
p_female <- 0.58
n_female_per_year <- rbinom(n_years, n_per_year, p_female)
n_male_per_year <- n_per_year - n_female_per_year

# fixed and linear coefficients
alpha <- 0.5 # intercept
b_length_a <- -0.2 # effect of length before breakpoint
b_length_b <- -0.1 # effect of length after breakpoint
b_male <- 0.05 # effect of sex (being male)
b_temperature <- 0.1 # effect of temperature
b_trend <- 0.01 # effect of year of recapture

# covariates
length_sex_means <- c(0.25, -0.25) # mean length f/m
temperature_means <- rnorm(n_years, mean = 1.4, sd = 0.01) # seq(1:n_years) # test
df <- data.table(recap_year = rep(1:n_years, n_per_year),
                 length = c(unlist(sapply(n_female_per_year,
                                          rnorm,
                                          length_sex_means[1],
                                          1)),
                            unlist(sapply(n_male_per_year,
                                          rnorm,
                                          length_sex_means[2],
                                          1))),
                 sex = factor(rep(c(1, 2),
                                  c(sum(n_female_per_year),
                                    sum(n_male_per_year))),
                              labels = c("female", "male")),
                 temperature = rnorm(n_total,
                                     temperature_means[rep(1:n_years,
                                                           n_per_year)],
                                     1))
df[, "tag_year" := (recap_year + 3) - sample(x = 1:3, size = n_total,
                                             prob = c(0.33, 0.33, 0.33), replace = TRUE)]
df[, "year" := as.numeric(recap_year)]

# random intercept with tag year
alpha_sd <- 1
alpha_iid <- rnorm(n = length(unique(df$tag_year)), mean = 0, sd = alpha_sd) # effect of each year

# random breakpoints
brk_0 <- -1
brk_male <- 0.2
brk_temperature <- 0.2
brk_year_sd <- 0.3
brks_year <- rnorm(n = n_years, mean = 0,sd = brk_year_sd)

# response variables
min_growth <- min(dat$growth_raised)
eps <- 0.3

# set sex as binary - female default
df[, sex := ifelse(sex == "female", 0, 1)]

# Models -----------------------------------------------------------------------

## m5 ==========================================================================

# simulate response
# no change-point in length, with tag year RE
df[, "m5" := (alpha + alpha_iid[tag_year] +
                b_length_a * length),
   by = row.names(df)]

# sample response from normal distribution
df[, "growth_m5" := rtruncnorm(.N, mean = m5, sd = eps, a = min_growth)]

# plot
p_m5_direct <- ggplot(df, aes(y = m5, x = length, color = sex)) +
  geom_point(alpha = 0.25)
p_m5_sim <- ggplot(df, aes(y = growth_m5, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m5_direct + p_m5_sim)
# save plot
png(here::here("plots", "m5_sim_dat.png"),
    width = 6, height = 4, units = "in", res = 200)
print(p_m5_direct + p_m5_sim)
dev.off()

# prepare data
m5_dat <- list(growth = df$growth_m5,
               length = df$length,
               tag_year = df$tag_year,
               N = length(df$growth_m5),
               `T` = length(unique(df$tag_year)))

# fit model
fit_m5 <- rstan::stan(file = here::here("stan_models", "m5.stan"),
                      model_name = "m5",
                      data = m5_dat,
                      chains = 3,
                      iter = 4000,
                      init = 0,
                      cores = 4,
                      seed = 1408)
# save
save(fit_m5, file = here::here("results", "fits", "test", "m5_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m5)
m5_summary <- rstan::summary(fit_m5, pars = c("alpha", "alpha_iid", "b1a", "sigma"))
m5_summary <- m5_summary$summary %>% as.data.frame()
m5_summary$name <- rownames(m5_summary)
m5_summary$type <- "estimated"
m5_summary <- data.table(m5_summary)
print(m5_summary)

# loo
loo::loo(fit_m5)

# waic
log_lik_m5 <- loo::extract_log_lik(fit_m5)
loo::waic(log_lik_m5)

# plots
m5_post <- as.array(fit_m5)
m5_trace <- mcmc_trace(m5_post, pars = c("alpha", "alpha_iid[1]", "b1a", "sigma"))
m5_hist <- mcmc_hist(m5_post, pars = c("alpha", "alpha_iid[1]", "b1a", "sigma"))
print(m5_trace / m5_hist)
# save
png(here::here("plots", "m5_diagnostics.png"),
    width = 6, height = 8, units = "in", res = 200)
print(m5_trace / m5_hist)
dev.off()

# add to data frame
m5_true <- data.frame(name = m5_summary$name,
                      mean = c(alpha, alpha_iid, b_length_a, eps),
                      sd = NA,
                      type = "true")
m5_post_comparison <- rbind(m5_true, m5_summary[, .(name, mean, sd, type)])

m5_post_comp <- ggplot(data = m5_post_comparison,
                       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
m5_post_comp
# save plot
png(here::here("plots", "m5_post_comp.png"),
    width = 8, height = 4, units = "in", res = 200)
print(m5_post_comp)
dev.off()

## m4 ==========================================================================

# simulate response
# single change-point in length, with tag year RE
df[, "m4" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0,
            b_length_a * length,
            b_length_a * brk_0 + b_length_b * (length - brk_0)),
   by = row.names(df)]

# sample response from normal distribution
df[, "growth_m4" := rtruncnorm(.N, mean = m4, sd = eps, a = min_growth)]

# plot
p_m4_direct <- ggplot(df, aes(y = m4, x = length, color = sex)) +
  geom_point(alpha = 0.25)
p_m4_sim <- ggplot(df, aes(y = growth_m4, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m4_direct + p_m4_sim)
# save plot
png(here::here("plots", "m4_sim_dat.png"),
    width = 6, height = 4, units = "in", res = 200)
print(p_m4_direct + p_m4_sim)
dev.off()

# prepare data
m4_dat <- list(growth = df$growth_m4,
               length = df$length,
               tag_year = df$tag_year,
               N = length(df$growth_m4),
               `T` = length(unique(df$tag_year)))

# fit stan model
fit_m4 <- rstan::stan(file = here::here("stan_models", "m4.stan"),
                      model_name = "m4",
                      data = m4_dat,
                      chains = 3,
                      iter = 4000,
                      init = 0,
                      cores = 4,
                      seed = 1408)
# save
save(fit_m4, file = here::here("results", "fits", "test", "m4_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m4)
m4_summary <- rstan::summary(fit_m4, pars = c("alpha", "alpha_iid", "omega", "b1a", "b1b", "sigma"))
m4_summary <- m4_summary$summary %>% as.data.frame()
m4_summary$name <- rownames(m4_summary)
m4_summary$type <- "estimated"
m4_summary <- data.table(m4_summary)
print(m4_summary)

# loo
loo::loo(fit_m4)

# waic
log_lik_m4 <- loo::extract_log_lik(fit_m4)
loo::waic(log_lik_m4)

# plots
m4_post <- as.array(fit_m4)
m4_trace <- mcmc_trace(m4_post, pars = c("alpha", "alpha_iid[1]", "omega", "b1a", "b1b", "sigma"))
m4_hist <- mcmc_hist(m4_post, pars = c("alpha", "alpha_iid[1]", "omega", "b1a", "b1b", "sigma"))
print(m4_trace / m4_hist)
# save
png(here::here("plots", "m4_diagnostics.png"),
    width = 6, height = 8, units = "in", res = 200)
print(m4_trace / m4_hist)
dev.off()

# add to data frame
m4_true <- data.frame(name = m4_summary$name,
                      mean = c(alpha, alpha_iid, brk_0, b_length_a, b_length_b, eps),
                      sd = NA,
                      type = "true")
m4_post_comparison <- rbind(m4_true, m4_summary[, .(name, mean, sd, type)])

m4_post_comp <- ggplot(data = m4_post_comparison,
                       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
m4_post_comp
# save plot
png(here::here("plots", "m4_post_comp.png"),
    width = 8, height = 4, units = "in", res = 200)
print(m4_post_comp)
dev.off()

## m3c =========================================================================

# simulate response
# sex-specific change point, with tag year RE
df[sex == 0,
   "m3c" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0,
            b_length_a * length,
            b_length_a * (brk_0) + b_length_b * (length - brk_0))]

df[sex == 1,
   "m3c" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0 + brk_male,
            b_length_a * length,
            b_length_a * (brk_0 + brk_male) + b_length_b * (length - (brk_0 + brk_male)))]

# sample response from normal distribution
df[, "growth_m3c" := rtruncnorm(.N, mean = m3c, sd = eps, a = min_growth)]

# plot
p_m3c_direct <- ggplot(df, aes(y = m3c, x = length, color = sex)) +
  geom_point(alpha = 0.25)
p_m3c_sim <- ggplot(df, aes(y = growth_m3c, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m3c_direct + p_m3c_sim)
# save plot
png(here::here("plots", "m3c_sim_dat.png"),
    width = 6, height = 4, units = "in", res = 200)
print(p_m3c_direct + p_m3c_sim)
dev.off()

# prepare data
m3c_dat <- list(growth = df$growth_m3c,
                length = df$length,
                sex = df$sex,
                tag_year = df$tag_year,
                N = length(df$growth_m3c),
                `T` = length(unique(df$tag_year)))

# fit stan model
fit_m3c <- rstan::stan(file = here::here("stan_models", "m3c.stan"),
                       model_name = "m3c",
                       data = m3c_dat,
                       chains = 3,
                       iter = 4000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m3c, file = here::here("results", "fits", "test", "m3c_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m3c)
m3c_summary <- rstan::summary(fit_m3c, pars = c("alpha", "alpha_iid", "omega", "omega_sex", "b1a", "b1b", "sigma"))
m3c_summary <- m3c_summary$summary %>% as.data.frame()
m3c_summary$name <- rownames(m3c_summary)
m3c_summary$type <- "estimated"
m3c_summary <- data.table(m3c_summary)
print(m3c_summary)

# loo
loo::loo(fit_m3c)

# waic
log_lik_m3c <- loo::extract_log_lik(fit_m3c)
loo::waic(log_lik_m3c)

# plots
m3c_post <- as.array(fit_m3c)
m3c_trace <- mcmc_trace(m3c_post, pars = c("alpha", "alpha_iid[1]", "omega", "omega_sex", "b1a", "b1b", "sigma"))
m3c_hist <- mcmc_hist(m3c_post, pars = c("alpha", "alpha_iid[1]", "omega", "omega_sex", "b1a", "b1b", "sigma"))
print(m3c_trace / m3c_hist)
# save
png(here::here("plots", "m3c_diagnostics.png"),
    width = 6, height = 8, units = "in", res = 200)
print(m3c_trace / m3c_hist)
dev.off()

# add to data frame
m3c_true <- data.frame(name = m3c_summary$name,
                      mean = c(alpha, alpha_iid, brk_0, brk_male, b_length_a, b_length_b, eps),
                      sd = NA,
                      type = "true")
m3c_post_comparison <- rbind(m3c_true, m3c_summary[, .(name, mean, sd, type)])

m3c_post_comp <- ggplot(data = m3c_post_comparison,
                       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
m3c_post_comp
# save plot
png(here::here("plots", "m3c_post_comp.png"),
    width = 8, height = 4, units = "in", res = 200)
print(m3c_post_comp)
dev.off()

## m3b =========================================================================

# year-specific change points in length, with tag year RE
df[, "m3b" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brks_year[recap_year],
            b_length_a * length,
            b_length_a * (brk_0 + brks_year[recap_year]) + b_length_b * (length - (brk_0 + brks_year[recap_year])))]

# sample response from normal distribution
df[, "growth_m3b" := rtruncnorm(.N, mean = m3b, sd = eps, a = min_growth)]

# plot
p_m3b_direct <- ggplot(df, aes(y = m3b, x = length, color = sex)) +
  geom_point(alpha = 0.25)
p_m3b_sim <- ggplot(df, aes(y = growth_m3b, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m3b_direct + p_m3b_sim)
# save plot
png(here::here("plots", "m3b_sim_dat.png"),
    width = 6, height = 4, units = "in", res = 200)
print(p_m3b_direct + p_m3b_sim)
dev.off()

# prepare data
m3b_dat <- list(growth = df$growth_m3b,
                length = df$length,
                sex = df$sex,
                tag_year = df$tag_year,
                recap_year = df$recap_year,
                N = length(df$growth_m3b),
                R = length(unique(df$recap_year)),
                `T` = length(unique(df$tag_year)))

# fit stan model
fit_m3b <- rstan::stan(file = here::here("stan_models", "m3b.stan"),
                       model_name = "m3b",
                       data = m3b_dat,
                       chains = 3,
                       iter = 4000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m3b, file = here::here("results", "fits", "test", "m3b_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m3b)
m3b_summary <- rstan::summary(fit_m3b, pars = c("alpha", "alpha_iid", "omega", "omega_recap", "b1a", "b1b", "sigma"))
m3b_summary <- m3b_summary$summary %>% as.data.frame()
m3b_summary$name <- rownames(m3b_summary)
m3b_summary$type <- "estimated"
m3b_summary <- data.table(m3b_summary)
print(m3b_summary)

# loo
loo::loo(fit_m3b)

# waic
log_lik_m3b <- loo::extract_log_lik(fit_m3b)
loo::waic(log_lik_m3b)

# plots
m3b_post <- as.array(fit_m3b)
m3b_trace <- mcmc_trace(m3b_post, pars = c("alpha", "alpha_iid[1]", "omega", "omega_recap[1]", "b1a", "b1b", "sigma"))
m3b_hist <- mcmc_hist(m3b_post, pars = c("alpha", "alpha_iid[1]", "omega", "omega_recap[1]", "b1a", "b1b", "sigma"))
print(m3b_trace / m3b_hist)
# save
png(here::here("plots", "m3b_diagnostics.png"),
    width = 6, height = 8, units = "in", res = 200)
print(m3b_trace / m3b_hist)
dev.off()

# add to data frame
m3b_true <- data.frame(name = m3b_summary$name,
                       mean = c(alpha, alpha_iid, brk_0, brks_year, b_length_a, b_length_b, eps),
                       sd = NA,
                       type = "true")
m3b_post_comparison <- rbind(m3b_true, m3b_summary[, .(name, mean, sd, type)])

m3b_post_comp <- ggplot(data = m3b_post_comparison,
                        aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
m3b_post_comp
# save plot
png(here::here("plots", "m3b_post_comp.png"),
    width = 8, height = 4, units = "in", res = 200)
print(m3b_post_comp)
dev.off()

## m3a =========================================================================

# simulate response
# year- & sex-specific change-points in length, with tag year RE
df[sex == 0, "m3a" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brks_year[recap_year],
            b_length_a * length,
            b_length_a * (brk_0 + brks_year[recap_year]) + b_length_b * (length - (brk_0 + brks_year[recap_year])))]

df[sex == 1, "m3a" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brk_male + brks_year[recap_year],
            b_length_a * length,
            b_length_a * (brk_0 + brk_male + brks_year[recap_year]) + b_length_b * (length - (brk_0 + brk_male + brks_year[recap_year])))]

# sample response from normal distribution
df[, "growth_m3a" := rtruncnorm(.N, mean = m3a, sd = eps, a = min_growth)]

# plot
p_m3a_direct <- ggplot(df, aes(y = m3a, x = length, color = sex)) +
  geom_point(alpha = 0.25)
p_m3a_sim <- ggplot(df, aes(y = growth_m3a, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m3a_direct + p_m3a_sim)
# save plot
png(here::here("plots", "m3a_sim_dat.png"),
    width = 6, height = 4, units = "in", res = 200)
print(p_m3a_direct + p_m3a_sim)
dev.off()

# prepare data
m3a_dat <- list(growth = df$growth_m3a,
                length = df$length,
                sex = df$sex,
                tag_year = df$tag_year,
                recap_year = df$recap_year,
                N = length(df$growth_m3a),
                R = length(unique(df$recap_year)),
                `T` = length(unique(df$tag_year)))

# fit stan model
fit_m3a <- rstan::stan(file = here::here("stan_models", "m3a.stan"),
                       model_name = "m3a",
                       data = m3a_dat,
                       chains = 3,
                       iter = 4000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m3a, file = here::here("results", "fits", "test", "m3a_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m3a)
m3a_summary <- rstan::summary(fit_m3a, pars = c("alpha", "alpha_iid", "omega", "omega_sex", "omega_recap", "b1a", "b1b", "sigma"))
m3a_summary <- m3a_summary$summary %>% as.data.frame()
m3a_summary$name <- rownames(m3a_summary)
m3a_summary$type <- "estimated"
m3a_summary <- data.table(m3a_summary)
print(m3a_summary)

# loo
loo::loo(fit_m3a)

# waic
log_lik_m3a <- loo::extract_log_lik(fit_m3a)
loo::waic(log_lik_m3a)

# plots
m3a_post <- as.array(fit_m3a)
m3a_trace <- mcmc_trace(m3a_post, pars = c("alpha", "alpha_iid[1]", "omega", "omega_sex", "omega_recap[1]", "b1a", "b1b", "sigma"))
m3a_hist <- mcmc_hist(m3a_post, pars = c("alpha", "alpha_iid[1]", "omega", "omega_sex", "omega_recap[1]", "b1a", "b1b", "sigma"))
print(m3a_trace / m3a_hist)
# save
png(here::here("plots", "m3a_diagnostics.png"),
    width = 6, height = 8, units = "in", res = 200)
print(m3a_trace / m3a_hist)
dev.off()

# add to data frame
m3a_true <- data.frame(name = m3a_summary$name,
                       mean = c(alpha, alpha_iid, brk_0, brk_male, brks_year, b_length_a, b_length_b, eps),
                       sd = NA,
                       type = "true")
m3a_post_comparison <- rbind(m3a_true, m3a_summary[, .(name, mean, sd, type)])

m3a_post_comp <- ggplot(data = m3a_post_comparison,
                        aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
m3a_post_comp
# save plot
png(here::here("plots", "m3a_post_comp.png"),
    width = 8, height = 4, units = "in", res = 200)
print(m3a_post_comp)
dev.off()
