### Simulation code v2 ####

# libraries
library(here)
library(data.table)
library(magrittr)
library(ggplot2)
library(cmdstanr)
library(bayesplot)
library(truncnorm)

# simulation parameters ---------------------------------------------------

set.seed(1408) # set seed for reproducibility

## load data
load(here::here("data", "top-maturity-modeldata.RData"))

## choose outlier removal to use
dat <- dat_lst$iqr_3yr_data

# look at data numbers
length(unique(dat$year)) # no years
dat[, .N, by = sex] # prop sexes

## numbers
n_total <- 3408
n_years <- 16
n_per_year <- rep(n_total / n_years, n_years)
p_female <- 0.58
n_female_per_year <- rbinom(n_years, n_per_year, p_female)
n_male_per_year <- n_per_year - n_female_per_year

## fixed and linear coefficients
alpha <- 0.5 # intercept
b_length_a <- -0.2 # effect of length before breakpoint
b_length_b <- -0.1 # effect of length after breakpoint
b_sex <- c(0, 0.05) # f / m
b_temperature <- 0.1 # effect of temperature
b_trend <- 0.01 # effect of year of recapture

## covariates
length_sex_means <- c(0.25, -0.25)
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

## random intercept
alpha_sd <- 1
alpha_iid <- rnorm(n = length(unique(df$tag_year)), mean = 0, sd = alpha_sd) # Effect of each year

## random breakpoints
brk_0 <- -1
brk_male <- 0.2
brk_temperature <- 0.2
brk_year_sd <- 0.3
brks <- matrix(c(rnorm(n = n_years,
                       mean = 0,
                       sd = brk_year_sd),
                 rnorm(n = n_years,
                       mean = 0,
                       sd = brk_year_sd)),
               nrow = 2, byrow = TRUE)

## linear predictor

# no change-point in length, with tag year RE
df[, "m5" := (alpha + alpha_iid[tag_year] +
                b_length_a * length),
   by = row.names(df)]

# single change-point in length, with tag year RE
df[, "m4" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0,
            b_length_a * length,
            b_length_a * brk_0 + b_length_b * (length - brk_0)),
   by = row.names(df)]

# sex-specific change point, with tag year RE
df[sex == "female",
   "m3c" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0,
            b_length_a * length,
            b_length_a * (brk_0) + b_length_b * (length - brk_0))]

df[sex == "male",
   "m3c" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0 + brk_male,
            b_length_a * length,
            b_length_a * (brk_0 + brk_male) + b_length_b * (length - (brk_0 + brk_male)))]

# year-specific change points in length, with tag year RE
df[, "m3b" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brks[1, recap_year],
            b_length_a * length,
            b_length_a * (brk_0 + brks[1, recap_year]) + b_length_b * (length - (brk_0 + brks[1, recap_year])))]

# year- & sex-specific change-points in length, with tag year RE
df[sex == "female", "m3a" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brks[1, recap_year],
            b_length_a * length,
            b_length_a * (brk_0 + brks[1, recap_year]) + b_length_b * (length - (brk_0 + brks[1, recap_year])))]

df[sex == "male", "m3a" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brk_male + brks[1, recap_year],
            b_length_a * length,
            b_length_a * (brk_0 + brk_male + brks[1, recap_year]) + b_length_b * (length - (brk_0 + brk_male + brks[1, recap_year])))]

# sex and temperature-specific change point, with tag year RE
df[sex == "female",
   "m3z" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0 + brk_temperature * temperature,
            b_length_a * length,
            b_length_a * (brk_0 + brk_temperature * temperature) + b_length_b * (length - (brk_0 + brk_temperature * temperature)))]

df[sex == "male",
   "m3z" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0 + brk_male + brk_temperature * temperature,
            b_length_a * length,
            b_length_a * (brk_0 + brk_male + brk_temperature * temperature) + b_length_b * (length - (brk_0 + brk_male + brk_temperature * temperature)))]

# temperature-specific change point, with tag year RE
df[sex == "female",
   "m3y" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0 + brk_temperature * temperature,
            b_length_a * length,
            b_length_a * (brk_0 + brk_temperature * temperature) + b_length_b * (length - (brk_0 + brk_temperature * temperature)))]

df[sex == "male",
   "m3y" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0 + brk_temperature * temperature,
            b_length_a * length,
            b_length_a * (brk_0 + brk_temperature * temperature) + b_length_b * (length - (brk_0 + brk_temperature * temperature)))]

# temperature-specific change point, with main effect of sex and tag year RE
df[sex == "female",
   "m3x" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0 + brk_temperature * temperature,
            b_length_a * length + b_sex[1],
            b_length_a * (brk_0 + brk_temperature * temperature) + b_length_b * (length - (brk_0 + brk_temperature * temperature)) + b_sex[1])]

df[sex == "male",
   "m3x" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0 + brk_temperature * temperature,
            b_length_a * length + b_sex[2],
            b_length_a * (brk_0 + brk_temperature * temperature) + b_length_b * (length - (brk_0 + brk_temperature * temperature)) + b_sex[2])]

# sex and temperature-specific change point, with main effect of trend and tag year RE
df[sex == "female",
   "m3w" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0 + brk_temperature * temperature,
            b_length_a * length + year * b_trend,
            b_length_a * (brk_0 + brk_temperature * temperature) + b_length_b * (length - (brk_0 + brk_temperature * temperature)) + year * b_trend)]

df[sex == "male",
   "m3w" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0 + brk_male + brk_temperature * temperature,
            b_length_a * length + year * b_trend,
            b_length_a * (brk_0 + brk_male + brk_temperature * temperature) + b_length_b * (length - (brk_0 + brk_male + brk_temperature * temperature)) + year * b_trend)]

# sex-specific change-points in length, main effect of sex, with tag year RE
df[sex == "female",
   "m2c" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0,
            b_length_a * length + b_sex[1],
            b_length_a * (brk_0) + b_length_b * (length - brk_0) + b_sex[1])]

df[sex == "male",
   "m2c" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0 + brk_male,
            b_length_a * length + b_sex[2],
            b_length_a * (brk_0 + brk_male) + b_length_b * (length - (brk_0 + brk_male)) + b_sex[2])]

# year-specific change-points in length, main effect of sex, with tag year RE
df[sex == "female", "m2b" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brks[1, recap_year],
            b_length_a * length + b_sex[1],
            b_length_a * (brk_0 + brks[1, recap_year]) + b_length_b * (length - (brk_0 + brks[1, recap_year])) + b_sex[1])]

df[sex == "male", "m2b" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brks[1, recap_year],
            b_length_a * length + b_sex[2],
            b_length_a * (brk_0 + brks[1, recap_year]) + b_length_b * (length - (brk_0 + brks[1, recap_year])) + b_sex[2])]

# year- & sex-specific change-points in length, main effect of sex, with tag year RE
df[sex == "female", "m2a" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brks[1, recap_year],
            b_length_a * length + b_sex[1],
            b_length_a * (brk_0 + brks[1, recap_year]) + b_length_b * (length - (brk_0 + brks[1, recap_year])) + b_sex[1])]

df[sex == "male", "m2a" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brk_male + brks[1, recap_year],
            b_length_a * length + b_sex[2],
            b_length_a * (brk_0 + brk_male + brks[1, recap_year]) + b_length_b * (length - (brk_0 + brk_male + brks[1, recap_year])) + b_sex[2])]

# sex-specific change-points in length, main effects of sex & trend, with tag year RE
df[sex == "female",
   "m1c" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0,
            b_length_a * length + b_sex[1] + year * b_trend,
            b_length_a * (brk_0) + b_length_b * (length - brk_0) + b_sex[1] + year * b_trend)]

df[sex == "male",
   "m1c" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0 + brk_male,
            b_length_a * length + b_sex[2] + year * b_trend,
            b_length_a * (brk_0 + brk_male) + b_length_b * (length - (brk_0 + brk_male)) + b_sex[2] + year * b_trend)]

# year-specific change-points in length, main effect of sex, with tag year RE
df[sex == "female", "m1b" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brks[1, recap_year],
            b_length_a * length + b_sex[1] + year * b_trend,
            b_length_a * (brk_0 + brks[1, recap_year]) + b_length_b * (length - (brk_0 + brks[1, recap_year])) + b_sex[1] + year * b_trend)]

df[sex == "male", "m1b" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brks[1, recap_year],
            b_length_a * length + b_sex[2] + year * b_trend,
            b_length_a * (brk_0 + brks[1, recap_year]) + b_length_b * (length - (brk_0 + brks[1, recap_year])) + b_sex[2] + year * b_trend)]

# year- & sex-specific change-points in length, main effect of sex, with tag year RE
df[sex == "female", "m1a" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brks[1, recap_year],
            b_length_a * length + b_sex[1],
            b_length_a * (brk_0 + brks[1, recap_year]) + b_length_b * (length - (brk_0 + brks[1, recap_year])) + b_sex[1])]

df[sex == "male", "m1a" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brk_male + brks[1, recap_year],
            b_length_a * length + b_sex[2] + year * b_trend,
            b_length_a * (brk_0 + brk_male + brks[1, recap_year]) + b_length_b * (length - (brk_0 + brk_male + brks[1, recap_year])) + b_sex[2] + year * b_trend)]

# sex-specific change-points in length, main effects of sex & temperature, with tag year RE
df[sex == "female",
   "m0c" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0,
            b_length_a * length + b_sex[1] + temperature * b_temperature,
            b_length_a * (brk_0) + b_length_b * (length - brk_0) + b_sex[1] + temperature * b_temperature)]

df[sex == "male",
   "m0c" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0 + brk_male,
            b_length_a * length + b_sex[2] + temperature * b_temperature,
            b_length_a * (brk_0 + brk_male) + b_length_b * (length - (brk_0 + brk_male)) + b_sex[2] + temperature * b_temperature)]

# year-specific change-points in length, main effects of sex & temperature, with tag year RE
df[sex == "female", "m0b" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brks[1, recap_year],
            b_length_a * length + b_sex[1] + temperature * b_temperature,
            b_length_a * (brk_0 + brks[1, recap_year]) + b_length_b * (length - (brk_0 + brks[1, recap_year])) + b_sex[1] + temperature * b_temperature)]

df[sex == "male", "m0b" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brks[1, recap_year],
            b_length_a * length + b_sex[2] + temperature * b_temperature,
            b_length_a * (brk_0 + brks[1, recap_year]) + b_length_b * (length - (brk_0 + brks[1, recap_year])) + b_sex[2] + temperature * b_temperature)]

# year- & sex-specific change-points in length, main effects of sex & temperature, with tag year RE
df[sex == "female", "m0a" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brks[1, recap_year],
            b_length_a * length + b_sex[1] + temperature * b_temperature,
            b_length_a * (brk_0 + brks[1, recap_year]) + b_length_b * (length - (brk_0 + brks[1, recap_year])) + b_sex[1] + temperature * b_temperature)]

df[sex == "male", "m0a" := alpha + alpha_iid[tag_year] +
     ifelse(length < brk_0 + brk_male + brks[1, recap_year],
            b_length_a * length + b_sex[2] + temperature * b_temperature,
            b_length_a * (brk_0 + brk_male + brks[1, recap_year]) + b_length_b * (length - (brk_0 + brk_male + brks[1, recap_year])) + b_sex[2] + temperature * b_temperature)]

## response - sample from normal distribution
min_growth <- min(dat$growth_raised)
eps <- 0.5
df[, "growth_m5" := rnorm(.N, mean = m5, sd = eps)]
df[, "growth_m4" := rnorm(.N, mean = m4, sd = eps)]
df[, "growth_m3c" := rnorm(.N, mean = m3c, sd = eps)]
df[, "growth_m3b" := rnorm(.N, mean = m3b, sd = eps)]
df[, "growth_m3a" := rnorm(.N, mean = m3a, sd = eps)]
df[, "growth_m3z" := rtruncnorm(.N, mean = m3z, sd = eps, a = min_growth)]
df[, "growth_m3y" := rtruncnorm(.N, mean = m3y, sd = eps, a = min_growth)]
df[, "growth_m3x" := rtruncnorm(.N, mean = m3x, sd = eps, a = min_growth)]
df[, "growth_m3w" := rtruncnorm(.N, mean = m3w, sd = eps, a = min_growth)]
df[, "growth_m2c" := rtruncnorm(.N, mean = m2c, sd = eps, a = min_growth)]
df[, "growth_m2b" := rtruncnorm(.N, mean = m2b, sd = eps, a = min_growth)]
df[, "growth_m2a" := rtruncnorm(.N, mean = m2a, sd = eps, a = min_growth)]
df[, "growth_m1c" := rtruncnorm(.N, mean = m1c, sd = eps, a = min_growth)]
df[, "growth_m1b" := rtruncnorm(.N, mean = m1b, sd = eps, a = min_growth)]
df[, "growth_m1a" := rtruncnorm(.N, mean = m1a, sd = eps, a = min_growth)]
df[, "growth_m0c" := rtruncnorm(.N, mean = m0c, sd = eps, a = min_growth)]
df[, "growth_m0b" := rtruncnorm(.N, mean = m0b, sd = eps, a = min_growth)]
df[, "growth_m0a" := rtruncnorm(.N, mean = m0a, sd = eps, a = min_growth)]

df[, sex := ifelse(sex == "female", 0, 1)]

## plot it
p_m5 <- ggplot(df, aes(y = growth_m5, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m5)
p_m4 <- ggplot(df, aes(y = growth_m4, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m4)
p_m3c <- ggplot(df, aes(y = growth_m3c, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m3c)
p_m3b <- ggplot(df, aes(y = growth_m3b, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m3b)
p_m3a <- ggplot(df, aes(y = m3a, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m3a)
p_m3z <- ggplot(df, aes(y = m3z, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m3z)
p_m3y <- ggplot(df, aes(y = m3y, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m3y)
p_m3w <- ggplot(df, aes(y = m3w, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m3w)
p_m2c <- ggplot(df, aes(y = m2c, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m2c)
p_m2b <- ggplot(df, aes(y = m2b, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m2b)
p_m2a <- ggplot(df, aes(y = m2a, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m2a)
p_m1c <- ggplot(df, aes(y = m1c, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m1c)
p_m1b <- ggplot(df, aes(y = growth_m1b, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m1b)
p_m1a <- ggplot(df, aes(y = m1a, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m1a)
p_m0c <- ggplot(df, aes(y = m0c, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m0c)
p_m0b <- ggplot(df, aes(y = growth_m0b, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m0b)
p_m0a <- ggplot(df, aes(y = growth_m0a, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m0a)
p_dat <- ggplot(dat, aes(y = growth_raised, x = length_std, colour = sex)) +
  geom_point(alpha = 0.25)
print(p_dat)

# Run models -------------------------------------------------------------------

## m5 ==========================================================================

# prepare data
m5_dat <- list(growth = df$growth_m5,
               length = df$length,
               tag_year = df$tag_year,
               N = length(df$growth_m5))

# fit stan model
fit_m5 <- rstan::stan(file = here::here("stan_models", "m5.stan"),
                      model_name = "m5",
                      data = m5_dat,
                      chains = 3,
                      iter = 2000,
                      init = 0,
                      cores = 4,
                      seed = 1408)
# save
save(fit_m5, file = here::here("results", "fits", "test", "m5_iqr3yr_rstan_NUTS.Rdata")) # Update if data changes

# get summary
m5_summary <- rstan::summary(fit_m5, pars = c("alpha", "b1a", "sigma"))
print(m5_summary)

# loo
loo::loo(fit_m5)

# waic
log_lik_m5 <- loo::extract_log_lik(fit_m5)
waic(log_lik_m5)

# plots
m5_post <- as.array(fit_m5)
mcmc_trace(m5_post, pars = c("alpha", "b1a", "sigma"))
mcmc_hist(m5_post, pars = c("alpha", "b1a", "sigma"))

# add to data frame
m5_post_comparison <- data.frame(type = rep(c("true", "estimated"), 2),
                                 name = c(rep("alpha", 2),
                                          rep("b_length_a", 2)),
                                 mean = c(alpha, m5_summary$summary[1, 1],
                                          b_length_a, m5_summary$summary[2, 1]),
                                 sd = c(NA, m5_summary$summary[1, 3],
                                        NA, m5_summary$summary[2, 3]))

ggplot(data = m5_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

## m4 ==========================================================================

# prepare data
m4_dat <- list(growth = df$growth_m4,
               length = df$length,
               sex = df$sex,
               tag_year = df$tag_year,
               N = length(df$growth_m4),
               `T` = length(unique(df$tag_year)))

# fit stan model
fit_m4 <- rstan::stan(file = here::here("stan_models", "m4.stan"),
                      model_name = "m4",
                      data = m4_dat,
                      chains = 3,
                      iter = 2000,
                      init = 0,
                      cores = 4,
                      seed = 1408)
# save
save(fit_m4, file = here::here("results", "fits", "test", "m4_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m4)
m4_summary <- rstan::summary(fit_m4, pars = c("alpha", "omega", "b1a", "b1b", "sigma"))
print(m4_summary)

# loo
loo::loo(fit_m4)

# waic
log_lik_m4 <- loo::extract_log_lik(fit_m4)
waic(log_lik_m4)

# plots
m4_post <- as.array(fit_m4)
mcmc_trace(m4_post, pars = c("alpha", "omega", "b1a", "b1b", "sigma"))
mcmc_hist(m4_post, pars = c("alpha", "omega", "b1a", "b1b", "sigma"))

# add to data frame
m4_post_comparison <- data.frame(type = rep(c("true", "estimated"), 4),
                                 name = c(rep("alpha", 2),
                                          rep("omega", 2),
                                          rep("b_length_a", 2),
                                          rep("b_length_b", 2)),
                                 mean = c(alpha, m4_summary$summary[1, 1],
                                          brk_0, m4_summary$summary[2, 1],
                                          b_length_a, m4_summary$summary[3, 1],
                                          b_length_b, m4_summary$summary[4, 1]),
                                 sd = c(NA, m4_summary$summary[1, 3],
                                        NA, m4_summary$summary[2, 3],
                                        NA, m4_summary$summary[3, 3],
                                        NA, m4_summary$summary[4, 3]))

# plot
ggplot(data = m4_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

## m3c ==========================================================================

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
                       iter = 2000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m3c, file = here::here("results", "fits", "test", "m3c_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m3c)
m3c_summary <- rstan::summary(fit_m3c, pars = c("alpha", "omega_0", "omega_sex", "b1a", "b1b", "sigma"))
print(m3c_summary)

# loo
loo::loo(fit_m3c)

# waic
log_lik_m3c <- loo::extract_log_lik(fit_m3c)
loo::waic(log_lik_m3c)

# plots
m3c_post <- as.array(fit_m3c)
mcmc_trace(m3c_post, pars = c("alpha", "omega_0", "omega_sex", "b1a", "b1b", "sigma"))
mcmc_hist(m3c_post, pars = c("alpha", "omega_0", "omega_sex", "b1a", "b1b", "sigma"))

# add to data frame
m3c_post_comparison <- data.frame(type = rep(c("true", "estimated"), 5),
                                  name = c(rep("alpha", 2),
                                           rep("omega_0", 2),
                                           rep("omega_sex", 2),
                                           rep("b_length_a", 2),
                                           rep("b_length_b", 2)),
                                  mean = c(alpha, m3c_summary$summary[1, 1],
                                           brk_0, m3c_summary$summary[2, 1],
                                           brk_male, m3c_summary$summary[3, 1],
                                           b_length_a, m3c_summary$summary[4, 1],
                                           b_length_b, m3c_summary$summary[5, 1]),
                                  sd = c(NA, m3c_summary$summary[1, 3],
                                         NA, m3c_summary$summary[2, 3],
                                         NA, m3c_summary$summary[3, 3],
                                         NA, m3c_summary$summary[4, 3],
                                         NA, m3c_summary$summary[5, 3]))

# plot
ggplot(data = m3c_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

## m3b ==========================================================================

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
                       iter = 2000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m3b, file = here::here("results", "fits", "test", "m3b_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m3b)
m3b_summary <- rstan::summary(fit_m3b, pars = c("alpha", "omega_0", "omega_recap", "b1a", "b1b", "sigma"))
print(m3b_summary)

# loo
loo::loo(fit_m3b)

# waic
log_lik_m3b <- loo::extract_log_lik(fit_m3b)
loo::waic(log_lik_m3b)

# plots
m3b_post <- as.array(fit_m3b)
mcmc_trace(m3b_post, pars = c("alpha", "omega_0", "omega_recap[1]", "b1a", "b1b", "sigma"))
mcmc_hist(m3b_post, pars = c("alpha", "omega_0", "omega_recap[1]", "b1a", "b1b", "sigma"))

# add to data frame
m3b_post_comparison <- data.frame(type = rep(c("true", "estimated"), 20),
                                  name = c(rep("alpha", 2),
                                           rep("omega_0", 2),
                                           rep("omega_recap[1]", 2),
                                           rep("omega_recap[2]", 2),
                                           rep("omega_recap[3]", 2),
                                           rep("omega_recap[4]", 2),
                                           rep("omega_recap[5]", 2),
                                           rep("omega_recap[6]", 2),
                                           rep("omega_recap[7]", 2),
                                           rep("omega_recap[8]", 2),
                                           rep("omega_recap[9]", 2),
                                           rep("omega_recap[10]", 2),
                                           rep("omega_recap[11]", 2),
                                           rep("omega_recap[12]", 2),
                                           rep("omega_recap[13]", 2),
                                           rep("omega_recap[14]", 2),
                                           rep("omega_recap[15]", 2),
                                           rep("omega_recap[16]", 2),
                                           rep("b_length_a", 2),
                                           rep("b_length_b", 2)),
                                  mean = c(alpha, m3b_summary$summary[1, 1],
                                           brk_0, m3b_summary$summary[2, 1],
                                           brks[1, 1], m3b_summary$summary[3, 1],
                                           brks[1, 2], m3b_summary$summary[4, 1],
                                           brks[1, 3], m3b_summary$summary[5, 1],
                                           brks[1, 4], m3b_summary$summary[6, 1],
                                           brks[1, 5], m3b_summary$summary[7, 1],
                                           brks[1, 6], m3b_summary$summary[8, 1],
                                           brks[1, 7], m3b_summary$summary[9, 1],
                                           brks[1, 8], m3b_summary$summary[10, 1],
                                           brks[1, 9], m3b_summary$summary[11, 1],
                                           brks[1, 10], m3b_summary$summary[12, 1],
                                           brks[1, 11], m3b_summary$summary[13, 1],
                                           brks[1, 12], m3b_summary$summary[14, 1],
                                           brks[1, 13], m3b_summary$summary[15, 1],
                                           brks[1, 14], m3b_summary$summary[16, 1],
                                           brks[1, 15], m3b_summary$summary[17, 1],
                                           brks[1, 16], m3b_summary$summary[18, 1],
                                           b_length_a, m3b_summary$summary[19, 1],
                                           b_length_b, m3b_summary$summary[20, 1]),
                                  sd = c(NA, m3b_summary$summary[1, 3],
                                         NA, m3b_summary$summary[2, 3],
                                         NA, m3b_summary$summary[3, 3],
                                         NA, m3b_summary$summary[4, 3],
                                         NA, m3b_summary$summary[5, 3],
                                         NA, m3b_summary$summary[6, 3],
                                         NA, m3b_summary$summary[7, 3],
                                         NA, m3b_summary$summary[8, 3],
                                         NA, m3b_summary$summary[9, 3],
                                         NA, m3b_summary$summary[10, 3],
                                         NA, m3b_summary$summary[11, 3],
                                         NA, m3b_summary$summary[12, 3],
                                         NA, m3b_summary$summary[13, 3],
                                         NA, m3b_summary$summary[14, 3],
                                         NA, m3b_summary$summary[15, 3],
                                         NA, m3b_summary$summary[16, 3],
                                         NA, m3b_summary$summary[17, 3],
                                         NA, m3b_summary$summary[18, 3],
                                         NA, m3b_summary$summary[19, 3],
                                         NA, m3b_summary$summary[20, 3]))

# plot
ggplot(data = m3b_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")


## m3a ==========================================================================

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
                       iter = 2000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m3a, file = here::here("results", "fits", "test", "m3a_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m3a)
m3a_summary <- rstan::summary(fit_m3a, pars = c("alpha", "omega_0", "omega_sex", "omega_recap", "b1a", "b1b", "sigma"))
print(m3a_summary)

# loo
loo::loo(fit_m3a)

# waic
log_lik_m3a <- loo::extract_log_lik(fit_m3a)
loo::waic(log_lik_m3a)

# plots
m3a_post <- as.array(fit_m3a)
mcmc_trace(m3a_post, pars = c("alpha", "omega_0", "omega_recap[1]", "b1a", "b1b", "sigma"))
mcmc_hist(m3a_post, pars = c("alpha", "omega_0", "omega_recap[1]", "b1a", "b1b", "sigma"))

# add to data frame
m3a_post_comparison <- data.frame(type = rep(c("true", "estimated"), 21),
                                  name = c(rep("alpha", 2),
                                           rep("omega_0", 2),
                                           rep("omega_sex", 2),
                                           rep("omega_recap[1]", 2),
                                           rep("omega_recap[2]", 2),
                                           rep("omega_recap[3]", 2),
                                           rep("omega_recap[4]", 2),
                                           rep("omega_recap[5]", 2),
                                           rep("omega_recap[6]", 2),
                                           rep("omega_recap[7]", 2),
                                           rep("omega_recap[8]", 2),
                                           rep("omega_recap[9]", 2),
                                           rep("omega_recap[10]", 2),
                                           rep("omega_recap[11]", 2),
                                           rep("omega_recap[12]", 2),
                                           rep("omega_recap[13]", 2),
                                           rep("omega_recap[14]", 2),
                                           rep("omega_recap[15]", 2),
                                           rep("omega_recap[16]", 2),
                                           rep("b_length_a", 2),
                                           rep("b_length_b", 2)),
                                  mean = c(alpha, m3a_summary$summary[1, 1],
                                           brk_0, m3a_summary$summary[2, 1],
                                           brk_male, m3a_summary$summary[3, 1],
                                           brks[1, 1], m3a_summary$summary[4, 1],
                                           brks[1, 2], m3a_summary$summary[5, 1],
                                           brks[1, 3], m3a_summary$summary[6, 1],
                                           brks[1, 4], m3a_summary$summary[7, 1],
                                           brks[1, 5], m3a_summary$summary[8, 1],
                                           brks[1, 6], m3a_summary$summary[9, 1],
                                           brks[1, 7], m3a_summary$summary[10, 1],
                                           brks[1, 8], m3a_summary$summary[11, 1],
                                           brks[1, 9], m3a_summary$summary[12, 1],
                                           brks[1, 10], m3a_summary$summary[13, 1],
                                           brks[1, 11], m3a_summary$summary[14, 1],
                                           brks[1, 12], m3a_summary$summary[15, 1],
                                           brks[1, 13], m3a_summary$summary[16, 1],
                                           brks[1, 14], m3a_summary$summary[17, 1],
                                           brks[1, 15], m3a_summary$summary[18, 1],
                                           brks[1, 16], m3a_summary$summary[19, 1],
                                           b_length_a, m3a_summary$summary[20, 1],
                                           b_length_b, m3a_summary$summary[21, 1]),
                                  sd = c(NA, m3a_summary$summary[1, 3],
                                         NA, m3a_summary$summary[2, 3],
                                         NA, m3a_summary$summary[3, 3],
                                         NA, m3a_summary$summary[4, 3],
                                         NA, m3a_summary$summary[5, 3],
                                         NA, m3a_summary$summary[6, 3],
                                         NA, m3a_summary$summary[7, 3],
                                         NA, m3a_summary$summary[8, 3],
                                         NA, m3a_summary$summary[9, 3],
                                         NA, m3a_summary$summary[10, 3],
                                         NA, m3a_summary$summary[11, 3],
                                         NA, m3a_summary$summary[12, 3],
                                         NA, m3a_summary$summary[13, 3],
                                         NA, m3a_summary$summary[14, 3],
                                         NA, m3a_summary$summary[15, 3],
                                         NA, m3a_summary$summary[16, 3],
                                         NA, m3a_summary$summary[17, 3],
                                         NA, m3a_summary$summary[18, 3],
                                         NA, m3a_summary$summary[19, 3],
                                         NA, m3a_summary$summary[20, 3],
                                         NA, m3a_summary$summary[21, 3]))

# plot
ggplot(data = m3a_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

## m3z ==========================================================================

# prepare data
m3z_dat <- list(growth = df$growth_m3z,
                length = df$length,
                sex = df$sex,
                tag_year = df$tag_year,
                temperature = df$temperature,
                N = length(df$growth_m3z),
                `T` = length(unique(df$tag_year)))

# fit stan model
fit_m3z <- rstan::stan(file = here::here("stan_models", "m3z.stan"),
                       model_name = "m3z",
                       data = m3z_dat,
                       chains = 3,
                       iter = 2000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m3z, file = here::here("results", "fits", "test", "m3z_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m3z)
m3z_summary <- rstan::summary(fit_m3z, pars = c("alpha", "omega_0", "omega_sex", "omega_temp", "b1a", "b1b", "sigma"))
print(m3z_summary)

# loo
loo::loo(fit_m3z)

# waic
log_lik_m3z <- loo::extract_log_lik(fit_m3z)
loo::waic(log_lik_m3z)

# plots
m3z_post <- as.array(fit_m3z)
mcmc_trace(m3z_post, pars = c("alpha", "omega_0", "omega_sex", "omega_temp", "b1a", "b1b", "sigma"))
mcmc_hist(m3z_post, pars = c("alpha", "omega_0", "omega_sex", "omega_temp", "b1a", "b1b", "sigma"))

# add to data frame
m3z_post_comparison <- data.frame(type = rep(c("true", "estimated"), 6),
                                  name = c(rep("alpha", 2),
                                           rep("omega_0", 2),
                                           rep("omega_sex", 2),
                                           rep("omega_temp", 2),
                                           rep("b_length_a", 2),
                                           rep("b_length_b", 2)),
                                  mean = c(alpha, m3z_summary$summary[1, 1],
                                           brk_0, m3z_summary$summary[2, 1],
                                           brk_male, m3z_summary$summary[3, 1],
                                           brk_temperature, m3z_summary$summary[4, 1],
                                           b_length_a, m3z_summary$summary[5, 1],
                                           b_length_b, m3z_summary$summary[6, 1]),
                                  sd = c(NA, m3z_summary$summary[1, 3],
                                         NA, m3z_summary$summary[2, 3],
                                         NA, m3z_summary$summary[3, 3],
                                         NA, m3z_summary$summary[4, 3],
                                         NA, m3z_summary$summary[5, 3],
                                         NA, m3z_summary$summary[6, 3]))

# plot
ggplot(data = m3z_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")



## m3y ==========================================================================

# prepare data
m3y_dat <- list(growth = df$m3y,
                length = df$length,
                tag_year = df$tag_year,
                temperature = df$temperature,
                N = length(df$growth_m3y),
                `T` = length(unique(df$tag_year)))

# fit stan model
fit_m3y <- rstan::stan(file = here::here("stan_models", "m3y.stan"),
                       model_name = "m3y",
                       data = m3y_dat,
                       chains = 3,
                       iter = 2000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m3y, file = here::here("results", "fits", "test", "m3y_direct_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m3y)
m3y_summary <- rstan::summary(fit_m3y, pars = c("alpha", "omega_0", "omega_temp", "b1a", "b1b", "sigma"))
print(m3y_summary)

# loo
loo::loo(fit_m3y)

# waic
log_lik_m3y <- loo::extract_log_lik(fit_m3y)
loo::waic(log_lik_m3y)

# plots
m3y_post <- as.array(fit_m3y)
mcmc_trace(m3y_post, pars = c("alpha", "omega_0", "omega_temp", "b1a", "b1b", "sigma"))
mcmc_hist(m3y_post, pars = c("alpha", "omega_0", "omega_temp", "b1a", "b1b", "sigma"))

# add to data frame
m3y_post_comparison <- data.frame(type = rep(c("true", "estimated"), 10),
                                  name = c(rep("alpha", 2),
                                           rep("omega_0", 2),
                                           rep("omega_temp", 2),
                                           rep("b_length_a", 2),
                                           rep("b_length_b", 2)),
                                  mean = c(alpha, m3y_summary$summary[1, 1],
                                           brk_0, m3y_summary$summary[2, 1],
                                           brk_temperature, m3y_summary$summary[3, 1],
                                           b_length_a, m3y_summary$summary[4, 1],
                                           b_length_b, m3y_summary$summary[5, 1]),
                                  sd = c(NA, m3y_summary$summary[1, 3],
                                         NA, m3y_summary$summary[2, 3],
                                         NA, m3y_summary$summary[3, 3],
                                         NA, m3y_summary$summary[4, 3],
                                         NA, m3y_summary$summary[5, 3]))

# plot
ggplot(data = m3y_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

## m3x ==========================================================================

# prepare data
m3x_dat <- list(growth = df$growth_m3x,
                length = df$length,
                sex = df$sex,
                tag_year = df$tag_year,
                temperature = df$temperature,
                N = length(df$growth_m3x),
                `T` = length(unique(df$tag_year)))

# fit stan model
fit_m3x <- rstan::stan(file = here::here("stan_models", "m3x.stan"),
                       model_name = "m3x",
                       data = m3x_dat,
                       chains = 3,
                       iter = 2000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m3x, file = here::here("results", "fits", "test", "m3x_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m3x)
m3x_summary <- rstan::summary(fit_m3x, pars = c("alpha", "omega_0", "omega_temp", "b1a", "b1b", "b2", "sigma"))
print(m3x_summary)

# loo
loo::loo(fit_m3x)

# waic
log_lik_m3x <- loo::extract_log_lik(fit_m3x)
loo::waic(log_lik_m3x)

# plots
m3x_post <- as.array(fit_m3x)
mcmc_trace(m3x_post, pars = c("alpha", "omega_0", "omega_temp", "b1a", "b1b", "b2", "sigma"))
mcmc_hist(m3x_post, pars = c("alpha", "omega_0", "omega_temp", "b1a", "b1b", "b2", "sigma"))

# add to data frame
m3x_post_comparison <- data.frame(type = rep(c("true", "estimated"), 6),
                                  name = c(rep("alpha", 2),
                                           rep("omega_0", 2),
                                           rep("omega_temp", 2),
                                           rep("b_length_a", 2),
                                           rep("b_length_b", 2),
                                           rep("b_sex", 2)),
                                  mean = c(alpha, m3x_summary$summary[1, 1],
                                           brk_0, m3x_summary$summary[2, 1],
                                           brk_temperature, m3x_summary$summary[3, 1],
                                           b_length_a, m3x_summary$summary[4, 1],
                                           b_length_b, m3x_summary$summary[5, 1],
                                           b_sex[2], m3x_summary$summary[6, 1]),
                                  sd = c(NA, m3x_summary$summary[1, 3],
                                         NA, m3x_summary$summary[2, 3],
                                         NA, m3x_summary$summary[3, 3],
                                         NA, m3x_summary$summary[4, 3],
                                         NA, m3x_summary$summary[5, 3],
                                         NA, m3x_summary$summary[6, 3]))

# plot
ggplot(data = m3x_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

## m3w ==========================================================================

# prepare data
m3w_dat <- list(growth = df$growth_m3w,
                length = df$length,
                sex = df$sex,
                tag_year = df$tag_year,
                temperature = df$temperature,
                year = df$year,
                N = length(df$growth_m3w),
                `T` = length(unique(df$tag_year)))

# fit stan model
fit_m3w <- rstan::stan(file = here::here("stan_models", "m3w.stan"),
                       model_name = "m3w",
                       data = m3w_dat,
                       chains = 3,
                       iter = 2000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m3w, file = here::here("results", "fits", "test", "m3w_direct_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m3w)
m3w_summary <- rstan::summary(fit_m3w, pars = c("alpha", "omega_0", "omega_sex", "omega_temp", "b1a", "b1b", "b4", "sigma"))
print(m3w_summary)

# loo
loo::loo(fit_m3w)

# waic
log_lik_m3w <- loo::extract_log_lik(fit_m3w)
loo::waic(log_lik_m3w)

# plots
m3w_post <- as.array(fit_m3w)
mcmc_trace(m3w_post, pars = c("alpha", "omega_0", "omega_sex", "omega_temp", "b1a", "b1b", "b4", "sigma"))
mcmc_hist(m3w_post, pars = c("alpha", "omega_0", "omega_sex", "omega_temp", "b1a", "b1b", "b4", "sigma"))

# add to data frame
m3w_post_comparison <- data.frame(type = rep(c("true", "estimated"), 7),
                                  name = c(rep("alpha", 2),
                                           rep("omega_0", 2),
                                           rep("omega_sex", 2),
                                           rep("omega_temp", 2),
                                           rep("b_length_a", 2),
                                           rep("b_length_b", 2),
                                           rep("b_year", 2)),
                                  mean = c(alpha, m3w_summary$summary[1, 1],
                                           brk_0, m3w_summary$summary[2, 1],
                                           brk_male, m3w_summary$summary[3, 1],
                                           brk_temperature, m3w_summary$summary[4, 1],
                                           b_length_a, m3w_summary$summary[5, 1],
                                           b_length_b, m3w_summary$summary[6, 1],
                                           b_trend, m3w_summary$summary[7, 1]),
                                  sd = c(NA, m3w_summary$summary[1, 3],
                                         NA, m3w_summary$summary[2, 3],
                                         NA, m3w_summary$summary[3, 3],
                                         NA, m3w_summary$summary[4, 3],
                                         NA, m3w_summary$summary[5, 3],
                                         NA, m3w_summary$summary[6, 3],
                                         NA, m3w_summary$summary[7, 3]))

# plot
ggplot(data = m3w_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

## m2c ==========================================================================

# prepare data
m2c_dat <- list(growth = df$growth_m2c,
                length = df$length,
                sex = df$sex,
                tag_year = df$tag_year,
                N = length(df$growth_m2c),
                `T` = length(unique(df$tag_year)))

# fit stan model
fit_m2c <- rstan::stan(file = here::here("stan_models", "m2c.stan"),
                       model_name = "m2c",
                       data = m2c_dat,
                       chains = 3,
                       iter = 2000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m2c, file = here::here("results", "fits", "test", "m2c_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m2c)
m2c_summary <- rstan::summary(fit_m2c, pars = c("alpha", "omega_0", "omega_sex", "b1a", "b1b", "b2", "sigma"))
print(m2c_summary)

# loo
loo::loo(fit_m2c)

# waic
log_lik_m2c <- loo::extract_log_lik(fit_m2c)
loo::waic(log_lik_m2c)

# plots
m2c_post <- as.array(fit_m2c)
mcmc_trace(m2c_post, pars = c("alpha", "omega_0", "omega_sex", "b1a", "b1b", "b2", "sigma"))
mcmc_hist(m2c_post, pars = c("alpha", "omega_0", "omega_sex", "b1a", "b1b", "b2", "sigma"))

# add to data frame
m2c_post_comparison <- data.frame(type = rep(c("true", "estimated"), 6),
                                  name = c(rep("alpha", 2),
                                           rep("omega_0", 2),
                                           rep("omega_sex", 2),
                                           rep("b_length_a", 2),
                                           rep("b_length_b", 2),
                                           rep("b_sex", 2)),
                                  mean = c(alpha, m2c_summary$summary[1, 1],
                                           brk_0, m2c_summary$summary[2, 1],
                                           brk_male, m2c_summary$summary[3, 1],
                                           b_length_a, m2c_summary$summary[4, 1],
                                           b_length_b, m2c_summary$summary[5, 1],
                                           b_sex[2], m2c_summary$summary[6, 1]),
                                  sd = c(NA, m2c_summary$summary[1, 3],
                                         NA, m2c_summary$summary[2, 3],
                                         NA, m2c_summary$summary[3, 3],
                                         NA, m2c_summary$summary[4, 3],
                                         NA, m2c_summary$summary[5, 3],
                                         NA, m2c_summary$summary[6, 3]))

# plot
ggplot(data = m2c_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

## m2b ==========================================================================

# prepare data
m2b_dat <- list(growth = df$m2b,
                length = df$length,
                sex = df$sex,
                tag_year = df$tag_year,
                recap_year = df$recap_year,
                N = length(df$growth_m2b),
                R = length(unique(df$recap_year)),
                `T` = length(unique(df$tag_year)))

# fit stan model
fit_m2b <- rstan::stan(file = here::here("stan_models", "m2b.stan"),
                       model_name = "m2b",
                       data = m2b_dat,
                       chains = 3,
                       iter = 2000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m2b, file = here::here("results", "fits", "test", "m2b_direct_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m2b)
m2b_summary <- rstan::summary(fit_m2b, pars = c("alpha", "omega_0", "omega_recap", "b1a", "b1b", "b2", "sigma"))
print(m2b_summary)

# loo
loo::loo(fit_m2b)

# waic
log_lik_m2b <- loo::extract_log_lik(fit_m2b)
loo::waic(log_lik_m2b)

# plots
m2b_post <- as.array(fit_m2b)
mcmc_trace(m2b_post, pars = c("alpha", "omega_0", "omega_recap[1]", "b1a", "b1b", "b2", "sigma"))
mcmc_hist(m2b_post, pars = c("alpha", "omega_0", "omega_recap[1]", "b1a", "b1b", "b2", "sigma"))

# add to data frame
m2b_post_comparison <- data.frame(type = rep(c("true", "estimated"), 21),
                                  name = c(rep("alpha", 2),
                                           rep("omega_0", 2),
                                           rep("omega_recap[1]", 2),
                                           rep("omega_recap[2]", 2),
                                           rep("omega_recap[3]", 2),
                                           rep("omega_recap[4]", 2),
                                           rep("omega_recap[5]", 2),
                                           rep("omega_recap[6]", 2),
                                           rep("omega_recap[7]", 2),
                                           rep("omega_recap[8]", 2),
                                           rep("omega_recap[9]", 2),
                                           rep("omega_recap[10]", 2),
                                           rep("omega_recap[11]", 2),
                                           rep("omega_recap[12]", 2),
                                           rep("omega_recap[13]", 2),
                                           rep("omega_recap[14]", 2),
                                           rep("omega_recap[15]", 2),
                                           rep("omega_recap[16]", 2),
                                           rep("b_length_a", 2),
                                           rep("b_length_b", 2),
                                           rep("b_sex", 2)),
                                  mean = c(alpha, m2b_summary$summary[1, 1],
                                           brk_0, m2b_summary$summary[2, 1],
                                           brks[1, 1], m2b_summary$summary[3, 1],
                                           brks[1, 2], m2b_summary$summary[4, 1],
                                           brks[1, 3], m2b_summary$summary[5, 1],
                                           brks[1, 4], m2b_summary$summary[6, 1],
                                           brks[1, 5], m2b_summary$summary[7, 1],
                                           brks[1, 6], m2b_summary$summary[8, 1],
                                           brks[1, 7], m2b_summary$summary[9, 1],
                                           brks[1, 8], m2b_summary$summary[10, 1],
                                           brks[1, 9], m2b_summary$summary[11, 1],
                                           brks[1, 10], m2b_summary$summary[12, 1],
                                           brks[1, 11], m2b_summary$summary[13, 1],
                                           brks[1, 12], m2b_summary$summary[14, 1],
                                           brks[1, 13], m2b_summary$summary[15, 1],
                                           brks[1, 14], m2b_summary$summary[16, 1],
                                           brks[1, 15], m2b_summary$summary[17, 1],
                                           brks[1, 16], m2b_summary$summary[18, 1],
                                           b_length_a, m2b_summary$summary[19, 1],
                                           b_length_b, m2b_summary$summary[20, 1],
                                           b_sex[2], m2b_summary$summary[21, 1]),
                                  sd = c(NA, m2b_summary$summary[1, 3],
                                         NA, m2b_summary$summary[2, 3],
                                         NA, m2b_summary$summary[3, 3],
                                         NA, m2b_summary$summary[4, 3],
                                         NA, m2b_summary$summary[5, 3],
                                         NA, m2b_summary$summary[6, 3],
                                         NA, m2b_summary$summary[7, 3],
                                         NA, m2b_summary$summary[8, 3],
                                         NA, m2b_summary$summary[9, 3],
                                         NA, m2b_summary$summary[10, 3],
                                         NA, m2b_summary$summary[11, 3],
                                         NA, m2b_summary$summary[12, 3],
                                         NA, m2b_summary$summary[13, 3],
                                         NA, m2b_summary$summary[14, 3],
                                         NA, m2b_summary$summary[15, 3],
                                         NA, m2b_summary$summary[16, 3],
                                         NA, m2b_summary$summary[17, 3],
                                         NA, m2b_summary$summary[18, 3],
                                         NA, m2b_summary$summary[19, 3],
                                         NA, m2b_summary$summary[20, 3],
                                         NA, m2b_summary$summary[21, 3]))

# plot
ggplot(data = m2b_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

## m2a ==========================================================================

# prepare data
m2a_dat <- list(growth = df$growth_m2a,
                length = df$length,
                sex = df$sex,
                tag_year = df$tag_year,
                recap_year = df$recap_year,
                N = length(df$growth_m2a),
                R = length(unique(df$recap_year)),
                `T` = length(unique(df$tag_year)))

# fit stan model
fit_m2a <- rstan::stan(file = here::here("stan_models", "m2a.stan"),
                       model_name = "m2a",
                       data = m2a_dat,
                       chains = 3,
                       iter = 2000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m2a, file = here::here("results", "fits", "test", "m2a_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m2a)
m2a_summary <- rstan::summary(fit_m2a, pars = c("alpha", "omega_0", "omega_sex", "omega_recap", "b1a", "b1b", "b2", "sigma"))
print(m2a_summary)

# loo
loo::loo(fit_m2a)

# waic
log_lik_m2a <- loo::extract_log_lik(fit_m2a)
loo::waic(log_lik_m2a)

# plots
m2a_post <- as.array(fit_m2a)
mcmc_trace(m2a_post, pars = c("alpha", "omega_0", "omega_recap[1]", "b1a", "b1b", "b2", "sigma"))
mcmc_hist(m2a_post, pars = c("alpha", "omega_0", "omega_recap[1]", "b1a", "b1b", "b2", "sigma"))

# add to data frame
m2a_post_comparison <- data.frame(type = rep(c("true", "estimated"), 22),
                                  name = c(rep("alpha", 2),
                                           rep("omega_0", 2),
                                           rep("omega_sex", 2),
                                           rep("omega_recap[1]", 2),
                                           rep("omega_recap[2]", 2),
                                           rep("omega_recap[3]", 2),
                                           rep("omega_recap[4]", 2),
                                           rep("omega_recap[5]", 2),
                                           rep("omega_recap[6]", 2),
                                           rep("omega_recap[7]", 2),
                                           rep("omega_recap[8]", 2),
                                           rep("omega_recap[9]", 2),
                                           rep("omega_recap[10]", 2),
                                           rep("omega_recap[11]", 2),
                                           rep("omega_recap[12]", 2),
                                           rep("omega_recap[13]", 2),
                                           rep("omega_recap[14]", 2),
                                           rep("omega_recap[15]", 2),
                                           rep("omega_recap[16]", 2),
                                           rep("b_length_a", 2),
                                           rep("b_length_b", 2),
                                           rep("b_sex", 2)),
                                  mean = c(alpha, m2a_summary$summary[1, 1],
                                           brk_0, m2a_summary$summary[2, 1],
                                           brk_male, m2a_summary$summary[3, 1],
                                           brks[1, 1], m2a_summary$summary[4, 1],
                                           brks[1, 2], m2a_summary$summary[5, 1],
                                           brks[1, 3], m2a_summary$summary[6, 1],
                                           brks[1, 4], m2a_summary$summary[7, 1],
                                           brks[1, 5], m2a_summary$summary[8, 1],
                                           brks[1, 6], m2a_summary$summary[9, 1],
                                           brks[1, 7], m2a_summary$summary[10, 1],
                                           brks[1, 8], m2a_summary$summary[11, 1],
                                           brks[1, 9], m2a_summary$summary[12, 1],
                                           brks[1, 10], m2a_summary$summary[13, 1],
                                           brks[1, 11], m2a_summary$summary[14, 1],
                                           brks[1, 12], m2a_summary$summary[15, 1],
                                           brks[1, 13], m2a_summary$summary[16, 1],
                                           brks[1, 14], m2a_summary$summary[17, 1],
                                           brks[1, 15], m2a_summary$summary[18, 1],
                                           brks[1, 16], m2a_summary$summary[19, 1],
                                           b_length_a, m2a_summary$summary[20, 1],
                                           b_length_b, m2a_summary$summary[21, 1],
                                           b_sex[2], m2a_summary$summary[22, 1]),
                                  sd = c(NA, m2a_summary$summary[1, 3],
                                         NA, m2a_summary$summary[2, 3],
                                         NA, m2a_summary$summary[3, 3],
                                         NA, m2a_summary$summary[4, 3],
                                         NA, m2a_summary$summary[5, 3],
                                         NA, m2a_summary$summary[6, 3],
                                         NA, m2a_summary$summary[7, 3],
                                         NA, m2a_summary$summary[8, 3],
                                         NA, m2a_summary$summary[9, 3],
                                         NA, m2a_summary$summary[10, 3],
                                         NA, m2a_summary$summary[11, 3],
                                         NA, m2a_summary$summary[12, 3],
                                         NA, m2a_summary$summary[13, 3],
                                         NA, m2a_summary$summary[14, 3],
                                         NA, m2a_summary$summary[15, 3],
                                         NA, m2a_summary$summary[16, 3],
                                         NA, m2a_summary$summary[17, 3],
                                         NA, m2a_summary$summary[18, 3],
                                         NA, m2a_summary$summary[19, 3],
                                         NA, m2a_summary$summary[20, 3],
                                         NA, m2a_summary$summary[21, 3],
                                         NA, m2a_summary$summary[22, 3]))

# plot
ggplot(data = m2a_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

## m1c ==========================================================================

# prepare data
m1c_dat <- list(growth = df$growth_m1c,
                length = df$length,
                sex = df$sex,
                tag_year = df$tag_year,
                year = df$year,
                N = length(df$growth_m1c),
                `T` = length(unique(df$tag_year)))

# fit stan model
fit_m1c <- rstan::stan(file = here::here("stan_models", "m1c.stan"),
                       model_name = "m1c",
                       data = m1c_dat,
                       chains = 3,
                       iter = 2000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m1c, file = here::here("results", "fits", "test", "m1c_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m1c)
m1c_summary <- rstan::summary(fit_m1c, pars = c("alpha", "omega_0", "omega_sex", "b1a", "b1b", "b2", "b4", "sigma"))
print(m1c_summary)

# loo
loo::loo(fit_m1c)

# waic
log_lik_m1c <- loo::extract_log_lik(fit_m1c)
loo::waic(log_lik_m1c)

# plots
m1c_post <- as.array(fit_m1c)
mcmc_trace(m1c_post, pars = c("alpha", "omega_0", "omega_sex", "b1a", "b1b", "b2", "b4", "sigma"))
mcmc_hist(m1c_post, pars = c("alpha", "omega_0", "omega_sex", "b1a", "b1b", "b2", "b4", "sigma"))

# add to data frame
m1c_post_comparison <- data.frame(type = rep(c("true", "estimated"), 7),
                                  name = c(rep("alpha", 2),
                                           rep("omega_0", 2),
                                           rep("omega_sex", 2),
                                           rep("b_length_a", 2),
                                           rep("b_length_b", 2),
                                           rep("b_sex", 2),
                                           rep("b_trend", 2)),
                                  mean = c(alpha, m1c_summary$summary[1, 1],
                                           brk_0, m1c_summary$summary[2, 1],
                                           brk_male, m1c_summary$summary[3, 1],
                                           b_length_a, m1c_summary$summary[4, 1],
                                           b_length_b, m1c_summary$summary[5, 1],
                                           b_sex[2], m1c_summary$summary[6, 1],
                                           b_trend, m1c_summary$summary[7, 1]),
                                  sd = c(NA, m1c_summary$summary[1, 3],
                                         NA, m1c_summary$summary[2, 3],
                                         NA, m1c_summary$summary[3, 3],
                                         NA, m1c_summary$summary[4, 3],
                                         NA, m1c_summary$summary[5, 3],
                                         NA, m1c_summary$summary[6, 3],
                                         NA, m1c_summary$summary[7, 3]))

# plot
ggplot(data = m1c_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

## m1b ==========================================================================

# prepare data
m1b_dat <- list(growth = df$growth_m1b,
                length = df$length,
                sex = df$sex,
                tag_year = df$tag_year,
                recap_year = df$recap_year,
                year = df$year,
                N = length(df$growth_m1b),
                R = length(unique(df$recap_year)),
                `T` = length(unique(df$tag_year)))

# fit stan model
fit_m1b <- rstan::stan(file = here::here("stan_models", "m1b.stan"),
                       model_name = "m1b",
                       data = m1b_dat,
                       chains = 3,
                       iter = 2000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m1b, file = here::here("results", "fits", "test", "m1b_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m1b)
m1b_summary <- rstan::summary(fit_m1b, pars = c("alpha", "omega_0", "omega_recap", "b1a", "b1b", "b2", "b4", "sigma"))
print(m1b_summary)

# loo
loo::loo(fit_m1b)

# waic
log_lik_m1b <- loo::extract_log_lik(fit_m1b)
loo::waic(log_lik_m1b)

# plots
m1b_post <- as.array(fit_m1b)
mcmc_trace(m1b_post, pars = c("alpha", "omega_0", "omega_recap[1]", "b1a", "b1b", "b2", "b4", "sigma"))
mcmc_hist(m1b_post, pars = c("alpha", "omega_0", "omega_recap[1]", "b1a", "b1b", "b2", "b4", "sigma"))

# add to data frame
m1b_post_comparison <- data.frame(type = rep(c("true", "estimated"), 22),
                                  name = c(rep("alpha", 2),
                                           rep("omega_0", 2),
                                           rep("omega_recap[1]", 2),
                                           rep("omega_recap[2]", 2),
                                           rep("omega_recap[3]", 2),
                                           rep("omega_recap[4]", 2),
                                           rep("omega_recap[5]", 2),
                                           rep("omega_recap[6]", 2),
                                           rep("omega_recap[7]", 2),
                                           rep("omega_recap[8]", 2),
                                           rep("omega_recap[9]", 2),
                                           rep("omega_recap[10]", 2),
                                           rep("omega_recap[11]", 2),
                                           rep("omega_recap[12]", 2),
                                           rep("omega_recap[13]", 2),
                                           rep("omega_recap[14]", 2),
                                           rep("omega_recap[15]", 2),
                                           rep("omega_recap[16]", 2),
                                           rep("b_length_a", 2),
                                           rep("b_length_b", 2),
                                           rep("b_sex", 2),
                                           rep("b_trend", 2)),
                                  mean = c(alpha, m1b_summary$summary[1, 1],
                                           brk_0, m1b_summary$summary[2, 1],
                                           brks[1, 1], m1b_summary$summary[3, 1],
                                           brks[1, 2], m1b_summary$summary[4, 1],
                                           brks[1, 3], m1b_summary$summary[5, 1],
                                           brks[1, 4], m1b_summary$summary[6, 1],
                                           brks[1, 5], m1b_summary$summary[7, 1],
                                           brks[1, 6], m1b_summary$summary[8, 1],
                                           brks[1, 7], m1b_summary$summary[9, 1],
                                           brks[1, 8], m1b_summary$summary[10, 1],
                                           brks[1, 9], m1b_summary$summary[11, 1],
                                           brks[1, 10], m1b_summary$summary[12, 1],
                                           brks[1, 11], m1b_summary$summary[13, 1],
                                           brks[1, 12], m1b_summary$summary[14, 1],
                                           brks[1, 13], m1b_summary$summary[15, 1],
                                           brks[1, 14], m1b_summary$summary[16, 1],
                                           brks[1, 15], m1b_summary$summary[17, 1],
                                           brks[1, 16], m1b_summary$summary[18, 1],
                                           b_length_a, m1b_summary$summary[19, 1],
                                           b_length_b, m1b_summary$summary[20, 1],
                                           b_sex[2], m1b_summary$summary[21, 1],
                                           b_trend, m1b_summary$summary[22, 1]),
                                  sd = c(NA, m1b_summary$summary[1, 3],
                                         NA, m1b_summary$summary[2, 3],
                                         NA, m1b_summary$summary[3, 3],
                                         NA, m1b_summary$summary[4, 3],
                                         NA, m1b_summary$summary[5, 3],
                                         NA, m1b_summary$summary[6, 3],
                                         NA, m1b_summary$summary[7, 3],
                                         NA, m1b_summary$summary[8, 3],
                                         NA, m1b_summary$summary[9, 3],
                                         NA, m1b_summary$summary[10, 3],
                                         NA, m1b_summary$summary[11, 3],
                                         NA, m1b_summary$summary[12, 3],
                                         NA, m1b_summary$summary[13, 3],
                                         NA, m1b_summary$summary[14, 3],
                                         NA, m1b_summary$summary[15, 3],
                                         NA, m1b_summary$summary[16, 3],
                                         NA, m1b_summary$summary[17, 3],
                                         NA, m1b_summary$summary[18, 3],
                                         NA, m1b_summary$summary[19, 3],
                                         NA, m1b_summary$summary[20, 3],
                                         NA, m1b_summary$summary[21, 3],
                                         NA, m1b_summary$summary[22, 3]))

# plot
ggplot(data = m1b_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

## m1a ==========================================================================
# TODO: redo with corrected sim data
# prepare data
m1a_dat <- list(growth = df$growth_m1a,
                length = df$length,
                sex = df$sex,
                tag_year = df$tag_year,
                recap_year = df$recap_year,
                year = df$year,
                N = length(df$growth_m1a),
                R = length(unique(df$recap_year)),
                `T` = length(unique(df$tag_year)))

# fit stan model
fit_m1a <- rstan::stan(file = here::here("stan_models", "m1a.stan"),
                       model_name = "m1a",
                       data = m1a_dat,
                       chains = 3,
                       iter = 2000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m1a, file = here::here("results", "fits", "test", "m1a_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m1a)
m1a_summary <- rstan::summary(fit_m1a, pars = c("alpha", "omega_0", "omega_sex", "omega_recap", "b1a", "b1b", "b2", "b4", "sigma"))
print(m1a_summary)

# loo
loo::loo(fit_m1a)

# waic
log_lik_m1a <- loo::extract_log_lik(fit_m1a)
loo::waic(log_lik_m1a)

# plots
m1a_post <- as.array(fit_m1a)
mcmc_trace(m1a_post, pars = c("alpha", "omega_0", "omega_recap[1]", "b1a", "b1b", "b2", "b4", "sigma"))
mcmc_hist(m1a_post, pars = c("alpha", "omega_0", "omega_recap[1]", "b1a", "b1b", "b2", "b4", "sigma"))

# add to data frame
m1a_post_comparison <- data.frame(type = rep(c("true", "estimated"), 23),
                                  name = c(rep("alpha", 2),
                                           rep("omega_0", 2),
                                           rep("omega_sex", 2),
                                           rep("omega_recap[1]", 2),
                                           rep("omega_recap[2]", 2),
                                           rep("omega_recap[3]", 2),
                                           rep("omega_recap[4]", 2),
                                           rep("omega_recap[5]", 2),
                                           rep("omega_recap[6]", 2),
                                           rep("omega_recap[7]", 2),
                                           rep("omega_recap[8]", 2),
                                           rep("omega_recap[9]", 2),
                                           rep("omega_recap[10]", 2),
                                           rep("omega_recap[11]", 2),
                                           rep("omega_recap[12]", 2),
                                           rep("omega_recap[13]", 2),
                                           rep("omega_recap[14]", 2),
                                           rep("omega_recap[15]", 2),
                                           rep("omega_recap[16]", 2),
                                           rep("b_length_a", 2),
                                           rep("b_length_b", 2),
                                           rep("b_sex", 2),
                                           rep("b_trend", 2)),
                                  mean = c(alpha, m1a_summary$summary[1, 1],
                                           brk_0, m1a_summary$summary[2, 1],
                                           brk_male, m1a_summary$summary[3, 1],
                                           brks[1, 1], m1a_summary$summary[4, 1],
                                           brks[1, 2], m1a_summary$summary[5, 1],
                                           brks[1, 3], m1a_summary$summary[6, 1],
                                           brks[1, 4], m1a_summary$summary[7, 1],
                                           brks[1, 5], m1a_summary$summary[8, 1],
                                           brks[1, 6], m1a_summary$summary[9, 1],
                                           brks[1, 7], m1a_summary$summary[10, 1],
                                           brks[1, 8], m1a_summary$summary[11, 1],
                                           brks[1, 9], m1a_summary$summary[12, 1],
                                           brks[1, 10], m1a_summary$summary[13, 1],
                                           brks[1, 11], m1a_summary$summary[14, 1],
                                           brks[1, 12], m1a_summary$summary[15, 1],
                                           brks[1, 13], m1a_summary$summary[16, 1],
                                           brks[1, 14], m1a_summary$summary[17, 1],
                                           brks[1, 15], m1a_summary$summary[18, 1],
                                           brks[1, 16], m1a_summary$summary[19, 1],
                                           b_length_a, m1a_summary$summary[20, 1],
                                           b_length_b, m1a_summary$summary[21, 1],
                                           b_sex[2], m1a_summary$summary[22, 1],
                                           b_trend, m1a_summary$summary[23, 1]),
                                  sd = c(NA, m1a_summary$summary[1, 3],
                                         NA, m1a_summary$summary[2, 3],
                                         NA, m1a_summary$summary[3, 3],
                                         NA, m1a_summary$summary[4, 3],
                                         NA, m1a_summary$summary[5, 3],
                                         NA, m1a_summary$summary[6, 3],
                                         NA, m1a_summary$summary[7, 3],
                                         NA, m1a_summary$summary[8, 3],
                                         NA, m1a_summary$summary[9, 3],
                                         NA, m1a_summary$summary[10, 3],
                                         NA, m1a_summary$summary[11, 3],
                                         NA, m1a_summary$summary[12, 3],
                                         NA, m1a_summary$summary[13, 3],
                                         NA, m1a_summary$summary[14, 3],
                                         NA, m1a_summary$summary[15, 3],
                                         NA, m1a_summary$summary[16, 3],
                                         NA, m1a_summary$summary[17, 3],
                                         NA, m1a_summary$summary[18, 3],
                                         NA, m1a_summary$summary[19, 3],
                                         NA, m1a_summary$summary[20, 3],
                                         NA, m1a_summary$summary[21, 3],
                                         NA, m1a_summary$summary[22, 3],
                                         NA, m1a_summary$summary[23, 3]))

# plot
ggplot(data = m1a_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

## m0c ==========================================================================

# prepare data
m0c_dat <- list(growth = df$m0c,
                length = df$length,
                sex = df$sex,
                tag_year = df$tag_year,
                temperature = df$temperature,
                N = length(df$growth_m0c),
                `T` = length(unique(df$tag_year)))

# fit stan model
fit_m0c <- rstan::stan(file = here::here("stan_models", "m0c.stan"),
                       model_name = "m0c",
                       data = m0c_dat,
                       chains = 3,
                       iter = 2000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m0c, file = here::here("results", "fits", "test", "m0c_direct_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m0c)
m0c_summary <- rstan::summary(fit_m0c, pars = c("alpha", "omega_0", "omega_sex", "b1a", "b1b", "b2", "b3", "sigma"))
print(m0c_summary)

# loo
loo::loo(fit_m0c)

# waic
log_lik_m0c <- loo::extract_log_lik(fit_m0c)
loo::waic(log_lik_m0c)

# plots
m0c_post <- as.array(fit_m0c)
mcmc_trace(m0c_post, pars = c("alpha", "omega_0", "omega_sex", "omega_temperature", "b1a", "b1b", "b2", "b3", "sigma"))
mcmc_hist(m0c_post, pars = c("alpha", "omega_0", "omega_sex", "omega_temperature", "b1a", "b1b", "b2", "b3", "sigma"))

# add to data frame
m0c_post_comparison <- data.frame(type = rep(c("true", "estimated"), 8),
                                  name = c(rep("alpha", 2),
                                           rep("omega_0", 2),
                                           rep("omega_sex", 2),
                                           rep("omega_temp", 2),
                                           rep("b_length_a", 2),
                                           rep("b_length_b", 2),
                                           rep("b_sex", 2),
                                           rep("b_temp", 2)),
                                  mean = c(alpha, m0c_summary$summary[1, 1],
                                           brk_0, m0c_summary$summary[2, 1],
                                           brk_male, m0c_summary$summary[3, 1],
                                           brk_temperature, m0c_summary$summary[4, 1],
                                           b_length_a, m0c_summary$summary[5, 1],
                                           b_length_b, m0c_summary$summary[6, 1],
                                           b_sex[2], m0c_summary$summary[7, 1],
                                           b_temperature, m0c_summary$summary[8, 1]),
                                  sd = c(NA, m0c_summary$summary[1, 3],
                                         NA, m0c_summary$summary[2, 3],
                                         NA, m0c_summary$summary[3, 3],
                                         NA, m0c_summary$summary[4, 3],
                                         NA, m0c_summary$summary[5, 3],
                                         NA, m0c_summary$summary[6, 3],
                                         NA, m0c_summary$summary[7, 3],
                                         NA, m0c_summary$summary[8, 3]))

# plot
ggplot(data = m0c_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

## m0b ==========================================================================

# prepare data
m0b_dat <- list(growth = df$growth_m0b,
                length = df$length,
                sex = df$sex,
                tag_year = df$tag_year,
                recap_year = df$recap_year,
                temperature = df$temperature,
                N = length(df$growth_m0b),
                R = length(unique(df$recap_year)),
                `T` = length(unique(df$tag_year)))

# fit stan model
fit_m0b <- rstan::stan(file = here::here("stan_models", "m0b.stan"),
                       model_name = "m0b",
                       data = m0b_dat,
                       chains = 3,
                       iter = 2000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m0b, file = here::here("results", "fits", "test", "m0b_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m0b)
m0b_summary <- rstan::summary(fit_m0b, pars = c("alpha", "omega_0", "omega_recap", "b1a", "b1b", "b2", "b3", "sigma"))
print(m0b_summary)

# loo
loo::loo(fit_m0b)

# waic
log_lik_m0b <- loo::extract_log_lik(fit_m0b)
loo::waic(log_lik_m0b)

# plots
m0b_post <- as.array(fit_m0b)
mcmc_trace(m0b_post, pars = c("alpha", "omega_0", "omega_recap[1]", "b1a", "b1b", "b2", "b3", "sigma"))
mcmc_hist(m0b_post, pars = c("alpha", "omega_0", "omega_recap[1]", "b1a", "b1b", "b2", "b3", "sigma"))

# add to data frame
m0b_post_comparison <- data.frame(type = rep(c("true", "estimated"), 22),
                                  name = c(rep("alpha", 2),
                                           rep("omega_0", 2),
                                           rep("omega_recap[1]", 2),
                                           rep("omega_recap[2]", 2),
                                           rep("omega_recap[3]", 2),
                                           rep("omega_recap[4]", 2),
                                           rep("omega_recap[5]", 2),
                                           rep("omega_recap[6]", 2),
                                           rep("omega_recap[7]", 2),
                                           rep("omega_recap[8]", 2),
                                           rep("omega_recap[9]", 2),
                                           rep("omega_recap[10]", 2),
                                           rep("omega_recap[11]", 2),
                                           rep("omega_recap[12]", 2),
                                           rep("omega_recap[13]", 2),
                                           rep("omega_recap[14]", 2),
                                           rep("omega_recap[15]", 2),
                                           rep("omega_recap[16]", 2),
                                           rep("b_length_a", 2),
                                           rep("b_length_b", 2),
                                           rep("b_sex", 2),
                                           rep("b_temperature", 2)),
                                  mean = c(alpha, m0b_summary$summary[1, 1],
                                           brk_0, m0b_summary$summary[2, 1],
                                           brks[1, 1], m0b_summary$summary[3, 1],
                                           brks[1, 2], m0b_summary$summary[4, 1],
                                           brks[1, 3], m0b_summary$summary[5, 1],
                                           brks[1, 4], m0b_summary$summary[6, 1],
                                           brks[1, 5], m0b_summary$summary[7, 1],
                                           brks[1, 6], m0b_summary$summary[8, 1],
                                           brks[1, 7], m0b_summary$summary[9, 1],
                                           brks[1, 8], m0b_summary$summary[10, 1],
                                           brks[1, 9], m0b_summary$summary[11, 1],
                                           brks[1, 10], m0b_summary$summary[12, 1],
                                           brks[1, 11], m0b_summary$summary[13, 1],
                                           brks[1, 12], m0b_summary$summary[14, 1],
                                           brks[1, 13], m0b_summary$summary[15, 1],
                                           brks[1, 14], m0b_summary$summary[16, 1],
                                           brks[1, 15], m0b_summary$summary[17, 1],
                                           brks[1, 16], m0b_summary$summary[18, 1],
                                           b_length_a, m0b_summary$summary[19, 1],
                                           b_length_b, m0b_summary$summary[20, 1],
                                           b_sex[2], m0b_summary$summary[21, 1],
                                           b_temperature, m0b_summary$summary[22, 1]),
                                  sd = c(NA, m0b_summary$summary[1, 3],
                                         NA, m0b_summary$summary[2, 3],
                                         NA, m0b_summary$summary[3, 3],
                                         NA, m0b_summary$summary[4, 3],
                                         NA, m0b_summary$summary[5, 3],
                                         NA, m0b_summary$summary[6, 3],
                                         NA, m0b_summary$summary[7, 3],
                                         NA, m0b_summary$summary[8, 3],
                                         NA, m0b_summary$summary[9, 3],
                                         NA, m0b_summary$summary[10, 3],
                                         NA, m0b_summary$summary[11, 3],
                                         NA, m0b_summary$summary[12, 3],
                                         NA, m0b_summary$summary[13, 3],
                                         NA, m0b_summary$summary[14, 3],
                                         NA, m0b_summary$summary[15, 3],
                                         NA, m0b_summary$summary[16, 3],
                                         NA, m0b_summary$summary[17, 3],
                                         NA, m0b_summary$summary[18, 3],
                                         NA, m0b_summary$summary[19, 3],
                                         NA, m0b_summary$summary[20, 3],
                                         NA, m0b_summary$summary[21, 3],
                                         NA, m0b_summary$summary[22, 3]))

# plot
ggplot(data = m0b_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

## m0a ==========================================================================

# prepare data
m0a_dat <- list(growth = df$growth_m0a,
                length = df$length,
                sex = df$sex,
                tag_year = df$tag_year,
                recap_year = df$recap_year,
                temperature = df$temperature,
                N = length(df$growth_m0a),
                R = length(unique(df$recap_year)),
                `T` = length(unique(df$tag_year)))

# fit stan model
fit_m0a <- rstan::stan(file = here::here("stan_models", "m0a.stan"),
                       model_name = "m0a",
                       data = m0a_dat,
                       chains = 3,
                       iter = 2000,
                       init = 0,
                       cores = 4,
                       seed = 1408)
# save
save(fit_m0a, file = here::here("results", "fits", "test", "m0a_sim_rstan_NUTS.Rdata")) # Update if data changes

# get summary
rstan::summary(fit_m0a)
m0a_summary <- rstan::summary(fit_m0a, pars = c("alpha", "omega_0", "omega_sex", "omega_recap", "b1a", "b1b", "b2", "b3", "sigma"))
print(m0a_summary)

# loo
loo::loo(fit_m0a)

# waic
log_lik_m0a <- loo::extract_log_lik(fit_m0a)
loo::waic(log_lik_m0a)

# plots
m0a_post <- as.array(fit_m0a)
mcmc_trace(m0a_post, pars = c("alpha", "omega_0", "omega_recap[1]", "b1a", "b1b", "b2", "b3", "sigma"))
mcmc_hist(m0a_post, pars = c("alpha", "omega_0", "omega_recap[1]", "b1a", "b1b", "b2", "b3", "sigma"))

# add to data frame
m0a_post_comparison <- data.frame(type = rep(c("true", "estimated"), 23),
                                  name = c(rep("alpha", 2),
                                           rep("omega_0", 2),
                                           rep("omega_sex", 2),
                                           rep("omega_recap[1]", 2),
                                           rep("omega_recap[2]", 2),
                                           rep("omega_recap[3]", 2),
                                           rep("omega_recap[4]", 2),
                                           rep("omega_recap[5]", 2),
                                           rep("omega_recap[6]", 2),
                                           rep("omega_recap[7]", 2),
                                           rep("omega_recap[8]", 2),
                                           rep("omega_recap[9]", 2),
                                           rep("omega_recap[10]", 2),
                                           rep("omega_recap[11]", 2),
                                           rep("omega_recap[12]", 2),
                                           rep("omega_recap[13]", 2),
                                           rep("omega_recap[14]", 2),
                                           rep("omega_recap[15]", 2),
                                           rep("omega_recap[16]", 2),
                                           rep("b_length_a", 2),
                                           rep("b_length_b", 2),
                                           rep("b_sex", 2),
                                           rep("b_temperature", 2)),
                                  mean = c(alpha, m0a_summary$summary[1, 1],
                                           brk_0, m0a_summary$summary[2, 1],
                                           brk_male, m0a_summary$summary[3, 1],
                                           brks[1, 1], m0a_summary$summary[4, 1],
                                           brks[1, 2], m0a_summary$summary[5, 1],
                                           brks[1, 3], m0a_summary$summary[6, 1],
                                           brks[1, 4], m0a_summary$summary[7, 1],
                                           brks[1, 5], m0a_summary$summary[8, 1],
                                           brks[1, 6], m0a_summary$summary[9, 1],
                                           brks[1, 7], m0a_summary$summary[10, 1],
                                           brks[1, 8], m0a_summary$summary[11, 1],
                                           brks[1, 9], m0a_summary$summary[12, 1],
                                           brks[1, 10], m0a_summary$summary[13, 1],
                                           brks[1, 11], m0a_summary$summary[14, 1],
                                           brks[1, 12], m0a_summary$summary[15, 1],
                                           brks[1, 13], m0a_summary$summary[16, 1],
                                           brks[1, 14], m0a_summary$summary[17, 1],
                                           brks[1, 15], m0a_summary$summary[18, 1],
                                           brks[1, 16], m0a_summary$summary[19, 1],
                                           b_length_a, m0a_summary$summary[20, 1],
                                           b_length_b, m0a_summary$summary[21, 1],
                                           b_sex[2], m0a_summary$summary[22, 1],
                                           b_temperature, m0a_summary$summary[23, 1]),
                                  sd = c(NA, m0a_summary$summary[1, 3],
                                         NA, m0a_summary$summary[2, 3],
                                         NA, m0a_summary$summary[3, 3],
                                         NA, m0a_summary$summary[4, 3],
                                         NA, m0a_summary$summary[5, 3],
                                         NA, m0a_summary$summary[6, 3],
                                         NA, m0a_summary$summary[7, 3],
                                         NA, m0a_summary$summary[8, 3],
                                         NA, m0a_summary$summary[9, 3],
                                         NA, m0a_summary$summary[10, 3],
                                         NA, m0a_summary$summary[11, 3],
                                         NA, m0a_summary$summary[12, 3],
                                         NA, m0a_summary$summary[13, 3],
                                         NA, m0a_summary$summary[14, 3],
                                         NA, m0a_summary$summary[15, 3],
                                         NA, m0a_summary$summary[16, 3],
                                         NA, m0a_summary$summary[17, 3],
                                         NA, m0a_summary$summary[18, 3],
                                         NA, m0a_summary$summary[19, 3],
                                         NA, m0a_summary$summary[20, 3],
                                         NA, m0a_summary$summary[21, 3],
                                         NA, m0a_summary$summary[22, 3],
                                         NA, m0a_summary$summary[23, 3]))

# plot
ggplot(data = m0a_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

