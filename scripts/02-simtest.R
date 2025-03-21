# simulation test script --------------------------------------------------

# libraries
library(here)
library(data.table)
library(magrittr)
library(ggplot2)
library(cmdstanr)
library(brms)

## a script `sourced` by the .qmd to create and test a simulated example.

## Based on https://janhove.github.io/analysis/2018/07/04/bayesian-breakpoint-model
## but using `brms` interface over stan directly.


# info --------------------------------------------------------------------

## use changelog
# snippet changelog

## use snippets for todos
# snippet todo-bug
# snippet todo-check-me
# snippet todo-document-me
# snippet todo-fix-me
# snippet todo-optimise-me
# snippet todo-test-me

# use snippets for code chunks
# snippet saveplot
# snippet loadlatestdata


# change log --------------------------------------------------------------

## changelog


# additional libraries ----------------------------------------------------


# additional setup --------------------------------------------------------


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

## random intercept
alpha_sd <- 1
alpha_iid <- rnorm(n = length(unique(df$tag_year)), mean = 0, sd = alpha_sd) # Effect of each year

## random breakpoints
brk_0 <- -1
brk_sex <- c(0, 0.1) # f / m
brk_temperature <- 0.2
brk_year_sd <- 0.3
brks <- matrix(c(rnorm(n = n_years,
                       mean = brk_0 + brk_sex[1], # brk_0 + brk_sex[1] + brk_temperature * temperature_means,
                       sd = brk_year_sd),
                 rnorm(n = n_years,
                       mean = brk_0 + brk_sex[2], # brk_0 + brk_sex[2] + brk_temperature * temperature_means,
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
df[, "m3c" := alpha + alpha_iid[tag_year] +
     ifelse(length <= brk_0 + brks[sex],
            b_length_a * length,
            b_length_a * (brk_0 + brks[sex]) + b_length_b * (length - (brk_0 + brks[sex])))]

## response - sample from normal distribution
eps <- 0.1
df[, "growth_m5" := rtruncnorm(.N, mean = m5, sd = eps, a = 0)]
df[, "growth_m4" := rtruncnorm(.N, mean = m4, sd = eps, a = 0)]
df[, "growth_m3c" := rtruncnorm(.N, mean = m3c, sd = eps, a = 0)]

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
p_dat <- ggplot(dat, aes(y = growth_raised, x = length_std, colour = sex)) +
  geom_point(alpha = 0.25)
print(p_dat)

## models list
mod_lst <- list(

  # year- & sex-specific change-points in length, main effects of sex & temperature, with tag year RE
  "m0a" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
      (b2 * sex) + (b3 * temperature),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + sex + (1 | recap_year),
    b1a + b1b + b2 + b3 ~ 1,
    nl = TRUE
  ),

  # year-specific change-points in length, main effects of sex & temperature, with tag year RE
  "m0b" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
      (b2 * sex) + (b3 * temperature),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + (1 | recap_year),
    b1a + b1b + b2 + b3 ~ 1,
    nl = TRUE
  ),

  # sex-specific change-points in length, main effects of sex & temperature, with tag year RE
  "m0c" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
      (b2 * sex) + (b3 * temperature),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + sex,
    b1a + b1b + b2 + b3 ~ 1,
    nl = TRUE
  ),

  # year- & sex-specific change-points in length, main effects of sex & trend, with tag year RE
  "m1a" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
      (b2 * sex) + (b4 * trend),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + sex + (1 | recap_year),
    b1a + b1b + b2 + b4 ~ 1,
    nl = TRUE
  ),

  # year-specific change-points in length, main effects of sex & trend, with tag year RE
  "m1b" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
      (b2 * sex) + (b4 * trend),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + (1 | recap_year),
    b1a + b1b + b2 + b4 ~ 1,
    nl = TRUE
  ),

  # sex-specific change-points in length, main effects of sex & trend, with tag year RE
  "m1c" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
      (b2 * sex) + (b4 * trend),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + sex,
    b1a + b1b + b2 + b4 ~ 1,
    nl = TRUE
  ),

  # year- & sex-specific change-points in length, main effect of sex, with tag year RE
  "m2a" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
      (b2 * sex),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + sex + (1 | recap_year),
    b1a + b1b + b2 ~ 1,
    nl = TRUE
  ),

  # year-specific change-points in length, main effect of sex, with tag year RE
  "m2b" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
      (b2 * sex),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + (1 | recap_year),
    b1a + b1b + b2 ~ 1,
    nl = TRUE
  ),

  # sex-specific change-points in length, main effect of sex, with tag year RE
  "m2c" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
      (b2 * sex),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + sex,
    b1a + b1b + b2 ~ 1,
    nl = TRUE
  ),

  # year- & sex-specific change-points in length, with tag year RE
  "m3a" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + sex + (1 | recap_year),
    b1a + b1b ~ 1,
    nl = TRUE
  ),

  # year-specific change-points in length, with tag year RE
  "m3b" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + (1 | recap_year),
    b1a + b1b ~ 1,
    nl = TRUE
  ),

  # sex-specific change-points in length, with tag year RE
  "m3c" = bf(
    growth_m3c ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + sex,
    b1a + b1b ~ 1,
    nl = TRUE
  ),

  # single change-point in length, with tag year RE
  "m4" = bf(
    growth_m4 ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1,
    b1a + b1b ~ 1,
    nl = TRUE
  ),

  # no change-point in length, with tag year RE
  "m5" = bf(
    growth_m5 ~ alpha +
      b1a * length,
    alpha ~ 1 + (1 | tag_year),
    b1a ~ 1,
    nl = TRUE
  )

)

# test using segmented
# library(segmented)
#
# fit_glm <- glm(growth_m4 ~ length, data = df)
# summary(fit_glm)
# fit_seg <- segmented(fit_glm, seg.Z = ~length)
# fit_seg

min_length <- min(df$length)
max_length <- max(df$length)

## all priors
all_pri <- prior(normal(0, 3), nlpar = "alpha", lb = 0) +
  prior(normal(0, 3), nlpar = "b1a") +
  prior(normal(0, 3), nlpar = "b1b") +
  prior(normal(0, 3), nlpar = "b2") +
  prior(normal(0, 3), nlpar = "b3") +
  prior(normal(0, 3), nlpar = "b4") +
  prior(normal(0, 3), nlpar = "omega", lb = -3.36, ub = 4.04)

## priors list
pri_lst <- vector("list", length = length(mod_lst))
names(pri_lst) <- names(mod_lst)
for (i in 1:length(mod_lst)) {
  pri_lst[[i]] <- all_pri[match(names(mod_lst[[i]]$pforms), all_pri$nlpar), ]
}

# fit models
fit_model <- function(v) {
  fit <- brm(formula = mod_lst[[v]], prior = pri_lst[[v]],
             data = df,
             init = 0,
             chains = 3,
             cores = 4,
             iter = 4000,
             thin = 1,
             backend = "rstan")
  save(fit, file = here::here("results", "fits", paste0(names(mod_lst[v]),
                                                        "_brms_test.Rdata"))) # Update if data changes
  return(fit)
}

m5 <- fit_model(14)
m4 <- fit_model(13)
m3c <- fit_model(12)

# Compare fits -----------------------------------------------------------------

## m5 ==========================================================================

# Read in models
load(here::here("results", "fits", "m5_test.Rdata"))
m5 <- fit

# check diagnostics
plot(m5)
summary(m5)

# get posterior summary
m5_post <- posterior_summary(m5)

# add to data frame
m5_post_comparison <- data.frame(type = rep(c("true", "estimated"), 2),
                                 name = c(rep("alpha", 2),
                                          rep("b_length_a", 2),
                                          rep()),
                                 mean = c(alpha, m5_post[1, 1],
                                          b_length_a, m5_post[2, 1]),
                                 sd = c(NA, m5_post[1, 2],
                                        NA, m5_post[2, 2]))

ggplot(data = m5_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

## m4 ==========================================================================

# Read in models
load(here::here("results", "fits", "m4_test.Rdata"))
m4 <- fit

# check diagnostics
plot(m4)
summary(m4)
pairs(m4)

# get posterior summary
m4_post <- posterior_summary(m4)

# add to data frame
m4_post_comparison <- data.frame(type = rep(c("true", "estimated"), 4),
                                 name = c(rep("alpha", 2),
                                          rep("omega", 2),
                                          rep("b_length_a", 2),
                                          rep("b_length_b", 2)),
                                 mean = c(alpha, m4_post[1, 1],
                                          brk_0, m4_post[2, 1],
                                          b_length_a, m4_post[3, 1],
                                          b_length_b, m4_post[4, 1]),
                                 sd = c(NA, m4_post[1, 2],
                                        NA, m4_post[2, 2],
                                        NA, m4_post[3, 2],
                                        NA, m4_post[4, 2]))

ggplot(data = m4_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

## m3c ==========================================================================

# Read in models
load(here::here("results", "fits", "m3c_test.Rdata"))
m3c <- fit

# check diagnostics
plot(m3c)
summary(m3c)
pairs(m3c)

# get posterior summary
m3c_post <- posterior_summary(m3c)

# add to data frame
m3c_post_comparison <- data.frame(type = rep(c("true", "estimated"), 5),
                                 name = c(rep("alpha", 2),
                                          rep("omega_female", 2),
                                          rep("omega_male", 2),
                                          rep("b_length_a", 2),
                                          rep("b_length_b", 2)),
                                 mean = c(alpha, m3c_post[1, 1],
                                          brk_0 + brk_sex[1], m3c_post[2, 1],
                                          brk_sex[2], m3c_post[3, 1],
                                          b_length_a, m3c_post[4, 1],
                                          b_length_b, m3c_post[5, 1]),
                                 sd = c(NA, m3c_post[1, 2],
                                        NA, m3c_post[2, 2],
                                        NA, m3c_post[3, 2],
                                        NA, m3c_post[4, 2],
                                        NA, m3c_post[5, 2]))

ggplot(data = m3c_post_comparison,
       aes(x = name, y = mean, col = type)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")

