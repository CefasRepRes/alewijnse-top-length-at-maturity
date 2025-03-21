### Testing models with segmented package ####

# libraries
library(here)
library(data.table)
library(magrittr)
library(ggplot2)
library(segmented)
library(truncnorm)
library(patchwork)
library(loo)
library(nlme)

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
brks_year <- rnorm(n = n_years, mean = 0, sd = brk_year_sd)

# response variables
min_growth <- min(dat$growth_raised)
eps <- 0.3

# set sex as binary - female default
df[, sex := ifelse(sex == "female", 0, 1)]

# Models -----------------------------------------------------------------------

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
df[, "growth_m4_z" := (growth_m4 - mean(growth_m4)) / sd(growth_m4)]

# plot
p_m4_direct <- ggplot(df, aes(y = m4, x = length, color = sex)) +
  geom_point(alpha = 0.25)
p_m4_sim <- ggplot(df, aes(y = growth_m4, x = length, color = sex)) +
  geom_point(alpha = 0.25)
print(p_m4_direct + p_m4_sim)

# prepare data
m4_dat <- data.frame(growth = df$growth_m4,
                     length = df$length,
                     tag_year = df$tag_year)

# fit regular lm
fit_m4 <- nlme::lme(growth ~ length,
                    random = ~1|tag_year,
                    data = m4_dat)

# fit segmented lme
fit_m4_seg <- segmented.lme(fit_m4,
                            seg.Z = ~ length,
                            random = list(tag_year = pdDiag(~1 + length + U))) # fixed changepoint
plot.segmented.lme(fit_m4_seg, conf.level = 0.95)
fit_m4_seg$lme.fit
slope(fit_m4_seg)
fit_m4_coefs <- as.data.frame(fit_m4_seg$lme.fit$coefficients$fixed)
fit_m4_ran <- as.data.frame(fit_m4_seg$lme.fit$coefficients$random)
fit_m4_slopes <- as.data.frame(slope(fit_m4_seg))
fit_m4_summary <- data.frame(name = c("alpha", "brk_0", "b_length_a", "b_length_b", "eps"),
                             mean = c(fit_m4_coefs[1, 1],
                                      fit_m4_coefs[4, 1],
                                      fit_m4_slopes$Est.,
                                      fit_m4_seg$lme.fit$sigma),
                             sd = NA,
                             type = "estimated")

# add to data frame
m4_true <- data.frame(name = c("alpha", "brk_0", "b_length_a", "b_length_b", "eps"),
                      mean = c(alpha, brk_0, b_length_a, b_length_b, eps),
                      sd = NA,
                      type = "true")
m4_post_comparison <- rbind(m4_true, fit_m4_summary)

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
