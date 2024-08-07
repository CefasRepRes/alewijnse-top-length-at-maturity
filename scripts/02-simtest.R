# simulation test script --------------------------------------------------

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

## load data
load(here("data", "top-maturity-data.RData"))

## numbers
n_total <- 5490
n_years <- 18
n_per_year <- rep(n_total / n_years, n_years)
p_female <- 0.57
n_female_per_year <- rbinom(n_years, n_per_year, p_female)
n_male_per_year <- n_per_year - n_female_per_year

## fixed and linear coefficients
b_0 <- 0.5
b_length_a <- -0.1
b_length_b <- -0.05
b_sex <- c(0, 0.03) # f / m
b_temperature <- 0.1

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
df[, "tag_year" := (recap_year + 2) - sample(x = 1:2, size = n_total,
                                             prob = c(0.5, 0.5), replace = TRUE)]

## random intercept
b_0_sd <- 1
b_0_iid <- rnorm(n = length(unique(df$tag_year)), mean = 0, sd = b_0_sd)

## random breakpoints
brk_0 <- -2.5
brk_sex <- c(0, 3) # f / m
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
# df[, "eta" := b_0 + b_0_iid[tag_year] +
#      ifelse(length <= brks[sex, recap_year],
#             b_length_a * length,
#             b_length_a * brks[sex, recap_year] +
#               b_length_b * (length - brks[sex, recap_year])) +
#      b_sex[sex],
#    by = row.names(df)]
df[, "eta" := b_0 + b_0_iid[tag_year] +
     ifelse(length <= brks[sex, recap_year],
            b_length_a * length,
            b_length_a * brks[sex, recap_year] +
              b_length_b * (length - brks[sex, recap_year])),
   by = row.names(df)]

## check
if (do_chks) {
  foo <- sample(1:nrow(df), size = 10)
  for(i in foo) {
    t1 <- df[i, ]
    # t2 <- with(df[i, ], b_0 + b_0_iid[tag_year] +
    #              ifelse(length <= brks[sex, recap_year],
    #                     b_length_a * length,
    #                     b_length_a * brks[sex, recap_year] + b_length_b * (length - brks[sex, recap_year])) +
    #              b_sex[sex])
    t2 <- with(df[i, ], b_0 + b_0_iid[tag_year] +
                 ifelse(length <= brks[sex, recap_year],
                        b_length_a * length,
                        b_length_a * brks[sex, recap_year] + b_length_b * (length - brks[sex, recap_year])))
    print(t1)
    print(t2)
  }
}

## response
eps <- 0.5
df[, "growth" := rnorm(.N, mean = eta, sd = eps)]

## plot it
p <- ggplot(df, aes(y = growth, x = length, color = sex)) +
  geom_point() +
  sgg()
print(p)

message("consider C:\\Users\\SG14\\Projects_Working\\c8414b-tippingpoints\\working\\breakpoint-stan-good-example.R") # step(omega - length) >> inv_logit((omega - length) * 5)

## eg model
# eg_mod <- bf(
#   growth ~ alpha +
#     (b1a * length * inv_logit((omega - length) * 5)) + # step(omega - length)) +
#     ((b1a * omega + b1b * (length - omega)) * inv_logit((length - omega) * 5)) + # step(length - omega)) +
#     (b2 * sex),
#   alpha ~ 1 + (1 | tag_year),
#   omega ~ 1 + sex + temperature + (1 | recap_year),
#   b1a ~ 1, b1b ~ 1, b2 ~ 1,
#   nl = TRUE
# )
eg_mod <- bf(
  growth ~ alpha +
    (b1a * length * inv_logit((omega - length) * 5)) + # step(omega - length)) +
    ((b1a * omega + b1b * (length - omega)) * inv_logit((length - omega) * 5)), # step(length - omega)) +
  alpha ~ 1 + (1 | tag_year),
  omega ~ 1 + sex + temperature + (1 | recap_year),
  b1a ~ 1, b1b ~ 1,
  nl = TRUE
)

## eg prior
# eg_pri <- prior(normal(0, 3), nlpar = "alpha") +
#   prior(normal(0, 3), nlpar = "b1a") +
#   prior(normal(0, 3), nlpar = "b1b") +
#   prior(normal(0, 3), nlpar = "b2") +
#   prior(normal(0, 3), nlpar = "omega", lb = -3.45, ub = 7)
eg_pri <- prior(normal(0, 3), nlpar = "alpha") +
  prior(normal(0, 3), nlpar = "b1a") +
  prior(normal(0, 3), nlpar = "b1b") +
  prior(normal(0, 3), nlpar = "omega", lb = -3.45, ub = 7)

## eg fit
eg_fit <- brm(eg_mod,
              data = df,
              prior = eg_pri)

## eg plot
plot(conditional_effects(eg_fit), points = TRUE)
aa <- data.table(year = 1:nrow(ranef(eg_fit)$year[, , 1]),
                 ranef(eg_fit)$year[, , 1])
ggplot(aa, aes(x = year, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = `Q2.5`, ymax = `Q97.5`), width = 0.25)

## print summary
print(summary(eg_fit))


# create data -------------------------------------------------------------


# section -----------------------------------------------------------------


# save results ------------------------------------------------------------

# ## save list
# save_lst <- c(
#
#   ### model results
#   "res" # res
#
# )
#
# ## save image
# if (save_it) {
#   fle_nm <- paste0(prj_nm, "-results.RData")
#   save(list = save_lst, file = here("results", fle_nm))
# }
