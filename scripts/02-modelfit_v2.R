# model fitting script ----------------------------------------------------

## a script `sourced` by the .Rmd to fir and compare the model from Table 1.


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


# reading -----------------------------------------------------------------

## After difficulties with chain mixing and convergence fitting the toothfish
## growth-maturity data with code adapted from R package `mcp`, which uses JAGS
## to fit a extended (multiple, linear and non-linear, mixed) break-point
## regression, we have decided to try fitting these data using stan (via R
## package `brms`). Here, we compare a fit using JAGS and `brms` to verify that
## they return similar fits, and then fit models representing our testable
## hypotheses ("candidate model set") in `brms`.

## - https://discourse.mc-stan.org/t/brms-for-piecewise-glmm-with-random-change-point/9385/2
## - https://discourse.mc-stan.org/t/piecewise-linear-mixed-models-with-a-random-change-point/5306/12
## - https://janhove.github.io/analysis/2018/07/04/bayesian-breakpoint-model


# additional libraries ----------------------------------------------------


# additional setup --------------------------------------------------------


# read in organised data --------------------------------------------------

# ## read in latest processed data
# f_nms <- list.files(path = here("Data"), pattern = __DATANAME__)
# if (length(f_nms) != 0) {
#   f_dts <- as.Date(gsub("[a-zA-Z_.]", "\\1", f_nms), "%d%m%Y")
#   f_fle <- f_nms[order(f_dts, decreasing = TRUE)][1]
#   load(here("data", f_fle))
# }

# load data
load(here("data", "top-maturity-data.RData"))


# brms data ---------------------------------------------------------------

## make the complete brms data
df <- dat[, .("growth" = growth_raised,
              "length" = length_std,
              "sex" = sex,
              "temperature" = temperature_std,
              "trend" = year,
              "tag_year" = factor(tag_year),
              "recap_year" = factor(recap_year))]


# example fit -------------------------------------------------------------

if (run_egs) {

  ## eg model
  eg_mod <- bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
      (b2 * sex) + (b3 * temperature),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + sex + (1 | recap_year),
    b1a + b1b + b2 + b3 ~ 1,
    nl = TRUE
  )

  ## eg prior
  eg_pri <- prior(normal(0, 3), nlpar = "alpha") +
    prior(normal(0, 3), nlpar = "b1a") +
    prior(normal(0, 3), nlpar = "b1b") +
    prior(normal(0, 3), nlpar = "b2") +
    prior(normal(0, 3), nlpar = "b3") +
    prior(normal(0, 3), nlpar = "omega", lb = -3.45, ub = 7)

  ## alt fit
  eg_mod <- bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
      (b2 * sex),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + sex + temperature + (1 | recap_year),
    b1a ~ 1, b1b ~ 1, b2 ~ 1,
    nl = TRUE
  )

  eg_pri <- prior(normal(0, 3), nlpar = "alpha") +
    prior(normal(0, 3), nlpar = "b1a") +
    prior(normal(0, 3), nlpar = "b1b") +
    prior(normal(0, 3), nlpar = "b2") +
    prior(normal(0, 3), nlpar = "omega", lb = -3.45, ub = 7)

  ## eg fit
  eg_fit <- brm(eg_mod,
                data = df,
                prior = eg_pri,
                warmup = setts$nb,
                iter = Reduce("+", setts[c("nb", "ni")]),
                thin = setts$nt,
                chains = setts$nc,
                backend = "cmdstanr",
                save_pars = save_pars(all = TRUE),
                control = list(adapt_delta = 0.99))

  ## eg plot
  plot(conditional_effects(eg_fit), points = TRUE)
  aa <- data.table(year = 1:nrow(ranef(eg_fit)$year[, , 1]),
                   ranef(eg_fit)$year[, , 1])
  ggplot(aa, aes(x = year, y = Estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = `Q2.5`, ymax = `Q97.5`), width = 0.25)

  ## print summary
  print(summary(eg_fit))

}


# candidate model set -----------------------------------------------------

## model descriptions
tab1 <- read_xlsx(here("model-descriptions.xlsx"))
mod_desc <- as.list(tab1$Code)
names(mod_desc) <- tab1$`Terms included`

## models list
mod_lst <- list(

  # year- & sex-specific change-points in length, main effects of sex & temperature, with tag year RE
  "m0a" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
      (b2 * sex),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + sex + temperature + (1 | recap_year),
    b1a + b1b + b2 ~ 1,
    nl = TRUE
  ),

  # year-specific change-points in length, main effects of sex & temperature, with tag year RE
  "m0b" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
      (b2 * sex),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + temperature + (1 | recap_year),
    b1a + b1b + b2 ~ 1,
    nl = TRUE
  ),

  # sex-specific change-points in length, main effects of sex & temperature, with tag year RE
  "m0c" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
      (b2 * sex),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + sex + temperature,
    b1a + b1b + b2 ~ 1,
    nl = TRUE
  ),

  # year- & sex-specific change-points in length, main effects of sex & trend, with tag year RE
  "m1a" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
      (b2 * sex),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + sex + trend + (1 | recap_year),
    b1a + b1b + b2 ~ 1,
    nl = TRUE
  ),

  # year-specific change-points in length, main effects of sex & trend, with tag year RE
  "m1b" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
      (b2 * sex),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + trend + (1 | recap_year),
    b1a + b1b + b2 ~ 1,
    nl = TRUE
  ),

  # sex-specific change-points in length, main effects of sex & trend, with tag year RE
  "m1c" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)) +
      (b2 * sex),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + sex + trend,
    b1a + b1b + b2 ~ 1,
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
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1 + sex,
    b1a + b1b ~ 1,
    nl = TRUE
  ),

  # single change-point in length, with tag year RE
  "m4" = bf(
    growth ~ alpha +
      (b1a * length * step(omega - length)) +
      ((b1a * omega + b1b * (length - omega)) * step(length - omega)),
    alpha ~ 1 + (1 | tag_year),
    omega ~ 1,
    b1a + b1b ~ 1,
    nl = TRUE
  ),

  # no change-point in length, with tag year RE
  "m5" = bf(
    growth ~ alpha +
      b1a * length,
    alpha ~ 1 + (1 | tag_year),
    b1a ~ 1,
    nl = TRUE
  )

)


# priors ------------------------------------------------------------------

## all priors
all_pri <- prior(normal(0, 3), nlpar = "alpha") +
  prior(normal(0, 3), nlpar = "b1a") +
  prior(normal(0, 3), nlpar = "b1b") +
  prior(normal(0, 3), nlpar = "b2") +
  #prior(normal(0, 3), nlpar = "b3") +
  #prior(normal(0, 3), nlpar = "b4") +
  prior(normal(0, 3), nlpar = "omega", lb = -3.45, ub = 7)

## priors list
pri_lst <- vector("list", length = length(mod_lst))
names(pri_lst) <- names(mod_lst)
for (i in 1:length(mod_lst)) {
  pri_lst[[i]] <- all_pri[match(names(mod_lst[[i]]$pforms), all_pri$nlpar), ]
}


# fit the models ----------------------------------------------------------

## make the fits
fits <- lapply(1:length(mod_lst), function(v) {
  brm(formula = mod_lst[[v]], prior = pri_lst[[v]],
      data = df,
      init = 0,
      chains = 3,
      #backend = "cmdstanr",
      save_pars = save_pars(all = TRUE),
      control = list(adapt_delta = 0.99))
})


# add model selection criteria --------------------------------------------

## loo and waic
for (i in 1:length(fits)) {
  fits[[i]] <- add_criterion(fits[[i]], c("loo", "waic"))
}


# compare models ----------------------------------------------------------

## loo
loos <- lapply(fits, loo)
loo_tab <- loo_compare(loos)

## waic
waics <- lapply(fits, waic)
waic_tab <- loo_compare(waics)


# get best model ----------------------------------------------------------

## "best" model
best_model <- rownames(loo_tab)[1]

## "best" fit
best_fit <- fits[[best_model]]

# save results ------------------------------------------------------------

## save list
save_lst <- c(

  ### data
  "dat", # input data
  "df", # model data

  ### model priors and fits
  "mod_lst", # list of model formulas
  "pri_lst", # list of model priors
  "fits", # list of model fits
  "best_model", # best model in fits
  "best_fit" # best fit

)

## save image
if (save_it) {
  fle_nm <- paste0("top-maturity-modelfit.RData")
  save(list = save_lst, file = here("results", fle_nm))
}
