# model inference script --------------------------------------------------

## a script `sourced` by the .Rmd to do inference from the best model.


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


# load best model and data ------------------------------------------------

## load model and data
load(here("results", "top-maturity-modelfit.RData"))


# best model inference ----------------------------------------------------

## print summary
summary(best_fit)

## plot traces
mcmc_trace(best_fit)

## plot conditional effects
plot(conditional_effects(best_fit), points = TRUE)

## other things: coefficients, random effects, residuals, etc.

mm <- data.frame(ranef(best_fit, groups = "recap_year")[[1]])
colnames(mm) <- gsub("\\.omega_Intercept", "", colnames(mm))
mm$recap_year <- sort(unique(dat$season_ccamlr_recapture.x))

ggplot(mm, aes(x = recap_year)) +
  geom_point(aes(y = Estimate)) +
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5))


# save results ------------------------------------------------------------

## save out results
# res_nm <- __RESULTSNAME__
# f_nm <- paste0(res_nm, "_", tday, ".RData"))
# save(__RESULTS__,
#      file = here("results", f_nm)
