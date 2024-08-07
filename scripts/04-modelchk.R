# model checking script ---------------------------------------------------

## a script `sourced` by the .Rmd to check the models.


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


# visual assessment -------------------------------------------------------

## plot traces
mcmc_trace(best_fit)

## other things: posterior predictive checks, random effects, residuals, etc.


# save results ------------------------------------------------------------

# ## save list
# save_lst <- c(
#
#   ### stuff here
#   "stuff", # stuff
#   "mode_stuff" # more stuff
#
# )
#
# ## save image
# if (save_it) {
#   fle_nm <- paste0("top-maturity-modelchk.RData")
#   save(list = save_lst, file = here("results", fle_nm))
# }
