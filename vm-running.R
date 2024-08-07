# vm running script -------------------------------------------------------

## a script to run scripts on a Virtual Machine.


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


# vm running --------------------------------------------------------------

# clear workspace
rm(list = ls())

# document setup
library(here)
library(readxl)

# choose analyses
do__orgdata <- 1
do__explplots <- 0
do__modelfit <- 1
do__modelchk <- 0
do__modelinf <- 0

# r setup -----------------------------------------------------------------
source(here("scripts", "r-setup.R"))

# libraries & functions ---------------------------------------------------
source(here("scripts", "libraries-and-functions.R"))

# analysis options --------------------------------------------------------
source(here("scripts", "analysis-options.R"))

# analysis options --------------------------------------------------------
source(here("scripts", "global-variables.R"))

# if any analyses chosen, then do analysis
anly_choices <- sapply(ls(pattern = "do__"), function(v) eval(parse(text = v)))
do__anly <- ifelse(any(as.logical(anly_choices)), 1, 0)
if (do__anly) {

  # organise data -----------------------------------------------------------
  if (do__orgdata) source(here("scripts", "00-orgdata_v2.R"))

  # explplots script --------------------------------------------------------
  if (do__explplots) source(here("scripts", "01-explplots.R"))

  # modelfit script ---------------------------------------------------------
  if (do__modelfit) source(here("scripts", "02-modelfit_v2.R"))

  # modelchk script ---------------------------------------------------------
  if (do__modelchk) source(here("scripts", "03-modelchk.R"))

  # modelinf script ---------------------------------------------------------
  if (do__modelinf) source(here("scripts", "04-modelinf.R"))

  # close -------------------------------------------------------------------
  source(here("scripts", "close.R"))

}

