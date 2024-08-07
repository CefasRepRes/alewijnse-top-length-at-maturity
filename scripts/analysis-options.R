# analysis options --------------------------------------------------------

# MCMC settings
setts <- list("nb" = 2000,
              "ni" = 2000,
              "nt" = 1,
              "nc" = 8)
if (testing){
  setts <- lapply(setts, function(v) floor(v / 100) + 1)
}

# length bin plus group
length_bin_plus <- 110

# specific growth rate raising factor
growth_rf <- 100 # 1 for no scaling
## equivalent to calculating: [((ln(length_recap) - ln(length_rel)) / (days_at_liberty / growth_rf)) * 100]

