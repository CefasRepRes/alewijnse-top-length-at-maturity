# data organisation script ------------------------------------------------

## a script `sourced` by the .Rmd to organise the data for the analysis.

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


# read raw data -----------------------------------------------------------

## read in raw data
# use data.table::fread for csv
# use readxl::read_xlsx for xslx
# use odbc::dbConnect in "Connections" tab for accdb
# use odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/SG14/.../test.mdb") for mdb / accdb / etc

# ## read saved data
# jagdat <- readRDS("jagdat.rds")
#
# ## length standardisation values
# sdl <- 10.07784
# mul <- 83.28436
#
# ## mcmc settings
# setts <- data.frame("nc" = 3, "na" = 5000, "nb" = 5000, "ni" = 10000, "nt" = 100)
#
# ## transform growth to growth per 100 days to rescale variance
# jagdat$growth <- (jagdat$growth * 100)
#
# ## expected change points (on the length scale) based on data
# exp_cp_male <- 0.6663763 # approx 90 cm
# exp_cp_female <- -1.318175 # approx 70cm

# message
message("this should really start with RAW data")

# read in tag dataset with temp
tags <- fread(here("working", "reanalysis-20230713", "tags_with_av_temp.csv"))

# fix sex
tags[, sex := factor(ifelse(sex_code_recapture == "F", "Female", "Male"))]

# make a year for trend
tags[, year := as.integer(factor(season_ccamlr_recapture.x))]

# make a year of recapture and year of tagging/release;
## use recapture year for changepoint because:
##  (1) maturity staging is done at recapture only
##  (2) recapture year follows growth rather than preceding it
## then use year of tagging/release as RE to mop up noise
tags[, recap_year := as.integer(factor(season_ccamlr_recapture.x))]
tags[, tag_year := as.integer(factor(season_ccamlr_release))]

# try removing outliers

# create 10cm length bins, including a plus group
tags[, length_bin := lencat(length_total_release_cm, w = 10)]
tags[length_bin %in% seq(110, 400, 10), length_bin := length_bin_plus]
tags[, length_bin := factor(length_bin,
                            levels = sort(unique(length_bin)),
                            labels = gsub(as.character(length_bin_plus),
                                          paste0(length_bin_plus, "+"),
                                          sort(unique(length_bin))))]

# calculate binned data statistics
growth_sum <- tags[, .("n" = .N,
                       "mean_growth" = mean(specific_growth_rate, na.rm = TRUE),
                       "sd_growth" = sd(specific_growth_rate, na.rm = TRUE)),
                   by = .(length_bin)]
growth_sum[, c("sd_1", "sd_2") := .(mean_growth - sd_growth,
                                    mean_growth + 2 * sd_growth)]

# merge in binned data statistics
tag_growth <- merge(tags, growth_sum[, .(length_bin, sd_1, sd_2)], by = "length_bin")

# keep only rows for which growth is < less than mean + 2*sd and more than mean + sd
foo <- tag_growth[which(specific_growth_rate < sd_2 & specific_growth_rate > sd_1), ]

# limit to recaps within n_yr_liberty
dat <- foo[which(days_at_liberty <= (365 * n_yr_liberty)), ]

# standardise vars
dat[, "length_std" := scale(length_total_release_cm)]
dat[, "temperature_std" := scale(av_temp)]

# remove any rows where temperature_std = NA (n = 17); currently includes all of 2021 (n = 16)
dat <- dat[!which(is.na(temperature_std)), ]

# raise growth by raising factor
dat[, "growth_raised" := specific_growth_rate * growth_rf]


# save data ---------------------------------------------------------------

## save list
save_lst <- c(

  ### model data
  "dat" # dat

)

## save image
if (save_it) {
  fle_nm <- paste0("top-maturity-data.RData")
  save(list = save_lst, file = here("data", fle_nm))
}
