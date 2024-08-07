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

length_bin_plus <- 110 # to remove when running analysis script as this provided in analysis-options.R

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

# limit to recaps within 3 years
dat3 <- foo[which(days_at_liberty <= (365 * 3)), ]

# standardise vars
dat3[, "length_std" := scale(length_total_release_cm)]
dat3[, "temperature_std" := scale(av_temp)]

# remove any rows where temperature_std = NA (n = 17); currently includes all of 2021 (n = 16)
dat3 <- dat3[!which(is.na(temperature_std)), ]

# raise growth by raising factor
dat3[, "growth_raised" := specific_growth_rate * 100]

# limit to recaps within 2 years
dat2 <- foo[which(days_at_liberty <= (365 * 2)), ]

# standardise vars
dat2[, "length_std" := scale(length_total_release_cm)]
dat2[, "temperature_std" := scale(av_temp)]

# remove any rows where temperature_std = NA (n = 17); currently includes all of 2021 (n = 16)
dat2 <- dat2[!which(is.na(temperature_std)), ]

# raise growth by raising factor
dat2[, "growth_raised" := specific_growth_rate * 100]

# grubbs test outlier removal indicated subsetting data to ensure no outlier values at the extremes. Results in n = 103 fewer data points in the 3year recaps and n = 97 fewer points in the 2year recaps.
grubbs_dat <- foo[specific_growth_rate < 0.03 & specific_growth_rate > -0.025,]

# limit to recaps within 3 years
grubbs_dat3 <- grubbs_dat[which(days_at_liberty <= (365 * 3)), ]

# standardise vars
grubbs_dat3[, "length_std" := scale(length_total_release_cm)]
grubbs_dat3[, "temperature_std" := scale(av_temp)]

# remove any rows where temperature_std = NA (n = 17); currently includes all of 2021 (n = 16)
grubbs_dat3 <- grubbs_dat3[!which(is.na(temperature_std)), ]

# raise growth by raising factor
grubbs_dat3[, "growth_raised" := specific_growth_rate * 100]

# limit to recaps within 2 years
grubbs_dat2 <- grubbs_dat[which(days_at_liberty <= (365 * 2)), ]

# standardise vars
grubbs_dat2[, "length_std" := scale(length_total_release_cm)]
grubbs_dat2[, "temperature_std" := scale(av_temp)]

# remove any rows where temperature_std = NA (n = 17); currently includes all of 2021 (n = 16)
grubbs_dat2 <- grubbs_dat2[!which(is.na(temperature_std)), ]

# raise growth by raising factor
grubbs_dat2[, "growth_raised" := specific_growth_rate * 100]

# make a list of data
dat_lst <- list(
  ### model data
  "iqr_3yr_data" = dat3, # 3-year recaps, original outlier removal
  "iqr_2yr_data" = dat2, # 2-year recaps, original outlier removal
  "grubbs_3yr_data" = grubbs_dat3, # 3-year, grubbs test outlier removal
  "grubbs_2yr_data" = grubbs_dat2 # 2-year, grubbs test outlier removal
)


# save data ---------------------------------------------------------------

## save list
save_lst <- c(

  ### model data
  "dat_lst"

)

## save image
if (save_it) {
  for(i in 1:length(save_lst)) {
    fle_nm <- "top-maturity-modeldata.RData"
    save(list = save_lst, file = here("data", fle_nm))
  }
}
