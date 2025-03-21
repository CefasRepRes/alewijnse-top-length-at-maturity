# data organisation script ------------------------------------------------

## a script `sourced` by the .Rmd to organise the data for the analysis.

# libraries ---------------------------------------------------------------

library(lubridate)
library(ggplot2)
library(data.table)
library(FSA)
library(beepr)

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

# read raw data -----------------------------------------------------------

fp_raw <- "V:/FCOSO_restd_C6797_&_C6798/Data_Storage/CCAMLR_data/2024/705_GBR_2024-12-11/705_GBR_2024-12-11_rds/705_GBR_2024-12-11.Rds"

raw_data <- readRDS(fp_raw)

# convert to list of data tables
raw_dt <- lapply(raw_data, data.table::setDT)

## prepare tagging data using TE's tag matching code ===========================

# tag release
tag_rel <- raw_dt$OBS_HAUL_TAG_RELEASE

# linked releases and recaps
tag_link <- raw_dt$OBS_HAUL_TAG_LINK

# subset linked data to TOP and tags that were released and recaptured within 48.3
tag_link <- tag_link[which(taxon_code_release == 'TOP' &
                             taxon_code_recapture == 'TOP' &
                             asd_code_release == '483' &
                             asd_code_recapture == '483')]

# create col to id fish with 1 or 2 tags at release
tag_link[, prop_tag_rel := ifelse(tag_code_2_release == 'NA|NA|NA|NA', 1, 2)]

# find fish that were released outside of 48.3 using linked data and subset release data to remove these individuals
excl_tags <- tag_link[!which(asd_code_release == '483'), obs_haul_tag_release_id]
tag_rel <- tag_rel[!which(obs_haul_tag_release_id %in% excl_tags), ]

# subset tag releases to TOP
tag_rel <- tag_rel[which(taxon_code == 'TOP'), ]

# add season col and vessel name
tag_rel[, season_ccamlr:= lubridate::year(date_release)]

## identify good matched recaptures using Tim's existing code
# TODO: move to separate function
getAllTagRecaptures <- function(tmp_dt){
  recaptures <- merge(raw_dt$OBS_HAUL, raw_dt$OBS_HAUL_TAG_RECAPTURE, all.y=TRUE, by= "obs_haul_id")
  recaptures$season_ccamlr_recapture <- recaptures$season_ccamlr  # Get rid of NAs from recaps that don't match effort
  recaptures
}

# Get all recaptures of TOP in 483
recs <- getAllTagRecaptures(raw_dt)
recs <- subset(recs, taxon_code == "TOP" & asd_code == "483")

# Merge onto CCAMLR link table
recaptures.all <-  merge(raw_dt$OBS_HAUL_TAG_LINK, recs,
                         all.y = TRUE, by = "obs_haul_tag_recapture_id")
table(table(recaptures.all$obs_haul_tag_recapture_id))  # Duplicate matches
recaptures.all <- subset(recaptures.all, !duplicated(obs_haul_tag_recapture_id))
# Keep just the first match, maybe think of a better system in future

## note no. of duplicates removed

# Find good matches
recaptures.all$matched <- ifelse(recaptures.all$taglink_tagcount == 1,
                                 recaptures.all$taglink_score >= 3,
                                 recaptures.all$taglink_score >= 5)
recaptures.all$matched <- ifelse(is.na(recaptures.all$matched), FALSE, recaptures.all$matched)
table(table(recaptures.all$obs_haul_tag_recapture_id))  #Frequency of matches

# frequency of matches?
table(recaptures.all$matched)

# table of tag count by score
table(recaptures.all$taglink_tagcount, recaptures.all$taglink_score)

# subset to matched individuals
match_tag <- data.table(subset(recaptures.all, matched == 'TRUE'))

# check taxon and asd code
table(match_tag$taxon_code_release)
table(match_tag$asd_code_release)
table(match_tag$asd_code_recapture)
table(match_tag$taxon_code_recapture)

# subset to TOP and 483 releases only (recaptures only TOP and 483)
match_tag <- match_tag[which(taxon_code_release == 'TOP' & asd_code_release == '483'), ]

# remove those post 2020 (we don't have temp data)
match_tag <- match_tag[season_ccamlr <= 2020]

## Data cleaning ===============================================================

# select relevant columns
match_tag_TOP_483 <- match_tag[, .(season_ccamlr_release, length_total_release_cm,
                                   latitude_release, longitude_release,
                                   length_total_recapture_cm, days_at_liberty,
                                   sex_code_recapture, latitude_recapture.x,
                                   longitude_recapture.x, season_ccamlr_recapture.x,
                                   date_release, date_recapture.x)]

# chose those recaptured after no more than 3 years
match_tag_TOP_483 <- match_tag_TOP_483[days_at_liberty < 3*365 &
                                         days_at_liberty > 0]

# remove those with negative growth
match_tag_TOP_483 <- match_tag_TOP_483[length_total_recapture_cm > length_total_release_cm]

# calculate specific growth rate
match_tag_TOP_483[, specific_growth_rate := 100*(log(length_total_recapture_cm) - log(length_total_release_cm))/days_at_liberty]

# remove NAs
match_tag_TOP_483 <- match_tag_TOP_483[!is.na(specific_growth_rate)]

# subset to F/M sexes
match_tag_TOP_483 <- match_tag_TOP_483[sex_code_recapture %in% c('F', 'M')]

# plot growth rates for fish caught < 2 years at liberty
ggplot(match_tag_TOP_483, aes(x = length_total_release_cm, y = specific_growth_rate)) +
  geom_point(pch = 21) +
  facet_wrap(~sex_code_recapture) +
  theme_bw()

# Read in bottom temperature data ----------------------------------------------

# TODO: get from original source

# change resolution
temp_data <- terra::rast(here::here("data", "cmems_bottom_temp.nc"))
temp_data_res <- temp_data
terra::res(temp_data_res) <- c(0.2, 0.2)
temp_data_res <- terra::resample(temp_data, temp_data_res)

# extract data for tag release location
loc_temps <- as.data.table(terra::extract(temp_data_res,
                                          match_tag_TOP_483[, .(longitude_release, latitude_release)]))
colnames(loc_temps) <- c('id', as.character(terra::time(temp_data_res)))

loc_temps_melt <- melt(loc_temps, id.vars = "id", variable.name = "date", value.name = "bottom_temp")
#pp$id <- factor(pp$id)
loc_temps_melt$Date <- as.Date(loc_temps_melt$date)

# Match tag data and bottom temp -----------------------------------------------

match_tag_TOP_483[, id := 1:.N]

# loop
match_tag_TOP_483[, avg_temp := double()]
for(i in 1:nrow(match_tag_TOP_483)){
  tag <- match_tag_TOP_483[i, ]
  temps <- loc_temps_melt[Date >= tag$date_release &
                            Date <= tag$date_recapture.x &
                            id == tag$id]
  av_temp <- mean(temps$bottom_temp)
  match_tag_TOP_483[i, avg_temp := av_temp]
  print(i)
};beep() # beep when done

# write out tags with av_temp
data.table::fwrite(match_tag_TOP_483, here::here("data", 'tags_with_av_temp.csv'))

# Further data organisation ----------------------------------------------------

# read in data
tags <- data.table::fread(here::here("data", "tags_with_av_temp.csv"))

# remove 2020 (small sample size)
tags <- tags[season_ccamlr_release != "2020" &
               season_ccamlr_recapture.x != "2020"]

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

# remove those with no temperature results
tags <- tags[!is.na(avg_temp)]

# raise growth by raising factor
tags[, "growth_raised" := specific_growth_rate * 100]

# SD outlier removal -----------------------------------------------------------

length_bin_plus <- 110 # to remove when running analysis script as this provided in analysis-options.R

# create 10cm length bins, including a plus group
tags[, length_bin := FSA::lencat(length_total_release_cm, w = 10)]
tags[length_bin %in% seq(110, 400, 10), length_bin := length_bin_plus]
tags[, length_bin := factor(length_bin,
                            levels = sort(unique(length_bin)),
                            labels = gsub(as.character(length_bin_plus),
                                          paste0(length_bin_plus, "+"),
                                          sort(unique(length_bin))))]

# subset into 3 and 2 year returns
tags_3_yr <- tags[days_at_liberty <= 365 * 3]
tags_2_yr <- tags[days_at_liberty <= 365 * 2]

## Three year data =============================================================

# calculate binned data statistics
growth_sum_3_yr <- tags_3_yr[, .("n" = .N,
                                 "mean_growth" = mean(specific_growth_rate, na.rm = TRUE),
                                 "sd_growth" = sd(specific_growth_rate, na.rm = TRUE)),
                             by = .(length_bin)]
growth_sum_3_yr[, c("sd_1", "sd_2") := .(mean_growth - sd_growth,
                                         mean_growth + 2 * sd_growth)]

# merge in binned data statistics
tag_growth_3_yr <- merge(tags_3_yr, growth_sum_3_yr[, .(length_bin, sd_1, sd_2)], by = "length_bin")

# keep only rows for which growth is < less than mean + 2*sd and more than mean + sd
tag_growth_3_yr <- tag_growth_3_yr[which(specific_growth_rate < sd_2 & specific_growth_rate > sd_1), ]

# standardise vars
tag_growth_3_yr[, "length_std" := scale(length_total_release_cm)]
tag_growth_3_yr[, "temperature_std" := scale(avg_temp)]

# remove any rows where temperature_std = NA
tag_growth_3_yr <- tag_growth_3_yr[!which(is.na(temperature_std)), ]

## Two-year data ===============================================================

# calculate binned data statistics
growth_sum_2_yr <- tags_2_yr[, .("n" = .N,
                                 "mean_growth" = mean(specific_growth_rate, na.rm = TRUE),
                                 "sd_growth" = sd(specific_growth_rate, na.rm = TRUE)),
                             by = .(length_bin)]
growth_sum_2_yr[, c("sd_1", "sd_2") := .(mean_growth - sd_growth,
                                         mean_growth + 2 * sd_growth)]

# merge in binned data statistics
tag_growth_2_yr <- merge(tags_2_yr, growth_sum_2_yr[, .(length_bin, sd_1, sd_2)], by = "length_bin")

# keep only rows for which growth is < less than mean + 2*sd and more than mean + sd
tag_growth_2_yr <- tag_growth_2_yr[which(specific_growth_rate < sd_2 & specific_growth_rate > sd_1), ]

# standardise vars
tag_growth_2_yr[, "length_std" := scale(length_total_release_cm)]
tag_growth_2_yr[, "temperature_std" := scale(avg_temp)]

# remove any rows where temperature_std = NA
tag_growth_2_yr <- tag_growth_2_yr[!which(is.na(temperature_std)), ]

# Grubbs test ------------------------------------------------------------------

# TODO: find out how to do on each data set

## 3 years =====================================================================

grubbs_dat_3_yr <- tags_3_yr[specific_growth_rate < 0.03 & specific_growth_rate > -0.025]

# standardise vars
grubbs_dat_3_yr[, "length_std" := scale(length_total_release_cm)]
grubbs_dat_3_yr[, "temperature_std" := scale(avg_temp)]

grubbs_dat_3_yr <- grubbs_dat_3_yr[!which(is.na(temperature_std)), ]

## 2 years =====================================================================

# limit to recaps within 3 years
grubbs_dat_2_yr <- tags_2_yr[specific_growth_rate < 0.03 & specific_growth_rate > -0.025]

# standardise vars
grubbs_dat_2_yr[, "length_std" := scale(length_total_release_cm)]
grubbs_dat_2_yr[, "temperature_std" := scale(avg_temp)]

grubbs_dat_2_yr <- grubbs_dat_2_yr[!which(is.na(temperature_std)), ]

# z-score ----------------------------------------------------------------------

## 3 years =====================================================================

# standardise vars
tags_3_yr[, "length_std" := scale(length_total_release_cm)]
tags_3_yr[, "temperature_std" := scale(avg_temp)]
tags_3_yr[, "sgr_std" := scale(specific_growth_rate)]

# subset
z_score_3_yr <- tags_3_yr[length_std > -3 &
                            length_std < 3 &
                            temperature_std > -3 &
                            temperature_std < 3 &
                            sgr_std > -3 &
                            sgr_std < 3]

# standardise vars
tags_2_yr[, "length_std" := scale(length_total_release_cm)]
tags_2_yr[, "temperature_std" := scale(avg_temp)]
tags_2_yr[, "sgr_std" := scale(specific_growth_rate)]

# subset
z_score_2_yr <- tags_2_yr[length_std > -3 &
                            length_std < 3 &
                            temperature_std > -3 &
                            temperature_std < 3 &
                            sgr_std > -3 &
                            sgr_std < 3]

# # Rosner's test ----------------------------------------------------------------
#
# test <- rosnerTest(tags_3_yr[, specific_growth_rate], k = round(nrow(tags_3_yr)/10))
# test <- test$all.stats %>% data.table()
# test <- test[Outlier == TRUE]
#
# rosnerRemoval <- function(data, params){
#   outliers <- vector("list", length(params))
#   for(i in 1:length(params)){
#     param <- params[i]
#     out <- rosnerTest(dplyr::pull(data, param), k = round(nrow(data)/10))
#     out <- out$all.stats %>% data.table()
#     out <- out[Outlier == TRUE]
#     out <- out[, Value]
#     outliers[[i]] <- out
#   }
#   setNames(outliers, paste0(params))
#   for(i in 1:length(params)){
#     data <- filter(data, !(params[i] %in% outliers[[i]]))
#   }
#   return(data)
# }

# make a list of data
dat_lst <- list(
  ### model data
  "iqr_3yr_data" = tag_growth_3_yr, # 3-year recaps, original outlier removal
  "iqr_2yr_data" = tag_growth_2_yr, # 2-year recaps, original outlier removal
  "grubbs_3yr_data" = grubbs_dat_3_yr, # 3-year, grubbs test outlier removal
  "grubbs_2yr_data" = grubbs_dat_2_yr, # 2-year, grubbs test outlier removal
  "z_score_3yr_data" = z_score_3_yr, # 3-year, z-score outlier removal
  "z_score_2yr_data" = z_score_2_yr # 2-year, z-score outlier removal
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
    save(list = save_lst, file = here::here("data", fle_nm))
  }
}
