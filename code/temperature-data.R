#### Temp data ####

# load libraries
library(terra)
library(beepr)
library(here)
library(data.table)
library(ggplot2)

# Load data --------------------------------------------------------------------

# load data
source("C:/Users/sa20/OneDrive - CEFAS/Projects/southern_ocean/r-projects/master-data-wrangling/code/data-prep-483-TOP-age.R")

## 1998 - 2021 data ============================================================

# load
dat_1 <- terra::rast(here::here("data", "cmems_mod_glo_phy_my_0.083deg_P1M-m_1742823252769.nc"))
print(dat_1)

# extract
dat_1_res <- dat_1
terra::res(dat_1_res) <- c(0.2, 0.2)
dat_1_res <- terra::resample(dat_1, dat_1_res)
dat_1_temps <- as.data.table(terra::extract(dat_1_res,
                                            TOP_all_age_effort_dat[, .(longitude_set_start, latitude_set_start)]))
colnames(dat_1_temps) <- c('id', as.character(terra::time(dat_1_res)))
dat_1_temps <- melt(dat_1_temps,
                    id.vars = "id",
                    variable.name = "date",
                    value.name = "bottom_temp")

## 2021 - 2023 data ============================================================

# load
dat_2 <- terra::rast(here::here("data", "cmems_mod_glo_phy_myint_0.083deg_P1M-m_1742826010243.nc"))
print(dat_2)

# extract
dat_2_res <- dat_2
terra::res(dat_2_res) <- c(0.2, 0.2)
dat_2_res <- terra::resample(dat_2, dat_2_res)
dat_2_temps <- as.data.table(terra::extract(dat_2_res,
                                            TOP_all_age_effort_dat[, .(longitude_set_start, latitude_set_start)]))
colnames(dat_2_temps) <- c('id', as.character(terra::time(dat_2_res)))
dat_2_temps <- melt(dat_2_temps,
                    id.vars = "id",
                    variable.name = "date",
                    value.name = "bottom_temp")

## 1993 - 1998 data ============================================================

# load
dat_3 <- terra::rast(here::here("data", "cmems_mod_glo_phy_my_0.083deg_P1D-m_1747064766431.nc"))
print(dat_3)

# extract
dat_3_res <- dat_3
terra::res(dat_3_res) <- c(0.2, 0.2)
dat_3_res <- terra::resample(dat_3, dat_3_res)
dat_3_temps <- as.data.table(terra::extract(dat_3_res,
                                            TOP_all_age_effort_dat[, .(longitude_set_start, latitude_set_start)]))
colnames(dat_3_temps) <- c('id', as.character(terra::time(dat_3_res)))
dat_3_temps <- melt(dat_3_temps,
                    id.vars = "id",
                    variable.name = "date",
                    value.name = "bottom_temp")

## combine =====================================================================

loc_temps <- rbind(dat_1_temps, dat_2_temps) %>%
  rbind(dat_3_temps)

loc_temps <- unique(loc_temps)

loc_temps$Date <- as.character(loc_temps$date)
loc_temps$Date <- as.Date(lubridate::fast_strptime(loc_temps$Date, "%Y-%m-%d"))
str(loc_temps)

# Match tag data and bottom temp -----------------------------------------------

# check age spread
ggplot(TOP_all_age_effort_dat, aes(x = Age)) +
  geom_histogram()
min(TOP_all_age_effort_dat$Age)
max(TOP_all_age_effort_dat$Age)

# get location ID
TOP_all_age_effort_dat <- TOP_all_age_effort_dat[, id := 1:.N]

# extract catch date
TOP_all_age_effort_dat[, catch_date := as.Date(format(datetime_set_start, "%Y-%m-%d"))]

# estimate birth date
TOP_all_age_effort_dat[, birth_date := catch_date - lubridate::years(Age)]

# estimate birth date as of July that year
TOP_all_age_effort_dat[, birth_date_july := lubridate::round_date(birth_date,
                                                                  unit = "year")] # nearest year
TOP_all_age_effort_dat[, birth_date_july := paste0(lubridate::year(birth_date_july),
                                                   "-07-01")] # nearest July
TOP_all_age_effort_dat[, birth_date_july := as.Date(birth_date_july)] # convert to date

# filter to those 30 or younger
TOP_all_age_effort_dat <- TOP_all_age_effort_dat[Age <= 30]

# filter to those born after 1993
TOP_all_age_effort_dat <- TOP_all_age_effort_dat[birth_date >= as.Date("1993-01-01")]

# set base temp
base_temp <- 0

# loop to calculate degree days
TOP_all_age_effort_dat[, dd := double()]
TOP_all_age_effort_dat[, dd_avg := double()]
for(i in 1:nrow(TOP_all_age_effort_dat)){
  dat <- TOP_all_age_effort_dat[i, ]
  temps <- loc_temps[id == dat$id]
  temps <- temps[Date >= dat$birth_date &
                   Date <= dat$catch_date]
  temps <- temps[bottom_temp >= base_temp]
  TOP_all_age_effort_dat[i, dd := sum(temps$bottom_temp - base_temp)]
  TOP_all_age_effort_dat[i, dd_avg := mean(temps$bottom_temp - base_temp)]
  print(i)
};beep() # beep when done

# write out data with av_temp
data.table::fwrite(TOP_all_age_effort_dat, here::here("data",
                                                      paste0("age_dat_w_dd_base", base_temp, ".csv")))

# plot -------------------------------------------------------------------------

library(ggplot2)
ggplot(TOP_all_age_effort_dat[!is.na(Sex)], aes(x = dd, y = Length, col = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(TOP_all_age_effort_dat[!is.na(Sex) &
                                dd < 1], aes(x = dd, y = Length, col = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(TOP_all_age_effort_dat[!is.na(Sex)], aes(x = Age, y = dd, col = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(TOP_all_age_effort_dat[!is.na(Sex)], aes(x = dd_avg, y = Length, col = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(TOP_all_age_effort_dat[!is.na(Sex)], aes(x = Age, y = Length, col = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_bw()
