#### Temp data ####

# load libraries
library(terra)
library(beepr)
library(here)
library(data.table)

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

## combine =====================================================================

loc_temps <- rbind(dat_1_temps, dat_2_temps)

loc_temps$Date <- as.Date(loc_temps$date)

# Match tag data and bottom temp -----------------------------------------------

# get location ID
TOP_all_age_effort_dat[, id := 1:.N]

# extract month and year
TOP_all_age_effort_dat[, month_year := format(datetime_set_start, "%Y-%m")]
loc_temps[, month_year := format(Date, "%Y-%m")]

# loop
TOP_all_age_effort_dat[, temp := double()]
for(i in 1:nrow(TOP_all_age_effort_dat)){
  dat <- TOP_all_age_effort_dat[i, ]
  temps <- loc_temps[month_year == dat$month_year &
                            id == dat$id]
  TOP_all_age_effort_dat[i, temp := temps$bottom_temp]
  print(i)
};beep() # beep when done

# write out tags with av_temp
data.table::fwrite(TOP_all_age_effort_dat, here::here("data", 'age_dat_w_temp.csv'))
