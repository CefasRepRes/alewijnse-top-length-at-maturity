#### Temp data ####

test <- terra::rast(here::here("data", "cmems_mod_glo_phy_my_0.083deg_P1M-m_1742823252769.nc"))
print(test)

temp_data_res <- test
terra::res(temp_data_res) <- c(0.2, 0.2)
temp_data_res <- terra::resample(test, temp_data_res)

# extract data for tag release location
loc_temps <- as.data.table(terra::extract(temp_data_res,
                                          TOP_all_age_effort_dat[, .(longitude_set_start, latitude_set_start)]))
colnames(loc_temps) <- c('id', as.character(terra::time(temp_data_res)))

loc_temps_melt <- melt(loc_temps, id.vars = "id", variable.name = "date", value.name = "bottom_temp")
#pp$id <- factor(pp$id)
loc_temps_melt$Date <- as.Date(loc_temps_melt$date)

# Match tag data and bottom temp -----------------------------------------------

# get location ID
TOP_all_age_effort_dat[, id := 1:.N]

# extract month and year
TOP_all_age_effort_dat[, month_year := format(datetime_set_start, "%Y-%m")]
loc_temps_melt[, month_year := format(Date, "%Y-%m")]

# loop
TOP_all_age_effort_dat[, temp := double()]
for(i in 1:nrow(TOP_all_age_effort_dat)){
  tag <- TOP_all_age_effort_dat[i, ]
  temps <- loc_temps_melt[month_year == tag$month_year &
                            id == tag$id]
  TOP_all_age_effort_dat[i, temp := temps$bottom_temp]
  print(i)
};beep() # beep when done

# write out tags with av_temp
data.table::fwrite(TOP_all_age_effort_dat, here::here("data", 'age_dat_w_temp.csv'))
