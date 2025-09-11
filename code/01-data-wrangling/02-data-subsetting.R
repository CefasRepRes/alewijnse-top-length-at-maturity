### Data subsetting

# libraries
library(data.table)
library(magrittr)

# load config
config <- yaml::yaml.load_file(here::here("configs", "config.yml"))

# load data
dd_dat <- data.table::fread(here::here("data", 'age_dat_w_dm_base_0.csv'))
dd_dat <- dd_dat[Sex %in% c("Male", "Female")]

# subset to 2010 onwards
dd_dat_2010 <- dd_dat[Year >= 2010]

# subset by vessel
unique(dd_dat_2010$Vesselname)
dd_dat_2010 <- dd_dat_2010[Vesselname %in% config$vessels]
unique(dd_dat_2010$Vesselname)

# convert age to days
dd_dat_2010[, Age_days := catch_date - birth_date_july]

# save
write.csv(dd_dat_2010, here::here("data", "age_dat_w_dm_base_0_subset.csv"),
          row.names = FALSE)
