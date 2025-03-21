#### Data exploration - age approach ####

library(here)
library(data.table)
library(ggplot2)
library(magrittr)

# Run age prep
source("C:/Users/sa20/OneDrive - CEFAS/Projects/southern_ocean/r-projects/master-data-wrangling/code/data-prep-483-TOP-age.R")

# Data preparation -------------------------------------------------------------

# remove those where sex == NA
analysis_dat <- TOP_all_age_dat[!is.na(Sex)]

# remove mature individuals from survey data
analysis_dat <- analysis_dat[!(dataset == "Survey" & maturity_status == "Mature")]

# Summarise --------------------------------------------------------------------

female_year_age_summary <- table(analysis_dat[Sex == "Female"]$Year,
                                 analysis_dat[Sex == "Female"]$Age)
write.csv(female_year_age_summary, here::here("outputs", "female_year_age_data_summary.csv"))

male_year_age_summary <- table(analysis_dat[Sex == "Male"]$Year,
                                 analysis_dat[Sex == "Male"]$Age)
write.csv(male_year_age_summary, here::here("outputs", "male_year_age_data_summary.csv"))

year_age_summary <- table(analysis_dat$Year, analysis_dat$Age)
write.csv(year_age_summary, here::here("outputs", "year_age_data_summary.csv"))

age_summary <- table(analysis_dat$Age)

# Plot -------------------------------------------------------------------------

ggplot(analysis_dat, aes(x = Age, y = Length, col = Sex)) +
  geom_point(alpha = 0.7) +
  facet_wrap(.~ Year + Sex)
