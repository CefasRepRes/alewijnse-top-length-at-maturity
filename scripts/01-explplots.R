# exploratory plots script ------------------------------------------------

## a script `sourced` by the .Rmd to do exploratory plots.

# libraries
library(here)
library(ggplot2)

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


# read in organised data --------------------------------------------------

# ## read in latest processed data
# f_nms <- list.files(path = here("Data"), pattern = __DATANAME__)
# if (length(f_nms) != 0) {
#   f_dts <- as.Date(gsub("[a-zA-Z_.]", "\\1", f_nms), "%d%m%Y")
#   f_fle <- f_nms[order(f_dts, decreasing = TRUE)][1]
#   load(here("data", f_fle))
# }

# load data
load(here::here("data", "top-maturity-modeldata.RData"))

dat <- dat_lst$z_score_3yr_data

# exploratory plots -------------------------------------------------------

## temperature by tag id
temp_plot <- ggplot(dat, aes(x = avg_temp)) +
  geom_histogram(colour = "black", fill = "grey80") +
  ylab("Count") +
  xlab("Average temperature (bottom) during time at liberty") +
  sgg()
if (make_plots) {
  jpeg(here("plots", "temperature-plot.jpg"),
       res = 300, width = (480 * 5), height = (480 * 5))
  print(temp_plot)
  dev.off()
}


## growth by tag year
tag_year_plot <- ggplot(dat, aes(x = factor(season_ccamlr_release),
                                 y = specific_growth_rate)) +
  geom_boxplot(colour = "black", fill = "grey80") +
  ylab("Specific growth rate") +
  xlab("Year of tagging") +
  sgg()
if (make_plots) {
  jpeg(here("plots", "tagyear-growth-plot.jpg"),
       res = 300, width = (480 * 5), height = (480 * 5))
  print(tag_year_plot)
  dev.off()
}


## growth by temperature
temp_growth_plot <- ggplot(dat, aes(x = avg_temp,
                                    y = specific_growth_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Specific growth rate") +
  xlab("Average temperature (bottom) during time at liberty") +
  sgg()
if (make_plots) {
  jpeg(here("plots", "temp-growth-plot.jpg"),
       res = 300, width = (480 * 5), height = (480 * 5))
  print(temp_growth_plot)
  dev.off()
}

## growth by sex
growth_sex_plot <- ggplot(dat, aes(x = sex,
                                   y = avg_temp)) +
  geom_boxplot() +
  ylab("Specific growth rate") +
  xlab("Sex") +
  sgg()
if (make_plots) {
  jpeg(here("plots", "growth-sex-plot.jpg"),
       res = 300, width = (480 * 5), height = (480 * 5))
  print(growth_sex_plot)
  dev.off()
}

## growth by temperature and sex
temp_growth_sex_plot <- ggplot(dat, aes(x = avg_temp,
                                        y = specific_growth_rate,
                                        colour = sex)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Specific growth rate") +
  xlab("Average temperature (bottom) during time at liberty") +
  sgg()
if (make_plots) {
  jpeg(here("plots", "temp-growth-sex-plot.jpg"),
       res = 300, width = (480 * 5), height = (480 * 5))
  print(temp_growth_sex_plot)
  dev.off()
}

## growth by length
length_growth_sex_plot <- ggplot(dat, aes(x = length_total_release_cm,
                                        y = specific_growth_rate,
                                        colour = sex)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Specific growth rate") +
  xlab("Length at release (cm)") +
  sgg()
if (make_plots) {
  jpeg(here("plots", "length-growth-sex-plot.jpg"),
       res = 300, width = (480 * 5), height = (480 * 5))
  print(temp_growth_sex_plot)
  dev.off()
}

## growth by length and year
length_growth_year_plot <- ggplot(dat, aes(x = length_total_release_cm,
                                          y = specific_growth_rate,
                                          colour = as.factor(year))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Specific growth rate") +
  xlab("Length at release (cm)") +
  sgg()
if (make_plots) {
  jpeg(here("plots", "length-growth-year-plot.jpg"),
       res = 300, width = (480 * 5), height = (480 * 5))
  print(length_growth_year_plot)
  dev.off()
}

## bottom temperature by year
temp_year_plot <- ggplot(dat, aes(x = as.factor(year),
                                  y = avg_temp)) +
  geom_boxplot() +
  xlab("Year of recapture") +
  ylab("Average bottom temperature") +
  sgg()
if (make_plots) {
  jpeg(here("plots", "bottom-temp-year-plot.jpg"),
       res = 300, width = (480 * 5), height = (480 * 5))
  print(temp_year_plot)
  dev.off()
}

## length by year
length_recap_plot <- ggplot(dat, aes(x = as.factor(recap_year), y = length_std)) +
  geom_violin(fill = "grey") +
  sgg()
length_tag_plot <- ggplot(dat, aes(x = as.factor(tag_year), y = length_std)) +
  geom_violin(fill = "grey") +
  sgg()
if (make_plots) {
  png(here("plots", "length-year-plot.png"),
       res = 300, width = (480 * 5), height = (480 * 5))
  print(length_tag_plot / length_recap_plot)
  dev.off()
}

# save plots --------------------------------------------------------------

## save list
save_lst <- c(

  ### data
  "dat", # data

  ### plots
  "temp_plot", # temperature-plot
  "tag_year_plot", # tagyear-growth-plot
  "temp_growth_plot", # temp-growth-plot
  "growth_sex_plot", # growth-sex-plot
  "temp_growth_sex_plot" # temp-growth-sex-plot

)

## save image
if (save_it) {
  fle_nm <- paste0("top-maturity-explplots.RData")
  save(list = save_lst, file = here("plots", fle_nm))
}
