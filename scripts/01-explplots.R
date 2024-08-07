# exploratory plots script ------------------------------------------------

## a script `sourced` by the .Rmd to do exploratory plots.

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
load(here("data", "top-maturity-data.RData"))


# exploratory plots -------------------------------------------------------

## temperature by tag id
temp_plot <- ggplot(dat, aes(x = av_temp)) +
  geom_histogram(colour = "black", fill = "grey80") +
  ylab("Count") +
  xlab("Average temperature (bottom) during time at liberty") +
  sgg()
if (make_plots) {
  cairo_pdf(here("plots", "temperature-plot.pdf"),
            width = 7, height = 7)
  print(temp_plot)
  dev.off()
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
  cairo_pdf(here("plots", "tagyear-growth-plot.pdf"),
            width = 7, height = 7)
  print(tag_year_plot)
  dev.off()
  jpeg(here("plots", "tagyear-growth-plot.jpg"),
       res = 300, width = (480 * 5), height = (480 * 5))
  print(tag_year_plot)
  dev.off()
}


## growth by temperature
temp_growth_plot <- ggplot(dat, aes(x = av_temp,
                                    y = specific_growth_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Specific growth rate") +
  xlab("Average temperature (bottom) during time at liberty") +
  sgg()
if (make_plots) {
  cairo_pdf(here("plots", "temp-growth-plot.pdf"),
            width = 7, height = 7)
  print(temp_growth_plot)
  dev.off()
  jpeg(here("plots", "temp-growth-plot.jpg"),
       res = 300, width = (480 * 5), height = (480 * 5))
  print(temp_growth_plot)
  dev.off()
}

## growth by sex
growth_sex_plot <- ggplot(dat, aes(x = sex,
                                   y = av_temp)) +
  geom_boxplot() +
  ylab("Specific growth rate") +
  xlab("Average temperature (bottom) during time at liberty") +
  sgg()
if (make_plots) {
  cairo_pdf(here("plots", "growth-sex-plot.pdf"),
            width = 7, height = 7)
  print(growth_sex_plot)
  dev.off()
  jpeg(here("plots", "growth-sex-plot.jpg"),
       res = 300, width = (480 * 5), height = (480 * 5))
  print(growth_sex_plot)
  dev.off()
}

## growth by temperature and sex
temp_growth_sex_plot <- ggplot(dat, aes(x = av_temp,
                                        y = specific_growth_rate,
                                        colour = sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Specific growth rate") +
  xlab("Average temperature (bottom) during time at liberty") +
  sgg()
if (make_plots) {
  cairo_pdf(here("plots", "temp-growth-sex-plot.pdf"),
            width = 7, height = 7)
  print(temp_growth_sex_plot)
  dev.off()
  jpeg(here("plots", "temp-growth-sex-plot.jpg"),
       res = 300, width = (480 * 5), height = (480 * 5))
  print(temp_growth_sex_plot)
  dev.off()
}


message("do more plots!!!!!!!!!!!!!!!!!!!!!!")


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
