#### Map code ####

# libraries
library(data.table)
library(magrittr)
library(ggplot2)
library(sf)
library(here)
library(CCAMLRGIS)
library(cowplot)

# load data
dat <- data.table::fread(here::here("data", "age_dat_w_dd_base_0_subset.csv"))

# aggregate by lat long
# round
dat[, latitude_set_start := round(latitude_set_start, 2)]
dat[, longitude_set_start := round(longitude_set_start, 2)]

# aggregate
dat_agg <- dat[, .N, by = c("latitude_set_start", "longitude_set_start")]

# load maps
bathy <- read_sf("V:/FCOSO/Working_Area/1. Research Projects/Bycatch/5-day Bycatch_monitoring/GIS/shapefiles/48_bathy_line.shp")
unique(bathy$CONTOUR)
bathy <- dplyr::filter(bathy, CONTOUR %in% c(-100,
                                             -200,
                                             -400,
                                             -2700))
sg <- read_sf("V:/FCOSO/Working_Area/3. GIS_open/shapefiles/south_georgia.shp")

# plot base map
map <- ggplot() +
  geom_sf(data = sg, fill = 'grey20', col = "grey60") +
  geom_sf(data = bathy, col = "grey75", fill = NA, linewidth = 0.2) +
  geom_point(data = dat_agg, aes(y = latitude_set_start,
                                   x = longitude_set_start,
                                   size = N),
             pch = 21, fill = "#AACCCC", col = "grey40") +
  coord_sf(xlim = c(-44, -33), ylim = c(-52.5, -56)) +
  xlab("") +
  ylab("") +
  theme_bw() +
  guides(size = guide_legend(title = "N"))

map

# load maps
coast <- load_Coastline()
coast <- dplyr::filter(coast, srcvrsn == "Ant. coastline V7.8 and Sub-Ant. coastline V7.3")
asds <- load_ASDs()
asd_483 <- dplyr::filter(asds, GAR_Short_Label == "483")

# plot inset map
inset <- ggplot() +
  geom_sf(data = asd_483, fill = "#AACCCC") +
  geom_sf(data = coast) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_line(color = "grey75"))
inset

# plot full map
full_map <- ggdraw() +
  draw_plot(map) +
  draw_plot(inset, height = 0.3, width = 0.3,
            x = 0.05, y = 0.24)
#full_map

# save
png(here::here("outputs", "plots", "map.png"),
    width = 8, height = 6, units = "in", res = 250)
full_map
dev.off()
