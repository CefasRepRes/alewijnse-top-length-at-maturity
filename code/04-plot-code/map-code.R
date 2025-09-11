#### Map code ####

# libraries
library(data.table)
library(magrittr)
library(ggplot2)
library(sf)
library(here)
library(CCAMLRGIS)
library(cowplot)
library(plyr)

# load data
dat <- data.table::fread(here::here("data", "age_dat_w_dd_base_0_subset.csv"))

# aggregate by lat long
# round
dat[, latitude_set_start := plyr::round_any(latitude_set_start, 0.2)]
dat[, longitude_set_start := plyr::round_any(longitude_set_start, 0.2)]

# aggregate
dat_agg <- dat[, .N, by = c("latitude_set_start", "longitude_set_start")]

# load maps
coast <- load_Coastline()
coast_trans <- st_transform(coast, 4326)
coast <- dplyr::filter(coast, srcvrsn == "Ant. coastline V7.8 and Sub-Ant. coastline V7.3")
asds <- load_ASDs()
asd_483 <- dplyr::filter(asds, GAR_Short_Label == "483")


# plot base map
map <- ggplot() +
  geom_sf(data = coast_trans) +
  geom_point(data = dat_agg, aes(y = latitude_set_start,
                                   x = longitude_set_start,
                                   size = N),
             pch = 21, fill = "#AACCCC", col = "grey40") +
  coord_sf(xlim = c(-44, -33), ylim = c(-52.5, -56)) +
  xlab("") +
  ylab("") +
  theme_bw() +
  # guides(size = guide_legend(title = "N"))
  labs(x = "Longitude", y = "Latitude", size = "Number of\nindividuals")

map

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
  draw_plot(inset, height = 0.295, width = 0.295,
            x = 0.05, y = 0.255)
#full_map

# save
png(here::here("outputs", "plots", "map.png"),
    width = 8, height = 6, units = "in", res = 250)
full_map
dev.off()
