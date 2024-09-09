# libraries & functions ---------------------------------------------------

# libraries
library(here)
library(ggplot2)
library(extrafont)
library(data.table)
library(FSA) # for lencat
library(brms)
options(mc.cores = 4)
library(bayesplot)
library(patchwork)

## potentially useful packages
# library(mathml) # for producing latex output from expressions - see https://github.com/mgondan/mathml

# functions

# ggplot style
sgg <- function(fs = 16) {
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.x = element_text(size = fs),
        axis.text = element_text(size = fs),
        axis.title = element_text(size = fs),
        legend.title = element_text(size = fs),
        legend.text = element_text(size = fs),
        complete = FALSE)
}

# remove space between panels
squeeze_panels <- theme(panel.spacing = grid::unit(0, "lines"))
