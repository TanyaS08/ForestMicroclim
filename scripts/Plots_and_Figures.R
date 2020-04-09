# Swedish Microclimate Project
# Dalarna, Sweden - July 2019
#
# Authors: Ditte M. Christhansen, Kristoffer Hylander, 
#          Tanya Strydom
# Contact: tanya.strydom@icloud.com

#' ------------------------------------------------------------------#
#'  DATA IMPORTING AND DATAFRAMES
#'  - Import and clean data using Gr3_data_import_checking.R
#'    Can also be found in PFTC5_Gr3 Repo at: 
#'    (https://github.com/TanyaS08/PFTC5_Gr3/blob/master/scripts/Gr3_data_import_checking.R)
#'  - plots are stored as objects in list
#'  - each number in the corresonds to a species
#'        1 = Babilophizoa
#'        2 = Calamagrostis
#'        3 = Linnaea
#'        4 = Hypnum
#'  - check path for destination folder when saving figures -> section 0, b
#' ------------------------------------------------------------------#

#' ------------------------------------------------------------------#
#'   TO DO:
#'  - Work on refining the Field Sites Map
#'       Place inset in main map
#'       Export as a file
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(cartogram)
library(ggplot2)
library(maps)
library(maptools)
library(mapview)
library(grid)
library(ggmap)
library(jtools)
library(gridExtra)
library(grid)

### >> b) Administrative ----
#set save destiantion for figures
#currently: 
setwd("/Users/tanyastrydom/Documents/Uni/Uni/Thesis/ForestMicroclim/figures")

#Generate plank panels when compiling final figures
blank <- grid.rect(gp=gpar(col="white"))

### 1) Map of Field Sites ----
### >> a) Sweden overview ----
#could use as an insert
world <- world
dat <- read.delim("Gen.txt", header = T)
ords <- dat[c(17,3,35,52,55,43,27),]
swe = world %>% 
  filter(name_long == "Sweden")

overplot <- ggplot(swe) +
  geom_sf(aes(geometry = geom)) + 
  geom_polygon(data=ords, aes(x=long, y=lat),
               color="red", fill = "white") +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  geom_point(aes(x = 18.06, y = 59.33), shape = 16,
             color = "black", size = 3)


### >> b) Site map ----
gen <- read.delim("Gen.txt", header = T)

#base map dimensions using co-ords
height <- max(gen$lat) - min(gen$lat)
width <- max(gen$long) - min(gen$long)
sac_borders <- c(bottom  = min(gen$lat)  - 0.1 * height, 
                 top     = max(gen$lat)  + 0.1 * height,
                 left    = min(gen$long) - 0.1 * width,
                 right   = max(gen$long) + 0.4 * width)

map <- get_stamenmap(sac_borders, zoom = 10, maptype = "terrain-background")
pnt <- rep(1,55)

ggmap(map) + 
  geom_point(data = gen, mapping = aes(x = long, y = lat),
             shape = 19, size = 2, colour = "red4") +
  scale_color_distiller(palette = "YlOrRd", direction = 1) +
  theme(legend.position = "none",
        axis.title = element_blank())

### 2) Model outputs ----
### >> a) Barbilophozia ----
### >>>> i) Estimate plot ----

plot.barb.est <-
  plot_summs(mod.PA[[1]],
             colors = "Rainbow",
             coefs = c("Mean annual temperature" = "MAT",
                       "Canopy gap" = "canopy_gap")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"), 
        axis.text.y =element_text(size=14)) +
  theme_bw()

ggsave("Barbilophozia_estimate.png", 
       plot.barb.est,
       width = 11,
       height = 8)

### >>>> ii) Predict plot ----

plot.barb.pred <-
  plot.PA[[1]] +
  facet_wrap(vars(group),
             labeller = as_labeller(c("MAT" = "Mean annual temperature",
                                      "canopy_gap" = "Canopy gap"))) +
  theme(axis.text=element_text(size=14),
        strip.text.x = element_text(size = 12))

ggsave("Barbilophozia_predict.png", 
       plot.barb.pred,
       width = 11,
       height = 8.5)

### >> b) Calamagrostis ----
### >>>> i) Estimate plot ----

plot.cal.est <-
  plot_summs(mod.area[[2]],
             mod.density[[2]],
             mod.flower.noint[[2]],
             colors = "Rainbow",
             coefs = c("Mean annual temperature" = "MAT",
                       "Maximum temperature" = "gs_max95",
                       "Canopy gap" = "canopy_gap",
                       "\u0394 volume" = "vol",
                       "\u0394 volume:Canopy gap" = "canopy_gap:vol"),
             model.names = c("Area",
                             "Density",
                             "Reproductive potential")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"), 
        axis.text.y =element_text(size=14)) +
  theme_bw()

ggsave("Calamagrostis_estimate.png", 
       plot.cal.est,
       width = 13,
       height = 8)

### >>>> ii) Predict plot ----

plot.cal.pred <-
  grid.arrange(
    #prediction for area
    plot.area[[2]] +
      facet_wrap(vars(group),
                 labeller = as_labeller(c("MAT" = "Mean annual temperature",
                                          "gs_max95" = "Maximum temperature",
                                          "canopy_gap" = "Canopy gap",
                                          "vol" = "\u0394 volume",
                                          "canopy_gap:vol" = "\u0394 volume:Canopy gap"))) +
      labs(tag = "A-i") +
      xlab(NULL) +
      theme(axis.text=element_text(size=14),
            strip.text.x = element_text(size = 12)),
    #heat plot for area
    heat.area[[2]] +
      labs(tag = "A-ii") +
      xlab("\u0394 volume") +
      ylab("Canopy gap") +
      geom_vline(aes(xintercept = 0),
                 linetype = 2) +
      geom_segment(aes(
        x = 0.2,
        xend = 2,
        y = 1.67,
        yend = 1.67),
        arrow = arrow()) +
      geom_segment(aes(
        x = -0.2,
        xend = -2,
        y = 1.67,
        yend = 1.67),
        arrow = arrow()) + 
      annotate(geom = "text", x=0.5, y=1.82, 
               label = "Got darker") + 
      annotate(geom = "text", x=-0.5, y=1.82, 
               label = "Got lighter") +
      theme(axis.text=element_text(size=14),
            strip.text.x = element_text(size = 12)),
    #prediction for density
    plot.density[[2]] +
      facet_wrap(vars(group),
                 labeller = as_labeller(c("MAT" = "Mean annual temperature",
                                          "gs_max95" = "Maximum temperature",
                                          "canopy_gap" = "Canopy gap",
                                          "vol" = "\u0394 volume",
                                          "canopy_gap:vol" = "\u0394 volume:Canopy gap"))) +
      labs(tag = "B-i") +
      xlab(NULL) +
      theme(axis.text=element_text(size=14),
            strip.text.x = element_text(size = 12)),
    #heat plot for density
    heat.density[[2]] +
      labs(tag = "B-ii") +
      xlab("\u0394 volume") +
      ylab("Canopy gap") +
      geom_vline(aes(xintercept = 0),
                 linetype = 2) +
      geom_segment(aes(
        x = 0.2,
        xend = 2,
        y = 1.67,
        yend = 1.67),
        arrow = arrow()) +
      geom_segment(aes(
        x = -0.2,
        xend = -2,
        y = 1.67,
        yend = 1.67),
        arrow = arrow()) + 
      annotate(geom = "text", x=0.5, y=1.82, 
               label = "Got darker") + 
      annotate(geom = "text", x=-0.5, y=1.82, 
               label = "Got lighter") +
      theme(axis.text=element_text(size=14),
            strip.text.x = element_text(size = 12)),
    #predition for reproductive output
    plot.flower.noint[[2]] +
      facet_wrap(vars(group),
                 labeller = as_labeller(c("MAT" = "Mean annual temperature",
                                          "gs_max95" = "Maximum temperature",
                                          "canopy_gap" = "Canopy gap",
                                          "vol" = "\u0394 volume",
                                          "canopy_gap:vol" = "\u0394 volume:Canopy gap"))) +
      labs(tag = "C",
           title = "Reproductive potential") +
      xlab(NULL) +
      theme(axis.text=element_text(size=14),
            strip.text.x = element_text(size = 12))
  )

ggsave("Calamagrostis_predict.png", 
       plot.cal.pred,
       width = 11,
       height = 10)

### >> c) Linnaea ----
### >>>> i) Estimate plot ----

plot.lin.est <-
  plot_summs(mod.PA.noint[[3]],
             mod.area[[3]],
             mod.density[[3]],
             mod.flower.noint[[3]],
             colors = "Rainbow",
             coefs = c("Mean annual temperature" = "MAT",
                       "Maximum temperature" = "gs_max95",
                       "Canopy gap" = "canopy_gap",
                       "\u0394 volume" = "vol",
                       "\u0394 volume:Canopy gap" = "canopy_gap:vol"),
             model.names = c("Presence/absence",
                             "Area",
                             "Density",
                             "Reproductive potential")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"), 
        axis.text.y =element_text(size=14)) +
  theme_bw()

ggsave("Linnaea_estimate.png", 
       plot.lin.est,
       width = 13,
       height = 8)

### >>>> ii) Predict plot ----

plot.lin.pred <- 
  grid.arrange(
    #plot prediction for P/A
    plot.PA.noint[[3]] +
      facet_wrap(vars(group),
                 labeller = as_labeller(c("MAT" = "Mean annual temperature",
                                          "gs_max95" = "Maximum temperature",
                                          "canopy_gap" = "Canopy gap",
                                          "vol" = "\u0394 volume",
                                          "canopy_gap:vol" = "\u0394 volume:Canopy gap"))) +
      labs(tag = "A") +
      xlab(NULL) +
      theme(axis.text=element_text(size=14),
            strip.text.x = element_text(size = 12)),
    #blank panel
    blank,
    #predictions for area
    plot.area[[3]] +
      facet_wrap(vars(group),
                 labeller = as_labeller(c("MAT" = "Mean annual temperature",
                                          "gs_max95" = "Maximum temperature",
                                          "canopy_gap" = "Canopy gap",
                                          "vol" = "\u0394 volume",
                                          "canopy_gap:vol" = "\u0394 volume:Canopy gap"))) +
      labs(tag = "B-i") +
      xlab(NULL) +
      theme(axis.text=element_text(size=14),
            strip.text.x = element_text(size = 12)),
    #heat plot for area
    heat.area[[3]] +
      labs(tag = "B-ii") +
      xlab("\u0394 volume") +
      ylab("Canopy gap") +
      geom_vline(aes(xintercept = 0),
                 linetype = 2) +
      geom_segment(aes(
        x = 0.2,
        xend = 2,
        y = 1.67,
        yend = 1.67),
        arrow = arrow()) +
      geom_segment(aes(
        x = -0.2,
        xend = -2,
        y = 1.67,
        yend = 1.67),
        arrow = arrow()) + 
      annotate(geom = "text", x=0.5, y=1.82, 
               label = "Got darker") + 
      annotate(geom = "text", x=-0.5, y=1.82, 
               label = "Got lighter") +
      theme(axis.text=element_text(size=14),
            strip.text.x = element_text(size = 12)),
    #predictions for density
    plot.density[[3]] +
      facet_wrap(vars(group),
                 labeller = as_labeller(c("MAT" = "Mean annual temperature",
                                          "gs_max95" = "Maximum temperature",
                                          "canopy_gap" = "Canopy gap",
                                          "vol" = "\u0394 volume",
                                          "canopy_gap:vol" = "\u0394 volume:Canopy gap"))) +
      labs(tag = "C-i") +
      xlab(NULL) +
      theme(axis.text=element_text(size=14),
            strip.text.x = element_text(size = 10)), 
    #heat plot for density
    heat.density[[3]] +
      labs(tag = "C-ii") +
      xlab("\u0394 volume") +
      ylab("Canopy gap") +
      geom_vline(aes(xintercept = 0),
                 linetype = 2) +
      geom_segment(aes(
        x = 0.2,
        xend = 2,
        y = 1.67,
        yend = 1.67),
        arrow = arrow()) +
      geom_segment(aes(
        x = -0.2,
        xend = -2,
        y = 1.67,
        yend = 1.67),
        arrow = arrow()) + 
      annotate(geom = "text", x=0.5, y=1.82, 
               label = "Got darker") + 
      annotate(geom = "text", x=-0.5, y=1.82, 
               label = "Got lighter") +
      theme(axis.text=element_text(size=14),
            strip.text.x = element_text(size = 12)),
    #predictions for reproductive potential
    plot.flower.noint[[3]] +
      facet_wrap(vars(group),
                 labeller = as_labeller(c("MAT" = "Mean annual temperature",
                                          "gs_max95" = "Maximum temperature",
                                          "canopy_gap" = "Canopy gap",
                                          "vol" = "\u0394 volume",
                                          "canopy_gap:vol" = "\u0394 volume:Canopy gap"))) +
      labs(tag = "D",
           title = "Reproductive potential") +
      xlab(NULL) +
      theme(axis.text=element_text(size=14),
            strip.text.x = element_text(size = 12)),
    ncol = 2
  )

ggsave("Linnaea_predict.png", 
       plot.lin.pred,
       width = 12,
       height = 13)

### >> d) Hypnum ----
### >>>> i) Estimate plots ----

plot.hyp.est <-
  plot_summs(mod.PA.noint[[4]],
             mod.area[[4]],
             colors = "Rainbow",
             coefs = c("Mean annual temperature" = "MAT",
                       "Maximum temperature" = "gs_max95",
                       "Canopy gap" = "canopy_gap",
                       "\u0394 volume" = "vol",
                       "\u0394 volume:Canopy gap" = "canopy_gap:vol",
                       "Number of boulders" = "boulder"),
             model.names = c("Presence/absence",
                             "Area")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"), 
        axis.text.y =element_text(size=14)) +
  theme_bw()

ggsave("Hypnum_estimate.png", 
       plot.hyp.est,
       width = 11,
       height = 8)

### >>>> ii) Prediction Plots ----

plot.hyp.pred <-
  grid.arrange(
    #predict plot P/A
    plot.PA.noint[[4]] +
      facet_wrap(vars(group),
                 labeller = as_labeller(c("MAT" = "Mean annual temperature",
                                          "gs_max95" = "Maximum temperature",
                                          "canopy_gap" = "Canopy gap",
                                          "vol" = "\u0394 volume",
                                          "canopy_gap:vol" = "\u0394 volume:Canopy gap",
                                          "boulder" = "Number of boulders"))) +
      labs(tag = "A") +
      xlab(NULL) +
      theme(axis.text=element_text(size=14),
            strip.text.x = element_text(size = 10)),
    #blank panel
    blank,
    #predict plot area
    plot.area[[4]] +
      facet_wrap(vars(group),
                 labeller = as_labeller(c("MAT" = "Mean annual temperature",
                                          "gs_max95" = "Maximum temperature",
                                          "canopy_gap" = "Canopy gap",
                                          "vol" = "\u0394 volume",
                                          "canopy_gap:vol" = "\u0394 volume:Canopy gap",
                                          "boulder" = "Number of boulders"))) +
      labs(tag = "B-i") +
      xlab(NULL) +
      theme(axis.text=element_text(size=14),
            strip.text.x = element_text(size = 9.5)),
    #heat plot area
    heat.area[[4]] +
      labs(tag = "B-ii") +
      xlab("\u0394 volume") +
      ylab("Canopy gap") +
      geom_vline(aes(xintercept = 0),
                 linetype = 2) +
      geom_segment(aes(
        x = 0.2,
        xend = 2,
        y = 1.67,
        yend = 1.67),
        arrow = arrow()) +
      geom_segment(aes(
        x = -0.2,
        xend = -2,
        y = 1.67,
        yend = 1.67),
        arrow = arrow()) + 
      annotate(geom = "text", x=0.5, y=1.82, 
               label = "Got darker") + 
      annotate(geom = "text", x=-0.5, y=1.82, 
               label = "Got lighter"),
    ncol = 2
  )

ggsave("Hypnum_predict.png", 
       plot.hyp.pred,
       width = 12,
       height = 8)

# End of script ----


