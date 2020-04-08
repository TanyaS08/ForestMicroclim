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
#' ------------------------------------------------------------------#

#' ------------------------------------------------------------------#
#'   TO DO:
#'  - Need to update trait variables (when available) that are selected 
#'    when converting to long format -> ca. l. 50
#'  - TBD: hierarchy when imputing/bootstrapping
#'    for now using: Site > Treatment > PlotID
#'        - changing this requires modifying select and gather when 
#'          making the long dfs
#'  - TBD: number of reps and sample size for bootstrapping
#'  - Rank sites from high to low as opposed to alphabetical
#'    for plotting
#'  - if we do decide to plot outputs maybe set better colour scheme
#'    manually
#'  - might be worth exchanging gather for pivot_longer() as gather()
#'    is a depreciated function  
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

### 1) Map of Field Sites ----
### >> a) Sweden overview ----
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
#could use as an insert
#need to remove the side panels though...

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
### >> a) Sweden overview ----

# End of script ----


