# Swedish Microclimate Project
# Dalarna, Sweden - July 2019
#
# Authors: Ditte M. Christhansen, Kristoffer Hylander, 
#          Tanya Strydom
# Contact: tanya.strydom@icloud.com

#' ------------------------------------------------------------------#
#'  - This script is used to concatinate and clean the raw species
#'    and site data collected in the field
#' ------------------------------------------------------------------#

#' ------------------------------------------------------------------#
#'   TO DO:
#'  - #add total area for Linnea and Calamagrostis
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
library(tidyverse)
library(tidylog)
library(readxl)

### 1) Environmental data ----

env.raw <- read_excel(file.path("data_raw", "Env.xlsx"))

### >> a) Environmental data ----


### 2) Species data ----
### >> a) Barbilophozia ----
barbilophozia.raw <- read_excel(file.path("data_raw", "Dataset.xlsx"),
                                sheet = "Barbilophozia")

barbilophozia.clean <-
  barbilophozia.raw %>%
  #re-class variables
  transmute(
    Site = as.factor(Site),
    Subpop = as.factor(Subpop),
    Area = as.numeric(Area)) %>%
  group_by(Site) %>% 
  #this creates a variable that counts the number of subpopulations
  add_tally() %>%
  #sum variables to the site level
  summarise(
    #total area
    Area = sum(Area),
    #Number of subpopulations extracted from the tally as all values are the same for the site can
    #calcualte the mean
    Number_subpop = mean(n))

#Write file
write.csv(barbilophozia.clean,
          file.path("data_processed", "barbilophozia.csv"))
  
### >> a) Calamagrostis ----
calamagrostis.raw <- read_excel(file.path("data_raw", "Dataset.xlsx"),
                                sheet = "Calamagrostis")

calamagrostis.clean <-
  calamagrostis.raw %>%
  #re-class variables
  mutate(
    Site = as.factor(Site)) %>%
  group_by(Site) %>% 
  #this creates a variable that counts the number of subpopulations
  add_tally() %>%
  #sum variables to the site level
  summarise(
    #total area
    ##TODO
    #Number of subpopulations extracted from the tally as all values are the same for the site can
    #calcualte the mean
    Number_subpop = mean(n),
    Number_flower = sum(Flower),
    Number_cells = sum(Cells))


### End of script ----