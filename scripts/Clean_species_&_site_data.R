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
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
library(tidyverse)
library(tidylog)
library(readxl)

### 1) Environmental data ----

env.raw <- read_excel(file.path("data_raw", "Env.xlsx"))

#this df is needed to get total areas for Calamgrostis and Linnaea
site.raw <- read_excel(file.path("data_raw", "Dataset.xlsx"),
                       sheet = "General") %>%
  mutate(Site = as.factor(Site))

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
  
### >> b) Calamagrostis ----
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
    #Number of subpopulations extracted from the tally as all values are the same for the site can calcualte the mean
    Number_subpop = mean(n),
    Number_reproduce = sum(Flower),
    Number_cells = sum(Cells)) %>%
  #add total area data
  left_join(.,
            site.raw %>%
              #select only site number and area for Calamagrostis - also rename now
              transmute(.,
                        Site = Site,
                        Area = Calamagrostis),
            by = "Site")

#Write file
write.csv(calamagrostis.clean,
          file.path("data_processed", "calamagrostis.csv"))

### >> c) Linnaea ----
linnaea.raw <- read_excel(file.path("data_raw", "Dataset.xlsx"),
                                sheet = "Linnea")

linnaea.clean <-
  linnaea.raw %>%
  #re-class variables
  mutate(
    Site = as.factor(Site)) %>%
  group_by(Site) %>% 
  #this creates a variable that counts the number of subpopulations
  add_tally() %>%
  #sum variables to the site level
  summarise(
    #Number of subpopulations extracted from the tally as all values are the same for the site can
    #calcualte the mean
    Number_subpop = mean(n),
    Number_reproduce = sum(flower),
    Number_cells = sum(cell)) %>%
  #add total area data
  left_join(.,
            site.raw %>%
              #select only site number and area for Linnaea - also rename now
              transmute(.,
                        Site = Site,
                        Area = Linnea),
            by = "Site")
  

#Write file
write.csv(linnaea.clean,
          file.path("data_processed", "linnaea.csv"))

### >> d) Hypnum ----
hypnum.raw <- read_excel(file.path("data_raw", "Dataset.xlsx"),
                          sheet = "Hypnum")

hypnum.clean <-
  hypnum.raw %>%
  #re-class variables
  mutate(
    Site = as.factor(Site),
    Area = as.numeric(Area)) %>%
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
    Number_reproduce = sum(Spore),
    Area = sum(Area))

#Write file
write.csv(hypnum.clean,
          file.path("data_processed", "hypnum.csv"))

### End of script ----