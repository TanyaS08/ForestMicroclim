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
library(ggplot2)
library(plyr)
library(dplyr)
library(ggbiplot)
library(factoextra)
library(FactoMineR)
library(lme4)
library(lmerTest)
library(MASS)
library(car)
library(BBmisc)
library(corrplot)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(tidylog)

### 1) PCA Temperature ----
### >> a) Dependencies ----

traits_raw <- read.csv(file.path("data", "data_raw", "PFTC5_Peru_2020_LeafTraits_cleaned_20-03-21.csv"), 
                       header = T, 
                       sep = ",")

---
  title: "Loggers"
author: "Tanya Strydom"
date: "10/30/2019"
output: html_document
---
  
  ```{r libraries, warning=FALSE}
library(dplyr)
library(ggplot2)
library(purrr)
library(tibble)
library(tidyr)
library(corrplot)

dat <- read.csv("logger_trimmed.csv",
                header = T)
```

#Scaling soil RH
```{r scaling RH}

#'scaling factor' per logger/site
sm_max_95 <-
  dat %>%
  #filter(Loc == "GT") %>%
  group_by(Site, Loc, year, dayofyear) %>%
  summarise(max = max(Humidity)) %>% #get daily max vals
  ungroup() %>%
  group_by(Site, Loc) %>%
  summarise(max_sm = max(max), #site max
            sm_max95 = list(enframe(quantile(`max`, probs = 0.95), #95% of site max
                                    name = NULL,
                                    value = "sm_max95"))) %>% 
  unnest(cols = c(sm_max95))

site <- pull(sm_max_95, Site) #vector to index against
loc <- pull(sm_max_95, Loc) #vector to index against
RH_95 <- pull(sm_max_95, sm_max95) #vector to index against
Output <- vector("list",nrow(sm_max_95)) #empty lists

for (i in 1:nrow(sm_max_95)) { #compute scaled RH vals
  
  Output[[i]] <-
    dat %>%
    filter(Loc == loc[i], #per location
           Site == site[i]) %>% #per site
    mutate(.,
           Humid_new = ifelse(Humidity > RH_95[i],
                              100,
                              (Humidity*100)/RH_95[i])) #scaling factor
  
}

Humid_new <- do.call(rbind.data.frame, Output) #flatten list

dat <-
  dat %>%
  mutate(
    Humid_new = Humid_new$Humid_new
  )
```

Followed the scaling method put forward by Asherhoft and Gollan (2013)

#Growing Season  

```{r growing season - length}

#Extract max dailiy vals for air loggers
x <- dat %>%
  filter(Loc == "AT") %>%
  group_by(Site, year, dayofyear) %>%
  summarise(max(Celsius))

#seq of 1/0 for temps >5 & < 35
x$test <- ifelse(x$`max(Celsius)` > 5 & x$`max(Celsius)` < 35 
                 ,1,0)

#can sum all 1's to calc growing season length
grow_season <-
  x %>%
  group_by(Site) %>%
  summarise(gs_length = sum(test))

```

Here we have to ask ourselves if it is okay to just sum all days with the 'correct' max values or if we should look at periods that might 'reset' plant growth/development e.g. long cold periods/excessive heat. Another thing to consider is the fact that our data are split by the winter so should we include values from 2018s summer in this analysis or reset the growing season to after the winter. I think that since we aren't measuring values that are an 'immediate' effect of the current summers growth (except for reproductive output) it should be okay to include data from 2018. It might make sennse that for the reproduction analysis we trim this data to exlude last summer in further temperature calculations.   
Also should we again look at not just a 'singular' max value but the 95%?  
Also this was done using air temperature - should we consider soil moisture in some way?

```{r growing season max/mean}
gs <- x[!(x$test==0),c(1,2,3,4)] #creates template so we can do calcs on growing days

gs_calc <- inner_join(gs,dat,
                      by = c("Site", "year", "dayofyear")) #this produces days that growing can potentially occur

gs_air <-
  gs_calc %>%
  filter(Loc == "AT") %>%
  group_by(Site) %>%
  summarise(gs_max = max(Celsius),
            gs_max95 = list(enframe(quantile(`max(Celsius)`, probs = 0.95),
                                 name = NULL,
                                 value = "gs_max95")),
            gs_mean = mean(Celsius),
            gs_airhumid = mean(Humid_new)) %>% 
  unnest(cols = c(gs_max95))

#soil moisture
gs_sm <- 
  gs_calc %>%
  filter(Loc == "GT") %>%
  group_by(Site) %>%
  summarise(gs_sm_mean = mean(Humid_new))


gs_stats <- 
  grow_season %>%
  full_join(.,gs_air,
            by = "Site") %>%
  full_join(.,gs_sm,
            by = "Site")

```
  
Note to self: although we have Soil Humidity data for site six since we dont have an air logger we cant calculate the growing season so thus we my default loose this site as well for soil measurements.

#Mean Summer Temp  

```{r mean summer temp}
summer <-
  dat %>%
  filter(month > 5 & month < 9) %>%
  group_by(Site) %>%
  summarise(summer_temp_mean = mean(Celsius[Loc == "AT"]), #only for air temp
            summer_sm_mean = mean(Humid_new[Loc == "GT"]) #only for soil RH
            )

```


#Mean annual temp
```{r MAT}
MAT <-
  dat %>%
  filter(Loc == "AT") %>%
  group_by(Site) %>%
  summarise(MAT = mean(Celsius))
  
```


#Temperature seasonality

Defiend as temp 'variation over a year based on $\sigma^2$ of weekly average temp' - since we don't have a 'clean' start on 01/01  end 31/12 dataset I will partition the number of days into weeks from when we started recording to when we stopped 

```{r temp seasonality}

week_var <- # create a 'week' grouping variable
  dat %>%
  filter(Loc == "AT") %>%
  group_by(Site,  dayofyear) %>%
  transmute(mean = mean(Celsius)) %>% #way to gather site by day of year
  distinct() %>% #removes duplicates
  ungroup() %>%
  mutate(week = c(0, rep(1:(nrow(week_var)-1)%/%7)), #reps number for 7 diff days
         mean = NULL)

Temp_var <-
  dat %>%
  filter(Loc == "AT") %>% #remove ground temps
  left_join(., #add the 'week' grouping variable
            week_var,
            by = c("Site",
                   "dayofyear")) %>%
  group_by(Site, week) %>%
  summarise(mean = mean(Celsius)) %>% #weakly mean
  group_by(Site) %>%
  summarise(temp_var = sd(mean)) #annual variance of means

```

#Minimum temp in coldest period
Defined as minimum temp during the coldest week/month. I will calculate the mean monthly temp and then select the monnth that is lowest and from there I can extract the lowest/min temp

```{r min temp}

min_temp <-
  dat %>%
  filter(Loc == "AT") %>% #remove ground loggers
  group_by(Site, month, year) %>%
  summarise(mean = mean(Celsius)) %>% #mean for each month
  group_by(Site) %>% 
  filter(.,
         mean == min(mean)) %>% #find month with min/lowest mean
  left_join(., #use as template to join orig datatset
            dat,
            by = c("Site",
                   "month",
                   "year")) %>%
  group_by(Site) %>%
  summarise(min = min(Celsius), #calculates min (using 95%)
            min95 = list(enframe(quantile(min, probs = 0.95),
                                 name = NULL,
                                 value = "min95"))) %>% 
  unnest(cols = c(min95))

```

#Temp annual range

The temperature variation over the year calculated as the difference between the max temp of the warmest period and the min of the coldest period. Here I will define cold and warm period similar to that for mean temp i.e. as the months with the highest/lowest means and then max - min

```{r annual range}

min_range <-
  dat %>%
  filter(Loc == "AT") %>% #remove ground loggers
  group_by(Site, month, year) %>%
  summarise(mean = mean(Celsius)) %>% #mean for each month
  group_by(Site) %>%
  filter(.,
         mean == max(mean)) %>% #find month with max/lowest mean
  left_join(., #use as template to join orig datatset
            dat,
            by = c("Site",
                   "month",
                   "year")) %>%
  group_by(Site) %>%
  summarise(max = max(Celsius), #calculates max (using 95%)
            max95 = list(enframe(quantile(max, probs = 0.95),
                                 name = NULL,
                                 value = "max95"))) %>% 
  unnest(cols = c(max95)) %>%
  left_join(., #join max and min datasets
            min_temp,
            by = "Site") %>%
  mutate(range = max - min, #calc range
         range95 = max95 - min95)


```

#Diurnal range

This is calculated as the mean weakly temperature ranges over a year. So caluclate the min and max per week and then mean of those

```{r diuranl range}

daily_range <-
  dat %>%
  filter(Loc == "AT")%>% #remove ground temps
  left_join(., #add the 'week' grouping variable
            week_var,
            by = c("Site",
                   "dayofyear")) %>% #remove ground loggers
  group_by(Site, week, year) %>%
  summarise(min = min(Celsius),
            max = max (Celsius),
            range = max - min) %>% #mean for each week
  group_by(Site) %>%
  summarise(daily_range = mean(range))

```

#Concat dataset
```{r comp dataset}

all_data <- 
  MAT %>%
  full_join(.,
            summer,
            by = "Site") %>%
  full_join(.,
            gs_stats,
            by = "Site") %>%
  full_join(.,
            Temp_var,
            by = "Site") %>%
  full_join(.,
            min_range,
            by = "Site") %>%
  full_join(.,
            daily_range,
            by = "Site")


write.csv(all_data,
          "temp_data.csv")

```

# End of script ----