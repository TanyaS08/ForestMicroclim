# Swedish Microclimate Project
# Dalarna, Sweden - July 2019
#
# Authors: Ditte M. Christhansen, Kristoffer Hylander, 
#          Tanya Strydom
# Contact: tanya.strydom@icloud.com

#' ------------------------------------------------------------------#
#'  - This script summarises the raw microclimate data and creates a
#'    new .csv file called temp_data.csv and can be found in the repo at:
#'    ()
#'  - Calculating of microclimate variables follows the definitions 
#'    outlined in Gardener et al. (2019) 
#'  - check path for destination folder when saving figures -> section 0, b
#' ------------------------------------------------------------------#

#' ------------------------------------------------------------------#
#'   TO DO:
#'  - 
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
library(dplyr)
library(ggplot2)
library(purrr)
library(tibble)
library(tidyr)
library(corrplot)
library(tidylog)

### >> b) Import data ----

logger_raw <- read.csv(file.path("data_raw", "logger_trimmed.csv"),
                header = T, 
                sep = ",")

### 1) Scaling soil Relative Humidity ----

#' ------------------------------------------------------------------#
#' #Followed the scaling method put forward by Asherhoft and Gollan (2013)
#' #This corrects for potentially erronious RH readings
#' ------------------------------------------------------------------#

### >> a) Get max and 95th percentile of max RH ----
# scaling factor per logger/site - 95th percentile
RH_max_95 <-
  logger_raw %>%
  #select only gound loggers
  #filter(Loc == "GT") %>%
  #group data by day - need to split by year
  group_by(Site,
           Loc,
           year,
           dayofyear) %>%
  #get daily max values
  summarise(max = max(Humidity)) %>% 
  ungroup() %>%
  #group by site
  group_by(Site,
           Loc) %>%
  #calculate site max
  summarise(max_sm = max(max), 
  #95% of site max
            sm_max95 = list(enframe(quantile(`max`, probs = 0.95), 
                                    name = NULL,
                                    value = "sm_max95"))) %>% 
  unnest(cols = c(sm_max95))

### >> b) Apply correction factor ----

site <- pull(RH_max_95, Site) 
#vector to index against
loc <- pull(RH_max_95, Loc) 
#vector to index against
RH_95 <- pull(RH_max_95, sm_max95) 
#empty list to store outputs
Output <- vector("list",nrow(RH_max_95)) 


for (i in 1:nrow(RH_max_95)) { 
  
  Output[[i]] <-
    logger_raw %>%
    #select correct Location and Site
    filter(Loc == loc[i], #per location
           Site == site[i]) %>% #per site
    #Apply correction factor
    mutate(.,
           RH_new = ifelse(Humidity > RH_95[i],
                           100,
                           (Humidity*100)/RH_95[i])) #scaling factor
  
}

#flatten list
RH_new <- do.call(rbind.data.frame, Output) 

#Add scaled RH vals to df
logger_raw <-
  logger_raw %>%
  mutate(
    RH_new = RH_new$RH_new
  )

### 2) Growing Season ---- 

#' ------------------------------------------------------------------#
#' #A growing day is above 5º and below 35º
#' ------------------------------------------------------------------#

### >> a) Calculating growing season length ----
x <- logger_raw %>%
  #select air loggers
  filter(Loc == "AT") %>%
  #group by day
  group_by(Site, 
           year, 
           dayofyear) %>%
  summarise(max(Celsius))

#seq of 1/0 for temps >5 & < 35
#so we can exclude non growing days
x$test <- ifelse(x$`max(Celsius)` > 5 & x$`max(Celsius)` < 35 
                 ,1,0)

#can sum all 1's to calc growing season length
grow_season <-
  x %>%
  group_by(Site) %>%
  summarise(gs_length = sum(test))

#creates template so we can do calcs on growing days
gs_calc <- x[!(x$test==0), #extarcts vales not equal to zero
             c(1,2,3,4)] %>% #keep cols 1:4
  #this produces days that growing can potentially occur
  inner_join(.,
             logger_raw,
             by = c("Site",
                    "year",
                    "dayofyear")) 

### >> b) Growing season max air temps ----
#this calculates the maximum air temperature of growing days
gs_air <-
  gs_calc %>%
  #selct only air loggers
  filter(Loc == "AT") %>%
  #group by site
  group_by(Site) %>%
  #extract max and 95th percentile of max
  summarise(gs_max = max(Celsius),
            gs_max95 = list(enframe(quantile(`max(Celsius)`, probs = 0.95),
                                 name = NULL,
                                 value = "gs_max95")),
            #mean temp duirng growing season
            gs_mean = mean(Celsius)) %>% 
  unnest(cols = c(gs_max95))

### >> c) growing season df ----

gs_stats <- 
  grow_season %>%
  full_join(.,gs_air,
            by = "Site")


### 3) Mean Summer Temperatures ---- 

#' ------------------------------------------------------------------#
#' Summer is defined as the months of June - August
#' Mean temperature calcualted ofr this period only
#' ------------------------------------------------------------------#

summer <-
  logger_raw %>%
  filter(month > 5 & month < 9) %>%
  group_by(Site) %>%
  summarise(summer_temp_mean = mean(Celsius[Loc == "AT"]), #only for air temp
            summer_sm_mean = mean(RH_new[Loc == "GT"]) #only for soil RH
            )


### 4) Mean Annual Temperature ---- 

MAT <-
  logger_raw %>%
  filter(Loc == "AT") %>%
  group_by(Site) %>%
  summarise(MAT = mean(Celsius))
  

### 5) Temperature seasonality ---- 

#' ------------------------------------------------------------------#
#' Defiend as temp 'variation over a year based on \sigma^2 
#'   of the weekly average temp'
#' since we don't have a 'clean'  i.e. start on 01/01 end 31/12
#'   I will partition the number of days into weeks from when we 
#'   started recording to when we stopped 
#' ------------------------------------------------------------------#

### >> a) create a 'week' grouping variable ----

week_var <- 
  logger_raw %>%
  #selct air data
  filter(Loc == "AT") %>%
  #group out days
  group_by(Site,  dayofyear) %>%
  #way to gather site by day of year
  transmute(mean = mean(Celsius)) %>% 
  #removes duplicates
  distinct() %>% 
  ungroup() %>%
  #reps same number for 7 days
  mutate(week = c(0, rep(1:(nrow(.)-1)%/%7)), 
         #remove mean col
         mean = NULL)

### >> b) calculate temperature variability ----

Temp_var <-
  logger_raw %>%
  #remove ground temps
  filter(Loc == "AT") %>% 
  #add the 'week' grouping variable
  left_join(., 
            week_var,
            by = c("Site",
                   "dayofyear")) %>%
  #group by week
  group_by(Site, week) %>%
  #weakly mean
  summarise(mean = mean(Celsius)) %>% 
  group_by(Site) %>%
  #weekly variance of means
  summarise(temp_var = sd(mean)) 

### 6) Minimum temperature in coldest period ---- 

#' ------------------------------------------------------------------#
#' Defined as minimum temp during the coldest week/month.
#' I calculated the mean monthly temp and then selected the month 
#'    that was lowest and from there I can extracted the lowest/min temp
#' Minimum temerature caluclated using the 95th percitile of the
#'    minimum value
#' ------------------------------------------------------------------#


min_temp <-
  logger_raw %>%
  #remove ground loggers
  filter(Loc == "AT") %>% 
  group_by(Site, month, year) %>%
  #mean for each month
  summarise(mean = mean(Celsius)) %>% 
  group_by(Site) %>% 
  #find month with min/lowest mean
  filter(.,
         mean == min(mean)) %>% 
  #use as template to join original logger_raw df
  left_join(., 
            logger_raw,
            by = c("Site",
                   "month",
                   "year")) %>%
  group_by(Site) %>%
  #calculates min (using 95%)
  summarise(min = min(Celsius), 
            min95 = list(enframe(quantile(min, probs = 0.95),
                                 name = NULL,
                                 value = "min95"))) %>% 
  unnest(cols = c(min95))

### 7) Annual temperature range ---- 

#' ------------------------------------------------------------------#
#' The temperature variation over the year calculated as the 
#'    difference between the max temp of the warmest period 
#'    and the min of the coldest period. 
#' Here I will define cold and warm period similar to that for 
#'    mean temp i.e. as the months with the highest/lowest means 
#'    and then max - min
#' Using the minimum tempwerature df from section 6
#' ------------------------------------------------------------------#

min_range <-
  logger_raw %>%
  #remove ground loggers
  filter(Loc == "AT") %>% 
  group_by(Site, month, year) %>%
  #mean for each month
  summarise(mean = mean(Celsius)) %>% 
  group_by(Site) %>%
  #find month with max/lowest mean
  filter(.,
         mean == max(mean)) %>% 
  #use as template to join original logger_raw df
  left_join(., 
            logger_raw,
            by = c("Site",
                   "month",
                   "year")) %>%
  group_by(Site) %>%
  #calculates max (using 95%)
  summarise(max = max(Celsius), 
            max95 = list(enframe(quantile(max, probs = 0.95),
                                 name = NULL,
                                 value = "max95"))) %>% 
  unnest(cols = c(max95)) %>%
  #join max and min logger_rawasets
  left_join(., 
            min_temp,
            by = "Site") %>%
  #calculate range
  mutate(range = max - min, 
         range95 = max95 - min95)


### 8) Diurnal range ---- 

#' ------------------------------------------------------------------#
#' This is calculated as the mean weakly temperature ranges over a year.
#' So caluclate the min and max per week and then mean of those
#' ------------------------------------------------------------------#

daily_range <-
  logger_raw %>%
  #remove ground temps
  filter(Loc == "AT")%>% 
  left_join(., 
            #add the 'week' grouping variable
            week_var,
            by = c("Site",
                   "dayofyear")) %>% 
  group_by(Site, week, year) %>%
  #calcualtions for for each week
  summarise(min = min(Celsius),
            max = max (Celsius),
            range = max - min) %>% 
  group_by(Site) %>%
  summarise(daily_range = mean(range))

### 9) Concatinate all df's ---- 

all_logger_processed <- 
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

#save to data_processed folder
write.csv(all_logger_processed,
          file.path("data_processed", "microclimate_summary_site.csv"))


# End of script ----