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
#'  - Work on refining the Field Sites Map
#'       Place inset in main map
#'       Export as a file
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

### 2) Calcualting Growing Season Length ---- 

#' ------------------------------------------------------------------#
#' #A frowing day is above 5º and below 35º
#' #This corrects for potentially erronious RH readings
#' ------------------------------------------------------------------#

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


gs_calc <- x[!(x$test==0),c(1,2,3,4)] %>% #creates template so we can do calcs on growing days
  #this produces days that growing can potentially occur
  inner_join(.,
             logger_raw,
             by = c("Site",
                    "year",
                    "dayofyear")) 

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



#Mean Summer Temp  

summer <-
  logger_raw %>%
  filter(month > 5 & month < 9) %>%
  group_by(Site) %>%
  summarise(summer_temp_mean = mean(Celsius[Loc == "AT"]), #only for air temp
            summer_sm_mean = mean(Humid_new[Loc == "GT"]) #only for soil RH
            )



#Mean annual temp
MAT <-
  logger_raw %>%
  filter(Loc == "AT") %>%
  group_by(Site) %>%
  summarise(MAT = mean(Celsius))
  


#Temperature seasonality

#Defiend as temp 'variation over a year based on $\sigma^2$ of weekly average temp' - since we don't have a 'clean' start on 01/01  end 31/12 logger_rawaset I will partition the number of days into weeks from when we started recording to when we stopped 


week_var <- # create a 'week' grouping variable
  logger_raw %>%
  filter(Loc == "AT") %>%
  group_by(Site,  dayofyear) %>%
  transmute(mean = mean(Celsius)) %>% #way to gather site by day of year
  distinct() %>% #removes duplicates
  ungroup() %>%
  mutate(week = c(0, rep(1:(nrow(week_var)-1)%/%7)), #reps number for 7 diff days
         mean = NULL)

Temp_var <-
  logger_raw %>%
  filter(Loc == "AT") %>% #remove ground temps
  left_join(., #add the 'week' grouping variable
            week_var,
            by = c("Site",
                   "dayofyear")) %>%
  group_by(Site, week) %>%
  summarise(mean = mean(Celsius)) %>% #weakly mean
  group_by(Site) %>%
  summarise(temp_var = sd(mean)) #annual variance of means


#Minimum temp in coldest period
#Defined as minimum temp during the coldest week/month. I will calculate the mean monthly temp and then select the monnth that is lowest and from there I can extract the lowest/min temp


min_temp <-
  logger_raw %>%
  filter(Loc == "AT") %>% #remove ground loggers
  group_by(Site, month, year) %>%
  summarise(mean = mean(Celsius)) %>% #mean for each month
  group_by(Site) %>% 
  filter(.,
         mean == min(mean)) %>% #find month with min/lowest mean
  left_join(., #use as template to join orig logger_rawatset
            logger_raw,
            by = c("Site",
                   "month",
                   "year")) %>%
  group_by(Site) %>%
  summarise(min = min(Celsius), #calculates min (using 95%)
            min95 = list(enframe(quantile(min, probs = 0.95),
                                 name = NULL,
                                 value = "min95"))) %>% 
  unnest(cols = c(min95))


#Temp annual range

#The temperature variation over the year calculated as the difference between the max temp of the warmest period and the min of the coldest period. Here I will define cold and warm period similar to that for mean temp i.e. as the months with the highest/lowest means and then max - min


min_range <-
  logger_raw %>%
  filter(Loc == "AT") %>% #remove ground loggers
  group_by(Site, month, year) %>%
  summarise(mean = mean(Celsius)) %>% #mean for each month
  group_by(Site) %>%
  filter(.,
         mean == max(mean)) %>% #find month with max/lowest mean
  left_join(., #use as template to join orig logger_rawatset
            logger_raw,
            by = c("Site",
                   "month",
                   "year")) %>%
  group_by(Site) %>%
  summarise(max = max(Celsius), #calculates max (using 95%)
            max95 = list(enframe(quantile(max, probs = 0.95),
                                 name = NULL,
                                 value = "max95"))) %>% 
  unnest(cols = c(max95)) %>%
  left_join(., #join max and min logger_rawasets
            min_temp,
            by = "Site") %>%
  mutate(range = max - min, #calc range
         range95 = max95 - min95)



#Diurnal range

#This is calculated as the mean weakly temperature ranges over a year. 
#So caluclate the min and max per week and then mean of those


daily_range <-
  logger_raw %>%
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


#Concat logger_rawaset

all_logger_rawa <- 
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


write.csv(all_logger_rawa,
          "temp_data.csv")


# End of script ----