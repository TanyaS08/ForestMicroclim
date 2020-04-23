# Swedish Microclimate Project
# Dalarna, Sweden - July 2019
#
# Authors: Ditte M. Christhansen, Kristoffer Hylander, 
#          Tanya Strydom
# Contact: tanya.strydom@icloud.com

#' ------------------------------------------------------------------#
#'  - This script runs the generalised linear models for species
#'    population paramters and microclimate variables
#'  - This script can be run without needing to run any other 
#'    dependencies/scripts.
#' ------------------------------------------------------------------#

#' ------------------------------------------------------------------#
#'   TO DO:
#'   - Automated model selection between with and without 
#'     interaction term
#'   - insert if function/loop for when there is no model to plot/predict
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
library(tidyverse)
library(tidylog)
library(readr)
library(sjPlot)

### 1) Data Import ----
### >> a) Dataframes ----

#Predictor variables dataframe
microclim <- 
  read.csv(file.path("data_processed", "Env_cleaned.csv"),
           header = T,
           sep = ";") %>%
  transmute(
    Site = Site,
    treat = MaNonegeCat,
    vol = vol_2018 - vol_2005, #calc \delta vals
    pine = Pine_2018 - Pine_2005, #calc \delta vals
    basal = BasalArea_2018 - BasalArea_2005, #calc \delta vals
    boulder = boulder,
    canopy_gap = canopy_gap
    #add microclimate data from loggers
  ) %>%
  full_join(.,
            read.csv(file.path("data_processed", "microclimate_summary_site.csv"),
                     header = T),
            by = "Site") %>%
  dplyr::select(Site, #pred variables used
                MAT,
                gs_max95,
                min95,
                vol,
                pine,
                boulder,
                treat,
                canopy_gap) %>%
  #create a treatment variable - binary 1 = yes, 0 = no nitrients added
  mutate(treat = ifelse(treat == "Thinning", 1, 0)) %>%
  #scale all predictor vars excepts treament and site
  mutate_at(.,
            vars(-c(Site, treat)),
            scale) %>%  
  mutate_at(.,
            vars(-c(Site, treat)),
            as.numeric)

##Species dataframes
#saved as list with each species being one elemnt
spp_list <-
  list(barb = read.csv(file.path("data_processed", "barbilophozia.csv"),
                       header = T,
                       row.names = 1),
       cal = read.csv(file.path("data_processed", "calamagrostis.csv"),
                      header = T,
                      row.names = 1),
       lin = read.csv(file.path("data_processed", "linnaea.csv"),
                      header = T,
                      row.names = 1),
       hyp = read.csv(file.path("data_processed", "hypnum.csv"),
                      header = T,
                      row.names = 1) %>%
         #round up area
         mutate(Area = round(Area, digits = 0)))

### >> b) Create lists ----
ref_list <- list( #each spp has own list
  #List for Barbilophizia
  barb = list( 
    #list for presence/absence model
    PA = microclim %>%
      full_join(.,
                spp_list[[1]][,c("Site","Area")],
                by = "Site") %>%
      #create pres/abs var
      mutate(.,
             pres = ifelse(is.na(Area),0,1)) %>%
      #remove unwanted variables
      mutate(
        y = pres,
        Area = NULL,
        Site = NULL,
        pres = NULL
      ) %>%
      #remove NA's for models
      na.omit() %>%
      #make list
      as.list()
  ),
  #List for Calamagrostis
  cal = list(
    #list for presence/absence model
    PA = microclim %>%
      full_join(.,
                #select only site and area
                spp_list[[2]][,c("Site","Area")],
                by = "Site") %>%
      #creates pres/abs var
      mutate(.,
             pres = ifelse(is.na(Area),0,1)) %>% 
      #remove unwanted variables
      mutate(
        y = pres,
        Area = NULL,
        Site = NULL,
        pres = NULL
      ) %>%
      na.omit() %>%
      as.list(),
    #list for area model
    Area = microclim %>%
      full_join(.,
                spp_list[[2]][,c("Site","Area")],
                by = "Site") %>%
      mutate(
        y = Area,
        #this is the area for the 25 m x 25 m site
        area = rep(625, length(y)),
        Site = NULL,
      ) %>%
      na.omit() %>%
      as.list(),
    #list for reproducing model
    Flower = microclim %>%
      full_join(.,
                spp_list[[2]][,c("Site","Number_reproduce")],
                by = "Site") %>%
      mutate(
        y = Number_reproduce,
        Site = NULL,
        cells = NULL,
        Number_reproduce = NULL
      ) %>%
      #make variable binary: yes = flowring individual present i.e. > 0
      mutate(
        y = ifelse(y > 0, 1, 0)
      ) %>%
      na.omit() %>%
      as.list(),
    #List for density model
    Density = microclim %>%
      full_join(.,
                spp_list[[2]][,c("Site","Number_cells","Area")],
                by = "Site") %>%
      mutate(
        y = Number_cells,
        #only 20 subpops sampled in detail .'. if area > 20 then make 20
        #multiply by 100 for each 1 dm^2 in the 1 m^2
        area = ifelse(Area >20, 2000, Area*100), 
        Site = NULL,
        Number_cells = NULL,
      ) %>%
      na.omit() %>%
      as.list()
  ),
  #List for Linnaea
  lin = list(
    #list for presence/absence model
    PA = microclim %>%
      full_join(.,
                #select only site and area
                spp_list[[3]][,c("Site","Area")],
                by = "Site") %>%
      #creates pres/abs var
      mutate(.,
             pres = ifelse(is.na(Area),0,1)) %>% 
      #remove unwanted variables
      mutate(
        y = pres,
        Area = NULL,
        Site = NULL,
        pres = NULL
      ) %>%
      na.omit() %>%
      as.list(),
    #list for area model
    Area = microclim %>%
      full_join(.,
                spp_list[[3]][,c("Site","Area")],
                by = "Site") %>%
      mutate(
        y = Area,
        #this is the area for the 25 m x 25 m site
        area = rep(625, length(y)),
        Site = NULL,
      ) %>%
      na.omit() %>%
      as.list(),
    #list for reproducing model
    Flower = microclim %>%
      full_join(.,
                spp_list[[3]][,c("Site","Number_reproduce")],
                by = "Site") %>%
      mutate(
        y = Number_reproduce,
        Site = NULL,
        cells = NULL,
        Number_reproduce = NULL
      ) %>%
      #make variable binary: yes = flowring individual present i.e. > 0
      mutate(
        y = ifelse(y > 0, 1, 0)
      ) %>%
      na.omit() %>%
      as.list(),
    #List for density model
    Density = microclim %>%
      full_join(.,
                spp_list[[3]][,c("Site","Number_cells","Area")],
                by = "Site") %>%
      mutate(
        y = Number_cells,
        #only 20 subpops sampled in detail .'. if area > 20 then make 20
        #multiply by 100 for each 1 dm^2 in the 1 m^2
        area = ifelse(Area >20, 2000, Area*100), 
        Site = NULL,
        Number_cells = NULL,
      ) %>%
      na.omit() %>%
      as.list()
  ),
  #List for Hypnum
  hyp = list(
    #List for pres/abs model
    PA = microclim %>%
      full_join(.,
                spp_list[[4]][,c("Site","Area")],
                by = "Site") %>%
      #create pres/abs var
      mutate(.,
             pres = ifelse(is.na(Area),0,1)) %>% 
      mutate(
        y = pres,
        Area = NULL,
        Site = NULL,
        pres = NULL
      ) %>%
      na.omit() %>%
      as.list(),
    Area =  microclim %>%
      full_join(.,
                spp_list[[4]][,c("Site","Area")],
                by = "Site") %>%
      mutate(
        y = Area,
        area = NULL,
        Site = NULL,
      ) %>%
      na.omit() %>%
      as.list(),
    #model for reproductive potential
    Flower = microclim %>%
      full_join(.,
                spp_list[[4]][,c("Site","Number_reproduce")],
                by = "Site") %>%
      mutate(
        y = Number_reproduce,
        Site = NULL,
        Number_reproduce = NULL
      ) %>%   
      #make binary 1 = yes if sporulating i.e. > 0
      mutate(
        y = ifelse(y > 0, 1, 0)
      ) %>%
      na.omit() %>%
      as.list()
  )
)

### 2) Models ----
### >> a) Presence/Absence ----

#list to store full model
mod.PA <- vector('list', 4)
#list ot store model without interaction term
mod.PA.noint <- vector('list', 4)
# list to store plot outputs
plot.PA <- vector('list', 4)
plot.PA.noint <- vector('list', 4)

interact.PA <- vector('list', 4)
heat.PA <- vector('list', 4)

#Hypnum distribution has boulder as co-varaite .'.
#only Barbilophozia, Calamgrostis and Linnaea 
#in this loop
for (i in 1:3) {
  
  #full model
  mod.PA[[i]] <-    
    #include step down selection function
    step(glm(y ~ MAT + gs_max95 +
               canopy_gap * vol,
             #select correct df from list
             data = as.data.frame(ref_list[[i]][[1]]),
             family = binomial(link = "logit"))
    )
  
  #model without interaction
  mod.PA.noint[[i]] <-    
    #include step down selection function
    step(glm(y ~ MAT + gs_max95 +
               canopy_gap + vol,
             #select correct df from list
             data = as.data.frame(ref_list[[i]][[1]]),
             family = binomial(link = "logit"))
    )
  
  plot.PA[[i]] <-
    ggplot(data =  
             #put data in long form
             reshape2::melt(as.data.frame(ref_list[[i]][[1]]),
                            id.vars = c("y")) %>%
             #rename grouping variable
             rename(group = variable) %>%
             #add predicted values
             left_join(.,
                       do.call(rbind.data.frame, 
                               #this function creates presiction outputs
                               ggeffects::ggeffect(mod.PA[[i]])),
                       by = "group") %>%
             na.omit) +   
    #plot CI for predicted values
    geom_ribbon(aes(x = x,
                    ymin = conf.low,
                    ymax = conf.high),
                alpha = 0.3) +
    #split ot by different predictor variables
    facet_wrap(vars(group)) +
    #plot predicted vlaues
    geom_line(aes(x = x,
                  y = predicted)) +
    #plot recorded values
    geom_point(aes(y = y,
                   x = value)) +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle("Presence/Absence")
}

#This loop is for Hypnum
#using boulder as a co-variate
for (i in 4) {
  
  mod.PA[[i]] <-   
    #include step down selection function
    step(glm(y ~ MAT + gs_max95 + boulder + 
               canopy_gap * vol,
             data = ref_list[[i]][[1]],
             family = binomial(link = "logit"))
    )
  
  mod.PA.noint[[i]] <-   
    #include step down selection function
    step(glm(y ~ MAT + gs_max95 + boulder + 
               canopy_gap + vol,
             data = ref_list[[i]][[1]],
             family = binomial(link = "logit"))
    )
  
  
  plot.PA[[i]] <-
    ggplot(data =  
             #put data in long form
             reshape2::melt(as.data.frame(ref_list[[i]][[1]]),
                            id.vars = c("y")) %>%
             #rename grouping variable
             rename(group = variable) %>%
             #add predicted values
             left_join(.,
                       do.call(rbind.data.frame, 
                               #this function creates presiction outputs
                               ggeffects::ggeffect(mod.PA[[i]])),
                       by = "group") %>%
             na.omit) +   
    #plot CI for predicted values
    geom_ribbon(aes(x = x,
                    ymin = conf.low,
                    ymax = conf.high),
                alpha = 0.3) +
    #split ot by different predictor variables
    facet_wrap(vars(group)) +
    #plot predicted vlaues
    geom_line(aes(x = x,
                  y = predicted)) +
    #plot recorded values
    geom_point(aes(y = y,
                   x = value)) +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle("Presence/Absence") 
  
}


for (i in 2:3) {
  
  ggplot(data=transform(expand.grid(vol=seq(-2,2, 0.1),
                                    canopy_gap=seq(-2,2, 0.1)) %>% #this creates every possible combo of vals
                          mutate(MAT = rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                               canopy_gap=seq(-2,2, 0.1)))),
                                 gs_max95 = rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                                    canopy_gap=seq(-2,2, 0.1)))),
                                 boulder =  rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                                    canopy_gap=seq(-2,2, 0.1))))), 
                        predicted=predict.glm(mod.PA[[i]], expand.grid(vol=seq(-2,2, 0.1),
                                                                       canopy_gap=seq(-2,2, 0.1)) %>% #this creates every possible combo of vals
                                                mutate(MAT = rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                                                     canopy_gap=seq(-2,2, 0.1)))),
                                                       gs_max95 = rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                                                          canopy_gap=seq(-2,2, 0.1)))),
                                                       boulder =  rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                                                          canopy_gap=seq(-2,2, 0.1))))),
                                              type = "response")), 
         aes(y=canopy_gap, x=vol,)) +
    geom_tile(aes(fill = predicted)) +
    scale_fill_distiller(palette = "YlOrRd",
                         direction = 1) +
    theme_bw() +
    ggtitle("Pres/Abs")
  
  
}

### >> b) Area ----

#list to store full model
mod.area <- vector('list', 4)
#list ot store model without interaction term
mod.area.noint <- vector('list', 4)
# list to store plot outputs
plot.area <- vector('list', 4)

interact.area <- vector('list', 4)
heat.area <- vector('list', 4)

#Hypnum distribution predicted by poisson .'.
#only Calamgrostis and Linnaea 
#in this loop
for (i in 2:3) {
  
  #full model
  mod.area[[i]] <-  
    #iclude step down selection function
    step(glm(cbind(y, (area-y)) ~ MAT + gs_max95 +
               canopy_gap * vol,
             #select correct df from list
             data = as.data.frame(ref_list[[i]][[2]]),
             family = binomial())
    )
  
  #model without interaction
  mod.area.noint[[i]] <- 
    #iclude step down selection function
    step(glm(cbind(y, (area-y)) ~ MAT + gs_max95 +
               canopy_gap + vol,
             #select correct df from list
             data = as.data.frame(ref_list[[i]][[2]]),
             family = binomial())
    )
  
  plot.area[[i]] <-
    ggplot(data =  
             #put data in long form
             reshape2::melt(as.data.frame(ref_list[[i]][[2]]) %>%
                              #turn y values into proabbility
                              mutate(y = y/area),
                            id.vars = c("y")) %>%
             #rename grouping variable
             rename(group = variable) %>%
             #add predicted values
             left_join(.,
                       do.call(rbind.data.frame, 
                               #this function creates presiction outputs
                               ggeffects::ggeffect(mod.area[[i]])),
                       by = "group") %>%
             na.omit) +   
    #plot CI for predicted values
    geom_ribbon(aes(x = x,
                    ymin = conf.low,
                    ymax = conf.high),
                alpha = 0.3) +
    #split ot by different predictor variables
    facet_wrap(vars(group)) +
    #plot predicted vlaues
    geom_line(aes(x = x,
                  y = predicted)) +
    #plot recorded values
    geom_point(aes(y = y,
                   x = value)) +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle("Area")
}

#This loop is for Hypnum
#using a poisson distribution
for (i in 4) {
  
  mod.area[[i]] <- step(glm(y ~ MAT + gs_max95 + boulder + 
                              canopy_gap * vol,
                            data = ref_list[[i]][[2]],
                            family = poisson(link = "log"))
  )
  
  mod.area.noint[[i]] <- step(glm(y ~ MAT + gs_max95 + boulder + 
                                    canopy_gap + vol,
                                  data = ref_list[[i]][[2]],
                                  family = poisson(link = "log"))
  )
  
  
  plot.area[[i]] <-
    ggplot(data =  
             #put data in long form
             reshape2::melt(as.data.frame(ref_list[[i]][[2]]),
                            id.vars = c("y")) %>%
             #rename grouping variable
             rename(group = variable) %>%
             #add predicted values
             left_join(.,
                       do.call(rbind.data.frame, 
                               #this function creates presiction outputs
                               ggeffects::ggeffect(mod.area[[i]])),
                       by = "group") %>%
             na.omit) +   
    #plot CI for predicted values
    geom_ribbon(aes(x = x,
                    ymin = conf.low,
                    ymax = conf.high),
                alpha = 0.3) +
    #split ot by different predictor variables
    facet_wrap(vars(group)) +
    #plot predicted vlaues
    geom_line(aes(x = x,
                  y = predicted)) +
    #plot recorded values
    geom_point(aes(y = y,
                   x = value)) +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle("Area") 
  
}


for (i in 2:4) {
  
  heat.area[[i]] <-
    ggplot(data=transform(expand.grid(vol=seq(-2,2, 0.1),
                                      canopy_gap=seq(-2,2, 0.1)) %>% #this creates every possible combo of vals
                            mutate(MAT = rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                                 canopy_gap=seq(-2,2, 0.1)))),
                                   gs_max95 = rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                                      canopy_gap=seq(-2,2, 0.1)))),
                                   boulder =  rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                                      canopy_gap=seq(-2,2, 0.1))))), 
                          predicted=predict.glm(mod.area[[i]],
                                                expand.grid(vol=seq(-2,2, 0.1),
                                                            canopy_gap=seq(-2,2, 0.1)) %>% #this creates every possible combo of vals
                                                  mutate(MAT = rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                                                       canopy_gap=seq(-2,2, 0.1)))),
                                                         gs_max95 = rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                                                            canopy_gap=seq(-2,2, 0.1)))),
                                                         boulder =  rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                                                            canopy_gap=seq(-2,2, 0.1))))),
                                                type = "response")), 
           aes(y=canopy_gap, x=vol,)) +
    geom_tile(aes(fill = predicted)) +
    scale_fill_distiller(palette = "YlOrRd",
                         direction = 1) +
    theme_bw() +
    ggtitle("Area")
  
  
}

### >> c) Density ----

#list to store full model
mod.density <- vector('list', 4)
#list ot store model without interaction term
mod.density.noint <- vector('list', 4)
# list to store plot outputs
plot.density <- vector('list', 4)

interact.density <- vector('list', 4)
heat.density <- vector('list', 4)

#Hypnum distribution predicted by poisson .'.
#only Calamgrostis and Linnaea 
#in this loop
for (i in 2:3) {
  
  #full model
  mod.density[[i]] <- step(glm(cbind(y, (area-y)) ~ MAT + gs_max95 +
                                 canopy_gap * vol,
                               #select correct df from list
                               data = as.data.frame(ref_list[[i]][[4]]),
                               family = binomial())
  )
  
  #model without interaction
  mod.density.noint[[i]] <- step(glm(cbind(y, (area-y)) ~ MAT + gs_max95 +
                                       canopy_gap + vol,
                                     #select correct df from list
                                     data = as.data.frame(ref_list[[i]][[4]]),
                                     family = binomial())
  )
  
  plot.density[[i]] <-
    ggplot(data =  
             #put data in long form
             reshape2::melt(as.data.frame(ref_list[[i]][[4]]) %>%
                              #turn y values into proabbility
                              mutate(y = y/area),
                            id.vars = c("y")) %>%
             #rename grouping variable
             rename(group = variable) %>%
             #add predicted values
             left_join(.,
                       do.call(rbind.data.frame, 
                               #this function creates presiction outputs
                               ggeffects::ggeffect(mod.density[[i]])),
                       by = "group") %>%
             na.omit) +   
    #plot CI for predicted values
    geom_ribbon(aes(x = x,
                    ymin = conf.low,
                    ymax = conf.high),
                alpha = 0.3) +
    #split ot by different predictor variables
    facet_wrap(vars(group)) +
    #plot predicted vlaues
    geom_line(aes(x = x,
                  y = predicted)) +
    #plot recorded values
    geom_point(aes(y = y,
                   x = value)) +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle("Density")
}


for (i in 2:3) {
  
  heat.density[[i]] <-
    ggplot(data=transform(expand.grid(vol=seq(-2,2, 0.1),
                                      canopy_gap=seq(-2,2, 0.1)) %>% #this creates every possible combo of vals
                            mutate(MAT = rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                                 canopy_gap=seq(-2,2, 0.1)))),
                                   gs_max95 = rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                                      canopy_gap=seq(-2,2, 0.1)))),
                                   boulder =  rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                                      canopy_gap=seq(-2,2, 0.1))))), 
                          predicted=predict.glm(mod.density[[i]],
                                                expand.grid(vol=seq(-2,2, 0.1),
                                                            canopy_gap=seq(-2,2, 0.1)) %>% #this creates every possible combo of vals
                                                  mutate(MAT = rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                                                       canopy_gap=seq(-2,2, 0.1)))),
                                                         gs_max95 = rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                                                            canopy_gap=seq(-2,2, 0.1)))),
                                                         boulder =  rep(0, nrow(expand.grid(vol=seq(-2,2, 0.1),
                                                                                            canopy_gap=seq(-2,2, 0.1))))),
                                                type = "response")), 
           aes(y=canopy_gap, x=vol,)) +
    geom_tile(aes(fill = predicted)) +
    scale_fill_distiller(palette = "YlOrRd",
                         direction = 1) +
    theme_bw() +
    ggtitle("Density")
  
  
}

### >> d) Reproductive potential ----

#list to store full model
mod.flower <- vector('list', 4)
#list ot store model without interaction term
mod.flower.noint <- vector('list', 4)
# list to store plot outputs
plot.flower <- vector('list', 4)
plot.flower.noint <- vector('list', 4)


#Hypnum distribution has boulder as co-varaite .'.
#only Barbilophozia, Calamgrostis and Linnaea 
#in this loop
for (i in 2:3) {
  
  #full model
  mod.flower[[i]] <- step(glm(y ~ MAT + gs_max95 +
                                canopy_gap * vol,
                              #select correct df from list
                              data = as.data.frame(ref_list[[i]][[3]]),
                              family = binomial(link = "logit"))
  )
  
  #model without interaction
  mod.flower.noint[[i]] <- step(glm(y ~ MAT + gs_max95 +
                                      canopy_gap + vol,
                                    #select correct df from list
                                    data = as.data.frame(ref_list[[i]][[3]]),
                                    family = binomial(link = "logit"))
  )
  
  plot.flower[[i]] <-
    ggplot(data =  
             #put data in long form
             reshape2::melt(as.data.frame(ref_list[[i]][[3]]),
                            id.vars = c("y")) %>%
             #rename grouping variable
             rename(group = variable) %>%
             #add predicted values
             left_join(.,
                       do.call(rbind.data.frame, 
                               #this function creates presiction outputs
                               ggeffects::ggeffect(mod.flower[[i]])),
                       by = "group") %>%
             na.omit) +   
    #plot CI for predicted values
    geom_ribbon(aes(x = x,
                    ymin = conf.low,
                    ymax = conf.high),
                alpha = 0.3) +
    #split ot by different predictor variables
    facet_wrap(vars(group)) +
    #plot predicted vlaues
    geom_line(aes(x = x,
                  y = predicted)) +
    #plot recorded values
    geom_point(aes(y = y,
                   x = value)) +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle("Presence/Absence")
}

#This loop is for Hypnum
#using boulder as a co-variate
for (i in 4) {
  
  mod.flower[[i]] <- step(glm(y ~ MAT + gs_max95 + boulder + 
                                canopy_gap * vol,
                              data = ref_list[[i]][[3]],
                              family = binomial(link = "logit"))
  )
  
  mod.flower.noint[[i]] <- step(glm(y ~ MAT + gs_max95 + boulder + 
                                      canopy_gap + vol,
                                    data = ref_list[[i]][[3]],
                                    family = binomial(link = "logit"))
  )
  
  
  plot.flower[[i]] <-
    ggplot(data =  
             #put data in long form
             reshape2::melt(as.data.frame(ref_list[[i]][[3]]),
                            id.vars = c("y")) %>%
             #rename grouping variable
             rename(group = variable) %>%
             #add predicted values
             left_join(.,
                       do.call(rbind.data.frame, 
                               #this function creates presiction outputs
                               ggeffects::ggeffect(mod.flower[[i]])),
                       by = "group") %>%
             na.omit) +   
    #plot CI for predicted values
    geom_ribbon(aes(x = x,
                    ymin = conf.low,
                    ymax = conf.high),
                alpha = 0.3) +
    #split ot by different predictor variables
    facet_wrap(vars(group)) +
    #plot predicted vlaues
    geom_line(aes(x = x,
                  y = predicted)) +
    #plot recorded values
    geom_point(aes(y = y,
                   x = value)) +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle("Presence/Absence") 
  
}


### End of script ----