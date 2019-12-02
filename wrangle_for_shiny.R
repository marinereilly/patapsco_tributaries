library(dplyr)
library(lubridate)
library(broom)
library(NASL)
library(purrr)
library(tidyr)
#library(googledrive)
#library(googlesheets)


#Goal is to create a wide df from all of the combined data and the different calculated
#values then make it long form for the shiny app.

#This is to make a datatable of units for each parameter
param<-c("kd",
         "Tchla",
         "Achl",
         "phaeophytin",
         "tss",
         "delta_sigmat",
         "mixed")
units<-c("m-1",
         "ug/L",
         "ug/L",
         "ug/L",
         "mg/L",
         "none",
         "none")
param_units<-data.frame(param, units)

######starting with kd which has its own script in theory.#####
#run the light_for_real script until you get to having a df named lp

patapsco_wide<-lp %>% 
  select(creek, station, date, month, year, kd=b) %>% 
  mutate(my=paste0(month," ",year),
         depth= NA) %>% 
  select(creek, station, depth, date, my, kd)
View(patapsco_wide)

readr::write_rds(patapsco_wide, "patapsco_wide.rds")
rm(light_profiles,light_profiles2,lp)


#####TSS Calcs#####
tss<-readRDS("tss.rds") 
#%>% 
#  select(date=date_collected, creek, station, depth, tss) %>% 
#  mutate(month=month(date, label=TRUE, abbr=TRUE),
#         year=year(date)) %>% 
#  mutate(my=paste0(month, " ",year)) %>% 
#  select(-month,-year)


#####bind things to the main dataframe#####
patapsco_wide<-readRDS(file = "patapsco_wide.rds")
chl<-readRDS("chl.rds") %>% 
  mutate(date=as.Date(date))
tss<-readRDS("tss.rds")
sigmat<-readRDS("sigmat.rds")

nodepth<-full_join(patapsco_wide, sigmat, by = c("creek", "date","station", "my")) 
wdepth<-full_join(chl,tss,by = c("creek", "date","station", "my", "depth")) %>% 
  drop_na(creek)
pat<-merge(nodepth,wdepth,by = c("creek", "date","station", "my"), 
          all = TRUE, sort = FALSE)
saveRDS(pat,"patapsco_wide.rds")  

patapsco_wide<-readRDS(file = "patapsco_wide.rds")
respiration<-readRDS(file="respiration.rds") %>% 
  mutate(station="middle")

pat<-merge(patapsco_wide, respiration, by = c("creek", "date","station", "my"), 
           all = TRUE, sort = FALSE)  
saveRDS(pat,"patapsco_wide.rds")

#####add some weird ratios#####
pat<-pat %>% 
  mutate(chl_tss=Tchla/1000/tss)
