library(dplyr)
library(lubridate)
library(readr)

#####pull in chlorophyll values from 2019#####
####STILL NEED TO ADD 2018###

library(readxl)
chl2019 <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/chlorophyll/CHLA-Summer19_correctedNov2019.xlsx",
                      col_types = c("text", "date", "date",
                                    "date", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric"),
                      skip = 13)

chl2019<-chl2019 %>% 
  select(sample_id = `Sample ID`, date = `Sample Date`, Tchla= `Total Chl-a (ug/L)`,
         Achl = `Active Chl (ug/L)`, phaeophytin = `Phaeophytin (ug/L)`) %>% 
  separate(col=sample_id, into = c("depth","creek","station"), sep = "-") %>% 
  mutate(creek=plyr::revalue(creek, c("MB"="Middle Branch",
                                      "IH"="Inner Harbour",
                                      "RC"="Rock",
                                      "SC"="Stoney",
                                      "BC"="Bear",
                                      "CC"="Curtis")),
         depth=plyr::revalue(depth, c("SW"="surface",
                                      "BW"="bottom")),
         station=plyr::revalue(station, c("D"="down", 
                                          "U"="upper", 
                                          "M"="middle")),
         month=month(date, label = TRUE, abbr = TRUE),
         year=year(date)) %>% 
  mutate(my=paste0(month," ",year)) %>% 
  select(-month, -year)

chl2018<-read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/chlorophyll/chlorophyll calcs_2018_correctedNov2019.xlsx")

chl2018<-chl2018 %>% 
  drop_na(Dilution) %>% 
  separate(col=Sample, into = c("depth","creek","station"),sep = "-") %>% 
  mutate(depth=recode(depth, !!!depth_labs), 
         creek=recode(creek, !!!creek_labs),
         station=recode(station, !!!station_labs)) %>% 
  select(depth, creek, station, date=`Sample Date`, Tchla=`Total Chl-a`, Achl=`Active Chl`,
         phaeophytin=Phaeophytin) %>% 
  mutate(month=month(date, label = TRUE, abbr = TRUE),
         year=year(date)) %>% 
  mutate(my=paste0(month," ",year))
chl<-chl2018 %>% 
  bind_rows(., chl2019) %>% 
  select(-month,-year)

saveRDS(chl, "chl.rds")    
  
