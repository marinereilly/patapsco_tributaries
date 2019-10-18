library(dplyr)
library(tidyr)
library(lubridate)

#when you run this on your computer you need to swap Z and H
tss <- read_excel("Z:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/Patapsco_tss_2019.xlsx")
tss<-tss %>% 
  mutate(date_collected=ymd(date_collected),
         date_weighed=ymd(date_weighed))
View(tss)

check<-tss %>% 
  group_by(date_collected, creek, station, depth) %>% 
  arrange(date_weighed) %>% 
  
