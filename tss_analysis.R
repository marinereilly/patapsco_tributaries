library(dplyr)
library(lubridate)

#load 2018 data
library(readxl)
tss_2018 <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/patapsco_TSS.xlsx",
sheet = "data_for_processing")
View(tss_2018)

#load 2019 data
tss_2019 <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/Patapsco_tss_2019.xlsx",
sheet = "data_processing")
tss_2019<-tss_2019 %>% 
  rename(filter_id=filter_num)
View(tss_2019)

filter_weights <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/filter_weights.xlsx")

tss_2019<-left_join(tss_2019, filter_weights, by="filter_id")

df<- tss_2019 %>% 
  group_by(date_collected,creek,station,depth, filter_id,amt_filtered,filter_weight) %>% 
  summarise(dry_weight=mean(dry_weight))
df2<-df %>% 
  mutate(matter=dry_weight-filter_weight) %>% 
  mutate(tss=signif(matter*1000/(amt_filtered/1000),digits=4)) %>% 
  ungroup() %>% 
  mutate(date_collected=ymd(date_collected))

#combine 2018 and 2019
tss<-tss_2018 %>% 
  mutate(date=as.Date(date)) %>% 
  rename(amt_filtered=amount_filtered, dry_weight=final_weight, date_collected=date) %>% 
  full_join(df2,.)

tss<-tss %>% 
  mutate(creek=case_when(
    creek=="bear"          ~ "Bear",
    creek=="curtis"        ~ "Curtis",
    creek=="inner"         ~ "Inner Harbour",
    creek=="Inner Harbor"  ~ "Inner Harbour",
    creek=="middle"        ~ "Middle Branch",
    creek=="rock"          ~ "Rock",
    creek=="stoney"        ~ "Stoney",
    TRUE                   ~ creek
  ))

saveRDS(tss, "tss.rds")



  