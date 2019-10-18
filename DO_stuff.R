library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(broom)
library(tidyr)

resp<- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/respiration_data.xlsx")
prof<- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/vertical_profile_data.xlsx")

prof$date<-as.POSIXct.Date(prof$date)

prof2<-prof %>%
  filter(station=="middle")%>% 
  mutate(creek=case_when(
    creek == "Inner harbour"     ~ "Inner Harbor",
    creek == "Inner Harbour"     ~ "Inner Harbor",
    TRUE                         ~ as.character(creek)
  )) %>% 
  group_by(creek, date) %>% 
  filter(sonde_depth==max(sonde_depth)) %>% 
  mutate(ODO_conc=as.numeric(ODO_conc)) 

join<-resp %>% 
  filter(type=="Initial") %>% 
  mutate(date=date_collected) %>% 
  full_join(., prof2, by=c("creek","date"))

plot<-join %>% 
  ggplot(aes(x=DO_conc, y=ODO_conc, color=creek))+
  geom_point()+xlab("Respiration")+ylab("sonde")+theme_classic()
plot

lm<-lm(data = join, DO_conc ~ ODO_conc)
summary(lm)
lm2<-tidy(lm)
