library(dplyr)
library(lubridate)
library(tidyr)
library(readxl)
library(ggplot2)

vp <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/vertical_profile_data.xlsx", na = "NA")
vp_rc <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/2019_RockCreek_NSF.xlsx")
Vertical_Profile_Data_July_2019_drew <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/Vertical_Profile_Data_July_2019_drew.xlsx",
sheet = "Stoney Creek")

vp_sc<-Vertical_Profile_Data_July_2019_drew


vp_jul<-vp %>% 
  mutate(date=ymd(date)) %>% 
  mutate(month=month(date, label = TRUE, abbr = TRUE), 
         year=year(date)) %>% 
  mutate(my=paste0(month, " ", year)) %>%
  filter(my=="Jul 2019") %>% 
  filter(creek=="Rock"|creek=="Stoney") %>% 
  select(creek, station, my, date, sonde_depth, temperature, ODO_conc, salinity)

vp_rc2<-vp_rc %>% 
  mutate(month=month(Date, label = TRUE, abbr = TRUE), 
         year=year(Date)) %>% 
  mutate(my=paste0(month, " ", year)) %>%
  filter(my=="Jul 2019") %>% 
  mutate(station=case_when(
    Station=="RC1"     ~ "upper",
    Station=="RC7"     ~ "middle",
    Station=="RC9b"    ~ "down"
  )) %>% 
  filter(is.na(station)==FALSE) %>% 
  mutate(creek="Rock",
         date=as.Date(Date)) %>% 
  select(creek, station, my, date, sonde_depth=`Sample Depth (m)`,
         temperature=`Water Temp (C)`, ODO_conc=`Dissolved O2 (mg/L)`,
         salinity=`Salinity (PSU)`) %>% 
  filter(date<as.POSIXct("2019-07-23"))
  
vp_jul2<-bind_rows(vp_jul, vp_rc2)

vp_sc2<-vp_sc %>% 
  separate(col=Station, into = c("creek","blah","blah2","stn"), sep = " ") %>% 
  select(-blah, -blah2) %>% 
  mutate(station=case_when(
    stn=="1"     ~"upper",
    stn=="3"     ~"middle",
    stn=="4"     ~"down"
  )) %>% 
  filter(is.na(station)==FALSE) %>% 
  mutate(date=as.Date(Date),
         month=month(Date, label = TRUE, abbr = TRUE), 
         year=year(Date)) %>% 
  mutate(my=paste0(month, " ", year)) %>%
  select(creek, station, my, date, sonde_depth=`Sample Depth (m)`,
         temperature=`Water Temp (oC)`, ODO_conc=`Dissolved O2 (mg/L)`,
         salinity=`Salinity (PSU)`)

vp_full<-bind_rows(vp_jul2,vp_sc2)

rm(vp_jul, vp_jul2, vp_rc, vp_rc2,vp_sc,vp_sc2)

vp<-pivot_longer(vp_full,names_to = "parameter", values_to = "measurement", cols = c(temperature:salinity))

pal<-c("2019-07-09"="navy",
       "2019-07-10"="darkorchid",
       "2019-07-11"="forestgreen",
       "2019-07-22"="maroon2")

rock<-vp %>% 
  filter(creek=="Rock") %>% 
  group_by(station,date) %>% 
  arrange(sonde_depth) %>% 
  ggplot(., aes(x=measurement, y=sonde_depth, color=as.factor(date)))+
  geom_point()+
  geom_path(size=1)+
  scale_color_manual(values = pal)+
  ylab("depth (m)")+
  scale_y_reverse(expand = c(0, 0))+
  scale_x_continuous(position = "top")+
  theme_classic()+
  theme(strip.text.x = element_text(size=10, face="bold"),
        strip.background = element_rect(colour="black", fill="aliceblue"))+
  facet_grid(station~parameter, scales = "free")+
  ggtitle("Rock Creek July 2019 Profile Comparisons")
rock
ggsave(filename = "figures/rock_sonde_comp.png")
stoney<-vp %>% 
  filter(creek=="Stoney") %>% 
  group_by(station,date) %>% 
  arrange(sonde_depth) %>% 
  ggplot(., aes(x=measurement, y=sonde_depth, color=as.factor(date)))+
  geom_point()+
  geom_path(size=1)+
  scale_color_manual(values = pal)+
  ylab("depth (m)")+
  scale_y_reverse(expand = c(0, 0))+
  scale_x_continuous(position = "top")+
  theme_classic()+
  theme(strip.text.x = element_text(size=10, face="bold"),
        strip.background = element_rect(colour="black", fill="aliceblue"))+
  facet_grid(station~parameter, scales = "free")+
  ggtitle("Stoney CreekJuly 2019 Profile Comparisons")
stoney
ggsave(file="figures/stoney_sonde_comp.png")
