library(tidyverse)

nasl <- read_csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/NASL/HARRIS_PATAPSCO_052819.csv")
nasl<-nasl %>% mutate(Result=as.numeric(Result))
nasl<-nasl %>% 
  drop_na('Sample ID')
  

nasl2<-nasl %>%
  select('Sample ID') 
nasl2$plotid<-nasl2$`Sample ID`
nasl2<-nasl2 %>% 
  separate('Sample ID', into = c("depth", "creek", "distance"),
           sep= "-")
nasl<-nasl %>% 
  select(plotid='Sample ID', date='Sample Date',Parameter, Result) %>% 
  full_join(., nasl2) 
nasl$depth<-factor(nasl$depth, levels=c("SW", "BW"))
nasl$date<-mdy(nasl$date)
nasl$month<-months(nasl$date)
nasl$year<-year(nasl$date)
nasl<-nasl %>% 
  mutate(p_date=
           paste0(Parameter, " ~ ", month," ", year)) %>% 
  select(-date, -month, -year) 
  

View(nasl)

station_names<-c(
  'D'="Down",
  'M'="Middle",
  'U'="Upper")
depth_pal<-c("SW"="goldenrod2", "BW"="firebrick3")

#This only works if all sites sampled in the same month - they were not in 2018
#so you would want to replace p_date with Parameter
nasl_nest<-nasl %>% 
  group_by(p_date) %>% 
  nest()
nasl_plot<-nasl_nest %>% 
  mutate(plot=map2(data, p_date, ~ggplot(data=.x) +
                     ggtitle(.y)+
                     theme_classic()+
                     theme(axis.line = element_line(linetype = "solid"), 
                           axis.ticks = element_line(size = 1), 
                           panel.grid.major = element_line(colour = "gray80", 
                                                           linetype = "dotted"), panel.grid.minor = element_line(colour = "gray90", 
                                                                                                                 linetype = "dotted"), axis.title = element_text(size = 12), 
                           axis.text = element_text(size = 10), 
                           plot.title = element_text(size = 16, 
                                                     face = "bold"))+
                     geom_bar(aes(x=creek, y=Result, fill=depth), position="dodge", stat="identity", color="black")+
                     scale_fill_manual(values = depth_pal)+
                     facet_wrap(distance~., nrow=1, labeller = as_labeller(station_names))
  ))
                     
nasl_plot$plot[[3]] #too look at one of the plots

if(!dir.exists("./figures")){ #if a figures folder does not exist, create it.
  dir.create("./figures")
}
#use the map function with ggsave to save named figures. 
dir.create("./figures/nasl_plots")
map2(paste0("./figures/nasl_plots/", nasl_plot$p_date, ".jpg"), nasl_plot$plot, ggsave)



#Calvert NASL DATA addition
library(readxl)
calvert <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/NASL/calvertcreeks_2016_07_21.xlsx")
library(stringr)
calvert<-calvert %>% 
  drop_na(Parameter)
calvert<-calvert %>% 
  mutate(depth=str_sub(`Sample ID`,-1)) %>% 
  mutate(depth2=case_when(
    depth=="E"  ~ "SW",
    depth=="M"  ~ "BW",
    depth=="S"  ~ "SW",
    depth=="B"  ~ "BW",
    TRUE        ~ "Cookies")) %>% 
  mutate(creek=str_sub(`Sample ID`,1,1))%>% 
  filter(creek=="H"|
           creek=="I"|
           creek=="S") 
calvert_mean<-calvert %>% 
  group_by(creek, depth2, Parameter) %>% 
  summarise(Result=mean(Result)) %>% 
  mutate(pat_trib="NO") %>% 
  filter(!c(Parameter=="DON"|
              Parameter=="NH4"|
              Parameter=="NO23"))
write_csv(calvert_mean,"calvert-means-2016.csv")

# Junk below here
pat_cal<-calvert_mean %>% 
  rename(depth=depth2) %>% 
  full_join(nasl,.)
pat_cal$creek <- factor(pat_cal$creek, levels = c("RC", "SC", "CC", "MB", "IH", "BC","H","I","S"))

ca_nest<-calvert_mean %>% 
  group_by(Parameter) %>% 
  nest() 
cal_plot<-ca_nest %>% 
  mutate(plot=map2(data, Parameter, ~ggplot(data=.x) +
                     ggtitle(.y)+
                     theme_classic()+
                     theme(axis.line = element_line(linetype = "solid"), 
                           axis.ticks = element_line(size = 1), 
                           panel.grid.major = element_line(colour = "gray80", 
                                                           linetype = "dotted"), panel.grid.minor = element_line(colour = "gray90", 
                                                                                                                 linetype = "dotted"), axis.title = element_text(size = 12), 
                           axis.text = element_text(size = 10), 
                           plot.title = element_text(size = 16, 
                                                     face = "bold"))+
                     geom_bar(aes(x=creek, y=Result, fill=depth), position="dodge", stat="identity")
  ))
cal_plot$plot[[2]]
