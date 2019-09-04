#####Packages to open#####
library(tidyverse)
library(lubridate)

#####created functions#####
data_load<-function(x){
  library(readxl)
  setwd("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data")
  target_file<-paste0("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/",x,".xlsx")
  read_excel(path = target_file)
}

site_id<-function(x){
  library(plyr)
  x$cr_ab<-as.factor(x$creek)
  x$cr_ab<-revalue(x$cr_ab, c("Middle Branch"= "MB",
                              "Inner Harbour"= "IH",
                              "Rock"="RC",
                              "Stoney"="SC",
                              "Bear"="BC",
                              "Curtis"="CC"))
  x$st_ab<-as.factor(x$station)
  x$st_ab<-revalue(x$st_ab, c("down"="D", "upper"="U", "middle"="M"))
  x$siteid<-paste(x$cr_ab, x$st_ab, sep="_")
  x<-subset(x, select =-c(cr_ab, st_ab))
}

#####load data#####
vertical_profile <- data_load("vertical_profile_data")

#####Create site id#####
vertical_profile<-site_id(vertical_profile)


#####Formatting Vertical Profiles for Plotting#####
vertical_profile2<-vertical_profile %>% 
  select(siteid,creek,station,date, boat_depth, sonde_depth,
         temperature,ODO_sat,ODO_conc,sp_cond_2018,sp_cond_2019,salinity,
         turbidity_ntu,`chl (ug/L)`) %>% #This gets rid of the weather stations and other data I'm not interested in right now
  gather(., key = parameter, value = measurement, temperature,ODO_sat,ODO_conc,
         sp_cond_2018,sp_cond_2019,salinity,turbidity_ntu,`chl (ug/L)`) #Converts the data from wide format to long format
#####Make NAs actually NAs#####
vertical_profile2<-vertical_profile2 %>% 
  naniar::replace_with_na(replace=list(measurement= "NA"))
vertical_profile2$measurement<-as.numeric(vertical_profile2$measurement)
vp<-vertical_profile2
vp$parameter<-revalue(vp$parameter, c("chl (ug/L)"="chl_ugL"))
vp$date<-ymd(vp$date)
vp<-vp %>% 
  mutate(year=year(date),
         month=month(date, label=TRUE))
vp

#####Select 2019 and temperature, oxygen and salinity only#####
vp2<-vp %>% 
  filter(!year==2018)%>% 
  mutate(filt=case_when(
    parameter=="temperature"    ~1,
    parameter=="salinity"       ~1,
    parameter=="ODO_conc"       ~1,
    TRUE                        ~0))%>%
  filter(filt==1) %>% 
  select(-filt)
vp_nest<-vp2 %>% 
  group_by(creek, year, month) %>% 
  nest()
vp_nest<-vp_nest %>% 
  mutate(plot = pmap(., ~ ggplot(data=..4, 
                               aes(x=measurement, y=sonde_depth, 
                                   color=station, shape=station))+
                       geom_point()+
                       geom_path(size=1)+
                       scale_color_manual(values=c("down" ="coral2",
                                                "middle"= "darkcyan",
                                                "upper"="mediumblue"))+
                       geom_hline(aes(yintercept = boat_depth+0.5), color="white")+
                       ggtitle(paste0(..1, " ~ ", ..3, " ", ..2))+
                       ylab("depth (m)")+
                       scale_y_reverse(expand = c(0, 0))+
                       scale_x_continuous(position = "top")+
                       theme_classic()+
                       theme(strip.text.x = element_text(size=10, face="bold"),
                         strip.background = element_rect(colour="black", fill="aliceblue"))+
                       facet_grid(~parameter, scales = "free")
    
  )
  )
vp_nest$plot[[2]]

#####make plots faceted by parameter and month!#####
vpnest_all<-vp2 %>% 
  group_by(creek, year) %>% 
  nest()
vpnest_all<-vpnest_all %>% 
  mutate(plot = pmap(., ~ ggplot(data=..3, 
                                 aes(x=measurement, y=sonde_depth, 
                                     color=station, shape=station))+
                       geom_point()+
                       geom_path(size=1)+
                       scale_color_manual(values=c("down" ="coral2",
                                                   "middle"= "darkcyan",
                                                   "upper"="mediumblue"))+
                       geom_hline(aes(yintercept = boat_depth+0.5), color="white")+
                       ggtitle(paste0(..1, " ~ ", ..2))+
                       ylab("depth (m)")+
                       scale_y_reverse(expand = c(0, 0))+
                       scale_x_continuous(position = "top")+
                       theme_classic()+
                       theme(strip.text.x = element_text(size=10, face="bold"),
                             strip.background = element_rect(colour="black", fill="aliceblue"))+
                       facet_grid(month~parameter, scales = "free_x")
                     
  )
  )
vpnest_all$plot[[5]]

#####When you are happy with the plots save them!#####
if(!dir.exists("./figures")){ #if a figures folder does not exist, create it.
  dir.create("./figures")
}
#use the map function with ggsave to save named figures. 
dir.create("./figures/temp_do_sal")
map2(paste0("./figures/temp_do_sal/", vpnest_all$creek, ".jpg"), vpnest_all$plot, ggsave)
map2(paste0("./figures/temp_do_sal/", vp_nest$creek,"_",vp_nest$month, ".jpg"), vp_nest$plot, ggsave)