#####Packages to open#####
library(tidyverse)

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
light_profile<-data_load("patapsco_light_profiles")

#####Create site id#####
vertical_profile<-site_id(vertical_profile)
light_profile<-site_id(light_profile)

#####Formatting Vertical Profiles for Plotting#####
vertical_profile2<-vertical_profile %>% 
  select(siteid,creek,station,date, boat_depth, sonde_depth,
         temperature,ODO_sat,ODO_conc,sp_cond,salinity,
         turbidity_ntu,`chl (ug/L)`) %>% 
  gather(., key = parameter, value = measurement, temperature,ODO_sat,ODO_conc,
         sp_cond,salinity,turbidity_ntu,`chl (ug/L)`)
vp<-vertical_profile2
vp$plot_id<-paste0(vp$siteid, "_",vp$parameter)
vp$measurement<-as.numeric(vp$measurement)
vnest<-vp %>% 
  group_by(plot_id) %>% 
  nest()
#####Making the Plots#####
v_plot<-vnest %>% 
  mutate(plot=map2(data, plot_id, ~ggplot(data=.x) +
                     ggtitle(.y)+
                     theme_classic()+
                     scale_y_reverse(expand = c(0, 0))+
                     scale_x_continuous(position = "top")+
                     ylab("sonde depth (m)")+
                     theme(axis.line = element_line(linetype = "solid"), 
                           axis.ticks = element_line(size = 1), 
                           panel.grid.major = element_line(colour = "gray80", 
                                                           linetype = "dotted"), panel.grid.minor = element_line(colour = "gray90", 
                                                                                                                 linetype = "dotted"), axis.title = element_text(size = 12), 
                           axis.text = element_text(size = 10), 
                           plot.title = element_text(size = 16, 
                                                     face = "bold"))+
                     geom_point(aes(x=measurement, y=sonde_depth),size=2, color="turquoise3")+
                     geom_path(aes(x=measurement, y=sonde_depth),size=1, color="turquoise3")+
                     geom_hline(aes(yintercept=boat_depth+0.2), color="white")))
v_plot$plot[[1]] #too look at one of the plots

if(!dir.exists("./figures")){ #if a figures folder does not exist, create it.
  dir.create("./figures")
}
#use the map function with ggsave to save named figures. 
dir.create("./figures/vp_plots")
map2(paste0("./figures/vp_plots/", v_plot$plot_id, ".jpg"), v_plot$plot, ggsave)
map2(paste0("./figures/vp_plots/", v_plot$plot_id, ".pdf"), v_plot$plot, ggsave)
