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
  x$siteid<-paste(x$cr_ab, x$st_ab, x$date, sep="_")
  x<-subset(x, select =-c(cr_ab, st_ab))
}

#####load data#####
light_profile<-data_load("patapsco_light_profiles")

#####Create site id#####
light_profile<-site_id(light_profile)

#####Formatting Vertical Profiles for Plotting#####

#####Formatting Light Profiles for Plotting by creek#####
light_profile2<-light_profile %>% 
  mutate(Iratio= Iz/Io) %>% #ratio of surface to light at depth
  select(siteid, creek, station, date, Iratio, z, boat_depth=depth) #selecting just the needed columns

light_profile2$plot_id<-paste0(light_profile2$creek, " ", light_profile2$date)

l_nest<-light_profile2 %>%
  select(-siteid) %>% 
  group_by(plot_id) %>% 
  nest()
#####make iratio plots#####
l_plots<-l_nest %>% 
  mutate(plot=map2(data, plot_id, ~ggplot(data=.x) +
                     ggtitle(.y)+
                     theme_classic()+
                     scale_y_reverse(expand = c(0, 0))+
                     scale_x_continuous(position = "top", limits = c(0,1))+
                     ylab("depth (m)")+
                     theme(axis.line = element_line(linetype = "solid"), 
                           axis.ticks = element_line(size = 1), 
                           panel.grid.major = element_line(colour = "gray80", 
                                                           linetype = "dotted"), panel.grid.minor = element_line(colour = "gray90", 
                                                                                                                 linetype = "dotted"), axis.title = element_text(size = 12), 
                           axis.text = element_text(size = 10), 
                           plot.title = element_text(size = 16, 
                                                     face = "bold"))+
                     geom_point(aes(x=Iratio, y=z, color=station),size=2)+
                     geom_path(aes(x=Iratio, y=z, color=station),size=1)+
                     geom_hline(aes(yintercept=1.75), color="white")))
l_plots$plot[[3]]

if(!dir.exists("./figures")){ #if a figures folder does not exist, create it.
  dir.create("./figures")
}
#use the map function with ggsave to save named figures. 
dir.create("./figures/lp_plots")
map2(paste0("./figures/lp_plots/", l_plots$plot_id, ".jpg"), l_plots$plot, ggsave)
map2(paste0("./figures/lp_plots/", l_plots$plot_id, ".pdf"), l_plots$plot, ggsave)

#####Trying to make kd equations work!!!#####
stat_kd<-function(df)
{
  library(plyr)
  library(broom)
  ddply(df,"siteid",
        function(u) {
          r <- nls(Iratio ~ a*exp(b*z), data=u, start=list(a=0.5, b=-2.5))
          tidy(r)%>% 
            select(-(std.error:p.value)) %>% 
            spread(term, estimate)})
}

pat_kd<-stat_kd(light_profile2)

k_plots<-pat_kd %>% 
  pmap(~(ggplot()+
           stat_function(aes(x=seq(0,5,0.01)),
                         fun = function(x,b) 1000*exp(b*x),
                         args = list(b =..3), color= "red", size = 1)+
           labs(title = paste0(..1,"_light"),
                subtitle = paste0("kd = ",..3))+
           xlab("Depth (m)")+
           ylab("Iz")+
           coord_flip()+
           scale_y_continuous(position = "bottom")+
           scale_x_reverse()+
           theme_classic()
  ))

#####Trying to make light plots with data and kd model prediction!###
lk_nest<-light_profile2 %>% 
  group_by(plot_id,boat_depth) %>% 
  nest() %>% 
  mutate(kd=map(data, ~stat_kd(.)))%>% 
  unnest(kd) %>% 
  select(plot_id,a,b,boat_depth)


lk_plots<-lk_nest %>% 
  pmap(~(ggplot()+
           stat_function(aes(x=seq(0,..4,0.01)),
                         fun = function(x,a,b) a*exp(b*x),
                         args = list(a=..2, b =..3), color= "red", size = 1)+
           labs(title = ..1,
                subtitle = paste0("kd = ",..3))+
           geom_point(data= ..5, aes(x=z, y=Iratio))+
           xlab("Depth (m)")+
           ylab("Iz")+
           coord_flip()+
           scale_y_continuous(position = "bottom")+
           scale_x_reverse()+
           theme_classic()
  ))

Myplots = lk_plots
# saves all flux regresssions in single pdf
pdf("pat_light_kd_plots.pdf")
Myplots
dev.off
