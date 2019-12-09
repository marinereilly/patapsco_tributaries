library(ggplot2)
library(dplyr)
library(tidyr)

pat_wide<-readRDS("patapsco_wide.rds")
#pat_wide<-pat_wide %>% 
  #rename(respiration=DO_conc_mg_Lhr) %>% 
  #select(creek,station,depth, date,my:mixed,Tchla:chl_tss)
#saveRDS(pat_wide,"patapsco_wide.rds")

pat_long<-pat_wide %>% 
  select(-mixed) %>% 
  pivot_longer(.,cols = kd:chl_tss,
               names_to="parameter",
               values_to="measurement")%>% 
  filter(!(parameter=="respiration" & depth=="surface"),
         !(parameter=="delta_sigmat" & depth=="bottom"))

pat_full<-pat_long 
pat_full$my<-factor(pat_full$my, 
                    levels = c("Aug 2018", "Sep 2018","May 2019","Jul 2019","Sep 2019"))
pat_full$station<-factor(pat_full$station,
                         levels = c("upper", "middle","down"))
pat_full$depth<-factor(pat_full$depth,
                       levels = c("surface","bottom"))

date1<-as.Date("2019-05-01")
date2<-as.Date("2019-09-30")
wqparameter<-"Achl"
creekpick<-"Middle Branch"

depth_pal<-c("surface"="goldenrod2", "bottom"="firebrick3")
station_pal<-c("down" ="coral2",
               "middle"= "darkcyan",
               "upper"="mediumblue")

coolplot<-pat_full %>% 
  filter(date>=date1) %>% 
  filter(date<=date2) %>%
  filter(parameter==wqparameter) %>% 
  #filter(creek==creekpick) %>% 
  ggplot(aes(x=creek, y=measurement, fill=my))+
  geom_col(position="dodge", color="black")+
  scale_fill_viridis_d()+
  ggtitle(paste0(creekpick, " ~ ", wqparameter))+
  facet_grid(depth~station)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.background = element_rect(fill = "white"),
        #panel.border = element_rect(linetype = "solid", fill=NA),
        strip.background = element_rect(colour = "black", fill = "white"),
        legend.title = element_blank())
a  
  