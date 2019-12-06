library(dplyr)
library(ggplot2)
library(purrr)
library(lubridate)

library(readxl)
light_profiles <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/patapsco_light_profiles.xlsx")
View(light_profiles)

light_profiles<-light_profiles %>% 
  select(-time, -entered_by, -comments, -qa_by) %>% 
  mutate(., Iratio= Iz/Io,
         sample_id=paste0(creek," ", station, " ", date))

stat_kd<-function(df)
{
  library(plyr)
  library(tidyr)
  library(broom)
  ddply(df,"sample_id",
        function(u) {
          r <- nls(Iratio ~ a*exp(b*z), data=u, start=list(a=0.5, b=-2.5))
          tidy(r)%>% 
            select(-(std.error:p.value)) %>% 
            spread(term, estimate)})
}

light_profiles2<-stat_kd(light_profiles)

lp<-light_profiles2 %>% 
  full_join(light_profiles, .) %>% 
  select(-Io, -Iz)

lp$date<-ymd(lp$date) 
lp$month<-month(lp$date, label = TRUE)
lp$year<-year(lp$date)
  
lp<-lp %>% 
  mutate(., c_plotid=
           paste0(creek, "~", month," ",year), 
         st_plotid= paste0(station, "~", month," ",year)) %>% 
  select(-date, -month, -year, -sample_id)
lp<-lp %>% 
  select(-z,-Iratio) %>% 
  distinct()

#####Below is trying to auto plot these values####
lp_cr_nest<-lp %>% 
  select(-st_plotid,-creek) %>% 
  group_by(c_plotid) %>% 
  nest()



lp_st_nest<-lp %>% 
  select(-station, -c_plotid) %>% 
  group_by(st_plotid, creek, depth, a, b) %>% 
  nest()
#make plots
cr1<-lp_cr_nest %>% 
  filter(c_plotid=="Stoney~Aug 2018") %>% 
  unnest()
cr1<-cr1 %>% 
  select(1:4, kd=b)
kd_fun<-function(x,a,b) {
  a*exp(b*x)
  }
  
  
c<-cr1 %>% 
  ggplot()+
  stat_function(aes(x=seq(0,depth,0.01), color=station),
                fun = kd_fun,
                args = list(a= a, b = kd), 
                size = 1)+
  labs(title = c_plotid,
       subtitle = paste0("kd = ",kd))+
  xlab("Depth (m)")+
  ylab("Iz")+
  coord_flip()+
  scale_y_continuous(position = "bottom")+
  scale_x_reverse()+
  theme_classic()
c


lp_cr_plots<-lp_cr_nest %>% 
  mutate(plot=
  map2(data,c_plotid, ~(ggplot(data=.x)+
         stat_function(aes(x=seq(0,.x$depth,0.01), color=station),
                       fun = function(x,r,q) r*exp(q*x),
                       args = list(r= .x$a, q = .x$b), 
                       size = 1)+
         labs(title = .y,
              subtitle = paste0("kd = ",.x$b))+
         xlab("Depth (m)")+
         ylab("Iz")+
         coord_flip()+
         scale_y_continuous(position = "bottom")+
         scale_x_reverse()+
         theme_classic()
)))

lp_cr_plots$plot[[4]]

