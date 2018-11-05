### Run all of the functions on the pat_functions Script
####Load data####
#load whatever data you are working with:
vertical_profile <- data_load("vertical_profile_data")
light_profile<-data_load("patapsco_light_profiles")
stn_wp<-data_load("station_waypoints")
tss<-data_load("patapsco_TSS")

#GPS Coordinates:
#aug2018rs<-gpx_load("2018_08_28_rock_stoney")
#aug2018mh<-gpx_load("2018_08_29_middle_harbor")
#aug<-gpx_load("aug2018")
####Light Profile Prep####
light_profile<-light_profile %>% 
  mutate(Iratio=(Iz/Io))

####Create Site ID####
vertical_profile<-site_id(vertical_profile)
light_profile<-site_id(light_profile)
####Test Graph to figure out formatting####
stoney_u<-vertical_profile %>% 
  filter(creek=="Stoney", station=="upper")
a<-ggplot(stoney_u, aes(x=ODO_conc, y=sonde_depth))
b<-a+
  geom_point(size=2, color="turquoise3")+
  geom_line(size=1, color="turquoise3")+
  scale_y_reverse(limits = c(3,0), expand = c(0, 0))+
  ylab("Depth (m)")+
  xlab("Dissolved Oxygen (mg/L)")+
  theme_classic()+
  scale_x_continuous(position = "top", expand = c(0, 0),
                     limits=c(0,9),breaks = seq(0, 9, by = 1))+
  ggtitle("Stoney Creek Upper Station")+ 
  theme(axis.line = element_line(linetype = "solid"), 
    axis.ticks = element_line(size = 1), 
    panel.grid.major = element_line(colour = "gray80", 
        linetype = "dotted"), panel.grid.minor = element_line(colour = "gray90", 
        linetype = "dotted"), axis.title = element_text(size = 12), 
    axis.text = element_text(size = 10), 
    plot.title = element_text(size = 16, 
        face = "bold"))
b

######Screwing Around?#####
vertical_profile2<-vertical_profile %>% 
  select(siteid,creek,station,date, boat_depth, target_depth,sonde_depth,
         temperature,ODO_sat,ODO_conc,sp_cond,salinity,
         turbidity_ntu,`chl (ug/L)`) %>% 
  gather(., key = parameter, value = measurement, temperature,ODO_sat,ODO_conc,
         sp_cond,salinity,turbidity_ntu,`chl (ug/L)`)
vp_nest2<-vertical_profile2 %>% 
  group_by(siteid,creek, station,date,parameter) %>% 
  nest()
vp_plots<-vp_nest2 %>% 
  mutate(plot=map(data, ~ggplot(data=.x, aes(x=measurement, y=sonde_depth))+geom_point(), 
                  filename = paste0(siteid, parameter,".pdf")) %>% 
           select(filename, plot))

View(vertical_profile2)  
####Basic Graphs####


station_nest<-vertical_profile %>% 
  group_by(siteid,creek, station) %>% 
  nest()

station_DO<-station_nest %>% 
  mutate(plot = map2(data, siteid, ~ggplot(data = .x)+
                       geom_point(aes(x=ODO_conc, y=sonde_depth), size=2, color="turquoise3")+
                       geom_path(aes(x=ODO_conc, y=sonde_depth), size=1, color="turquoise3")+
                       scale_y_reverse(limits = c(9,0), expand = c(0, 0),breaks = seq(0, 9, by = 1))+
                       ylab("Depth (m)")+
                       xlab("Dissolved Oxygen (mg/L)")+
                       theme_classic()+
                       scale_x_continuous(position = "top", expand = c(0, 0), limits=c(0,14), breaks = seq(0, 14, by = 1))+
                       ggtitle(.y)+ 
                       theme(axis.line = element_line(linetype = "solid"), 
                             axis.ticks = element_line(size = 1), 
                             panel.grid.major = element_line(colour = "gray80", 
                                                             linetype = "dotted"), panel.grid.minor = element_line(colour = "gray90", 
                                                                                                                   linetype = "dotted"), axis.title = element_text(size = 12), 
                             axis.text = element_text(size = 10), 
                             plot.title = element_text(size = 16, 
                                                       face = "bold"))))



