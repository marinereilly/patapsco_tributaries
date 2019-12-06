vars<-levels(vp_nest2$parameter)
vars
vp_nest2$parameter<-as.factor(vp_nest2$parameter)

all_plots<-vp_nest2 %>% 
  select(siteid, parameter,data) %>% 
  map()


makeplots<-vp_nest2 %>% 
  pmap(siteid, parameter, ~ggplot(.data, aes(y=sonde_depth, x=measurement))+geom_point())

vp<-vertical_profile %>% 
  select(siteid, boat_depth,sonde_depth,temperature,ODO_sat,ODO_conc,salinity,pH,turbidity_ntu,`chl (ug/L)`)

dput(head(vp, n=18))


makeplots<-vp_nest2 %>% 
  mutate(plot=pmap(list(siteid,parameter, data), ~plot_fun()))

plot_fun<-function(df, siteid, parameter){
  df %>% 
    ggplot(aes(x=measurement, y=sonde_depth))+geom_point()
  +ggtitle(paste(siteid," ",parameter))+theme_minimal()
}

vp<-vertical_profile2
vp$plot_id<-paste0(vp$siteid, "_",vp$parameter)
vp$measurement<-as.numeric(vp$measurement)
vp<-vp %>% 
  group_by(plot_id) %>% 
  arrange(sonde_depth)
View(vp)
vnest<-vp %>% 
  group_by(plot_id) %>% 
  nest()
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
v_plot$plot[[1]]

if(!dir.exists("./figures")){ #if a figures folder does not exist, create it.
  dir.create("./figures")
}
#use the map function with ggsave to save named figures. 
dir.create("./figures/vp_plots")
map2(paste0("./figures/vp_plots/", v_plot$plot_id, ".jpg"), v_plot$plot, ggsave)
