


data_load<-function(x){
  library(readxl)
  setwd("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data")
  target_file<-paste0("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/",x,".xlsx")
  read_excel(path = target_file)
}

gpx_load<-function(x){
  library(tmaptools)
  library(sf)
  target_file<-paste0("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/waypoints",x,".gpx")
  read_GPX(file = target_file, as.sf = TRUE)
}#Ignore this until the Stupid GPS works again...

plot_analyte<-function(df, analyte){
  library(ggplot2)
  ggplot(data = df, aes(x=analyte, y=depth))+
    geom_point()+
    geom_line()+
    scale_y_reverse()+
    ylab("Depth (m)")+xlab(as.character(analyte))
  
}
  
  
#LightProfiles<-mutate(LightProfiles, Iratio=(Iz/Io))
#nls.stats <- ddply(LightProfiles, "Id",
#                   function(u) {
#                     r <- nls(Iratio ~ a*exp(b*z), data=u, start=list(a=0.5, b=-2.5))
#                      c(coef(summary(r)))})
#colnames(nls.stats)<-c("Id", "a", "b", "a.err", "b.err", "a.tval", "b.tval", "a.Pr(>|t|)", "b.Pr(>|t|)")
#Kd<-nls.stats


("Middle Branch"= MB, "Inner Harbor"= IH, "Rock"=RC, "Stoney"=SC, "Bear"=BC,
          "Curtis"=CC, "down"=D, "upper"=U, "mid"=M)
  
vertical_profile$cr_ab<-as.factor(vertical_profile$creek)
vertical_profile$cr_ab<-revalue(vertical_profile$cr_ab, c("Middle Branch"= "MB", 
                                                          "Inner Harbour"= "IH", 
                                                          "Rock"="RC",
                                                          "Stoney"="SC", 
                                                          "Bear"="BC",
                                                          "Curtis"="CC"))
  
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

make_plot<-function(df, analyte){
  max_v<-max(df[[analyte]])
  max_d<-max(df$target_depth)
  a_lab<-as.character(analyte)
  df_nest<-df %>% 
    group_by(siteid,creek, station) %>% 
    nest()
  
  df_analyte<-df_nest %>% 
    mutate(plot = map2(data, siteid, ~ggplot(data = .x)+
                         geom_point(aes(x=analyte, y=sonde_depth), size=2, color="turquoise3")+
                         geom_path(aes(x=analyte, y=sonde_depth), size=1, color="turquoise3")+
                         scale_y_reverse(limits = c(max_d,0), expand = c(0, 0),
                                         breaks = seq(0, max_d, by = 1))+
                         ylab("Depth (m)")+
                         xlab(a_lab)+
                         theme_classic()+
                         scale_x_continuous(position = "top", expand = c(0, 0),
                                            limits=c(0,max_v), 
                                            breaks = seq(0, max_v, by = 1))+
                         ggtitle(.y)+ 
                         theme(axis.line = element_line(linetype = "solid"), 
                               axis.ticks = element_line(size = 1), 
                               panel.grid.major = element_line(colour = "gray80", 
                                                               linetype = "dotted"), 
                               panel.grid.minor = element_line(colour = "gray90",                                                                                                                     linetype = "dotted"), axis.title = element_text(size = 12), 
                               axis.text = element_text(size = 10), 
                               plot.title = element_text(size = 16, 
                                                         face = "bold"))))
  
}
if(!dir.exists("./figures")){ #if a figures folder does not exist, create it.
  dir.create("./figures")
}
#use the map function with ggsave to save named figures. 
dir.create("./figures/analyte")
map2(paste0("./figures/analyte/", df_analyte$siteid, ".pdf"), df_analyte$plot, ggsave)
