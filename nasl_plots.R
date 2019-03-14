library(tidyverse)

nasl <- read_csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/NASL/HARRIS_PATAPSCO_092418.csv")

nasl2<-nasl %>%
  select('Sample ID') 
nasl2$plotid<-nasl2$`Sample ID`
nasl2<-nasl2 %>% 
  separate('Sample ID', into = c("depth", "creek", "distance"),
           sep= c(2, 4))
nasl<-nasl %>% 
  select(plotid='Sample ID', Parameter, Result) %>% 
  full_join(., nasl2)
View(nasl)

nasl_nest<-nasl %>% 
  group_by(Parameter) %>% 
  nest()
nasl_plot<-nasl_nest %>% 
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
                     geom_bar(aes(x=creek, y=Result, fill=depth), position="dodge", stat="identity")+
                     facet_wrap(distance~., nrow=1)
  ))
                     
nasl_plot$plot[[2]] #too look at one of the plots

if(!dir.exists("./figures")){ #if a figures folder does not exist, create it.
  dir.create("./figures")
}
#use the map function with ggsave to save named figures. 
dir.create("./figures/nasl_plots")
map2(paste0("./figures/nasl_plots/", nasl_plot$Parameter, ".jpg"), nasl_plot$plot, ggsave)
map2(paste0("./figures/nasl_plots/", nasl_plot$Parameter, ".pdf"), nasl_plot$plot, ggsave)