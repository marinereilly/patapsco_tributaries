library(dplyr)
library(tidyr)
library(ggplot2)

chl <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/chlorophyll/chlorophyll calcs.xlsx")


chl$sampleid<-chl$Sample
chl<-chl %>% 
  separate(Sample, into = c("depth", "creek", "distance"),
           sep= "-")
chl<-chl[!is.na(chl$`Sample Date`),]
a<-ggplot(data=chl) +
  ggtitle("Act Chl")+
  theme_classic()+
  theme(axis.line = element_line(linetype = "solid"), 
        axis.ticks = element_line(size = 1), 
        panel.grid.major = element_line(colour = "gray80", 
                                        linetype = "dotted"), panel.grid.minor = element_line(colour = "gray90", 
                                                                                              linetype = "dotted"), axis.title = element_text(size = 12), 
        axis.text = element_text(size = 10), 
        plot.title = element_text(size = 16, 
                                  face = "bold"))+
  geom_bar(aes(x=creek, y=`Active Chl`, fill=depth), position="dodge", stat="identity")+
  facet_wrap(distance~., nrow=1)
a
ggsave("Chl.jpg")
ggsave("Chl.pdf")


tss <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/patapsco_TSS.xlsx",
sheet = "data_for_processing")

tss$Creek<-case_when(
  tss$creek==""
)

b<-ggplot(data=tss) +
  ggtitle("TSS")+
  theme_classic()+
  theme(axis.line = element_line(linetype = "solid"), 
        axis.ticks = element_line(size = 1), 
        panel.grid.major = element_line(colour = "gray80", 
                                        linetype = "dotted"), panel.grid.minor = element_line(colour = "gray90", 
                                                                                              linetype = "dotted"), axis.title = element_text(size = 12), 
        axis.text = element_text(size = 10), 
        plot.title = element_text(size = 16, 
                                  face = "bold"))+
  geom_bar(aes(x=Creek, y=tss, fill=depth), position="dodge", stat="identity")+
  facet_wrap(station~., nrow=1)
b
ggsave("tss.jpg")
ggsave("tss.pdf")