library(NASL)
library(dplyr)
library(lubridate)
library(tidyr)

data1<-load_nasl("H://0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/NASL/HARRIS_PATAPSCO_052819_FINAL.xlsx")
data2<-load_nasl("H://0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/NASL/HARRIS_PATAPSCO_072419_FINAL.xlsx")
data3<-read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/NASL/HARRIS_PATAPSCO_092418.xlsx",
                  sheet = "Sheet1", skip = 23,
                  col_types = c("text", "date", "date", "text", "text",
                                            "date", "date", "text", "text", "numeric",
                                            "text", "text", "text", "text", "text")) %>% 
  drop_na(Result)  

stri_sub(data3$`Sample ID`,3,2)<-"-"
stri_sub(data3$`Sample ID`,6,1)<-"-"


df<-full_join(data1,data2) %>% 
  full_join(.,data3)
df<-mg_umol(df)

df_TN<-df %>% 
  filter(Parameter=="TDN"|Parameter=="PN") %>% 
  select(`Sample ID`,`Sample Date`,Parameter,umol) %>% 
  group_by(`Sample ID`, `Sample Date`) %>%
  pivot_wider(names_from = Parameter, values_from = umol) %>% 
  mutate(TN=TDN+PN)

df_TP<-df %>% 
  filter(Parameter=="TDP"|Parameter=="PP") %>% 
  select(`Sample ID`,`Sample Date`,Parameter,umol) %>% 
  group_by(`Sample ID`, `Sample Date`) %>%
  pivot_wider(names_from = Parameter, values_from = umol) %>% 
  mutate(TP=TDP+PP)
df_PC<-df %>% 
  filter(Parameter=="PC") %>% 
  select(`Sample ID`,`Sample Date`,Parameter,umol) %>% 
  group_by(`Sample ID`, `Sample Date`) %>%
  pivot_wider(names_from = Parameter, values_from = umol)

df_join<-df_TN %>% 
  full_join(., df_TP) %>% 
  full_join(., df_PC)

detach("package:stringr", unload=TRUE)

df_join<-df_join %>% 
  ungroup() %>% 
  separate(col=`Sample ID`, into = c("depth","creek","station"), sep="-") %>% 
  mutate(month=month(`Sample Date`, label=TRUE, abbr=TRUE),
         year=year(`Sample Date`), #%>%
         creek=plyr::revalue(creek, c("MB"="Middle Branch",
                                      "IH"="Inner Harbour",
                                      "RC"="Rock",
                                      "SC"="Stoney",
                                      "BC"="Bear",
                                      "CC"="Curtis")),
         depth=plyr::revalue(depth, c("SW"="surface",
                                      "BW"="bottom")),
         station=plyr::revalue(station, c("D"="down",
                                          "U"="upper",
                                          "M"="middle"))) %>% 
  mutate(my=paste0(month, " ",year)) %>% 
  select(-month, -year) %>% 
  select(creek, station, depth,my,`Sample Date`, TDN:PC)
