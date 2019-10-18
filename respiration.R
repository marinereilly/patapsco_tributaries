library(dplyr)
library(lubridate)
library(formattable)

library(readxl)
df <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/respiration_data.xlsx")
View(df)

df$datetime<-paste0(df$measured_date," ",df$measured_time)
df$datetime<-ymd_hm(df$datetime)
df$sampleid<-paste0(df$creek,"_", df$date_collected)

df_means<-df %>% 
  group_by(sampleid, datetime,type) %>%
  select(-salinity, -box_temp) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  arrange(sampleid, type)
View(df_means)


delta<-function(x){
  first(x)-last(x)
}

df_delta<-df_means %>% 
  select(sampleid, datetime, DO_conc, DO_percent, temperature) %>% 
  group_by(sampleid) %>% 
  summarise_all(delta)

df_delta$time_elapsed_hr<-as.numeric(df_delta$datetime, units="hours")
View(df_delta)  

df_rates<-df_delta
df_rates$DO_conc_mg_Lhr<-df_rates$DO_conc/df_rates$time_elapsed_hr
df_rates$DO_sat_hr<-df_rates$DO_percent/df_rates$time_elapsed_hr
df_rates$temp_change<-df_rates$temperature/df_rates$time_elapsed_hr

df_rates<-df_rates %>% 
  select(sampleid, time_elapsed_hr, DO_conc_mg_Lhr, DO_sat_hr)

View(df_rates)

formattable(df_rates,
            align= c("l", "c", "c", "r"),
            list(sampleid=formatter(
              "span", style= ~style(color = "grey", font.weight = "bold"))
            ))

#plot rates?
df_rates2<-df_rates %>% 
  separate(.,sampleid,into= c("creek","date_collected"), sep="_") %>% 
  mutate(date_collected=lubridate::ymd(date_collected)) %>% 
  mutate(month=lubridate::month(date_collected, label=TRUE))
pl<-df_rates2 %>% 
  ggplot(aes(x=creek, y=DO_conc_mg_Lhr, fill=month))
plo<-pl +
  geom_bar(stat="identity", position = "dodge", color="black")+
  theme_minimal()+
  ylab("Change in Dissolved Oxygen (mg/L hr-1)")+
  xlab("Creek")
plo
