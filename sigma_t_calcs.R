library(dplyr)
library(lubridate)
library(tidyr)
library(readxl)

vp <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Patapsco/data/RAW_DATA/vertical_profile_data.xlsx")

vp_sb<-vp %>% 
  group_by(creek, station, date) %>% 
  mutate(sb=case_when(
    min(sonde_depth)==sonde_depth      ~ "surface",
    max(sonde_depth)==sonde_depth      ~ "bottom",
    TRUE                               ~ "NA"
  ))
sigmat<-vp_sb %>% 
  filter(sb!="NA") %>% 
  select(creek,station,date,sb, temperature,salinity)
sigmat<-sigmat %>% 
  mutate(spec_grav=999.842594+
           (0.06793952*temperature)+
           (-0.00909529*temperature^2)+
           (0.0001001685*temperature^3)+
           (-0.0000011201*temperature^4)+
           (0.0000000065*temperature^5)+
           ((0.824493)+
              (-0.0040899*temperature)+
              (0.000076438*temperature^2)+
              (-0.0000008247*temperature^3)+
              (0.0000000054*temperature^4))*salinity) %>% 
  mutate(sigma_t=spec_grav+
           ((-0.00572466)+(0.00010227*temperature)+
              (-0.0000016546*temperature^2))*
           salinity^1.5+
           (0.00048314)*salinity*salinity/(1-0)-
           1000) %>% 
  select(-temperature, -salinity, -spec_grav) %>% 
  pivot_wider(names_from=sb, values_from=sigma_t) %>% 
  mutate(delta_sigmat=bottom-surface) %>% 
  mutate(mixed=case_when(
    delta_sigmat>1.5     ~ "stratified",
    delta_sigmat<1.5     ~ "well-mixed"
  )) %>% 
  mutate(month=month(date, label = TRUE, abbr = TRUE), 
         year=year(date)) %>% 
  mutate(my=paste0(month, " ", year)) %>% 
  select(date,creek,station,delta_sigmat, mixed,my) %>% 
  ungroup() %>% 
  mutate(date=ymd(date)) %>% 
  mutate(creek=case_when(
    creek=="Inner harbour"   ~ "Inner Harbour",
    TRUE                     ~ as.character(creek)
  ))
saveRDS(sigmat, "sigmat.rds")
