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



x_values <- seq(0, 8.8, 0.01)

prepped <- lp %>% 
  dplyr::mutate(
    x_values = purrr::map(seq_along(nrow(.)), ~x_values),
    y_values = purrr::pmap(list(x_values, b), 
                           function(x_values, b) 1000*exp(b*x_values))) %>%
  #dplyr::select(-intercept, -l_plot_size) %>%
  tidyr::unnest(x_values, y_values) %>%
  dplyr::group_nest(c_plotid)

plots<-prepped %>% 
  mutate(plot = purrr::map2(prepped$c_plotid, prepped$data, 
            function(title, input){
              ggplot(input, aes(y = x_values, x = y_values, color = station)) +
                geom_line() +
                labs(title = title) +
                xlab("light") +
                ylab("depth") +
                scale_y_reverse()+
                scale_x_continuous(position = "top")+
                theme_classic()
            }))
plots$plot[[6]]
