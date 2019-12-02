creek_names<-function(x){
  df$x<-dplyr::recode(x, c("Middle Branch"= "MB",
                              "Inner Harbour"= "IH",
                              "Rock"="RC",
                              "Stoney"="SC",
                              "Bear"="BC",
                              "Curtis"="CC"))
}

station_names<-function(x){
  df$x<-dplyr::recode(x, c("down"="D", 
                              "upper"="U", 
                              "middle"="M"))
}

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

creek_labs<-c("MB"="Middle Branch",
                 "IH"="Inner Harbour",
                 "RC"="Rock",
                 "SC"="Stoney",
                 "BC"="Bear",
                 "CC"="Curtis")
station_labs<-c("D"="down", 
              "U"="upper", 
              "M"="middle")
depth_labs<-c("SW"="surface",
              "BW"="bottom")
