creek_names<-function(df, x){
  df$x<-dplyr::recode(df$x, c("Middle Branch"= "MB",
                              "Inner Harbour"= "IH",
                              "Rock"="RC",
                              "Stoney"="SC",
                              "Bear"="BC",
                              "Curtis"="CC"))
}

station_names<-function(df,x){
  df$x<-dplyr::recode(df$x, c("down"="D", 
                              "upper"="U", 
                              "middle"="M"))
}