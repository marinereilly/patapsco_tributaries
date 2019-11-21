# Script to 1) separate interpolated data for metabolism calculations, 
# 2) run WtRegDO metabolism calculator, 3) plot metabolism data for analysis, 4) export data to csv file

#Install missing packages
install.packages('devtools')
library(devtools)
install_github('fawda123/WtRegDO')


# Load libraries and functions
library(WtRegDO)                #found on github.com, install_github('fawda123/WtRegDO')
library(doParallel)             #found on CRAN, used internal Install option
library(maptools)               #found on CRAN, used internal Install option
library(lubridate)              #found on CRAN, used internal Install option
library(ggplot2)                #found on CRAN, used internal Install option
registerDoParallel(cores = 7)   #7 cores taken from WtRegDO github site sample

#Load data if not running right after Interpolation.R
Dataread                    <- read.csv("RCCD2.csv",header=TRUE)

#Set date format of variable DateTimeStamp, pay attention to time zone
Dataread$DateTimeStamp    <- ymd_hms(Dataread$DateTimeStamp, tz = "EST")


#Filter out any timestamps with NA data
GoodData  <- na.omit(Dataread)
rm(Dataread)

# Location information
tz   <- 'EST'
lat  <- 39.14365        #Add rough lat of location for daylight calculations
long <- -76.51936       #Add rough long of location for daylight calculations

#use evalcor function to identify times when tidal and solar changes are not correlated
evalcor(GoodData, tz, lat, long, progress = TRUE)

#weighted regression, optimal window widths set from GitHub.com/fawda123/WtRegDO for testing
registerDoParallel(cores = detectCores() - 1)
wtreg  <- wtreg(GoodData, parallel = TRUE, wins = list(3, 1, 0.6), progress = TRUE, 
                        tz = tz, lat = lat, long = long)

#estimate ecosystem metabolism using observed DO time series
metab_obs  <- ecometab(wtreg, DO_var = 'DO_obs', tz = tz, 
                           lat = lat, long = long)

# estimate ecosystem metabolism using detided DO time series
metab_dtd  <- ecometab(wtreg, DO_var = 'DO_nrm', tz = tz, 
                           lat = lat, long = long)

#Plot metabolism observations
ggplot(metab_obs, aes(Date)) +
  labs(x = expression(), y = expression(paste("mmol  ", O[2], " ", m^-2, d^-1)), title = "Observational") +
  geom_line(aes(y = NEM, colour = "Neml")) + geom_point(aes(y = NEM, colour = "Nem"), size = 1) +
  geom_line(aes(y = Pg, colour = "Pgl")) + geom_point(aes(y = Pg, colour = "Pg"), size = 1) +
  geom_line(aes(y = Rt, colour = "Rtl")) + geom_point(aes(y = Rt, colour = "Rt"), size = 1) +
  scale_colour_manual("", breaks = c("Nem", "Pg","Rt"), values = c( "red", "red", "springgreen2", "springgreen2", "blue", "blue")) +
  theme(panel.background = element_rect(fill="white", colour = "black"), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_line(colour = "grey"))
ggsave("Observational.pdf", width = 8, height = 6, units = "in")

ggplot(metab_dtd, aes(Date)) +
  labs(x = expression(), y = expression(paste("mmol  ", O[2], " ", m^-2, d^-1)), title = "Detided") +
  geom_line(aes(y = NEM, colour = "Neml")) + geom_point(aes(y = NEM, colour = "Nem"), size = 1) +
  geom_line(aes(y = Pg, colour = "Pgl")) + geom_point(aes(y = Pg, colour = "Pg"), size = 1) +
  geom_line(aes(y = Rt, colour = "Rtl")) + geom_point(aes(y = Rt, colour = "Rt"), size = 1) +
  scale_colour_manual("", breaks = c("Nem", "Pg","Rt"), values = c( "red", "red", "springgreen2", "springgreen2", "blue", "blue")) +
  theme(panel.background = element_rect(fill="white", colour = "black"), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_line(colour = "grey"))
ggsave("Detided.pdf", width = 8, height = 6, units = "in")

##Exporting Data for further analysis
write.csv(GoodData,  file = "dataanalyzed.csv", row.names = FALSE)
write.csv(wtreg,     file = "regression.csv", row.names = FALSE)
write.csv(metab_obs, file = "metabobs.csv", row.names = FALSE)
write.csv(metab_dtd, file = "metabdtd.csv", row.names = FALSE)

#Run statistical summary
obsstats <- meteval(metab_obs)
dtdstats <- meteval(metab_dtd)

write.csv(obsstats, file = "obstats.csv", row.names = FALSE)
write.csv(dtdstats, file = "dtdstats.csv", row.names = FALSE)

#Save the workspace global environment for analysis without re-running regression
save.image(file = 'RCMetabolismWorkspace.RData')
