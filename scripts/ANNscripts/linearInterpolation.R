#### Will Barnett and Sarah Waldo, March 2018

### This script uses linear interpolation to fill gaps in the 
### driver/covariate time series used in the ANN

## Libraries
# install.packages("neuralnet", dependencies = TRUE)
library(reshape2)
library(neuralnet); library(ggplot2); library(suncalc); 
library(plyr); library(imputeTS); library(caret); library(nnet)
library(dplyr); library(zoo)
library(gmodels) #for ci function

### User-defined knobs for running different test versions:
#start and end of data set:
startdate<-"2017-01-01 00:00:00" 
enddate<-"2018-11-15 12:00:00" 

#can set runVer here if testing different driver sets
runVer<-"2020"

### Load data ------
#fluxDat<-read.csv("dataL2/annDataset_mdc_20190429.csv")
fluxDat<-EddyDataWithPosix.S
fluxDat$datetime <- as.POSIXct(fluxDat$RDateTime, tz = "Etc/GMT+5")

## There are duplicate rows for some reason. Get rid of them.
fluxDat <- subset(fluxDat, !duplicated(datetime))

#get PAR from the vanni weather station data frame, loaded from "scripts/readData.R" script
vanni30min$datetime<-vanni30min$RDateTime
vanni30min$staticPress.vws<-(vanni30min$bPress.vws+vanni30min$waterPressure.vws)/1000 #air pressure and water pressure are in Pa
# fluxDatFilled<-left_join(fluxDatFilled, select(vanni30min, datetime, par.vws),
#                          by="datetime")
fluxDat<-left_join(fluxDat, select(vanni30min, datetime, par.vws, waterT.vws, 
                                   airT.vws, windSp.vws, windDir.vws, staticPress.vws),
                         by="datetime")
##get sediment T from rbrTsub dataframe, loaded from "scripts/readData.R" script
rbrTsub$datetime<-rbrTsub$RDateTime

fluxDat<-left_join(fluxDat, select(rbrTsub, datetime, RBRmeanT_1.6),
                   by="datetime")
#sum(is.na(fluxDatFilled$par.vws))
sum(is.na(fluxDat$par.vws))
#fluxDatFilled$FilledPAR<-fluxDatFilled$par.vws
fluxDat$FilledPAR<-fluxDat$par.vws


fluxDat$FilledPAR<-fluxDat$par.vws
sum(is.na(fluxDat$par.vws))
# test5.2<-left_join(fluxDatFilled, select(vanni30min, datetime, par.vws), by="datetime")
# sum(is.na(test5.2$par.vws)) #0!!!
# fluxDatFilled$FilledPAR<-test5.2$par.vws

## Make date into Date class.

## Make RDateTime into POSIXct class
range(fluxDat$datetime)
fluxDat <-dplyr::filter(fluxDat, datetime>startdate, datetime<enddate)
fluxDat$date <- as.Date(fluxDat$datetime)


## Subset to variables that matter. Ignore gaps and date/time for now.
## sediment temp (RBRmeanT_1.6) ; static pressure (staticPress);
## change in staticP (difference of previous variable);
## u* (ustar); air temp (air_temperature)
# colsKeep <- c("ch4_flux", "date", "datetime","air_temperature", "ustar", "RBRmeanT_1.6",
#               "staticPress","par.vws","dailyRain.vws","waterT.vws",
#               "windDir.vws","windSp.vws","airT.vws","RH.vws","bPress.vws")
# fluxDat <- fluxDat[,colsKeep]
# ## Compute change in static pressure
# fluxDat$staticPressChg <- c(NA,diff(fluxDat$staticPress))


#######################################
######## Covariate Gap-Filling ########
#######################################

## Function to look at how many 30-minute gaps exist per day
plotGaps <- function(d, resp){
  
  # d <- fluxDat; resp = "RBRmeanT_1.6"
  dayGaps <- ddply(d, .(date), function(x){
    # x <- subset(fluxDat, date == "2017-02-01")
    return(data.frame("Gaps"=sum(is.na(x[,resp]))))
  })
  p <- ggplot(dayGaps, aes_string(x = "date", y = "Gaps")) + geom_bar(stat = "identity") +
    xlab("Date") + ylab("Number of 30-min Gaps per Day") + 
    ggtitle(paste("Daily Gap Plot for: ", resp, sep=""))
  return(p)
}

######## Sediment Temperature Gap-Filling
## Look at Water Temperature from U-Miami site
## compared to sediment temperature
plotGaps(fluxDat, "RBRmeanT_1.6")
ggplot(fluxDat, aes(x= waterT.vws, y = RBRmeanT_1.6)) + geom_point() +
geom_smooth(method="lm")
## The fit looks quite good.
sedLM <- lm(RBRmeanT_1.6 ~ waterT.vws, data = fluxDat)
summary(sedLM) # Very good r2
## Note: We're extrapolating sed temp when Water T is les than 3
## degrees Celsius. There are days in February where water temp is 1-C.
## Should sediment temp be predicted to be in the 5 range?
range(sedLM$model$waterT.vws)
## Use water temp to fill in sed temp
sedPreds <- predict(sedLM, 
                    newdata = data.frame("waterT.vws"=fluxDat$waterT.vws))
fluxDat$FilledSedT <- ifelse(is.na(fluxDat$RBRmeanT_1.6),
                               sedPreds, 
                               fluxDat$RBRmeanT_1.6)
sum(is.na(fluxDat$FilledSedT)) # 0
plotGaps(fluxDat, "FilledSedT")


######## Air Temperature Gap-Filling
plotGaps(fluxDat, "air_temperature")
sum(is.na(fluxDat$air_temperature))
## Use Miami-WS air temp, which is way more complete
sum(is.na(fluxDat$airT.vws))
## Note: the airT.vws data are in degrees-C, air_temperature is Kelvin.
## Doesn't really matter - it will get picked up in the model.
## Quick plot
#ggplot(fluxDat, aes(x = airT.vws, y = air_temperature)) + geom_point() +
#  geom_smooth(method="lm")
## The fit looks quite good.
airTLM <- lm(air_temperature ~ airT.vws, data = fluxDat)
summary(airTLM) # Very good r2 -- 0.9962
## Use Miami air temp to fill in eddy cov tower air temp
airPreds <- predict(airTLM, 
                    newdata = data.frame("airT.vws"=fluxDat$airT.vws))
fluxDat$FilledAirT <- ifelse(is.na(fluxDat$air_temperature),
                             airPreds, 
                             fluxDat$air_temperature)
sum(is.na(fluxDat$FilledAirT)) # Take the median
indNA <- which(is.na(fluxDat$FilledAirT))
fluxDat$FilledAirT[indNA] <- mean(c(fluxDat[(indNA-1),"FilledAirT"],fluxDat[(indNA+1),"FilledAirT"]))
sum(is.na(fluxDat$FilledAirT))
fluxDat$FilledAirT[indNA] <- mean(c(fluxDat[(indNA-48),"FilledAirT"],fluxDat[(indNA+48),"FilledAirT"])) #from surrounding days
sum(is.na(fluxDat$FilledAirT)) # 0
plotGaps(fluxDat, "FilledAirT")

######## Wind Speed Gap-Filling
## Wind speed column is wind_speed
## Miami weather station analog is windSp.vws
sum(is.na(fluxDat$wind_speed))
sum(is.na(fluxDat$windSp.vws))
## Quick plot
ggplot(fluxDat, aes(x = windSp.vws, y = wind_speed)) + geom_point() +
  geom_smooth(method="lm")
## A noisy fit. That's sort of expected.
windLM <- lm(wind_speed ~ windSp.vws, data = fluxDat)
## What is wind direction matters too?
windSpDirLM <- lm(wind_speed ~ windSp.vws + sin(pi * windDir.vws/180) +
                    cos(pi * windDir.vws/180), data = fluxDat)
summary(windLM) # OK R^2, not great -- 0.65
summary(windSpDirLM) # Slightly better -- 0.70
windPreds1 <- predict(windLM, newdata = data.frame("windSp.vws" = fluxDat$windSp.vws))
windPreds2 <- predict(windSpDirLM, newdata = data.frame("windSp.vws" = fluxDat$windSp.vws,
                                                        "windDir.vws" = fluxDat$windDir.vws))
df <- data.frame("Wind_Speed" = rep(fluxDat$wind_speed,2),
                 "Pred_Wind_Speed" = c(windPreds1, windPreds2),
                 "Model" = rep(c("Miami_Wind_Sp","Miami_Wind_Sp_And_Dir"), each = length(windPreds1)))
## Plot of the predictions
# ggplot(df, aes(x = Wind_Speed, y = Pred_Wind_Speed)) + geom_point() +
#   facet_wrap(facets = ~ Model) + geom_abline(slope = 1, intercept = 0, colour = "red")
## Use Miami wind speed and wind direction to fill in eddy flux tower wind speed
fluxDat$FilledWindSpeed <- ifelse(is.na(fluxDat$wind_speed),
                                  windPreds2,
                                  fluxDat$wind_speed)
sum(is.na(fluxDat$FilledWindSpeed)) # Take the median
indNA <- which(is.na(fluxDat$FilledWindSpeed))
fluxDat$FilledWindSpeed[indNA] <- mean(c(fluxDat[(indNA-48),"FilledWindSpeed"],fluxDat[(indNA+48),"FilledWindSpeed"]))
fluxDat$FilledWindSpeed[indNA]<-mean(fluxDat$FilledWindSpeed, na.rm=TRUE)
sum(is.na(fluxDat$FilledWindSpeed)) # 0
plotGaps(fluxDat, "FilledWindSpeed")

######## Static Pressure Gap-Filling
## There is a Miami weather station static pressure variable,
## staticPress.vws. Use that to build a model
#sum(is.na(fluxDat$staticPress))
sum(is.na(fluxDat$staticPress.vws))
plotGaps(fluxDat, "staticPress.vws")

fluxDat$FilledStaticPressure<-fluxDat$staticPress.vws

sum(is.na(fluxDat$FilledStaticPress)) # Take the median
indNA <- which(is.na(fluxDat$FilledStaticPress))
# Take the median
fluxDat$FilledStaticPress[indNA] <- mean(c(fluxDat[(indNA-1),"FilledStaticPress"],fluxDat[(indNA+1),"FilledStaticPress"]))
sum(is.na(fluxDat$FilledStaticPress)) # 0
plotGaps(fluxDat, "FilledStaticPress")

######## Static Pressure Change
## This is just the 1-lag difference from the static pressure
## column

fluxDat$FilledStaticPressChg <- c(NA, diff(fluxDat$FilledStaticPress))


####### Site Location
  fluxDat$FilledSite<-ifelse(fluxDat$datetime<"2018-04-18",
                                   1, #dock
                                   0) #aquatic tower


### Rename filled LE, H, and ustar columns
### fill any remaining gaps after mdc using the mean

### Latent heat flux (LE)
fluxDat$FilledLE<-fluxDat$LE_filled
sum(is.na(fluxDat$FilledLE)) #138 
indNA <- which(is.na(fluxDat$FilledLE))
#fluxDat$LE_filled[indNA] <- mean(c(fluxDat[(indNA-2),"LE_filled"],fluxDat[(indNA+2),"LE_filled"]))
plotGaps(fluxDat, "FilledLE") #big chunk in Jan/Feb 2017
plotGaps(filter(fluxDat, date<"2017-04-01"), "FilledLE")
summary(filter(fluxDat, datetime<"2017-03-01")) #mean = 16.2
fluxDat$FilledLE[indNA]<-16.3

### Sensible heat flux (H)  
sum(is.na(fluxDat$H_filled)) #140 
plotGaps(fluxDat, "H_filled")
fluxDat$FilledH<-fluxDat$H_filled
summary(filter(fluxDat, datetime<"2017-03-01")) #mean = -1.4
indNA <- which(is.na(fluxDat$H_filled))
fluxDat$FilledH<-fluxDat$H_filled
fluxDat$FilledH[indNA] <- -1.4 #median value
sum(is.na(fluxDat$FilledH)) 
summary(fluxDat$H_filled)  

tsTmp <- ts(fluxDat$H_filled)
#plotNA.distribution(tsTmp) #some gaps during tower resiting
ggplot_na_distribution(tsTmp)

### Friction velocity (ustar)
plotGaps(fluxDat, "ustar")
plotGaps(fluxDat, "ustar_filled") #no gaps
fluxDat$FilledUstar <- fluxDat$ustar_filled
sum(is.na(fluxDat$FilledUstar))
ggplot(filter(fluxDat, datetime>"2018-05-01 00:00:00"),
       aes(datetime, FilledUstar))+
  geom_point(alpha=0.1)


# fluxDatFilled<-read.csv("dataL2/annDataset_20190429.csv")
# fluxDatFilled$datetime <-as.POSIXct(fluxDatFilled$datetime, tz="UTC")
# 
# 
# fluxDatFilled<-left_join(fluxDatFilled, select(fuzzyRAD.df3, datetime, fuzzyRAD), by="datetime")
# sum(is.na(fluxDatFilled$fuzzyRAD))
# 
# fluxDat<-left_join(fluxDat, select(fluxDatFilled, datetime, FilledSedT, FilledAirT, FilledWindSpeed, 
#                                    FilledStaticPress, FilledStaticPressChg, FilledWD), by="datetime")
# fluxDatFilled<-left_join(fluxDatFilled, select(fluxDat, datetime, fuzzyRAD), by="datetime")
# #now just need LE, H, ustar, PAR




