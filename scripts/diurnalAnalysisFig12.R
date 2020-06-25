

#myWD<-"C:/R_Projects/actonFluxProject/"
pal<-wes_palette("Cavalcanti1", 5)  
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#setting up a dataframe for facet plots of weekly fluxes and drivers
#for getting a sense of trends on diurnal/daily scales
filledFluxDat<-read.csv("output/FluxDataWithFits.csv") #in the actonFluxProject directory
filledFluxDat$datetime<-as.POSIXct(filledFluxDat$datetime,
                                   format="%Y-%m-%d %H:%M:%S",
                                   tz="UTC")
weeklyD<-select(filledFluxDat,datetime)%>%
  group_by(datetime = cut(datetime, breaks = "1 week"))
weeklyD<-unique(weeklyD$datetime)
weeklyD<-as.Date(weeklyD)
#first go: important drivers in the ANN:
  #Filled LE
  #Filled SedT
  #Filled StaticPChange
###########these are in diurnal.df1------
filledSub<-select(filledFluxDat, datetime, FilledLE, FilledSedT, 
                  FilledStaticPressChg)

filled.g<-gather(filledSub, 'FilledLE',
                 'FilledSedT', 'FilledStaticPressChg', key="var", value="value")
filled.g$var2<-"driver"

fluxSub<-select(filledFluxDat, datetime, ch4_flux, ch4_preds5.1)
flux.g<-gather(fluxSub, 'ch4_flux', 'ch4_preds5.1', 
               key="var", value="value")
flux.g$var2<-ifelse(flux.g$var=="ch4_flux",
                    "measured",
                    "predicted")
mylist<-list()
mylist[[1]]<-flux.g
mylist[[2]]<-filled.g
diurnal.df1<-do.call("rbind", mylist)

diurnal.df1$var<-ifelse(diurnal.df1$var=="ch4_preds5.1",
                       "ch4_flux",
                       diurnal.df1$var)

pdf("/figures/diurnal1.pdf")
    #paper =  = "a4r") # landscape orientation
for (i in 1:(round(as.numeric(diff(range(filledFluxDat$datetime))))/7)) {  # each combination of site and lake
  startdate.i<-weeklyD[i]
  enddate.i<-weeklyD[i+1]
  data.i <- filter(diurnal.df1, datetime>startdate.i, datetime<=enddate.i)  # Pull out one week
  plot.i <- ggplot(data.i,  aes(x = datetime, y = value)) + #version w/o site identifier 
    geom_line(aes(color=as.factor(var2)))+
    facet_grid(var~.,
               scales="free")+
    geom_point(data=filter(data.i, var2=="measured"), aes(datetime, value),
               alpha=0.3, color="forest green", size=1)+
    #scale_color_brewer(type="div", palette=1, direction=1)+
    #scale_color_manual(values=cbPalette)+
    xlab("")
    #theme_bw()
   grid.arrange(plot.i, ncol = 1) # use to put two plots per page
}


dev.off() 
#########-------

#second go: dialing in to proximal/intuitive drivers
vanni30min<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/vanni30min.csv")
vanni30min$RDateTime<-as.POSIXct(vanni30min$RDateTime,
                                 format="%Y-%m-%d %H:%M:%S", tz="UTC")
campMet<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/campMet.csv")%>%
  mutate(RDateTime = as.POSIXct(RDateTime, tz="UTC"))

####PRECIP########
vanni30min<-vanni30min%>%
  mutate(rain30min = replace(rain30min, rain30min==-Inf, NA))
vanni30min$filledPrecip<-vanni30min$rain30min
vanni30min<-left_join(vanni30min, select(campMet, RDateTime, Rain_mm_tot), by="RDateTime")
for(i in 1:nrow(vanni30min)){
  vanni30min$filledPrecip[i] <- ifelse(is.na(vanni30min$filledPrecip[i]),
                                         vanni30min$Rain_mm_tot[i], 
                                         vanni30min$filledPrecip[i])}
# ggplot(vanni30min, aes(RDateTime, filledPrecip))+
#   geom_bar(stat="identity")+
#   geom_bar(data=vanni30min, aes(RDateTime, Rain_mm_tot), stat="identity", color="pink", alpha=0.3)+
#   ylim(0, 2)
# ggplot(filter(vanni30min, RDateTime>"2018-03-01", RDateTime<"2018-04-01"),
#        aes(RDateTime, filledPrecip))+
#   geom_bar(stat="identity")
# ggplot(filter(vanni30min, RDateTime>"2018-04-01", RDateTime<"2018-04-20"),
#        aes(RDateTime, Rain_mm_tot))+
#   geom_bar(stat="identity")

####RADIATION#######
campMet<-campMet%>%
  mutate(NR_Wm2_avg = replace(NR_Wm2_avg, NR_Wm2_avg< -1000, NA))
ggplot(campMet, aes(RDateTime, NR_Wm2_avg))+
  geom_line()
rad<-inner_join(select(campMet, RDateTime, NR_Wm2_avg), 
                select(vanni30min, RDateTime, par.vws),
                by="RDateTime")
ggplot(rad, aes(par.vws, NR_Wm2_avg))+
  geom_point(alpha=0.1)+
  geom_smooth(method="lm")

radLM<-lm(NR_Wm2_avg ~ par.vws, data=rad)
vanni30min<-left_join(vanni30min, select(rad, RDateTime, NR_Wm2_avg),
                      by="RDateTime")
  
summary(radLM) #r2 = 0.825
radPreds <- predict(radLM, 
                    newdata = data.frame("par.vws"=rad$par.vws))
vanni30min$FilledNR <- ifelse(is.na(vanni30min$NR_Wm2_avg),
                             radPreds, 
                             vanni30min$NR_Wm2_avg)
ggplot(vanni30min, aes(RDateTime, FilledNR))+
  geom_line(alpha=0.2)



#LE and met variables from epOutSubFilt (run qcEddyPro)
filledFluxDat$RDateTime<-filledFluxDat$datetime
filledFluxDat2<-left_join(filledFluxDat, select(epOutSubFilt, RDateTime, LE, air_pressure, wind_speed, ustar, e, es),
                          by="RDateTime")
filledFluxDat2<-left_join(filledFluxDat2, 
                          select(vanni30min, RDateTime, FilledNR, filledPrecip),
                          by="RDateTime")
filledFluxDat2$e_es<-filledFluxDat2$es-filledFluxDat2$e
filledFluxDat2$deltaT<-filledFluxDat2$FilledSedT-filledFluxDat2$FilledAirT+273.15
#Filled air T is in K -- convert to C
#filledSub2<-select(filledFluxDat2, RDateTime, FilledSedT,FilledAirT, FilledNR, air_pressure, wind_speed, filledPrecip, e_es)
filledSub2<-select(filledFluxDat2, RDateTime, deltaT, FilledNR, air_pressure, wind_speed, filledPrecip, e_es)

filled.g2<-gather(filledSub2, 
                 'deltaT', 'FilledNR', 'air_pressure', 
                 'wind_speed', 'filledPrecip', 'e_es',
                 key="var", value="value")
filled.g2$var2<-"driver"

fluxCSub<-select(filledFluxDat2, RDateTime, ch4_flux, ch4_preds5.1)
fluxC.g<-gather(fluxCSub, 'ch4_flux', 'ch4_preds5.1', 
               key="var", value="value")
fluxC.g$var2<-ifelse(fluxC.g$var=="ch4_flux",
                    "measured",
                    "predicted")
fluxLESub<-select(filledFluxDat2, RDateTime, LE, FilledLE)
fluxLE.g<-gather(fluxLESub, 'LE', 'FilledLE',
                 key="var", value="value")
fluxLE.g$var2<-ifelse(fluxLE.g$var=="LE",
                      "measured",
                      "predicted")

mylist2<-list()
mylist2[[1]]<-fluxC.g
mylist2[[2]]<-filled.g2
mylist2[[3]]<-fluxLE.g
diurnal.df2<-do.call("rbind", mylist2)

diurnal.df2$var<-ifelse(diurnal.df2$var=="ch4_preds5.1",
                        "ch4_flux",
                        diurnal.df2$var)
diurnal.df2$var<-ifelse(diurnal.df2$var=="FilledLE",
                        "LE",
                        diurnal.df2$var)
#put the variables in order
diurnal.df2$var<-ifelse(diurnal.df2$var == "ch4_flux", "a.ch4", diurnal.df2$var)
diurnal.df2$var<-ifelse(diurnal.df2$var == "LE", "b.LE", diurnal.df2$var)
diurnal.df2$var<-ifelse(diurnal.df2$var == "deltaT", "c.sedT-airT", diurnal.df2$var)
#diurnal.df2$var<-ifelse(diurnal.df2$var == "FilledSedT", "d.sedT", diurnal.df2$var)
diurnal.df2$var<-ifelse(diurnal.df2$var == "e_es", "e.e-es", diurnal.df2$var)
#diurnal.df2$var<-ifelse(diurnal.df2$var == "FilledNR", "e.netRad", diurnal.df2$var)
diurnal.df2$var<-ifelse(diurnal.df2$var == "air_pressure", "f.airP", diurnal.df2$var)
diurnal.df2$var<-ifelse(diurnal.df2$var == "wind_speed", "g.WS", diurnal.df2$var)
diurnal.df2$var<-ifelse(diurnal.df2$var == "filledPrecip", "h.rain", diurnal.df2$var)

diurnal.df3<-diurnal.df2
diurnal.df3$date<-diurnal.df3$RDateTime

pdf(file = paste(myWD,"/figures/diurnal2a_openair.pdf", sep=""))# ,paper == "a4r") # landscape orientation
for (i in 70:(round(as.numeric(diff(range(filledFluxDat$datetime))))/7)) {  # each combination of site and lake
  startdate.i<-weeklyD[i]
  enddate.i<-weeklyD[i+1]
  data1.i <- filter(diurnal.df2, RDateTime>startdate.i, RDateTime<=enddate.i)  # Pull out one week
   data2.i <- filter(diurnal.df3, RDateTime>startdate.i, RDateTime<=enddate.i, 
                     var == "a.ch4", var2 == "measured")
  plot.i<-ggplot(data1.i,  aes(x = RDateTime, y = value)) + #version w/o site identifier 
    geom_line(aes(color=as.factor(var2)))+
    facet_grid(var~.,
               scales="free")+
    geom_point(data=filter(data1.i, var2=="measured", var=="a.ch4"), aes(RDateTime, value),
               alpha=0.3, color="forest green", size=1)+
    geom_point(data=filter(data1.i, var2=="measured", var=="b.LE"), aes(RDateTime, value),
               alpha=0.3, color="forest green", size=1)+
    #scale_color_brewer(type="div", palette=1, direction=1)+
    #scale_color_manual(values=cbPalette)+
    xlab("")
  grid.arrange(plot.i) # use to put two plots per page
  #theme_bw()
    
    plot.ii<-timeVariation(data2.i, pollutant="value",
                                 statistic="median",
                                 normalise=FALSE)
    plot.iii<-plot(plot.ii, subset="hour")
  
}


dev.off() 

####new idea 7/2/2019, following Podjgrasjek et al., 2014, 
## they found diurnal cycles peaking at night, and used the 
## criteria of nighttime median values > 50% higher than daytime
## median values to say whether a given day had a diurnal cycle or not

##We have seen diurnal patterns with peaks during the day AND at night
## let's start with calendar day
epOutSubFiltGaps$ch4_flux_mgm2hr<-epOutSubFiltGaps$ch4_flux/1000*16*60*60
plotDay <- function(df, meanD, meanN, 
                    medD, medN, percMed, percMean){
  
  # # d <- fluxDat; resp = "RBRmeanT_1.6"
  # dayGaps <- ddply(d, .(date), function(x){
  #   # x <- subset(fluxDat, date == "2017-02-01")
  #   return(data.frame("Gaps"=sum(is.na(x[,resp]))))
  # })
  p <- ggplot(df, aes_string(x="RDateTime", y="ch4_flux_mgm2hr")) +
    geom_point(aes(color=as.factor(daytime_gf_round)))+
    scale_color_manual(values=c("#006699", "#33FF33"))+
    geom_hline(yintercept = c(meanD, meanN), 
               color=c("#33FF33", "#006699"))+
    geom_hline(yintercept = c(medD, medN),
               linetype="dashed",
               color=c("#33FF33", "#006699"))+
    labs(title=paste("Daytime Median CH4 is", round(percMed, 2), 
                     "% of Nighttime \nDaytime Mean CH4 is", round(percMean, 2), 
                     "% of Nighttime", sep=" "))+
    theme_minimal()+
    theme(legend.position = "none")
  p
  return(p)
}

plotDay(data.i, meanD.i, meanN.i,
        medD.i, medN.i, percDiffmed.i, percDiffmean.i)

n <- length(unique(epOutSubFiltGaps$date))
temp <- rep(NA, n)

OUT <- data.frame(date = temp, night_ch4_med = temp,day_ch4_med=temp,
                  night_ch4_mean=temp, day_ch4_mean=temp,
                  perc_diff_med=temp, perc_diff_mean=temp)
                  
pdf(file = paste(myWD,"/figures/diurnalQuant.pdf", sep=""),
    width=6,
    height=4)
for (i in 1:length(unique(epOutSubFiltGaps$date))) {  # each combination of site and lake
  date.i<-unique(epOutSubFiltGaps$date)[i]
  OUT[i, "date"]<-date.i
  data.i<-filter(epOutSubFiltGaps, date == date.i)
  
  pm.i<-filter(data.i, daytime_gf_round==0)
  day.i<-filter(data.i, daytime_gf_round==1)
  nObsN.i<-(24-sum(is.na(pm.i$ch4_flux)))
  nObsD.i<-(24-sum(is.na(day.i$ch4_flux)))
  
  medN.i = ifelse(nObsN.i>8, median(pm.i$ch4_flux_mgm2hr, na.rm=TRUE), NA)
  meanN.i = ifelse(nObsN.i>8, mean(pm.i$ch4_flux_mgm2hr, na.rm=TRUE), NA)
  medD.i = ifelse(nObsD.i>10, median(day.i$ch4_flux_mgm2hr, na.rm=TRUE), NA)
  meanD.i = ifelse(nObsD.i>10, mean(day.i$ch4_flux_mgm2hr, na.rm=TRUE), NA)
  percDiffmed.i<-(medD.i-medN.i)/medN.i*100
  percDiffmean.i<-(meanD.i-meanN.i)/meanN.i*100
  
  OUT[i, "night_ch4_med"]=medN.i
  OUT[i, "day_ch4_med"]= medD.i
  OUT[i, "perc_diff_med"]=percDiffmed.i
  OUT[i, "night_ch4_mean"]= meanN.i
  OUT[i, "day_ch4_mean"]= meanD.i
  OUT[i, "perc_diff_mean"]=percDiffmean.i
  

  
  if(nObsD.i>10){
        plot.i<- plotDay(data.i, meanD.i, meanN.i,
                 medD.i, medN.i, percDiffmed.i, percDiffmean.i)

    grid.arrange(plot.i)}
}

dev.off()

#proportion of days that had enough observation periods to 
#test for diurnal pattern at S1:
OUT$Rdate<-as.Date(OUT$date, format="%m/%d/%Y")
diurnal_stats<-OUT

write.table(diurnal_stats, 
            file=("C:/R_Projects/actonFluxProject/output/diurnalStats.csv"),
            sep=",",
            row.names=FALSE)

OUT_S1<-filter(OUT, Rdate<"2018-05-01")

S1_pm<-355/460 #77% NAs
S1_day<-328/460 #71% NAs
S1_all<-404/460 #88% NAs, so 12% coverage, 56 days

ggplot(OUT_S1, aes(perc_diff_med))+
  geom_histogram()+
  xlim(-200, 200)


OUT_S2<-filter(OUT, Rdate>"2018-05-01")
summary(OUT_S2)
S2_pm<-72/196 #38% NAs
S2_day<-58/196 #30% NAs
S2_all<-84/196 #43% NAs, so 57% coverage, 112 days


OUT$perc_diff<-(OUT$day_ch4-OUT$night_ch4)/OUT$night_ch4

summary(OUT$perc_diff)
OUT$Rdate<-as.Date(OUT$date, format="%m/%d/%Y")

ggplot(filter(OUT, Rdate>"2018-05-31", Rdate<"2018-08-01"), aes(Rdate, perc_diff))+
  geom_point(alpha=0.4)+
  geom_line()+
  ylim(-2, 2)

ggplot(data.i, aes(RDateTime, ch4_flux))+
  geom_point()

  startdate.i<-weeklyD[i]
  enddate.i<-weeklyD[i+1]
  data1.i <- filter(diurnal.df2, RDateTime>startdate.i, RDateTime<=enddate.i)  # Pull out one week
  data2.i <- filter(diurnal.df3, RDateTime>startdate.i, RDateTime<=enddate.i, 
                    var == "a.ch4", var2 == "measured")
  plot.i<-ggplot(data1.i,  aes(x = RDateTime, y = value)) + #version w/o site identifier 
    geom_line(aes(color=as.factor(var2)))+
    facet_grid(var~.,
               scales="free")+
    geom_point(data=filter(data1.i, var2=="measured", var=="a.ch4"), aes(RDateTime, value),
               alpha=0.3, color="forest green", size=1)+
    geom_point(data=filter(data1.i, var2=="measured", var=="b.LE"), aes(RDateTime, value),
               alpha=0.3, color="forest green", size=1)+
    #scale_color_brewer(type="div", palette=1, direction=1)+
    #scale_color_manual(values=cbPalette)+
    xlab("")
  grid.arrange(plot.i) # use to put two plots per page
  #theme_bw()
  
  plot.ii<-timeVariation(data2.i, pollutant="value",
                         statistic="median",
                         normalise=FALSE)
  plot.iii<-plot(plot.ii, subset="hour")
  
}



#######third go: chemistry data from vanni stream data collection 
###https://portal.edirepository.org/nis/mapbrowse?packageid=edi.256.1
  #Ammonia
  #nitrate
  #soluble reactive phosphorus
  #suspended solids

#still want LE

filledSub<-select(filledFluxDat, datetime, FilledLE)

filled.g<-gather(filledSub, 'FilledLE',
                 key="var", value="value")
filled.g$var2<-"driver"

fluxSub<-select(filledFluxDat, datetime, ch4_flux, ch4_preds5.1)
fluxSub<-fluxSub%>%
  mutate(ch4_flux = ch4_flux*60*60*16/1000,
         ch4_preds5.1 = ch4_preds5.1*60*60*16/1000)
flux.g<-gather(fluxSub, 'ch4_flux', 'ch4_preds5.1', 
               key="var", value="value")
flux.g$var2<-ifelse(flux.g$var=="ch4_flux",
                    "measured",
                    "predicted")
# chemSub<-(filter(dt4, DateTime>"2017-01-01"))
# chemSub$datetime<-chemSub$DateTime
# chemSub<-select(chemSub, -Site, -DateTime)
# chem.g<-gather(chemSub, 'Ammonia', 'Nitrate', 'SolubleReactivePhosphorus', 'SuspendedSolids',
#                key='var', value='value')
DailyMassDelivery<-DailyMassDelivery%>%
  mutate(datetime = date,
         value = inletNutrients)
chem.g<-as.data.frame(select(DailyMassDelivery, datetime, value, var))
chem.g<-select(chem.g, -date)
chem.g$var2<-"driver"
mylist<-list()
mylist[[1]]<-flux.g
mylist[[2]]<-filled.g
mylist[[3]]<-chem.g
diurnal.df3<-do.call("rbind", mylist)

diurnal.df3$var<-ifelse(diurnal.df3$var=="ch4_preds5.1",
                        "ch4_flux",
                        diurnal.df3$var)

pdf(paste(myWD,"/figures/diurnal3.pdf", sep=""))
#paper =  = "a4r") # landscape orientation
for (i in 1:(round(as.numeric(diff(range(filledFluxDat$datetime))))/7)) {  # each combination of site and lake
  startdate.i<-weeklyD[i]
  enddate.i<-weeklyD[i+1]
  data.i <- filter(diurnal.df3, datetime>startdate.i, datetime<=enddate.i)  # Pull out one week
  plot.i <- ggplot(data.i,  aes(x = datetime, y = value)) + #version w/o site identifier 
    geom_line(aes(color=as.factor(var2)), alpha=1)+
    facet_grid(var~.,
               scales="free")+
    geom_point(data=filter(data.i, var2=="driver", var!="FilledLE"), aes(datetime, value),
               alpha=0.5, color="red", size=1.5)+
    geom_point(data=filter(data.i, var2=="measured"), aes(datetime, value),
               alpha=0.3, color="forest green", size=1)+
    #scale_color_brewer(type="div", palette=1, direction=1)+
    #scale_color_manual(values=cbPalette)+
    xlab("")
  #theme_bw()
  grid.arrange(plot.i, ncol = 1) # use to put two plots per page
}


dev.off() 








#Example of low fluxes, no diurnal pattern:
ggplot(filter(epOutSubFilt, RDateTime>"2017-03-29", RDateTime<"2017-04-10"), 
       aes(RDateTime, ch4_flux))+
  # annotate("rect", xmin=as.POSIXct(as.Date("2019-05-24")),
  #          xmax=as.POSIXct(as.Date("2019-06-04")),
  #          ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line(alpha=0.5, color="red")+
  geom_point(alpha=0.1, color="red")+
  # scale_x_datetime(date_breaks = "4 days",
  #                  labels=date_format("%b %d"))+
  ylim(-0.1, 0.3)
#ylim(-10, 10)+
facet_grid(year~.)


noDlow<-filter(epOutSubFilt, RDateTime>"2017-03-29", RDateTime<"2017-04-10") 
noDlow$date<-noDlow$RDateTime
noDlow.p<-timeVariation(noDlow, pollutant="ch4_flux",
                        statistic="median", 
                        # xlab=c("hour", "hour of day, 2018",
                        #        "month", "weekday"),
                        normalise=FALSE)
plot(noDlow.p, subset="hour")


#Example of high fluxes, no diurnal pattern:
ggplot(filter(epOutSubFilt, RDateTime>"2018-05-25", RDateTime<"2018-06-04"), 
       aes(RDateTime, ch4_flux))+
  # annotate("rect", xmin=as.POSIXct(as.Date("2019-05-24")),
  #          xmax=as.POSIXct(as.Date("2019-06-04")),
  #          ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line(alpha=0.5, color="red")+
  geom_point(alpha=0.1, color="red")+
  # scale_x_datetime(date_breaks = "4 days",
  #                  labels=date_format("%b %d"))+
  ylim(-0.1, 1.5)

noDhigh<-filter(epOutSubFilt, RDateTime>"2018-05-25", RDateTime<"2018-06-04") 
noDhigh$date<-noDhigh$RDateTime
noDhigh.p<-timeVariation(noDhigh, pollutant="ch4_flux",
                        statistic="median", 
                        # xlab=c("hour", "hour of day, 2018",
                        #        "month", "weekday"),
                        normalise=FALSE)
plot(noDhigh.p, subset="hour")


#Example of diurnal pattern, in phase with LE:
#before and after spring burst

#Trying to collapse x-axis around the spring burst
#https://stackoverflow.com/questions/35511951/r-ggplot2-collapse-or-remove-segment-of-y-axis-from-scatter-plot
library(scales)
squish_trans <- function(from, to, factor) {
  
  trans <- function(x) {
    
    # get indices for the relevant regions
    isq <- x > from & x < to
    ito <- x >= to
    
    # apply transformation
    x[isq] <- from + (x[isq] - from)/factor
    x[ito] <- from + (to - from)/factor + (x[ito] - to)
    
    return(x)
  }
  
  inv <- function(x) {
    
    # get indices for the relevant regions
    isq <- x > from & x < from + (to - from)/factor
    ito <- x >= from + (to - from)/factor
    
    # apply transformation
    x[isq] <- from + (x[isq] - from) * factor
    x[ito] <- to + (x[ito] - (from + (to - from)/factor))
    
    return(x)
  }
  
  # return the transformation
  return(trans_new("squished", trans, inv))
}


epOutSubFilt<-epOutSubFilt%>%
  mutate(yesDhighInd = 0,
         BurstInd="",
         yesDhighInd = replace(yesDhighInd, RDateTime>"2018-05-22" & RDateTime<"2018-05-25", 1),
         BurstInd = replace(BurstInd, RDateTime>"2018-05-22" & RDateTime<"2018-05-25", "a. Pre-Burst"),
         yesDhighInd = replace(yesDhighInd, RDateTime>"2018-06-04" & RDateTime<"2018-06-06", 1),
         BurstInd = replace(BurstInd, RDateTime>"2018-06-04" & RDateTime<"2018-06-06", "b. Post-Burst"))

# squishStart<-as.numeric(epOutSubFilt$RDateTime[7000])
# squishEnd<-as.numeric(epOutSubFilt$RDateTime[7400])
# squishFactor<-60*60*24*3
# epOutSubFilt$timeNumeric<-as.numeric(epOutSubFilt$RDateTime)
# sum(is.na(epOutSubFilt$timeNumeric))

diurnalLE<-filter(epOutSubFilt, yesDhighInd==1, ch4_flux<1.25)

diurnalLEfit<-lm(ch4_flux~LE, data=diurnalLE)
summary(diurnalLEfit) #R2 = 0.56

#SI figure of CH4 flux as a function of LE
ggplot(filter(epOutSubFilt, yesDhighInd==1, ch4_flux<1.25),
       aes(LE, ch4_flux/1000*60*60*16))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x)+
  ylab(expression(F[CH4]~(mg~m^-2~hr^-1)))+
  xlab(expression(LE~(Wm^-2)))+
  theme_bw()+
  ggtitle("R^2=0.56")

ggsave(filename="plots/SI_ch4vsLE.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

# ggplot(filter(epOutSubFilt, yesDhighInd==1, ch4_flux<1.25),
#        aes(timeNumeric, ch4_flux))+
#   # annotate("rect", xmin=as.POSIXct(as.Date("2019-05-24")),
#   #          xmax=as.POSIXct(as.Date("2019-06-04")),
#   #          ymin=-Inf, ymax=Inf, alpha=0.5)+
#   geom_line(alpha=0.5, color="red")+
#   geom_line(alpha=0.5, color="red")+
#   geom_point(alpha=0.1, color="red")+
#   facet_grid(.~BurstInd)+
#   scale_x_continuous(trans = squish_trans(squishStart, squishEnd, squishFactor))
#                      #breaks = seq(-6, 6, by = 2)
#   # scale_x_datetime(date_breaks = "4 days",
#   #                  labels=date_format("%b %d"))+
#   #ylim(-0.1, 1.5)


#Figure 11 ingredients:
#ggplot(filter(epOutSubFilt, yesDhighInd==1, ch4_flux<1.25, BurstInd=="a. Pre-Burst"),
ggplot(filter(epOutSubFilt, RDateTime<"2018-05-24 20:30:00", RDateTime>"2018-05-22 09:30", ch4_flux<1.25),
       aes(RDateTime, ch4_flux/1000*60*60*16))+
  annotate("rect", xmin=as.POSIXct(("2018-05-22 20:52"), tz="UTC"), 
           xmax=as.POSIXct(("2018-05-23 06:18"), tz="UTC"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.POSIXct(("2018-05-23 20:53"), tz="UTC"), 
           xmax=as.POSIXct(("2018-05-24 06:17"), tz="UTC"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  # geom_smooth(span=0.2, se=FALSE)+
  # geom_line(alpha=0.6)+
  # geom_point(alpha=0.8, size=1.5)+
  geom_line(alpha=0.6, color = "blue")+
  geom_point(alpha=0.8, size=1.5, color = "blue")+
  ylab(expression(F[CH4]~(mg~CH[4]~m^-2~hr^-1)))+
  xlab("")+
  ylim(0, 70)+
  theme_bw()+
  scale_x_datetime(minor_breaks = c(as.POSIXct(("2018-05-23 00:00"), tz="UTC"),
                                     as.POSIXct(("2018-05-24 00:00"), tz="UTC"),
                                    as.POSIXct(("2018-05-25 00:00"), tz="UTC")),
                    #date_breaks = "24 hours",
                    breaks= c(as.POSIXct(("2018-05-22 12:00"), tz="UTC"),
                              as.POSIXct(("2018-05-23 12:00"), tz="UTC"),
                              as.POSIXct(("2018-05-24 12:00"), tz="UTC")),
                    labels=date_format("%Y-%m-%d %H:%M"))
ggsave(filename=paste0(projectWD, "/figures/diurnalFig11a.tiff"), #if this doesn't work, save with resolution of 450 x 450 
       width=5.25,height=3, units="in",
       dpi=800,compression="lzw")


ggplot(filter(epOutSubFilt, yesDhighInd==1, ch4_flux<1.25, BurstInd=="b. Post-Burst"),
       aes(RDateTime, ch4_flux/1000*60*60*16))+
  # annotate("rect", xmin=as.POSIXct(("2018-06-03 23:02"), tz="UTC"), 
  #          xmax=as.POSIXct(("2018-06-04 06:12"), tz="UTC"),
  #          ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.POSIXct(("2018-06-04 21:02"), tz="UTC"), 
           xmax=as.POSIXct(("2018-06-05 06:12"), tz="UTC"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.POSIXct(("2018-06-05 21:03"), tz="UTC"), 
           xmax=as.POSIXct(("2018-06-06 06:11"), tz="UTC"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  #geom_smooth(span=0.3, se=FALSE)+
  geom_line(color = "blue", alpha=0.6)+
  geom_point(color = "blue", alpha=0.8, size=1.5)+
  #ylab(expression(F[CH4]~(mg~m^-2~hr^-1)))+
  ylab("")+
  xlab("")+
  ylim(0, 70)+
  theme_bw()+
  scale_x_datetime(minor_breaks = c(as.POSIXct(("2018-06-05 00:00"), tz="UTC"),
                              as.POSIXct(("2018-06-06 00:00"), tz="UTC")),
                   #date_breaks = "24 hours",
                   breaks= c(as.POSIXct(("2018-06-04 12:00"), tz="UTC"),
                                   as.POSIXct(("2018-06-05 12:00"), tz="UTC")),
                   labels=date_format("%Y-%m-%d %H:%M"))
ggsave(filename="plots/diurnalFig11ai_noLoess.tiff", #if this doesn't work, save with resolution of 450 x 450 
       width=4.5,height=3, units="in",
       dpi=800,compression="lzw")

library(RColorBrewer)


#Panel c of Figure 11: aggregated diurnal patterns of Fch4 and LE
yesDhigh<-filter(epOutSubFilt, yesDhighInd==1, ch4_flux<1.25) 
yesDhigh$date<-yesDhigh$RDateTime
yesDhigh.p<-timeVariation(yesDhigh, pollutant=c("ch4_flux", "LE"),
                         statistic="mean", 
                         # xlab=c("hour", "hour of day, 2018",
                         #        "month", "weekday"),
                         normalise=TRUE,
                         cols=c("blue","dark grey"))
plot(yesDhigh.p, subset="hour", theme.legend=NULL)

ggsave(filename="plots/diurnalFig11c.tiff", #if this doesn't work, save with resolution of 450 x 450 
       width=3,height=3, units="in",
       dpi=800,compression="lzw")

#Example of diurnal pattern, out of phase with LE:
#before and after spring burst

epOutSubFilt<-epOutSubFilt%>%
  mutate(yesDhighInd = 0,
         yesDhighInd = replace(yesDhighInd, RDateTime>"2018-05-22" & RDateTime<"2018-05-25", 1),
         yesDhighInd = replace(yesDhighInd, RDateTime>"2018-06-04" & RDateTime<"2018-06-06", 1))

#inverse period
ggplot(filter(epOutSubFilt, RDateTime>"2018-08-26 10:00", RDateTime<"2018-08-31"), 
       aes(RDateTime, ch4_flux/1000*60*60*16))+
  annotate("rect", xmin=as.POSIXct(("2018-08-26 20:16"), tz="UTC"),
           xmax=as.POSIXct(("2018-08-27 07:02"), tz="UTC"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.POSIXct(("2018-08-27 20:17"), tz="UTC"),
           xmax=as.POSIXct(("2018-08-28 07:03"), tz="UTC"),
            ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.POSIXct(("2018-08-28 20:16"), tz="UTC"),
           xmax=as.POSIXct(("2018-08-29 07:04"), tz="UTC"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.POSIXct(("2018-08-29 20:14"), tz="UTC"),
           xmax=as.POSIXct(("2018-08-30 07:05"), tz="UTC"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.POSIXct(("2018-08-30 20:13"), tz="UTC"),
           xmax=as.POSIXct(("2018-08-31 06:06"), tz="UTC"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  # geom_smooth(span=0.2, se=FALSE)+
  # geom_line(alpha=0.6)+
  # geom_point(alpha=0.8, size=1.5)+
  geom_line(alpha=0.6, color="blue")+
  geom_point(alpha=0.8, size=1.5, color="blue")+
  ylab(expression(F[CH4]~(mg~CH[4]~m^-2~hr^-1)))+
  xlab("")+
  ylim(0, 70)+
  theme_bw()+
   scale_x_datetime(date_breaks = "24 hours",
                   labels=date_format("%Y-%m-%d %H:%M"))
ggsave(filename=paste0(projectWD, "/figures/diurnalFig11b.tiff"), #if this doesn't work, save with resolution of 450 x 450 
       width=9.5,height=3, units="in",
       dpi=800,compression="lzw")

  #ylim(-0.1, 1.5)
#copy to clipboard: 1125 x 450
#1125 350

