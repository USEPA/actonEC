
######## CHAMBER DIFFUSIVE FLUXES ############
#do this first so that the total shallow and deep site daily fluxes 
#can be plotted in Figure 4

ggplot(filter(chamData, !is.na(year)), aes(monthday, co2.drate.mg.h.best))+
  geom_point(aes(color=siteID), alpha=0.8)+
  scale_x_datetime(breaks=date_breaks("6 weeks"),
                   labels=date_format("%d %b"),
                   name="Date")+
  facet_grid(year~.)
ggplot(filter(chamData, !is.na(year), siteID=="shallow W"), 
       aes(chmDeplyDtTm, ch4.drate.mg.h.best))+
  geom_line()+
  geom_point(aes(color=siteID), alpha=0.8, width=1.5*10^5)+
  scale_x_datetime(breaks=date_breaks("6 weeks"),
                   labels=date_format("%d %b"),
                   name="Date")+
  #facet_grid(year~.)+
  theme_bw()

chamData.d<-filter(chamData, siteID=="deep")
#chamData2018d<-filter(chamDataSub, year == "2018", siteID=="deep")
chamData.s<-filter(chamData, siteID=="shallow W")
#chamData2018s<-filter(chamDataSub, year == "2018", siteID=="shallow")

## average instances of >1 chamber measurement on the same day:
## for the deep site:
chamData.d<-chamData.d%>%
  mutate(Rdate=as.Date(chmDeplyDtTm))%>%
  group_by(Rdate)%>%
  mutate(co2.drate.mg.h.best=mean(co2.drate.mg.h.best),
         ch4.drate.mg.h.best=mean(ch4.drate.mg.h.best),
         chmDeplyDtTm=min(chmDeplyDtTm))

## and the shallow site:
chamData.s<-chamData.s%>%
  mutate(Rdate=as.Date(chmDeplyDtTm))%>%
  group_by(Rdate)%>%
  mutate(co2.drate.mg.h.best=mean(co2.drate.mg.h.best),
         ch4.drate.mg.h.best=mean(ch4.drate.mg.h.best),
         chmDeplyDtTm=min(chmDeplyDtTm))

# ggplot(chamData.d, aes(Rdate, ch4.drate.mg.h.best))+
#   geom_point()+
#   geom_line()
# chamData.d$date<-chamData.d$Rdate
# 
# ggplot(chamData.s, aes(Rdate, ch4.drate.mg.h.best))+
#   geom_point()+
#   geom_line()

## want to join chamber data with funnel data,
## gap-fill chamber data using linear interpolation
## then calculate a "total" emission for the U12 and U14 sites,
## eb+diff emissions
## want to do this with daily average eb data for Fig 2
## 2-hr eb data for cumulative, Fig 7
df12.gc$date<-as.Date(df12.gc$date)
df14.gc$date<-as.Date(df14.gc$date)      

DailyShalFluxes<-df14.gc %>%
  group_by(date) %>%
  dplyr::summarize(meanCH4Flux = (mean(ebCh4mgM2h, na.rm=TRUE)))
DailyShalFluxes<-DailyShalFluxes%>%
  mutate(date=as.POSIXct(DailyShalFluxes$date, tz="etc/GMT+5"),
         monthday = format(date, format="%m-%d %H:%M"))# %>%
DailyShalFluxes$monthday<-as.Date(DailyShalFluxes$monthday, format="%m-%d %H:%M")


DailyShalFluxes$Rdate <- as.Date(DailyShalFluxes$date)
DailyShalFluxes<-left_join(DailyShalFluxes, select(dailyEb14, dailyFracErr, Rdate),
                           by="Rdate")
DailyShalFluxes$Label<-"b) Shallow Site"
sum(is.na(DailyShalFluxes$meanCH4Flux)) #262

DailyDeepFluxes<-df12.gc %>%
  group_by(date) %>%
  dplyr::summarize(meanCH4Flux = (mean(ebCh4mgM2h, na.rm=TRUE)))
DailyDeepFluxes<-DailyDeepFluxes%>%
  mutate(date=as.POSIXct(DailyDeepFluxes$date, tz="etc/GMT+5"))
dailyEb12<-mutate(dailyEb12,
                  Rdate = date,
                  dailyFracErr = dailyVolEb2Err/dailyVolEb2)
DailyDeepFluxes$Rdate <- as.Date(DailyDeepFluxes$date)
DailyDeepFluxes<-left_join(DailyDeepFluxes, select(dailyEb12, dailyFracErr, Rdate),
                           by="Rdate")

DailyDeepFluxes$Label<-"c) Deep Site"
DailyDeepFluxes$Rdate<-as.Date(DailyDeepFluxes$date)

DailyDeepTfluxes<-left_join(DailyDeepFluxes, 
                            select(chamData.d, ch4.drate.mg.h.best, Rdate), by="Rdate")
DailyDeepTfluxes<-DailyDeepTfluxes %>%
  mutate(ch4.drate.interp = na.approx(ch4.drate.mg.h.best, rule=2),
         ch4.drate.interp = replace(ch4.drate.interp, Rdate>"2017-12-11" & Rdate<"2018-06-07", 0),
         meanCH4flux.interp = na.approx(meanCH4Flux, rule=2),
         ch4.trate = ch4.drate.interp+meanCH4flux.interp,
         ch4.trate = replace(ch4.trate, Rdate>"2018-01-01" & Rdate <"2018-05-01", 0),
         ch4.t_cumu = cumsum(ch4.trate)*24/1000,
         ch4.trate = replace(ch4.trate, Rdate>"2018-01-01" & Rdate <"2018-05-01", NA),
         ch4.dailyErr = meanCH4Flux*dailyFracErr,
         year = year(Rdate),
         monthday = format(Rdate, format="%m-%d %H:%M"))# %>%
DailyDeepTfluxes$monthday<-as.Date(DailyDeepTfluxes$monthday, format="%m-%d %H:%M")

#calculating 
df12.gc$Rdate<-as.Date(df12.gc$date)
df12Cham<-left_join(select(df12.gc, volEb2FE, ebCh4mgM2h, timeframe0.5, Rdate), #the two hour time window is a moving avg
                    select(chamData.d, ch4.drate.mg.h.best, Rdate), by="Rdate")

df12Cham<-df12Cham %>%
  mutate(ch4.drate.interp = na.approx(ch4.drate.mg.h.best, rule=2),
         ch4.drate.interp = replace(ch4.drate.interp, Rdate>"2017-12-11" & Rdate<"2018-06-07", 0),
         ch4.eb.interp = na.approx(ebCh4mgM2h, rule=2),
         ch4.trate = ch4.drate.interp+ch4.eb.interp,
         ch4.trate = replace(ch4.trate, Rdate>"2018-01-01" & Rdate <"2018-05-01", 0),
         ch4.t_cumu = cumsum(ch4.trate)/2/1000,
         ch4.trate = replace(ch4.trate, Rdate>"2018-01-01" & Rdate <"2018-05-01", NA),
         year = year(Rdate),
         monthday = format(Rdate, format="%m-%d %H:%M"),
         date = timeframe0.5,
         Label = "c) Deep Site")# %>%
df12Cham$monthday<-as.Date(df12Cham$monthday, format="%m-%d %H:%M")

ggplot(df12Cham, aes(Rdate, ch4.trate))+
  geom_line()

###ERROR
df12Cham<-mutate(df12Cham,
                 volEb2FE_zero = replace(volEb2FE, abs(volEb2FE)>10, 0),
                 volEb2FE_zero = replace(volEb2FE_zero, is.na(volEb2FE_zero), 0),
                 volEb2FE_zero = replace(volEb2FE_zero, is.nan(volEb2FE_zero), 0),
                 volEb2FE_zero = replace(volEb2FE_zero, is.infinite(volEb2FE_zero), 0),
                 ebCh4mgM2h_zero = replace(ebCh4mgM2h, is.na(volEb2FE), 0),
                 ebCh4mgM2h_zero = replace(ebCh4mgM2h_zero, is.na(ebCh4mgM2h_zero), 0),
                 ebCh4mgM2hErr_zero = ebCh4mgM2h_zero*volEb2FE_zero,
                 ch4.trate_zero = replace(ch4.trate, is.na(ch4.trate), 0),
                 #ch4.eb.cumu = cumsum(ebCh4mgM2h_zero)*2/1000,
                 ch4.eb.cumuUCI = cumsum(ch4.trate_zero+ebCh4mgM2hErr_zero)/2/1000,
                 ch4.eb.cumuLCI = cumsum(ch4.trate_zero-ebCh4mgM2hErr_zero)/2/1000)

ggplot(df12Cham, aes(Rdate, ch4.t_cumu))+
  geom_line()+
  geom_ribbon(aes(ymin=ch4.eb.cumuLCI,
                  ymax=ch4.eb.cumuUCI), alpha=0.2)


rowNum<-which(grepl("2018-01-01", df12Cham$Rdate))
value17<-df12Cham$ch4.t_cumu[rowNum[1]]
value17errU<-df12Cham$ch4.eb.cumuUCI[rowNum[1]]
value17errL<-df12Cham$ch4.eb.cumuLCI[rowNum[1]]

for(i in 1:nrow(df12Cham)){
  df12Cham$ch4.t_cumu_yr[i]<-ifelse(df12Cham$year[i]<2018,
                                    df12Cham$ch4.t_cumu[i],
                                    df12Cham$ch4.t_cumu[i] - value17)
  df12Cham$ch4.t_cumu_yr_L95[i]<-ifelse(df12Cham$year[i]<2018,
                                        df12Cham$ch4.eb.cumuLCI[i],
                                        df12Cham$ch4.eb.cumuLCI[i] - value17errL)
  df12Cham$ch4.t_cumu_yr_U95[i]<-ifelse(df12Cham$year[i]<2018,
                                        df12Cham$ch4.eb.cumuUCI[i],
                                        df12Cham$ch4.eb.cumuUCI[i] - value17errU)
}


DailyDeepTfluxes<-DailyDeepTfluxes%>%
  mutate(ch4.t_cumu_yr = replace(ch4.t_cumu_yr, monthday<"2019-05-01", NA),
         ch4.t_cumu_yr_L95=ch4.t_cumu_yr-ch4.cumuErr_yr,
         ch4.t_cumu_yr_U95=ch4.t_cumu_yr+ch4.cumuErr_yr
  )


####summer cumulative info for Table 2################
summer17DeepAFT<-cumsum(subset(DailyDeepTfluxes, Rdate>"2017-05-01"&
                                 Rdate<"2017-10-01")$meanCH4flux.interp)*24/1000 #units: g/m2
summer17DeepTot<-cumsum(subset(DailyDeepTfluxes, Rdate>"2017-05-01"&
                                 Rdate<"2017-10-01")$ch4.trate)*24/1000 #units: g/m2
summer17DateDeep<-subset(DailyDeepTfluxes, Rdate>"2017-05-01"&
                           Rdate<"2017-10-01")$Rdate
summer18DeepAFT<-cumsum(subset(DailyDeepTfluxes, Rdate>"2018-05-01"&
                                 Rdate<"2018-10-01")$meanCH4flux.interp)*24/1000 #units: g/m2
summer18DeepTot<-cumsum(subset(DailyDeepTfluxes, Rdate>"2018-05-01"&
                                 Rdate<"2018-10-01")$ch4.trate)*24/1000 #units: g/m2
summer18DateDeep<-subset(DailyDeepTfluxes, Rdate>"2018-05-01"&
                           Rdate<"2018-10-01")$Rdate
print(paste("Deep AFT Summer 2017 Avg FCH4:", 
            round(summer17DeepAFT[length(summer17DeepAFT)]/length(summer17DeepAFT)*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))
print(paste("Deep Tot Summer 2017 Avg FCH4:", 
            round(summer17DeepTot[length(summer17DeepTot)]/length(summer17DeepTot)*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))
print(paste("Deep AFT Summer 2018 Avg FCH4:", 
            round(summer18DeepAFT[length(summer18DeepAFT)]/length(summer18DeepAFT)*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))
print(paste("Deep Tot Summer 2018 Avg FCH4:", 
            round(summer18DeepTot[length(summer18DeepTot)]/length(summer18DeepTot)*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))
# combinedDate<-c(summerDate, summerDate)
# combinedData<-c(summerDeepTot, summerDeepAFT)
# combinedIndex<-c(rep("total", 145), rep("AFT", 145))

# testDF<-data.frame(combinedDate, combinedData, combinedIndex)
# 
# ggplot(testDF, aes(combinedDate, combinedData))+
#   geom_line(aes(color=combinedIndex))
############################

ggplot(DailyDeepTfluxes, aes(Rdate, ch4.trate))+
  geom_line()+
  geom_line(data=DailyDeepTfluxes, aes(Rdate, meanCH4Flux), color="red")

#shallow site
DailyShalFluxes$Rdate<-as.Date(DailyShalFluxes$date)

DailyShalTfluxes<-left_join(DailyShalFluxes, 
                            select(chamData.s, ch4.drate.mg.h.best, Rdate), by="Rdate")
DailyShalTfluxes<-DailyShalTfluxes %>%
  mutate(ch4.drate.interp = na.approx(ch4.drate.mg.h.best, rule=2),
         ch4.drate.interp = replace(ch4.drate.interp, Rdate>"2017-12-11" & Rdate<"2018-06-07", 0),
         meanCH4flux.interp=na.approx(meanCH4Flux, rule=2), 
         meanCH4flux.interp = replace(meanCH4flux.interp, Rdate>"2017-10-03" & Rdate<"2018-06-06", 0),
         ch4.trate = ch4.drate.interp+meanCH4flux.interp,
         ch4.t_cumu = cumsum(ch4.trate)*24/1000,
         ch4.trate = replace(ch4.trate, Rdate>"2018-01-01" & Rdate<"2018-05-01", NA),
         ch4.ecum = cumsum(meanCH4flux.interp)*24/1000, 
         ch4.dailyErr = meanCH4Flux*dailyFracErr,
         year = year(Rdate),
         monthday = format(Rdate, format="%m-%d %H:%M"))# %>%
DailyShalTfluxes$monthday<-as.Date(DailyShalTfluxes$monthday, format="%m-%d %H:%M")

df14.gc$Rdate<-as.Date(df14.gc$date)
df14Cham<-left_join(select(df14.gc, volEb2FE, ebCh4mgM2h, timeframe0.5, Rdate), #the two hour time window is a moving avg
                    select(chamData.s, ch4.drate.mg.h.best, Rdate), by="Rdate")

df14Cham<-df14Cham %>%
  mutate(ch4.drate.interp = na.approx(ch4.drate.mg.h.best, rule=2),
         ch4.drate.interp = replace(ch4.drate.interp, Rdate>"2017-12-11" & Rdate<"2018-06-07", 0),
         ch4.eb.interp = na.approx(ebCh4mgM2h, rule=2),
         ch4.trate = ch4.drate.interp+ch4.eb.interp,
         ch4.trate = replace(ch4.trate, Rdate>"2018-01-01" & Rdate <"2018-05-01", 0),
         ch4.t_cumu = cumsum(ch4.trate)/2/1000,
         ch4.trate = replace(ch4.trate, Rdate>"2018-01-01" & Rdate <"2018-05-01", NA),
         year = year(Rdate),
         Label = "b) Shallow Site",
         date = timeframe0.5,
         monthday = format(Rdate, format="%m-%d %H:%M"))# %>%
df14Cham$monthday<-as.Date(df14Cham$monthday, format="%m-%d %H:%M")





###ERROR
df14Cham<-mutate(df14Cham,
                 volEb2FE_zero = replace(volEb2FE, abs(volEb2FE)>10, 0),
                 volEb2FE_zero = replace(volEb2FE_zero, is.na(volEb2FE_zero), 0),
                 volEb2FE_zero = replace(volEb2FE_zero, is.nan(volEb2FE_zero), 0),
                 volEb2FE_zero = replace(volEb2FE_zero, is.infinite(volEb2FE_zero), 0),
                 ebCh4mgM2h_zero = replace(ebCh4mgM2h, is.na(volEb2FE), 0),
                 ebCh4mgM2h_zero = replace(ebCh4mgM2h_zero, is.na(ebCh4mgM2h_zero), 0),
                 ebCh4mgM2hErr_zero = ebCh4mgM2h_zero*volEb2FE_zero,
                 ch4.trate_zero = replace(ch4.trate, is.na(ch4.trate), 0),
                 #ch4.eb.cumu = cumsum(ebCh4mgM2h_zero)*2/1000,
                 ch4.eb.cumuUCI = cumsum(ch4.trate_zero+ebCh4mgM2hErr_zero)/2/1000,
                 ch4.eb.cumuLCI = cumsum(ch4.trate_zero-ebCh4mgM2hErr_zero)/2/1000)


ggplot(df14Cham, aes(Rdate, ch4.t_cumu))+
  geom_line()+
  geom_ribbon(aes(ymin=ch4.eb.cumuLCI,
                  ymax=ch4.eb.cumuUCI), alpha=0.2)

rowNum<-which(grepl("2018-01-01", df14Cham$Rdate))
value17<-df14Cham$ch4.t_cumu[rowNum[1]]
value17errU<-df14Cham$ch4.eb.cumuUCI[rowNum[1]]
value17errL<-df14Cham$ch4.eb.cumuLCI[rowNum[1]]

for(i in 1:nrow(df14.gc)){
  df14Cham$ch4.t_cumu_yr[i]<-ifelse(df14Cham$year[i]<2018,
                                    df14Cham$ch4.t_cumu[i],
                                    df14Cham$ch4.t_cumu[i] - value17)
  df14Cham$ch4.t_cumu_yr_U95[i]<-ifelse(df14Cham$year[i]<2018,
                                        df14Cham$ch4.eb.cumuUCI[i],
                                        df14Cham$ch4.eb.cumuUCI[i] - value17errU)
  df14Cham$ch4.t_cumu_yr_L95[i]<-ifelse(df14Cham$year[i]<2018,
                                        df14Cham$ch4.eb.cumuLCI[i],
                                        df14Cham$ch4.eb.cumuLCI[i] - value17errL)
}

ggplot(df14Cham, aes(Rdate, ch4.t_cumu_yr))+
  geom_line()+
  geom_ribbon(aes(ymin=ch4.t_cumu_yr_L95,
                  ymax=ch4.t_cumu_yr_U95), alpha=0.2)

ggplot(filter(DailyShalTfluxes, monthday>"2019-05-01"), aes(monthday, meanCH4Flux))+
  geom_point(alpha=0.3)+
  geom_errorbar(aes(ymin=meanCH4Flux-ch4.dailyErr,
                    ymax=meanCH4Flux+ch4.dailyErr))+
  facet_grid(year~.)

DailyShalTfluxes<-DailyShalTfluxes%>%
  mutate(ch4.dailyErr = replace(ch4.dailyErr, is.nan(ch4.dailyErr), NA),
         ch4.dailyErr = replace(ch4.dailyErr, ch4.dailyErr> 14, NA))

DailyShalTfluxes<-as.data.frame(DailyShalTfluxes)

DailyShalTfluxes$ch4.cumuErr[1]<-DailyShalTfluxes$ch4.dailyErr[1]

for(i in 2:nrow(DailyShalTfluxes)){
  DailyShalTfluxes$ch4.cumuErr[i]<-ifelse(!is.na(DailyShalTfluxes$ch4.dailyErr[i]),
                                          sqrt(DailyShalTfluxes$ch4.cumuErr[i-1]^2+
                                                 DailyShalTfluxes$ch4.dailyErr[i]^2),
                                          DailyShalTfluxes$ch4.cumuErr[i-1])
}

rowNum<-which(grepl("2018-01-01", DailyShalTfluxes$Rdate))
value17<-DailyShalTfluxes$ch4.t_cumu[rowNum]
value17err<-DailyShalTfluxes$ch4.cumuErr[rowNum]

for(i in 1:nrow(DailyShalTfluxes)){
  DailyShalTfluxes$ch4.t_cumu_yr[i]<-ifelse(DailyShalTfluxes$year[i]<2018,
                                            DailyShalTfluxes$ch4.t_cumu[i],
                                            DailyShalTfluxes$ch4.t_cumu[i] - value17)
  DailyShalTfluxes$ch4.cumuErr_yr[i]<-ifelse(DailyShalTfluxes$year[i]<2018,
                                             DailyShalTfluxes$ch4.cumuErr[i],
                                             DailyShalTfluxes$ch4.cumuErr[i] - value17err)
}



DailyShalTfluxes<-DailyShalTfluxes%>%
  mutate(ch4.t_cumu_yr = replace(ch4.t_cumu_yr, monthday<"2019-05-01", NA),
         ch4.t_cumu_yr_L95=ch4.t_cumu_yr-ch4.cumuErr_yr,
         ch4.t_cumu_yr_U95=ch4.t_cumu_yr+ch4.cumuErr_yr
  )


ggplot(DailyShalTfluxes, aes(Rdate, ch4.drate.interp))+
  geom_line()+
  geom_point(aes(Rdate, ch4.drate.mg.h.best))
#ylab("Cumulative Diffusive CH4 (g m-2)")

ggplot(filter(DailyShalTfluxes, Rdate>"2017-12-01"), aes(Rdate, meanCH4Flux))+
  geom_point()+
  geom_line(aes(Rdate, meanCH4flux.interp))

ggplot(DailyShalTfluxes, aes(Rdate, ch4.ecum))+
  geom_line()+
  ylab("Cumulative Diffusive CH4 (g m-2)")

ggplot(DailyShalTfluxes, aes(Rdate, ch4.trate))+
  geom_line()+
  geom_line(data=DailyShalTfluxes, aes(Rdate, meanCH4Flux), color="red")

#######summer cumulative info for Table 2#######

summer17ShalAFT<-cumsum(subset(DailyShalTfluxes, Rdate>"2017-05-01"&
                                 Rdate<"2017-10-01")$meanCH4flux.interp)*24/1000 #units: g/m2
summer17ShalTot<-cumsum(subset(DailyShalTfluxes, Rdate>"2017-05-01"&
                                 Rdate<"2017-10-01")$ch4.trate)*24/1000 #units: g/m2
summer17DateShal<-subset(DailyShalTfluxes, Rdate>"2017-05-01"&
                           Rdate<"2017-10-01")$Rdate
summer18ShalAFT<-cumsum(subset(DailyShalTfluxes, Rdate>"2018-05-01"&
                                 Rdate<"2018-10-01")$meanCH4flux.interp)*24/1000 #units: g/m2
summer18ShalTot<-cumsum(subset(DailyShalTfluxes, Rdate>"2018-05-01"&
                                 Rdate<"2018-10-01")$ch4.trate)*24/1000 #units: g/m2
summer18DateShal<-subset(DailyShalTfluxes, Rdate>"2018-05-01"&
                           Rdate<"2018-10-01")$Rdate

print(paste("Shal AFT Summer 2017 Avg FCH4:", 
            round(summer17ShalAFT[length(summer17ShalAFT)]/length(summer17ShalAFT)*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))
print(paste("Shal Tot Summer 2017 Avg FCH4:", 
            round(summer17ShalTot[length(summer17ShalTot)]/length(summer17ShalTot)*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))
print(paste("Shal AFT Summer 2018 Avg FCH4:", 
            round(summer18ShalAFT[length(summer18ShalAFT)]/length(summer18ShalAFT)*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))
print(paste("Shal Tot Summer 2018 Avg FCH4:", 
            round(summer18ShalTot[length(summer18ShalTot)]/length(summer18ShalTot)*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))

combinedDate<-c(rep(summer17DateDeep, 2), rep(summer17DateShal, 2), 
                rep(summer18DateDeep, 2), rep(summer18DateShal, 2))
combinedData<-c(summer17DeepTot, summer17DeepAFT, summer17ShalTot, summer17ShalAFT,
                summer18DeepTot, summer18DeepAFT, summer18ShalTot, summer18ShalAFT)
combinedSiteIndex<-c(rep("deep", 145), rep("deep", 145), 
                     rep("shal", 144), rep("shal", 144),
                     rep("deep", 152), rep("deep", 152),
                     rep("shal", 153), rep("shal", 153))
combinedMethodIndex<-c(rep("total", 145), rep("AFT", 145), 
                       rep("total", 144), rep("AFT", 144),
                       rep("total", 152), rep("AFT", 152),
                       rep("total", 153), rep("AFT", 153))

testDF<-data.frame(combinedDate, combinedData, combinedSiteIndex, combinedMethodIndex)
testDF$year<-year(testDF$combinedDate)
testDF$monthday<-format(testDF$combinedDate, format="%m-%d %H:%M")%>%
  as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC")

ggplot(testDF, aes(monthday, combinedData))+
  geom_line(aes(color=combinedSiteIndex, linetype=combinedMethodIndex))+
  facet_grid(.~year)+
  xlab("")+
  ylab(expression(Cumulative~Areal~Emissions~(g~CH[4]~m^-2)))

# chamData2017d<-subset(chamData2017d, !duplicated(Rdate))
# chamData2017d<-chamData2017d%>%
#   mutate(cumlCO2 = cumsum(co2.drate.mg.h.best),
#          cumlCH4 = cumsum(ch4.drate.mg.h.best),
#          elapsedHrs = c(NA, as.duration(diff(chmDeplyDtTm))/dhours(1)))
# 
# 
# chamData2017Test<-chamData2017%>%
#   group_by(siteID)
# mutate(elapsed=c(0, diff(as.numeric(chmDeplyDtTm))))
# 
# as.duration(diff(chamData2017d$chmDeplyDtTm))
#################################

#######ANN filled EP Info#########################################


epDataFilled<-annIN2

#epDataFilled<-read.csv("C:/R_Projects/actonFluxProject/output/fluxDataFilled6.01.csv")
#epDataFilled$datetime<-as.POSIXct(epDataFilled$datetime, tz="etc/GMT+5")
epDataFilled<-epDataFilled%>%
  mutate(date=as.Date(epDataFilled$datetime),
         year = year(datetime),
         monthday = format(datetime, format="%m-%d %H:%M"),
         iceInd = 0,
         iceInd = replace(iceInd, datetime>="2017-12-27" & datetime<="2018-01-10", 1),
         iceInd = replace(iceInd, datetime>="2018-01-13" & datetime<="2018-01-21", 1),
         iceInd = replace(iceInd, datetime>="2018-02-05" & datetime<="2018-02-15", 1))# %>%
epDataFilled$monthday<-as.Date(epDataFilled$monthday, format="%m-%d %H:%M")

epDataFilled$seasonInd<-if_else(epDataFilled$monthday<"2019-05-01" | epDataFilled$monthday>"2019-10-01",
                                1,
                                0)
ggplot(epDataFilled, aes(datetime, seasonInd))+
  geom_line()

epSeaonal<-epDataFilled%>%
  group_by(seasonInd)%>%
  dplyr::summarise(meanCH4Flux = mean(ch4_filled, na.rm=TRUE),
                   sdCH4Flux = sd(ch4_filled, na.rm=TRUE),
                   nch4 = n(),
                   seCH4Flux = sdCH4Flux/sqrt(nch4))
##calculating mean flux during ice cover:
mean(subset(epDataFilled, iceInd==1)$ch4_flux, na.rm=TRUE)*60*60*16/1000
sd(subset(epDataFilled, iceInd==1)$ch4_flux, na.rm=TRUE)*60*60*16/1000

##warm season 2017:
mean(subset(epDataFilled, datetime>="2017-05-01" & datetime<"2017-10-01")$ch4_flux, na.rm=TRUE)*60*60*16/1000
sd(subset(epDataFilled, datetime>="2017-05-01" & datetime<"2017-10-01")$ch4_flux, na.rm=TRUE)*60*60*16/1000

##warm season 2018:
mean(subset(epDataFilled, datetime>="2018-05-01" & datetime<"2018-10-01")$ch4_flux, na.rm=TRUE)*60*60*16/1000
sd(subset(epDataFilled, datetime>="2018-05-01" & datetime<"2018-10-01")$ch4_flux, na.rm=TRUE)*60*60*16/1000

DailyANNFluxes<-epDataFilled %>%
  group_by(date) %>%
  dplyr::summarize(ch4.trate = (mean(ch4_filled, na.rm=TRUE))/1000*16*60*60,
                   ch4.trateU95 = (mean(ch4_U95, na.rm=TRUE))/1000*16*60*60,
                   ch4.trateL95 = (mean(ch4_L95, na.rm=TRUE))/1000*16*60*60)
#year = year(datetime),

#sdCH4Flux = (sd(ch4_flux, na.rm=TRUE)/1000*16*60*60),
#meanANNch4Flux = (mean(ch4_preds6.01, na.rm=TRUE)/1000*16*60*60))
DailyANNFluxes<-DailyANNFluxes%>%
  mutate(date=as.POSIXct(DailyANNFluxes$date, tz="etc/GMT+5"),
         year = year(date),
         ch4.t_cumu = cumsum(ch4.trate)*24/1000,
         ch4.t_cumuU95 = cumsum(ch4.trateU95)*24/1000,
         ch4.t_cumuL95 = cumsum(ch4.trateL95)*24/1000,
         monthday = format(date, format="%m-%d %H:%M"))# %>%
DailyANNFluxes$monthday<-as.Date(DailyANNFluxes$monthday, format="%m-%d %H:%M")

rowNum<-which(grepl("2018-01-01", DailyANNFluxes$date))
value17<-DailyANNFluxes$ch4.t_cumu[rowNum]
value17L<-DailyANNFluxes$ch4.t_cumuL95[rowNum]
value17U<-DailyANNFluxes$ch4.t_cumuU95[rowNum]

ws2017<-DailyANNFluxes$ch4.t_cumu[which(grepl("2017-10-01",DailyANNFluxes$date))]-
  DailyANNFluxes$ch4.t_cumu[which(grepl("2017-05-01",DailyANNFluxes$date))]
ws2017L<-DailyANNFluxes$ch4.t_cumuL95[which(grepl("2017-10-01",DailyANNFluxes$date))]-
  DailyANNFluxes$ch4.t_cumuL95[which(grepl("2017-05-01",DailyANNFluxes$date))]
ws2017U<-DailyANNFluxes$ch4.t_cumuU95[which(grepl("2017-10-01",DailyANNFluxes$date))]-
  DailyANNFluxes$ch4.t_cumuU95[which(grepl("2017-05-01",DailyANNFluxes$date))]
ndays<-as.numeric(DailyANNFluxes$date[which(grepl("2017-10-01",
                                                  DailyANNFluxes$date))]-
                    DailyANNFluxes$date[which(grepl("2017-05-01",
                                                    DailyANNFluxes$date))])
ws2018<-DailyANNFluxes$ch4.t_cumu[which(grepl("2018-10-01",DailyANNFluxes$date))]-
  DailyANNFluxes$ch4.t_cumu[which(grepl("2018-05-01",DailyANNFluxes$date))]
ws2018L<-DailyANNFluxes$ch4.t_cumuL95[which(grepl("2018-10-01",DailyANNFluxes$date))]-
  DailyANNFluxes$ch4.t_cumuL95[which(grepl("2018-05-01",DailyANNFluxes$date))]
print(paste("Mean Warm Season Fluxes 2017", 
            (ws2018/ndays)*1000/24, sep=""))
print(paste("L95 Warm Season Fluxes 2017", 
            (ws2018L/ndays)*(1000/24), sep=""))

for(i in 1:nrow(DailyANNFluxes)){
  DailyANNFluxes$ch4.t_cumu_yr[i]<-ifelse(DailyANNFluxes$year[i]<2018,
                                          DailyANNFluxes$ch4.t_cumu[i],
                                          DailyANNFluxes$ch4.t_cumu[i] - value17)
  DailyANNFluxes$ch4.t_cumu_yr_U95[i]<-ifelse(DailyANNFluxes$year[i]<2018,
                                              DailyANNFluxes$ch4.t_cumuU95[i],
                                              DailyANNFluxes$ch4.t_cumuU95[i] - value17U)
  DailyANNFluxes$ch4.t_cumu_yr_L95[i]<-ifelse(DailyANNFluxes$year[i]<2018,
                                              DailyANNFluxes$ch4.t_cumuL95[i],
                                              DailyANNFluxes$ch4.t_cumuL95[i] - value17L)
}


DailyANNFluxes$Label<-"a) Eddy Covaraince"

ggplot(DailyANNFluxes, aes(date, ch4.t_cumu))+
  geom_line()

maxVal<-max(DailyANNFluxes$ch4.trate)
rowNum<-which(grepl(maxVal, DailyANNFluxes$ch4.trate))
DailyANNFluxes[515,]

sb2017<-DailyANNFluxes[145:156,]
mean(sb2017$ch4.trate)
sd(sb2017$ch4.trate)
##########################

DailyANNF4<-select(DailyANNFluxes, date, ch4.trate, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)
ShalSiteF4<-select(df14Cham, date, ch4.trate, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)
#ShalSiteF4<-select(DailyShalTfluxes, date, ch4.trate, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)
DeeptSiteF4<-select(df12Cham, date, ch4.trate, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)
#DeeptSiteF4<-select(DailyDeepTfluxes, date, ch4.trate, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)


########GRTS info#############


epDataFilled$ch4_flux.f3<-epDataFilled$ch4_flux*60*60*16/1000
epDataFilled$ch4_flux_filled.f3<-epDataFilled$ch4_filled*60*60*16/1000
epDataFilled$Label<-"a) Eddy Covaraince"

epDataFilledF3<-select(epDataFilled, datetime, Label, ch4_flux.f3, ch4_flux_filled.f3)

df12.gc$ch4_flux.f3<-df12.gc$ebCh4mgM2h
df12.gc$ch4_flux_filled.f3<-df12.gc$ebCh4mgM2h
df12.gc$datetime<-df12.gc$date.timeHH
df12.gc$Label<-"c) Deep Site"

deepAFTf3<-select(df12.gc, datetime, Label, ch4_flux.f3, ch4_flux_filled.f3)

df14.gc$ch4_flux.f3<-df14.gc$ebCh4mgM2h
df14.gc$ch4_flux_filled.f3<-df14.gc$ebCh4mgM2h
df14.gc$datetime<-df14.gc$date.timeHH
df14.gc$Label<-"b) Shallow Site"

shalAFTf3<-select(df14.gc, datetime, Label, ch4_flux.f3, ch4_flux_filled.f3)

#adding GRTS data -- need to run GRTS code
file.edit('scriptsAndRmd/GRTS/masterScriptActonGRTS.R')
meanVariance.c<-meanVariance.c%>%
  mutate(date = as.POSIXct(meanVariance.c$deplyDt, format="%m/%d/%Y", tz="UTC"),
         Label = "d) Lake Surveys",
         ch4.trate.FE = ch4.trate.mg.h_StdError/ch4.trate.mg.h_Estimate)

ggplot(meanVariance.c,
       aes(Lake_Name, ch4.erate.mg.h_Estimate))+
  geom_point()+
  geom_errorbar(aes(ymax = ch4.erate.mg.h_UCB95Pct,
                    ymin = ch4.erate.mg.h_LCB95Pct))+
  #ylim(0, 15)+
  #xlim("Acton Lake 07", "Acton Lake 08")+
  labs(x="", y=expression(CH[4]~Ebullition~(mg~CH[4]~m^{-2}~hr^{-1})))

grts_ts<-left_join(select(DailyANNF4, date), 
                   select(meanVariance.c, date, Label, ch4.drate.mg.m2.h_Estimate, ch4.trate.mg.h_Estimate, ch4.erate.mg.h_Estimate,
                          ch4.trate.FE),
                   by="date")
rowNum<-which(grepl("2017-07-10", grts_ts$date))

#setup
grts_ts<-grts_ts%>%
  mutate(ch4.d_gf = ch4.drate.mg.m2.h_Estimate,
         ch4.e_gf = ch4.erate.mg.h_Estimate,
         ch4.t_gf = ch4.trate.mg.h_Estimate)

#diffusive
grts_ts<-grts_ts%>%
  mutate(ch4.d_gf = replace(ch4.d_gf, date>"2017-05-01" & date<"2017-07-10", grts_ts$ch4.d_gf[which(grepl("2017-07-10", grts_ts$date))]),
         ch4.d_gf = replace(ch4.d_gf, date>"2018-05-01" & date<"2018-07-10", grts_ts$ch4.d_gf[which(grepl("2018-07-10", grts_ts$date))]),
         ch4.d_gf = replace(ch4.d_gf, date>"2018-09-19" & date<"2018-10-01", grts_ts$ch4.d_gf[which(grepl("2018-09-19", grts_ts$date))]),
         ch4.d_gf = replace(ch4.d_gf, date<"2017-05-01", 0),
         ch4.d_gf = replace(ch4.d_gf, date>"2017-10-04" & date<"2018-05-01", 0),
         ch4.d_gf = replace(ch4.d_gf, date>"2018-10-01", 0),
         ch4.d_gf = na.approx(ch4.d_gf, rule=2),
         ch4.d_gf_grts_cumu = cumsum(ch4.d_gf)/1000*24)

#ebullitive
grts_ts<-grts_ts%>%
  mutate(ch4.e_gf = replace(ch4.e_gf, date>"2017-05-01" & date<"2017-07-10", grts_ts$ch4.e_gf[which(grepl("2017-07-10", grts_ts$date))]),
         ch4.e_gf = replace(ch4.e_gf, date>"2018-05-01" & date<"2018-07-10", grts_ts$ch4.e_gf[which(grepl("2018-07-10", grts_ts$date))]),
         ch4.e_gf = replace(ch4.e_gf, date>"2018-09-19" & date<"2018-10-01", grts_ts$ch4.e_gf[which(grepl("2018-09-19", grts_ts$date))]),
         ch4.e_gf = replace(ch4.e_gf, date<"2017-05-01", 0),
         ch4.e_gf = replace(ch4.e_gf, date>"2017-10-04" & date<"2018-05-01", 0),
         ch4.e_gf = replace(ch4.e_gf, date>"2018-10-01", 0),
         ch4.e_gf = na.approx(ch4.e_gf, rule=2),
         ch4.e_gf_grts_cumu = cumsum(ch4.e_gf)/1000*24)

#total
grts_ts<-grts_ts%>%
  mutate(ch4.t_gf = replace(ch4.t_gf, date>"2017-05-01" & date<"2017-07-10", grts_ts$ch4.t_gf[which(grepl("2017-07-10", grts_ts$date))]),
         ch4.t_gf = replace(ch4.t_gf, date>"2018-05-01" & date<"2018-07-10", grts_ts$ch4.t_gf[which(grepl("2018-07-10", grts_ts$date))]),
         ch4.t_gf = replace(ch4.t_gf, date>"2018-09-19" & date<"2018-10-01", grts_ts$ch4.t_gf[which(grepl("2018-09-19", grts_ts$date))]),
         ch4.t_gf = replace(ch4.t_gf, date<"2017-05-01", 0),
         ch4.t_gf = replace(ch4.t_gf, date>"2017-10-04" & date<"2018-05-01", 0),
         ch4.t_gf = replace(ch4.t_gf, date>"2018-10-01", 0),
         ch4.t_gf = na.approx(ch4.t_gf, rule=2),
         ch4.t_cumu = cumsum(ch4.t_gf)/1000*24,
         ch4.t_cumuPt = as.numeric(NA),
         ch4.t_cumuErr = as.numeric(NA))

for(i in 1:nrow(grts_ts)){
  grts_ts$ch4.t_cumuPt[i] <- ifelse(is.na(grts_ts$ch4.trate.FE[i]),
                                    grts_ts$ch4.t_cumuPt[i],
                                    grts_ts$ch4.t_cumu[i])
  grts_ts$ch4.t_cumuErr[i]<- ifelse(is.na(grts_ts$ch4.trate.FE[i]),
                                    grts_ts$ch4.t_cumuErr[i],
                                    grts_ts$ch4.t_cumu[i]*grts_ts$ch4.trate.FE[i])
}


#####Test plots, cuml calcs####################
ggplot(grts_ts, aes(date, ch4.trate.mg.h_Estimate))+
  geom_point()+
  geom_point(aes(date, ch4.erate.mg.h_Estimate), color="red")+
  geom_point(aes(date, ch4.drate.mg.m2.h_Estimate), color = "blue")+
  geom_line(aes(date, ch4.t_gf))+
  geom_line(aes(date, ch4.e_gf), color="red")+
  geom_line(aes(date, ch4.d_gf), color="blue")+
  ylim(0, 50)
ggplot(grts_ts, aes(date, ch4.t_cumu))+
  geom_line()+
  geom_errorbar(data=grts_ts, aes(date, ch4.t_cumuPt), ymin=ch4.t_cumuPt-ch4.t_cumuErr,
                ymax=ch4.t_cumuPt+ch4.t_cumuErr)

ggplot(grts_ts, aes(date, ch4.t_cumuPt))+
  geom_point()+
  geom_errorbar(data = grts_ts,
                aes(ymin=(ch4.t_cumuPt-ch4.t_cumuErr),
                    ymax=(ch4.t_cumuPt+ch4.t_cumuErr)))

grts_cuml2017<-grts_ts[which(grepl("2017-10-05", grts_ts$date)), "ch4_gf_grts_cumu"]
grtsDays2017<-as.numeric(as.Date("2017-10-04")-as.Date("2017-05-01"))

grts_cuml2018<-grts_ts[which(grepl("2018-10-01", grts_ts$date)), "ch4_gf_grts_cumu"]-grts_cuml2017
grtsDays2018<-as.numeric(as.Date("2017-10-01")-as.Date("2017-05-01"))

print(paste("GRTS Summer 2017 Avg FCH4:", 
            round(grts_cuml2017/grtsDays2017*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))
print(paste("GRTS Summer 2018 Avg FCH4:", 
            round(grts_cuml2018/grtsDays2017*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))
###################
GRTSF4<-grts_ts%>%
  mutate(ch4.t_gf = replace(ch4.t_gf, date<"2017-05-01", NA),
         ch4.t_gf = replace(ch4.t_gf, date>"2017-10-04" & date<"2018-05-01", NA),
         ch4.t_gf = replace(ch4.t_gf, date>"2018-10-01", NA),
         ch4.trate = ch4.t_gf,
         ch4.tPt = ch4.t_cumuPt,
         ch4.FE = ch4.trate.FE,
         Label = "d) Lake-Wide Surveys",
         year = year(date),
         monthday = format(date, format="%m-%d %H:%M"))# %>%
GRTSF4$monthday<-as.Date(GRTSF4$monthday, format="%m-%d %H:%M")

rowNum<-which(grepl("2018-01-01", GRTSF4$date))
value17<-GRTSF4$ch4.t_cumu[rowNum]

for(i in 1:nrow(GRTSF4)){
  GRTSF4$ch4.t_cumu_yr[i]<-ifelse(GRTSF4$year[i]<2018,
                                  GRTSF4$ch4.t_cumu[i],
                                  GRTSF4$ch4.t_cumu[i] - value17)
  GRTSF4$ch4.tPt_yr[i]<-ifelse(GRTSF4$year[i]<2018,
                               GRTSF4$ch4.tPt[i],
                               GRTSF4$ch4.tPt[i] - value17)
  GRTSF4$ch4.tPt_err[i]<-GRTSF4$ch4.tPt_yr[i]*GRTSF4$ch4.FE[i]
  
}

GRTSF4<-GRTSF4%>%
  mutate(ch4.t_cumu_yr = replace(ch4.t_cumu_yr, monthday<"2019-05-01", NA),
         ch4.t_cumu_yr = replace(ch4.t_cumu_yr, monthday>"2019-10-04", NA),
         ch4.t_cumu_yr_L95 = ch4.tPt_yr-ch4.tPt_err,
         ch4.t_cumu_yr_U95 = ch4.tPt_yr+ch4.tPt_err
  )

GRTSf3<-meanVariance.c%>%
  mutate(datetime = date,
         Label = "d) Lake-Wide Surveys",
         ch4_flux.f3 = ch4.trate.mg.h_Estimate,
         ch4_flux_filled.f3 = ch4.trate.mg.h_StdError)
#########################

#format: date as POSIXct, ch4.trate as num, label as chr
dailyF4list<-list()
dailyF4list[[1]]<-as.data.frame(DailyANNF4)
dailyF4list[[2]]<-as.data.frame(ShalSiteF4)
dailyF4list[[3]]<-as.data.frame(DeeptSiteF4)
dailyF4list[[4]]<-as.data.frame(select(GRTSF4, date, ch4.trate, Label, year, ch4.t_cumu, 
                                       monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_L95, ch4.t_cumu_yr_U95))

dailyFluxF4<-do.call("rbind", dailyF4list)

#format: datetime as POSIXct, ch4_flux.f3 and ch4_flux_filled.f3 as num, Label as chr
F3list<-list()
F3list[[1]]<-epDataFilledF3
F3list[[2]]<-deepAFTf3
F3list[[3]]<-shalAFTf3
F3list[[4]]<-select(GRTSf3, datetime, Label, ch4_flux.f3, ch4_flux_filled.f3)

flux30mintsF3<-do.call("rbind", F3list)

#Figure 2: time series of EC, shallow and deep AFT pseudo-continuous flux measurements
Figure2<-ggplot(flux30mintsF3, aes(datetime, ch4_flux_filled.f3))+
  annotate("rect", xmin=as.POSIXct(as.Date("2017-05-24")),
           xmax=as.POSIXct(as.Date("2017-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.POSIXct(as.Date("2018-05-24")),
           xmax=as.POSIXct(as.Date("2018-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_point(alpha=0.1, size=1, color="gray")+
  geom_point(data=flux30mintsF3, aes(datetime, ch4_flux.f3), size=1, alpha=0.2)+
  geom_line(data=dailyFluxF4, aes(date, ch4.trate), color="red", size=1)+
  geom_errorbar(data = filter(flux30mintsF3, Label == "d) Lake-Wide Surveys"),
                aes(ymax = ch4_flux.f3+ch4_flux_filled.f3*1.96,
                    ymin = ch4_flux.f3-ch4_flux_filled.f3*1.96))+
  facet_grid(Label~.)+
  ylab(expression(CH[4]~Flux~(mg~m^-2~hr^-1)))+
  xlab("")+
  scale_x_datetime(date_breaks="3 months", date_minor_breaks = "1 month", 
                   labels=date_format("%b %Y"))+
  ylim(-20, 100)+
  theme_bw()

#Figure7<-
ggplot(filter(dailyFluxF4, year>2016), aes(monthday, ch4.t_cumu_yr))+
  geom_line(aes(color=Label, linetype=Label), size=1)+
  geom_ribbon(data=filter(dailyFluxF4, year>2016), 
              aes(x = monthday, 
                  ymin = ch4.t_cumu_yr_L95,
                  ymax = ch4.t_cumu_yr_U95,
                  color = Label, fill=Label),alpha=0.2)+
  facet_grid(.~year)+
  scale_x_date(date_minor_breaks = "1 month", 
               labels = date_format("%b"))+
  theme(legend.position = "top",
        legend.title=element_blank())+
  #theme_bw()+
  ylab(expression(Cumulative~CH[4]~Areal~Emissions~(g~m^-2)))+
  xlab("")+
  labs(fill = "Method")+
  theme_bw()




# figTest<-ggplot(dailyFluxF3, aes(date, meanCH4Flux))+
#   annotate("rect", xmin=as.POSIXct(as.Date("2017-05-24")),
#            xmax=as.POSIXct(as.Date("2017-06-04")),
#            ymin=-Inf, ymax=Inf, alpha=0.5)+
#   geom_line()+
#   facet_grid(Label~.)+
#   theme_bw()
# 
# Figure3+
#   annotate("rect", xmin = as.POSIXct(as.Date("2017-05-24")),
#            xmax = as.POSIXct(as.Date("2017-06-04")),
#            ymin = -25, ymax = 100, 
#            alpha = .5)
#   geom_vline(xintercept=as.POSIXct(as.Date(c("2017-05-24", "2017-06-04", "2018-05-24", "2018-06-04"))))


#zooming in on spring burst
Figure3a<-ggplot(filter(flux30mintsF3, datetime>"2017-05-01", datetime<"2017-07-01"),
                 aes(datetime, ch4_flux_filled.f3))+
  geom_point(alpha=0.1, size=1, color="gray")+
  geom_point(data=filter(flux30mintsF3, datetime>"2017-05-01", datetime<"2017-07-01"),
             aes(datetime, ch4_flux.f3), size=1, alpha=0.2)+
  geom_line(data=filter(dailyFluxF3, date>"2017-05-01", date<"2017-07-01"),
            aes(date, meanCH4Flux), color="red", size=1)+
  facet_grid(Label~.)+
  ylab(expression(CH[4]~Flux~(mg~m^-2~hr^-1)))+
  xlab("")+
  scale_x_datetime(date_breaks="1 month", date_minor_breaks = "1 week", 
                   labels=date_format("%b %Y"))+
  ylim(-20, 100)+
  theme_bw()



# ggplot(filter(epDataFilled, datetime>"2018-05-21", datetime<"2018-06-15"),
#        aes(datetime, ch4_preds6.01*60*60*16/1000))+
#   geom_line(alpha=0.5)+
#   geom_point(data=filter(epDataFilled, datetime>"2018-05-21", datetime<"2018-06-15"),
#              aes(datetime, ch4_flux*60*60*16/1000), color="red", alpha=0.1)+
#   ylim(1*60*60*16/1000, 2*60*60*16/1000)+
#   ylab("CH4 Flux (mg m-2 hr-1)")
# 
# max(epDataFilled$ch4_flux, na.rm=TRUE)
# select(epDataFilled, datetime, ch4_flux)%>%
#   filter(datetime>"2018-05-21", datetime<"2018-06-15")
# 
# ggplot

#### CUMULATIVE FUNNEL FLUXES ###########
DailyShalFluxes<-DailyShalFluxes %>% 
  mutate(ch4_flux_filled.f3 = na.approx(meanCH4Flux, rule=2),
         cumu_ch4 = cumsum(ch4_flux_filled.f3*24/1000)) #convert from mg ch4 m-2 hr-1 to g ch4 m-2

DailyDeepFluxes$ch4_filtered<-DailyDeepFluxes$meanCH4Flux
DailyDeepFluxes$ch4_filtered[66]<-NaN #spike in measurements

DailyDeepFluxes<-DailyDeepFluxes %>% 
  mutate(ch4_flux_filled.f3 = na.approx(ch4_filtered, rule=2),
         cumu_ch4 = cumsum(ch4_flux_filled.f3*24/1000)) #convert from mg ch4 m-2 hr-1 (per day for daily fluxes) to g ch4 m-2)                           

ggplot(DailyDeepFluxes, aes(date, ch4_flux_filled.f3))+
  geom_line()
ggplot(DailyShalFluxes, aes(date, ch4_flux_filled.f3))+
  geom_line()
ggplot(DailyDeepFluxes, aes(date, cumu_ch4))+
  geom_line()
ggplot(DailyShalFluxes, aes(date, cumu_ch4))+
  geom_line()

maydf17<-DailyDeepFluxes$cumu_ch4[1] #2017-05-09 20:00:00        0.0119
octdf17<-DailyDeepFluxes$cumu_ch4[145] #2017-09-30 20:00:00      19.2
maysf17<-DailyShalFluxes$cumu_ch4[1] #2017-05-09 20:00:00       0.00315
octsf17<-DailyShalFluxes$cumu_ch4[145] #2017-09-30 20:00:00       15.2

octdf17-maydf17 #summer 2017 deep site AFT
octsf17-maysf17 #summer 2017 shallow site AFT

novdf17<-DailyDeepFluxes$cumu_ch4[176] #2017-10-30 20:00:00       22.9     

novdf17-octdf17

#start of 2018 active funnel fluxes: 5/24 for deep site, June 6th for shallow site
offset.df18<-DailyDeepFluxes$cumu_ch4[380] #2018-05-23 20:00:00          25.9 
octdf18<-DailyDeepFluxes$cumu_ch4[510] #2018-09-30 20:00:00            48.3
novdf18<-DailyDeepFluxes$cumu_ch4[541] #2018-10-31 20:00:00            50.4
nov14.df18<-DailyDeepFluxes$cumu_ch4[555] # 2018-11-14 19:00:00     50.4

offset.sf18<-DailyShalFluxes$cumu_ch4[393] #2018-06-05 20:00:00       21.9
octsf18<-DailyShalFluxes$cumu_ch4[510] # 2018-09-30 20:00:00          42.3
novsf18<-DailyShalFluxes$cumu_ch4[541] #2018-10-31 20:00:00     43
nov14.df18<-DailyShalFluxes$cumu_ch4[555] #2018-11-14 19:00:00   43.0

#may 23 - sept 30th cumulative areal methane emissions (g ch4 m-2):
(octdf18-offset.df18+(octsf18-offset.sf18))/2

#shoulder (oct 1 - 31)
(novdf18-octdf18+(novsf18-octsf18))/2

