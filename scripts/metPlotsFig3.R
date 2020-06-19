

# rbrDaily<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/rbrDaily.csv")%>%
#   mutate(RDateTime = as.Date(RDateTime),
#          monthday = as.POSIXct(monthday, tz="UTC"),
#          year = as.numeric(year))
# buoyTdaily<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/buoyTdaily.csv")%>%
#   mutate(RDateTime = as.POSIXct(RDateTime, tz="UTC"),
#          date = as.Date(date))
# vanni30min<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/vanni30min.csv")
# vanni30min$RDateTime<-as.POSIXct(vanni30min$RDateTime,
#                                  format="%Y-%m-%d %H:%M:%S", tz="UTC")
# #the gap-filled driver dataset for ANN:
temp<-read.csv("data/annDataset_MDC.csv") #has rain
temp$datetime<-as.POSIXct(as.character(temp$datetime),
                             format="%Y-%m-%d %H:%M:%S")
temp$date<-as.Date(temp$datetime)
# 
# fluxDatFilled<-read.csv("output/fluxDataFilled6.01.csv")
# fluxDatFilled$datetime<-as.POSIXct(as.character(fluxDatFilled$datetime),
#                                    format="%Y-%m-%d %H:%M:%S")
# fluxDatFilled$date<-as.Date(fluxDatFilled$datetime)
# 

waterLevel<- select(temp, datetime, rain30min, dailyRain.vws, levelAdj.vws)
rm(temp)

#filter and gap-fill PAR (?)
ggplot(filter(fluxDat2, datetime>"2018-07-10", datetime<"2018-07-25"),
       aes(datetime, FilledPAR))+
  geom_line()
fluxDat2<-fluxDat2%>%
  mutate(FilledPAR = replace(FilledPAR, 
                             datetime>"2018-07-17 00:00:00" & 
                               datetime<"2018-07-19 00:00:00", NA),
         FilledPAR = replace(FilledPAR,
                             datetime>"2018-07-14 00:00:00" &
                               datetime<"2018-07-15 00:00:00", NA))


DailyFluxDat<-fluxDat2%>%
  group_by(date)%>%
  dplyr::summarize(meanPAR=mean(FilledPAR*60*60*24/10^6), #convert from umolm-2s-1 to mol m-2 d-1
                   meanAirT = mean(FilledAirT-273.15), #convert from K to C
                   meanSedT = mean(FilledSedT), #in C
                   precip = max(dailyRain.vws), #in mm
                   meanLE = mean(FilledLE, na.rm=TRUE), #W m-2
                   meanH = mean(FilledH, na.rm=TRUE),
                   mean_ch4 = mean(ch4_flux, na.rm=TRUE)) 

DailyFluxDat$year<-year(DailyFluxDat$date)
DailyFluxDat$monthday<- format(DailyFluxDat$date, format="%m-%d")
DailyFluxDat$monthday<-as.Date(DailyFluxDat$monthday, format="%m-%d")




MonthlyFluxDat<-as.data.frame(fluxDat2)%>%
  group_by(date=cut(date, breaks = "1 month"))%>%
  dplyr::summarize(meanPAR=mean(FilledPAR*60*60*24/10^6), #convert from umolm-2s-1 to mol m-2 d-1
                   meanAirT = mean(FilledAirT-273.15), #convert from K to C
                   meanSedT = mean(FilledSedT), #in C
                   precip = max(dailyRain.vws), #in mm
                   meanLE = mean(FilledLE, na.rm=TRUE), #W m-2
                   meanH = mean(FilledH, na.rm=TRUE),
                   bowR = meanH/meanLE,
                   mean_ch4 = mean(ch4_flux, na.rm=TRUE)) 
MonthlyFluxDat<-MonthlyFluxDat%>%
  mutate(Rdate=as.Date(MonthlyFluxDat$date),
         year = year(Rdate),
         monthday = format(Rdate, format="%m-%d %H:%M"))# %>%
MonthlyFluxDat$monthday<-as.Date(MonthlyFluxDat$monthday, format="%m-%d %H:%M")



#########Figure 2A: Daily mean air and sediment T #######  
#ggplot(filter(DailyFluxDat, date>"2017-12-30", date<"2018-02-15"), aes(date, meanAirT))+
ggplot(DailyFluxDat, aes(date, meanAirT))+
  annotate("rect", xmin=as.Date("2017-05-24"),
           xmax=as.Date("2017-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.Date("2018-05-24"),
           xmax=as.Date("2018-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line(color="red")+
  geom_line(data=DailyFluxDat, aes(date, meanSedT), alpha=0.8)+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
               breaks=date_breaks("2 month"),
               limits = c(as.Date("2017-01-01"), as.Date("2018-11-20")))+
  ylab(expression(T~(deg~C)))+
  xlab("")+
  geom_hline(yintercept=0, linetype = 2, alpha=0.2)+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme_bw()
###export by copying to clipboard, height = 200, width = 900


### Warm season stats:
mean(subset(DailyFluxDat, date>"2017-05-01", date<"2017-10-01")$meanAirT)
sd(subset(DailyFluxDat, date>"2017-05-01", date<"2017-10-01")$meanAirT)
mean(subset(DailyFluxDat, date>"2018-05-01", date<"2018-10-01")$meanAirT)
sd(subset(DailyFluxDat, date>"2018-05-01", date<"2018-10-01")$meanAirT)

mean(subset(DailyFluxDat, date>"2017-05-01", date<"2017-10-01")$meanSedT)
mean(subset(DailyFluxDat, date>"2018-05-01", date<"2018-10-01")$meanSedT)

mean(subset(DailyFluxDat, date>"2017-05-01", date<"2017-10-01")$meanLE)
mean(subset(DailyFluxDat, date>"2018-05-01", date<"2018-10-01")$meanLE)

mean(subset(DailyFluxDat, date>"2017-05-01", date<"2017-10-01")$meanH)
mean(subset(DailyFluxDat, date>"2018-05-01", date<"2018-10-01")$meanH)



########FIGURE 2B.a: Daily LE and H #######
ggplot(DailyFluxDat, aes(date, meanLE))+
  annotate("rect", xmin=as.Date("2017-05-24"),
           xmax=as.Date("2017-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.Date("2018-05-24"),
           xmax=as.Date("2018-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line(alpha=0.5)+
  geom_line(data=DailyFluxDat, aes(date, meanH, color="red"), alpha=0.5)+
  theme_bw()+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
               breaks=date_breaks("2 month"),
               limits = c(as.Date("2017-01-01"), as.Date("2018-11-20")))+
  ylab(expression(LE~and~H~(W~m^-2)))+
  xlab("")+
  theme(legend.position="none")


################FIGURE 2C: Precip##################

## DailyFluxDat precip has gaps:
ggplot(DailyFluxDat, aes(date, precip))+
  geom_bar(stat="identity")+
  theme_bw()+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
               breaks=date_breaks("2 month"))+
  ylab(expression(Precip~(mm)))+
  xlab("")

campMet$date<-as.Date(campMet$RDateTime)

DailyCamp<-campMet%>%
  group_by(date)%>%
  dplyr::summarize(loggerT = mean(PTemp_C_Avg, na.rm=TRUE),
                   totRain = sum(Rain_mm_tot, na.rm=TRUE),
                   meanNR = mean(NR_Wm2_avg, na.rm=TRUE),
                   meanWS = mean(WS_ms_Avg, na.rm=TRUE))

ggplot(DailyCamp, aes(date, totRain))+
  geom_bar(stat="identity")+
  theme_bw()+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
               breaks=date_breaks("2 month"))+
  ylab(expression(Precip~(mm)))+
  xlab("")
campMet$datetime<-campMet$RDateTime
rain_test<-inner_join(select(fluxDat, datetime, rain30min), 
                      select(campMet, datetime, Rain_mm_tot))
ggplot(rain_test, aes(rain30min, Rain_mm_tot))+
  geom_point(alpha=0.3)
ggplot(rain_test, aes(datetime, rain30min))+
  geom_line(alpha=0.5)+
  geom_line(data=rain_test, aes(datetime, Rain_mm_tot), color="red", alpha=0.5)

## load precip data from nearby National Atmospheric Deposition Program site
## http://nadp.slh.wisc.edu/data/sites/siteDetails.aspx?net=NTN&id=OH09

ntn<-read.csv(file = paste0(projectWD, "/data/NTN-OH09-d.csv"))%>%
  mutate(starttime = as.POSIXct(as.character(starttime), format="%m/%d/%Y %H:%M:%S", tz="UTC"),
         endtime = as.POSIXct(as.character(endtime), format="%m/%d/%Y %H:%M:%S", tz="UTC"))%>%
  filter(starttime>"2017-01-01")

## Combine NADP, Vanni met station, campbell met station rain data:

DailyFluxDat<-left_join(DailyFluxDat, select(DailyCamp, date, totRain), 
                        by="date")
DailyFluxDat<-left_join(DailyFluxDat, select(ntn, amount, date), by="date")
DailyFluxDat$FilledPrecip<-DailyFluxDat$precip

DailyFluxDat<-DailyFluxDat%>%
  mutate(FilledPrecip = replace(FilledPrecip, FilledPrecip==-Inf, NA))
for(i in 1:nrow(DailyFluxDat)){
  DailyFluxDat$FilledPrecip[i] <- ifelse(is.na(DailyFluxDat$FilledPrecip[i]),
                                         DailyFluxDat$totRain[i], 
                                         DailyFluxDat$precip[i])}
for(i in 1:nrow(DailyFluxDat)){
  DailyFluxDat$FilledPrecip[i]<-ifelse(is.na(DailyFluxDat$FilledPrecip[i]),
                                       DailyFluxDat$amount[i]*2.54*10, #from NADP
                                       DailyFluxDat$FilledPrecip[i])}

####FIGURE 2c: PRECIP
ggplot(DailyFluxDat, aes(date, FilledPrecip))+
  annotate("rect", xmin=as.Date("2017-05-24"),
           xmax=as.Date("2017-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.Date("2018-05-24"),
           xmax=as.Date("2018-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_bar(stat="identity")+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"),
               breaks=date_breaks("2 month"),
               limits = c(as.Date("2017-01-01"), as.Date("2018-11-20")))+
  theme_bw()+
  ylab(expression(Precip~(mm)))+
  xlab("")

write_csv(DailyFluxDat, path=paste0(projectWD, "/dataL2/Fig3abc.csv"))

## Precip Stats:
DailyFluxDat$CumuPrecip<-cumsum(DailyFluxDat$FilledPrecip)
DailyFluxDat[174,1]
DailyFluxDat[138,1]

rain1May2017<-as.numeric(DailyFluxDat[138,11])
rain1June2017<-as.numeric(DailyFluxDat[174,11])
rain1June2017-rain1May2017

DailyFluxDat[532,1]
DailyFluxDat[563,1]
rain1May2018<-as.numeric(DailyFluxDat[532,11])
rain1June2018<-as.numeric(DailyFluxDat[563,11])
rain1June2018-rain1May2018

#########Still missing a chunk in May, December#######

#####FIGURE 2D: Lake Level#########

ggplot(waterLevel, aes(datetime, levelAdj.vws))+
  annotate("rect", xmin=as.POSIXct(as.Date("2017-05-24")),
           xmax=as.POSIXct(as.Date("2017-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.POSIXct(as.Date("2018-05-24")),
           xmax=as.POSIXct(as.Date("2018-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line()+
  theme_bw()+
  scale_x_datetime(labels=date_format("%b %Y", tz="UTC"), 
                   breaks=date_breaks("2 month"),
                   limits = c(as.POSIXct(as.Date("2017-01-01")),
                              as.POSIXct(as.Date("2018-11-20"))))+
  ylab(expression(Water~Level~(m)))+
  xlab("")

write_csv(select(waterLevel, datetime, levelAdj.vws),
          path = paste0(projectWD, "/dataL2/Fig3e_data.csv"))

#####FIGURE 2D: Stream Discharge/Inflow#########
##see edi.256.1.r

#####FIGURE 2E: BV Frequency#########
##see hydroDynamicsVanniBuoy.R


# vanni30min$year<-year(vanni30min$RDateTime)
# vanni30min$monthday<-format(vanni30min$RDateTime, format="%m-%d %H:%M")%>%
#   as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC")
# 
# ggplot(filter(vanni30min, year>2016), aes(monthday, levelAdj.vws))+
#   annotate("rect", xmin=as.POSIXct(as.Date("2019-05-24")),
#            xmax=as.POSIXct(as.Date("2019-06-04")),
#            ymin=-Inf, ymax=Inf, alpha=0.5)+
#   geom_line()+
#   facet_grid(year~.)
# 
# # DailyEcFluxes$monthday2<-as.POSIXct(DailyEcFluxes$monthday)
# # 
# # ggplot(filter(vanni30min, year>2016, monthday>"2019-05-01", monthday<"2019-07-01"),
# #        aes(monthday, waterT.vws))+
# #   annotate("rect", xmin=as.POSIXct(as.Date("2019-05-24")),
# #            xmax=as.POSIXct(as.Date("2019-06-04")),
# #            ymin=-Inf, ymax=Inf, alpha=0.5)+
# #   geom_line()+
# #   geom_line(data=filter(DailyEcFluxes, monthday>"2019-05-01", monthday<"2019-07-01"),
# #             aes(monthday2, meanAirT), 
# #             color="red")+
# #   facet_grid(year~.)
# # 
# # 
# # DailyVWS<-vanni30min %>%
# #   group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
# #   dplyr::summarize(meanPAR = (mean(par.vws, na.rm=TRUE)),
# #                    meanWaterT = mean(waterT.vws, na.rm=TRUE),
# #                    totRain = sum(rain30min),
# #                    meanLakeLvl = mean(levelAdj.vws, na.rm=TRUE),
# #                    meanAirT = mean(airT.vws, na.rm=TRUE)
# #   )
# # 
# # DailyVWS<-DailyVWS %>%
# #   mutate(RDateTime=as.Date(DailyVWS$RDateTime),
# #          year = year(RDateTime),
# #          monthday = format(RDateTime, format="%m-%d %H:%M"))# %>%
# # DailyVWS$monthday<-as.Date(DailyVWS$monthday, format="%m-%d %H:%M")
# # 
# # 
# # fluxDatFilled$date<-as.Date(fluxDatFilled$datetime)
# # DailyFilled<-fluxDatFilled%>%
# #   group_by(date)%>%
# #   dplyr::summarize(meanAirT = mean(FilledAirT),
# #                    meanSedT = mean(FilledSedT))
# # 
# # 
# # dailyMet.g<-gather(data=select(DailyFluxDat, -precip, -totRain),
# #                    key=param, value=value, -date)
# # ggplot(dailyMet.g, aes(date, value))+
# #   geom_line()+
# #   facet_grid(param~.)
# # 
# # 
# # ###Tmpr: daily air, surface water, sediment T
# # DailyEcFluxes$T_label<-"Air"
# # DailyEcFluxes$meanT<-DailyEcFluxes$meanAirT
# # DailyVWS$T_label<-"Water"
# # DailyVWS$meanT<-DailyVWS$meanWaterT
# # rbrDaily$T_label<-"Sediment"
# # rbrDaily$meanT<-rbrDaily$rbrMeanT_1.6
# # 
# # myList<-list()
# # myList[[1]]<-select(DailyEcFluxes, RDateTime, meanT, T_label)
# # #myList[[2]]<-select(DailyVWS, RDateTime, meanT, T_label)
# # myList[[2]]<-select(rbrDaily, RDateTime, meanT, T_label)
# # tmprPlot<-do.call("rbind", myList)
# # 
# # ggplot(tmprPlot, aes(RDateTime, meanT))+
# #   geom_line(aes(color=T_label))
# # 
# # ggplot(DailyVWS, aes(RDateTime, meanAirT))+
# #   geom_line(color="red")+
# #   geom_line(data=DailyVWS, aes(RDateTime, meanWaterT))
# # 
# # 
# # 
