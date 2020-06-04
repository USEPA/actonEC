#### Script to make plots showing continuous FCH4 vs. sedT
#### on a log scale, showing ecoQ10
#### on a linear scale, with 2DKS threshold

vanni30min<-vanni30min%>%
  mutate(year = year(RDateTime),
         monthday = format(RDateTime, format="%m-%d %H:%M"))# %>%
vanni30min$monthday<-as.POSIXct(vanni30min$monthday, format="%m-%d %H:%M", tz="UTC")

ggplot(filter(vanni30min, monthday<"2020-06-01"),
       aes(monthday, waterT.vws))+
  geom_line(aes(color=as.factor(year)))

ggplot(filter(epOutSubFilt, monthday<"2020-06-01", monthday>"2020-04-01"),
       aes(monthday, ch4_flux))+
  geom_point(aes(color=as.factor(year)), alpha = 0.3)+
  facet_grid(year~.)+
  ylim(-0.5, 2)


ggplot(vanni30min, aes(RDateTime, waterT.vws))+
  geom_line(color="red", alpha=0.2)+
  geom_line(data=rbrTsub, aes(RDateTime, RBRmeanT_1.6), alpha=0.5)+
  geom_line(data=epOutSubFilt, aes(RDateTime, air_temperature-273.15), color="blue", alpha=0.7)

dailyFluxRbr<-left_join(DailyEcFluxes, 
                        select(rbrDaily, -year, -monthday), 
                        by="RDateTime") 
dailyFluxRbrPar<-left_join(dailyFluxRbr, select(DailyVWS, meanPAR, RDateTime),
                           by="RDateTime")
# epOutSubFilt$date<-epOutSubFilt$RDateTime
# epOutSubFilt18<-filter(epOutSubFilt, date>"2018-05-01", date<"2018-11-01")
# EpDiurnalPlot18<-timeVariation(epOutSubFilt18, pollutant="ch4_flux",
#                                  type="month", statistic="mean", 
#                              xlab=c("hour", "hour of day, 2018",
#                                     "month", "weekday"),
#                              normalise=FALSE, cols=pal)
# plot(EpDiurnalPlot18, subset="hour")

epOutSubFilt$Eddy_Covariance<-epOutSubFilt$ch4_flux/1000*16*60*60 #mg m-2 hr-1
epOutSubFilt$date<-epOutSubFilt$RDateTime
epOutSubFilt.diur<-dplyr::select(epOutSubFilt, date, Eddy_Covariance)%>%
  filter(date<"2018-04-01"| date>"2018-06-01")
df12.gc$Deep_Trap<-df12.gc$ebCh4mgM2h
df14.gc$Shallow_Trap<-df14.gc$ebCh4mgM2h
df12.gc.diur<-select(df12.gc, date, Deep_Trap)
df14.gc.diur<-select(df14.gc, date, Shallow_Trap)

diurnal.df<-left_join(df14.gc.diur, df12.gc.diur, by="date")
diurnal.df<-left_join(diurnal.df, epOutSubFilt.diur, by="date")


#pal<-wes_palette("IsleofDogs1", 3)
diurnal.18<-filter(diurnal.df, date>"2018-06-01", date<"2018-08-31")
diurnalP.18<-timeVariation(diurnal.18, pollutant=c("Shallow_Trap",
                                                   "Deep_Trap"),
                           type="month", statistic="mean", 
                           xlab=c("hour", "hour of day, 2018",
                                  "month", "weekday"),
                           normalise=TRUE, cols=c("#006666", "#FF9933"))
plot(diurnalP.18, subset="hour")

diurnalPec.18<-timeVariation(diurnal.18, pollutant="Eddy_Covariance",
                             type="month", statistic="mean", 
                             xlab=c("hour", "hour of day, 2018",
                                    "month", "weekday"),
                             normalise=TRUE, cols=c("#FF0000"))
plot(diurnalPec.18, subset="hour")

diurnal.17<-filter(diurnal.df, date>"2017-05-01", date<"2017-11-01")
diurnalP.17<-timeVariation(diurnal.17, pollutant=c("Eddy_Covariance",
                                                   "Shallow_Trap",
                                                   "Deep_Trap"),
                           type="month", statistic="mean", 
                           xlab=c("hour", "hour of day, 2017",
                                  "month", "weekday"),
                           normalise=FALSE, cols=c("#009966", "#FFCC00"))
plot(diurnalP.17, subset="hour") 

epOutSubRbr<-left_join(epOutSub, rbrTsub, by="RDateTime")%>%
  mutate(sedT= RBRmeanT_1.6,
         date=RDateTime,
         airP = air_pressure/1000)%>%
  filter(RDateTime>"2018-06-01", RDateTime<"2018-08-31")
pal<-wes_palette("Royal1", 2)
diurnalP.env<-timeVariation(epOutSubRbr, pollutant=c("airP"),
                            type="month", statistic="mean",
                            xlab=c("hour", "hour of day, 2018",
                                   "month", "weekday"),
                            normalise=TRUE, cols=pal)
plot(diurnalP.env, subset="hour")
####-------

df12.gc$date<-as.Date(df12.gc$date)
dailyMassFlux12<-df12.gc %>%
  group_by(date) %>%
  summarize(dailyEbCh4mgM2h = (mean(ebCh4mgM2h, na.rm=TRUE)), 
            sdEbCh4mgM2h = (sd(ebCh4mgM2h, na.rm=TRUE)))
dailyMassFlux12$site<-"deep"
dailyMassFlux12<-as.data.frame(dailyMassFlux12)

dailyMassFlux12<-mutate(dailyMassFlux12,
                        #date=as.Date(date.time),
                        #site="shallow",
                        year=year(date),
                        monthday = format(date, format="%m-%d %H:%M")%>%
                          as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC"))

df14.gc$date<-as.Date(df14.gc$date)
dailyMassFlux14<-df14.gc %>%
  group_by(date) %>%
  summarize(dailyEbCh4mgM2h = (mean(ebCh4mgM2h, na.rm=TRUE)), 
            sdEbCh4mgM2h = (sd(ebCh4mgM2h, na.rm=TRUE)))
dailyMassFlux14$site<-"shallow"
dailyMassFlux14<-as.data.frame(dailyMassFlux14)


dailyMassFlux14<-mutate(dailyMassFlux14,
                        #date=as.Date(date.time),
                        #site="shallow",
                        year=year(date),
                        monthday = format(date, format="%m-%d %H:%M")%>%
                          as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC"))

####temperatures----
buoyTdaily<-buoyTdaily%>%
  mutate(year = year(date),
         monthday = format(date, format="%m-%d %H:%M"))# %>%
buoyTdaily$monthday<-as.Date(buoyTdaily$monthday, format="%m-%d %H:%M")

# ggplot(filter(buoyTdaily, monthday<"2018-05-10"),
#        aes(monthday, buoyMeanT_08))+
#   geom_point(alpha=0.4, aes(color=as.factor(year)))
#not sure why I had this filtering step
#buoyTdailyFilt<-filter(buoyTdaily, date>"2017-05-08" & date<"2017-09-18") 
buoyTdaily$sedTbuoy<-buoyTdaily$buoyMeanT_08

dailyMassFlux12<-left_join(dailyMassFlux12, select(buoyTdaily, sedTbuoy, date), by="date")
dailyMassFlux12<-left_join(dailyMassFlux12, select(U12sonde, sondeTmpr, date), by="date")

ggplot(U12sonde, aes(date, sondeTmpr))+
  geom_point(aes(color=sondeDepth))

lmSondeBuoy<-lm(sedTbuoy ~ sondeTmpr, data=dailyMassFlux12)
summary(lmSondeBuoy) #R2 = 0.94
# ggplot(dailyMassFlux12, aes(sondeTmpr, sedTbuoy))+
#   geom_point()+
#   stat_smooth(method="lm")+
#   labs(title=paste("Adj R2 = ",signif(summary(lmSondeBuoy)$adj.r.squared, 2),
#                    "Intercept =",signif(lmSondeBuoy$coef[[1]],2 ),
#                    " Slope =",signif(lmSondeBuoy$coef[[2]], 2),
#                    " P =",signif(summary(lmSondeBuoy)$coef[2,4], 2)))
dailyMassFlux12$TmprAdj<-dailyMassFlux12$sondeTmpr*lmSondeBuoy$coef[[2]]+lmSondeBuoy$coef[[1]] #adjust the sonde temperature to reflect the buoy T
dailyMassFlux12<-dailyMassFlux12 %>% 
  mutate(sedTsonde = na.approx(TmprAdj, #replace NAs by interpolation,
                               #rule=1)) #rule=1 means it will return the NA outside of the interval
                               rule=2)) #rule=2 means it will return the closest extreme outside of the interval

ggplot(filter(dailyMassFlux12, date>"2017-04-01",date<"2018-12-01"), 
       aes(date, sedTsonde))+
  geom_point(alpha=0.5)+
  geom_point(aes(date, sedTbuoy), color="red", alpha=0.5)
geom_point(aes(date, sedTbuoy), color="blue", alpha=0.5)

###footprint
# ggplot(filter(epOutSubFilt, RDateTime<"2017-09-01", !is.na(ch4_flux)), aes(wind_dir, x_10))+
#   #geom_point(alpha=0.3)+, aes(color=ch4_flux))+
#   stat_summary_bin()+
#   coord_polar()+
#   ylim(0, 50)
# 
# epOutFtPtDock<-filter(epOutSubFilt, RDateTime<"2018-05-01", !is.na(ch4_flux))%>%
#   select(RDateTime, ch4_flux, wind_dir, x_peak, x_offset, 
#                   x_10, x_30, x_50, x_70, x_90)
# 
# epOutFPdock<-filter(epOutSubFilt, RDateTime<"2018-05-01 00:00", !is.na(ch4_flux))
# 
# ggplot(epOutFPdock, aes(wind_dir, x_50))+
#   geom_point()+
#   coord_polar()
# 
# epOutFPdock<-epOutFPdock%>%
#   mutate(x_10mean=stats.bin(wind_dir, x_10, breaks=48))
# epOutFPdock$x50_mean<-stats.b
# 
# epOutFtPtDock<-epOutFtPtDock[order(epOutFtPtDock$wind_dir),]  
# 
# zwat<-zoo::rollapply(epOutFtPtDock$x_10, width = 300,FUN = mean)
# epOutFtPtDock$x_10_smth<-c(rep(NA, 149), zwat, rep(NA, 150))
# 
# ggplot(epOutFtPt, aes(wind_dir, x_10_smth))+
#   geom_line()+
#   coord_polar()

#footprint$aq50mean<-stat_summary_bin(filter(epOutSubFilt, RDateTime>"2018-06-01", !is.na(ch4_flux)), aes(wind_dir, x_50))

#making a sedT column from the sedTsonde and sedTbuoy columns  
dailyMassFlux12$sedT<-ifelse(is.na(dailyMassFlux12$sedTbuoy),
                             dailyMassFlux12$sedTsonde,
                             dailyMassFlux12$sedTbuoy)

#buoy T only missing any points after sept 15th or so
ggplot(filter(dailyMassFlux12, date>"2017-05-01", date<"2017-08-01"), aes(date, sedTsonde))+
  geom_point(alpha=0.5)+
  geom_point(data=filter(dailyMassFlux12, date>"2017-05-01", date<"2017-08-01"),
             aes(date, sedT), color="red", alpha=0.5)+
  geom_point(data=filter(dailyMassFlux12, date>"2017-05-01", date<"2017-08-01"),
             aes(date, sedTbuoy), color="blue", alpha=0.5)

ggplot(dailyMassFlux12, aes(sedTbuoy, dailyEbCh4mgM2h))+
  geom_point(alpha=0.5)+
  geom_point(aes(sedTsonde, dailyEbCh4mgM2h), alpha=0.5, color="red")

rbrDaily$date<-rbrDaily$RDateTime

###Fill in missing RBR observations with water T from VWS
waterTcompare<-left_join(DailyVWS, select(rbrDaily, rbrMeanT_1.6, RDateTime),
                         by="RDateTime")
vanniRBRfit<-lm(rbrMeanT_1.6 ~ meanWaterT, data=waterTcompare)
summary(vanniRBRfit) #R2 = 0.99, slope = 0.876, intercept = 2.979

ggplot(waterTcompare, aes(meanWaterT, rbrMeanT_1.6))+
  geom_point(alpha=0.7)+
  geom_abline(slope=1, intercept=0)+
  geom_abline(slope=0.876, intercept = 2.979, color="red")

waterTcompare$sedT<-with(waterTcompare,
                         ifelse(is.na(rbrMeanT_1.6),
                                (meanWaterT*0.876+2.979), #value if true: rbr observation is NA
                                rbrMeanT_1.6)) #value if false
waterTcompare$date<-waterTcompare$RDateTime
waterTcompare$siteT<-"(d) Shallow"

waterTcompare<-filter(waterTcompare, RDateTime>"2017-01-01")

ggplot(filter(epOutSubFilt, RDateTime>"2018-05-01", RDateTime<"2018-06-15"), 
       aes(RDateTime, ch4_flux/1000*16*60*60))+
  geom_point(alpha=0.4)+
  scale_x_datetime(date_breaks="1 week", labels=date_format("%b-%d"),
                   date_minor_breaks = "1 day")

ggplot(waterTcompare,
       aes(monthday, sedT))+
  geom_line(aes(color=as.factor(year)), alpha=0.7)+
  scale_x_date(date_breaks=("3 month"), date_minor_breaks=("1 month"))+
  geom_vline(aes(xintercept=as.numeric(as.Date("2019-05-10"))))+
  geom_vline(aes(xintercept=as.numeric(as.Date("2019-06-06"))))
#spring burst 2019: May 10 - June 6
epBurst<-filter(epOutSubFilt, RDateTime>"2018-05-10", RDateTime<"2018-06-06")
mean(epBurst$ch4_flux/1000*16*60*60, na.rm=TRUE)

#dailyECfluxSedT<-left_join(DailyEcFluxes, select(waterTcompare, -siteT, -date, -year, -monthday), by="RDateTime")
#try with gap-filled data:
DailyANNFluxes$RDateTime<-as.Date(DailyANNFluxes$date)
dailyECfluxSedT<-left_join(DailyANNFluxes, select(waterTcompare, -siteT, -date, -year, -monthday), by="RDateTime")
dailyECfluxSedT$meanCH4Flux<-dailyECfluxSedT$ch4.trate

dailyMassFlux14<-left_join(dailyMassFlux14, select(waterTcompare, sedT, date), by="date") 
dailyMassFlux12$siteT<-"(e) Deep"
ggplot(dailyMassFlux14, aes(sedT, dailyEbCh4mgM2h))+
  geom_point()
ggplot(dailyMassFlux12, aes(sedT, dailyEbCh4mgM2h))+
  geom_point()
dailyECfluxSedT$year<-year(dailyECfluxSedT$RDateTime)
ggplot(dailyECfluxSedT, aes(sedT, meanCH4Flux))+
  geom_point(alpha=0.4)+
  facet_grid(year~.)

# buoyTdaily<-mutate(buoyTdaily,
#                    sedT = buoyMeanT_10,
#                    site = "(b) Deep",
#                    meanAirT = NA,
#                    year = year(date),
#                    monthday = format(date, format = "%m-%d %H:%M"))
# buoyTdaily$monthday<-as.Date(buoyTdaily$monthday, format = "%m-%d %H:%M")

# thresh<-tree(dailyEbCh4mgM2h~sedT, data=dailyMassFlux14,
#              control=tree.control(nobs = 293, minsize=100))
# print(thresh)
# plot(thresh)
# text(thresh)
#####2DKS Threshold test:
peaEC17<-filter(dailyECfluxSedT, !is.na(meanCH4Flux), !is.na(sedT), date<"2018-01-01")%>%
  select(sedT, meanCH4Flux)
# write.table(peaEC17, 
#             file="C:/R_Projects/actonFluxProject/Threshold test/ecAnn17.prn",
#             sep=" ",
#             row.names=FALSE)
#output from big2dks: 
  #D = 0.226 the dks value (the test statistic)
  #p = 0.000200 the p value (how many of the rerandomizations generated a bigger test stat than your data did)
  #the x and y coordinate where the "greatest" 
  #difference in the bivariate distribution occurs (if one exists)
  xEC17 = 14.103731 #non-gap filled: 19.171816 # orig gap-filled: 15.13
  yEC17 = 1.082012 #non gap filled: 2.666880  # orig gap-filled: 1.78
  ggplot(peaEC17, aes(sedT, meanCH4Flux))+
    geom_point(alpha=0.3)+
    geom_vline(xintercept = xEC17)+
    geom_hline(yintercept = yEC17)
  
peaEC18<-filter(dailyECfluxSedT, !is.na(meanCH4Flux), !is.na(sedT), date>"2018-01-01")%>%
    select(sedT, meanCH4Flux)
  # write.table(peaEC18, 
  #             file="C:/R_Projects/actonFluxProject/Threshold test/ecAnn18.prn",
  #             sep=" ",
  #             row.names=FALSE)
  #output from big2dks: 
  #D=  0.233847
  #P=  0.000200
  #x=  17.438162
  #y=  4.458330
  #D = 0.220929 the dks value (the test statistic)
  #p = 0.000200 the p value (how many of the rerandomizations generated a bigger test stat than your data did)
  #the x and y coordinate where the "greatest" 
  #difference in the bivariate distribution occurs (if one exists)
  xEC18 = 17.438162 # 19.454376 #gapfilled: 17.8
  yEC18 = 4.458330 #4.355505 #gapfilled: 5.67
  ggplot(peaEC18, aes(sedT, meanCH4Flux))+
    geom_point(alpha=0.3)+
    geom_vline(xintercept = xEC18)+
    geom_hline(yintercept = yEC18)

peaShal17<-filter(dailyMassFlux14, !is.na(dailyEbCh4mgM2h), !is.na(sedT), date<"2018-01-01")%>%
  select(sedT, dailyEbCh4mgM2h)
  # write.table(peaShal17, 
  #           file="C:/R_Projects/actonFluxProject/Threshold test/shal17.prn",
  #           sep=" ",
  #           row.names=FALSE)
    #output from big2dks: 
    #D = 0.165917 the dks value (the test statistic)
    #p = 0.000200 the p value (how many of the rerandomizations generated a bigger test stat than your data did)
    #the x and y coordinate where the "greatest" 
    #difference in the bivariate distribution occurs (if one exists)
    xShal17 = 22.150902
    yShal17 = 3.2414
  ggplot(peaShal17, aes(sedT, dailyEbCh4mgM2h))+
    geom_point(alpha=0.3)+
    geom_vline(xintercept = xShal17)+
    geom_hline(yintercept = yShal17)

peaShal18<-filter(dailyMassFlux14, !is.na(dailyEbCh4mgM2h), !is.na(sedT), date>"2018-01-01")%>%
  select(sedT, dailyEbCh4mgM2h)
  # write.table(peaShal18, 
  #           file="C:/R_Projects/actonFluxProject/Threshold test/shal18.prn",
  #           sep=" ",
  #           row.names=FALSE)
    #output from big2dks: 
    #D = 0.190676
    #p = 0.000200
    xShal18 = 23.013876
    yShal18 = 1.573
  ggplot(peaShal18, aes(sedT, dailyEbCh4mgM2h))+
    geom_point(alpha=0.3)+
    geom_vline(xintercept = xShal18)+
    geom_hline(yintercept = yShal18)

peaDeep17<-filter(dailyMassFlux12, !is.na(dailyEbCh4mgM2h), !is.na(sedT), date<"2018-01-01")%>%
  select(sedT, dailyEbCh4mgM2h)
  write.table(peaDeep17, 
            file="C:/R_Projects/actonFluxProject/Threshold test/deep17.prn",
            sep=" ",
            row.names=FALSE)
#output from big2dks: 
#D = 0.204766
#p = 0.000200
xDeep17 = 17.883177
yDeep17 = 5.518
ggplot(peaDeep17, aes(sedT, dailyEbCh4mgM2h))+
  geom_point(alpha=0.3)+
  geom_vline(xintercept = xDeep17)+
  geom_hline(yintercept = yDeep17)

peaDeep18<-filter(dailyMassFlux12, !is.na(dailyEbCh4mgM2h), !is.na(sedT), date>"2018-01-01")%>%
  select(sedT, dailyEbCh4mgM2h)
  write.table(peaDeep18, 
            file="C:/R_Projects/actonFluxProject/Threshold test/deep18.prn",
            sep=" ",
            row.names=FALSE)
    #output from big2dks: 
    #D =   0.138327
    #p =  0.000200
    xDeep18 = 13.296844
    yDeep18 =   3.096822
ggplot(peaDeep18, aes(sedT, dailyEbCh4mgM2h))+
  geom_point(alpha=0.3)+
  geom_vline(xintercept = xDeep18)+
  geom_hline(yintercept = yDeep18)
  xlim(11, 20)


#####Setting up for shallow vs deep T facet plot####################
# myTmprList <- list()
# myTmprList[[1]]<-select(waterTcompare, sedT, date, monthday, year, siteT)
# myTmprList[[2]]<-select(dailyMassFlux12, sedT, date, monthday, year, siteT)
# tmprShalDeep<-do.call("rbind", myTmprList)
# 
# tmprP.agu<-ggplot(tmprShalDeep, aes(monthday, sedT))+
#   geom_point(aes(color=as.factor(year)),
#              shape=16, size=1, alpha=0.5)+
#   # geom_smooth(aes(monthday, meanAirT, color=as.factor(year)), 
#   #             alpha=0.3, span=0.3, se=FALSE)+
#   ylab(expression(Daily~Mean~Temperature~(deg~C)))+
#   xlab("")+
#   #scale_color_manual(values=wes_palette(name="Royal1", 2))+
#   facet_grid(siteT~.)+
#   scale_x_date(labels=date_format("%b %d", tz="UTC"), 
#                breaks=date_breaks("1 month"))+
#   theme_bw()+
#   theme(axis.text.x=element_text(angle=45, hjust=1))
# 
# tmprP.agu
# 
# tmprP.agu+geom_vline(xintercept=as.numeric(tmprShalDeep$monthday[208]), linetype=4)+
#   geom_vline(xintercept=as.numeric(tmprShalDeep$monthday[235]), linetype = 5)
# 
# ggsave("tmprTS.tiff", path="//AA.AD.EPA.GOV/ORD/CIN/USERS/MAIN/Q-Z/swaldo/Net MyDocuments/conference_materials/agu2018/figures",
#        width=5.5, height=3)
# #filter for high emission part of year
# tmprP.aguES<-ggplot(filter(tmprShalDeep, monthday>"2018-04-01", monthday<"2018-12-01"),
#                     aes(monthday, sedT))+
#   geom_point(aes(color=as.factor(year)),
#              shape=16, size=1, alpha=0.5)+
#   # geom_smooth(aes(monthday, meanAirT, color=as.factor(year)), 
#   #             alpha=0.3, span=0.3, se=FALSE)+
#   ylab(expression(Daily~Mean~Temperature~(deg~C)))+
#   xlab("")+
#   scale_color_manual(values=wes_palette(name="Darjeeling1", 3))+
#   facet_grid(siteT~.)+
#   scale_x_date(labels=date_format("%b %d", tz="UTC"), 
#                breaks=date_breaks("1 month"))+
#   theme_bw()+
#   theme(axis.text.x=element_text(angle=45, hjust=1))

####----


dailyECfluxSedT<-dailyECfluxSedT%>%
  mutate(date = RDateTime,
         dailyEbCh4mgM2h = meanCH4Flux,
         sdEbCh4mgM2h = NA, #sdCH4Flux,
         site ="(a) Eddy Covariance",
         siteT = NA
  )
dailyECfluxSedT<-as.data.frame(dailyECfluxSedT)
dailyMassFlux12$site<-"(c) Deep Trap"
dailyMassFlux14$site<-"(b) Shallow Trap"
dailyMassFlux14$siteT<-NA

dailyMassEbList<-list()
dailyMassEbList[[1]]<-dailyMassFlux14
dailyMassEbList[[2]]<-select(dailyMassFlux12, -TmprAdj, -sedTbuoy, -sedTsonde, -sondeTmpr)
dailyMassEbList[[3]]<-select(dailyECfluxSedT, date, dailyEbCh4mgM2h, sdEbCh4mgM2h, site, year, monthday, sedT, siteT)
dailyMassEb<-do.call("rbind", dailyMassEbList)

xthresholds<-c(xEC17, xEC18, xShal17, xShal18, xDeep17, xDeep18)
ythresholds<-c(yEC17, yEC18, yShal17, yShal18, yDeep17, yDeep18)

twoDKS<-data.frame(site=c(rep("(a) Eddy Covariance", 2), rep("(b) Shallow Trap", 2),
                          rep("(c) Deep Trap", 2)),
                   year=c("2017", "2018","2017", "2018","2017", "2018"),
                   hintercept=xthresholds,
                   yintercept=ythresholds)
twoDKS<-twoDKS%>%
  mutate(site=as.character(site),
         year=as.character(year))
#Figure 2: similar to mass P3 agu, but with hh fluxes, two years, and vertical lines for spring burst



massFluxHH<-list()
massFluxHH[[1]]


massP3agu<-ggplot(dailyMassEb, aes(monthday, dailyEbCh4mgM2h))+
  geom_pointrange(mapping=aes(x=monthday, y=(dailyEbCh4mgM2h*24/1000), 
                              ymin=(dailyEbCh4mgM2h*24/1000-(sdEbCh4mgM2h/sqrt(24))*24/1000),
                              ymax=(dailyEbCh4mgM2h*24/1000+(sdEbCh4mgM2h/sqrt(24))*24/1000), color=as.factor(year)),
                  shape=16, size=0.15, alpha=0.5)+
  # geom_smooth(aes(monthday, dailyEbCh4mgM2h*24/1000, color=as.factor(year)), 
  #             alpha=0.5, span=0.3, se=FALSE)+
  geom_line(aes(monthday, dailyEbCh4mgM2h*24/1000, color=as.factor(year)), 
                         alpha=0.5)+
  facet_grid(site~., scales="free")+
  ylab(expression(Daily~Mean~CH[4]~Flux~(g~m^-2~d^-1)))+
  xlab("")+
  scale_color_manual(values=wes_palette(name="Darjeeling1", 2))+
  scale_x_datetime(labels=date_format("%b %d", tz="UTC"), 
                   breaks=date_breaks("1 month"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))
massP3agu

ggsave("fluxTS_ecFunnels_gc.tiff", path="//AA.AD.EPA.GOV/ORD/CIN/USERS/MAIN/Q-Z/swaldo/Net MyDocuments/conference_materials/agu2018/figures",
       width=5.5, height=4.5)

massP3aguES<-ggplot(filter(dailyMassEb, monthday>"2018-04-01", monthday<"2018-12-01"),
                    aes(monthday, dailyEbCh4mgM2h))+
  geom_pointrange(mapping=aes(x=monthday, y=(dailyEbCh4mgM2h*24/1000), 
                              ymin=(dailyEbCh4mgM2h*24/1000-(sdEbCh4mgM2h/sqrt(24))*24/1000),
                              ymax=(dailyEbCh4mgM2h*24/1000+(sdEbCh4mgM2h/sqrt(24))*24/1000), color=as.factor(year)),
                  shape=16, size=0.4, alpha=0.3)+
  #geom_smooth(aes(monthday, dailyEbCh4mgM2h*24/1000, color=as.factor(year)), 
  #            alpha=0.5, span=0.3, se=FALSE)+
  geom_line(aes(monthday, dailyEbCh4mgM2h*24/1000, color=as.factor(year)), 
              alpha=0.5)+
  facet_grid(site~.)+
  ylab(expression(Daily~Mean~CH[4]~Flux~(g~m^-2~d^-1)))+
  xlab("")+
  scale_color_manual(values=wes_palette(name="Darjeeling1", 2))+
  scale_x_datetime(labels=date_format("%b %d", tz="UTC"), 
                   breaks=date_breaks("1 month"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

massP3+geom_point(data=grts.df, aes(g.date, g.ch4.eb, color=g.site))

massP4<-ggplot(filter(dailyMassEb, date<"2017-11-05"), 
               aes(date, dailyEbCh4mgM2h))+
  geom_point(alpha=0.4)+
  facet_grid(site~.)+
  ylab(expression(Daily~CH[4]~Ebullition~(mg~m^{-2}~h^{-1})))+
  scale_x_date(labels=date_format("%b %d", tz="UTC"), 
               breaks=date_breaks("1 month"))+
  scale_fill_discrete(name="GRTS Results")+
  #theme_classic()+
  theme(legend.position = "bottom")
massP5<-massP4+geom_point(data=grts.df, aes(g.date, g.ch4.eb, color=g.site))
massP5+geom_errorbar(data=filter(dailyMassEb, date<"2017-11-05"),
                     aes(ymin=dailyEbCh4mgM2h-(sdEbCh4mgM2h/sqrt(48)), 
                         ymax=dailyEbCh4mgM2h+(sdEbCh4mgM2h/sqrt(48))), 
                     alpha=0.4)

tmprP2<-ggplot(filter(dailyMassEb, date>"2018-05-01", date<"2018-11-01",
                      year=="2018", site!="(a) Eddy Covariance"),
                      aes(date, sedT))+
  geom_point(aes(color=site))+
  #geom_smooth()+
  facet_grid(site~.)+
  scale_x_date(breaks=date_breaks("1 month"),
               labels=date_format("%b"))+
  theme_bw()
massP2<-ggplot(filter(dailyMassEb, date>"2018-05-01", date<"2018-11-01",
                      year=="2018"), #site!="(a) Eddy Covariance"),
               aes(date, dailyEbCh4mgM2h))+
  geom_line(aes(color=site), alpha=0.6)+
  #geom_smooth()+
  facet_grid(site~.)+ #, scales="free")+
  scale_x_date(breaks=date_breaks("1 month"),
               labels=date_format("%b"))+
  theme_bw()
ggplot(dailyMassEb, aes(sedT, dailyEbCh4mgM2h))+
  geom_point(aes(color=site), alpha=0.3)+
  facet_grid(year~.)
  #xlim(14.5, 15)
  scale_y_log10()

  
ggplot(dailyMassEb, aes(sedT, dailyEbCh4mgM2h))+
    geom_point(aes(color=site), alpha=0.3)+
  geom_smooth(method='nls', formula=y~a*exp(b*x), se=FALSE)+
  #stat_summary_bin(geom="point", aes(color=site))+
    facet_grid(site~year, scales="free" )

###################################3
####Figure 14 added 7/5/2019 #######
##############################3#####


str(dailyMassEb)
str(dailyMassFlux12)
str(dailyMassFlux14)

means14<-filter(dailyMassFlux14, monthday>"2019-05-01", monthday<"2019-11-01")%>%
  group_by(year)%>%
  summarize(mean_sedT = mean(sedT, na.rm=TRUE),
            meanCH4_eb = mean(dailyEbCh4mgM2h, na.rm=TRUE))

means12<-filter(dailyMassFlux12, monthday>"2019-05-01", monthday<"2019-11-01")%>%
  group_by(year)%>%
  summarize(mean_sedT = mean(sedT, na.rm=TRUE),
            meanCH4_eb = mean(dailyEbCh4mgM2h, na.rm=TRUE))

dailyMassFlux12<-dailyMassFlux12%>%
  mutate(sedT_norm=sedT,
         ch4Eb_norm=dailyEbCh4mgM2h)

for(i in 1:nrow(dailyMassFlux12)){
  dailyMassFlux12$ch4Eb_norm[i]<-ifelse(dailyMassFlux12$year[i]==2017,
                                        dailyMassFlux12$ch4Eb_norm[i]/means12$meanCH4_eb[1],
                                        dailyMassFlux12$ch4Eb_norm[i]/means12$meanCH4_eb[2])
  dailyMassFlux12$sedT_norm[i]<-ifelse(dailyMassFlux12$year[i]==2017,
                                       dailyMassFlux12$sedT_norm[i]/means12$mean_sedT[1],
                                       dailyMassFlux12$sedT_norm[i]/means12$mean_sedT[2])
}

ggplot(filter(dailyMassFlux12, monthday>"2019-04-15", monthday<"2019-11-01"),
       aes(monthday, ch4Eb_norm))+
  geom_line(alpha=0.5)+
  geom_line(data=filter(dailyMassFlux12, monthday>"2019-04-15", monthday<"2019-11-01"), 
            aes(monthday, sedT_norm),
            color="red", alpha=0.5)+
  facet_grid(.~year)

dailyMassFlux14<-dailyMassFlux14%>%
  mutate(sedT_norm=sedT,
         ch4Eb_norm=dailyEbCh4mgM2h)

for(i in 1:nrow(dailyMassFlux14)){
  dailyMassFlux14$ch4Eb_norm[i]<-ifelse(dailyMassFlux14$year[i]==2017,
                                        dailyMassFlux14$ch4Eb_norm[i]/means14$meanCH4_eb[1],
                                        dailyMassFlux14$ch4Eb_norm[i]/means14$meanCH4_eb[2])
  dailyMassFlux14$sedT_norm[i]<-ifelse(dailyMassFlux14$year[i]==2017,
                                       dailyMassFlux14$sedT_norm[i]/means14$mean_sedT[1],
                                       dailyMassFlux14$sedT_norm[i]/means14$mean_sedT[2])
}

ggplot(filter(dailyMassFlux14, monthday>"2019-04-15", monthday<"2019-11-01"),
       aes(monthday, ch4Eb_norm))+
  geom_line(alpha=0.5)+
  geom_line(data=filter(dailyMassFlux14, monthday>"2019-04-15", monthday<"2019-11-01"), 
            aes(monthday, sedT_norm),
            color="red", alpha=0.5)+
  facet_grid(.~year)

dailyMassFlux14_fig14<-gather(data=dailyMassFlux14, value=value, key=key, dailyEbCh4mgM2h, sedT)

ggplot(filter(dailyMassFlux14_fig14, monthday>"2019-04-15", monthday<"2019-11-01"),
       aes(monthday, value))+
  annotate("rect", xmin=as.POSIXct(as.Date("2019-08-02")),
           xmax=as.POSIXct(as.Date("2019-08-16")),
           ymin=-Inf, ymax=Inf, alpha=0.4)+
  geom_line(alpha=1, aes(color=as.factor(key)))+
  scale_color_manual(values=c("#333333", "#CC0033"))+
  geom_smooth(span=0.4, se=FALSE)+
  facet_grid(key~year, scales="free_y")+
  scale_x_datetime(labels=date_format("%b", tz="UTC"), 
                   breaks=date_breaks("1 month"))+
  xlab("")+
  theme_bw()+
  theme(legend.position="none")

dailyMassFlux12_fig14<-gather(data=dailyMassFlux12, value=value, key=key, dailyEbCh4mgM2h, sedT)

ggplot(filter(dailyMassFlux12_fig14, monthday>"2019-04-15", monthday<"2019-11-01"),
       aes(monthday, value))+
    annotate("rect", xmin=as.POSIXct(as.Date("2019-09-10")),
           xmax=as.POSIXct(as.Date("2019-09-24")),
           ymin=-Inf, ymax=Inf, alpha=0.4)+
  geom_line(alpha=1, aes(color=as.factor(key)), size=0.5)+
  scale_color_manual(values=c("#333333", "#CC0033"))+
  geom_smooth(span=0.3, se=FALSE)+
  facet_grid(key~year, scales="free_y")+
  scale_x_datetime(labels=date_format("%b", tz="UTC"), 
                   breaks=date_breaks("1 month"))+
  xlab("")+
  theme_bw()+
  theme(legend.position="none")

spatioTemporalList<-list()
spatioTemporalList[[1]]<-select(dailyMassFlux12_fig14, -sedTbuoy, -sondeTmpr, -TmprAdj, -sedTsonde)
spatioTemporalList[[2]]<-dailyMassFlux14_fig14

spatioTemporal<-do.call("rbind", spatioTemporalList)

spatioTemporal<-mutate(spatioTemporal,
       site=replace(site, site=="(c) Deep Trap", "(b)  Deep Site"),
       site=replace(site, site=="(b) Shallow Trap", "(a) Shallow Site"),
       key = replace(key, key=="dailyEbCh4mgM2h", "Ebullition"))

ggplot(filter(spatioTemporal, date>"2017-04-05", date<"2017-11-01"),
       aes(date, value))+
  annotate("rect", xmin=(as.Date("2017-08-02")),
           xmax=(as.Date("2017-08-16")),
           ymin=-Inf, ymax=Inf, alpha=0.2)+
       annotate("rect", xmin=(as.Date("2017-09-10")),
                xmax=(as.Date("2017-09-24")),
                ymin=-Inf, ymax=Inf, alpha=0.6)+
  geom_line(alpha=1, aes(color=as.factor(key)), size=0.5)+
  scale_color_manual(values=c("#333333", "#CC0033"))+
  geom_smooth(span=0.3, se=FALSE)+
  facet_grid(key~site, scales="free_y")+
  #scale_x_date(labels=date_format("%b", tz="UTC"), 
   #                breaks=date_breaks("1 month"))+
  xlab("")+
  ylab(expression(sedT~(deg~C)~~~~~Ebullition~(mg~CH[4]~m^-2~hr^-1)))+
  theme_bw()+
  theme(legend.position="none")
         

# normFunc<-function(x, y){
#   return(x/y)
# }
# 
# normFunc(5,2)
# 
# apply(dailyMassFlux12$sedT_norm, 1, normFunc(1, 17))

#by hand: only deep trap 2017 passes computation

ggplot(filter(dailyMassEb, site=="(a) Eddy Covariance", year == 2018),
       aes(sedT, dailyEbCh4mgM2h))+
  geom_point()+
  geom_smooth(method='nls', formula=y~a*exp(b*x), se=FALSE)

ggplot(filter(dailyMassEb, site=="(c) Deep Trap", year == 2018),
       aes(sedT, dailyEbCh4mgM2h))+
  geom_point()+
  geom_smooth(method='nls', formula=y~a*exp(b*x), se=FALSE)



  #xlim(14.5, 15)
  scale_y_log10()  

ggplot(dailyMassEb, aes(1/(sedT+273.15), log(dailyEbCh4mgM2h)))+
  #geom_point(aes(color=site), alpha=0.5)+
  stat_summary_bin(geom="point")+
  facet_grid(site~year, scales="free")
  
  #xlim(14.5, 15)+
  scale_y_log10()

#what's going on with the vertical lines in the deep trap regressions?
#at ~12.45-12.46C&18C in 2018; 15C in 2017

filter(dailyMassEb, sedT>12.45, sedT<12.46)
#10-30 thru 11-15 -- got it
filter(dailyMassEb, sedT>17.5, sedT<18, year=="2018")
# 9-21 thru 10-07. So this appears to be real: there is a flat period 
# where sedT doesn't vary much from 17.8C. But this is corroborated by 
# out own sonde measurements at the deep site on 9/18 and 10/3
filter(dailyMassEb, sedT>14.6, sedT<15, year=="2017")
# 6-11-17 thru 6-26-17 -- also checks out :/

dailyMassEb$log10eb<-log((dailyMassEb$dailyEbCh4mgM2h*24), base=10) #convert to mg m-2 d-1





lmDeepQ10_2017<-lm(log10eb ~ sedT, 
                   data=filter(dailyMassEb, site=="(c) Deep Trap", year=="2017"))
lmDeepQ10_2018<-lm(log10eb ~ sedT, 
                   data=filter(dailyMassEb, site=="(c) Deep Trap", year=="2018"))
lmShalQ10_2017<-lm(log10eb ~ sedT, 
                   data=filter(dailyMassEb, site=="(b) Shallow Trap", year == "2017"))
lmShalQ10_2018<-lm(log10eb ~ sedT, 
                   data=filter(dailyMassEb, site=="(b) Shallow Trap", year == "2018"))
lmEC.Q10_2017<-lm(log10eb ~ sedT, 
                  data=filter(dailyMassEb, site=="(a) Eddy Covariance", year == "2017"))
lmEC.Q10_2018<-lm(log10eb ~ sedT, 
                  data=filter(dailyMassEb, site=="(a) Eddy Covariance", year == "2018"))

ggplot(filter(dailyMassEb, year>2016), aes(sedT, log10eb))+
  geom_point(alpha=0.5)+
  facet_grid(site~year)+
  stat_smooth(method="lm")+
  xlim(10, 30)+
  ylim(-1.5, 3.5)+
  labs(x="Sediment Temperature (deg C)", y=expression(log10(F[CH4])))+
  #scale_color_manual(values=wes_palette(name="Darjeeling1", 2))+
  # labs(title=paste("Deep 2017 R2 = ",signif(summary(lmDeepQ10_2017)$adj.r.squared, 2),
  #                  "Deep 2017 Slope =",signif(lmDeepQ10_2017$coef[[2]],2 ),
  #                  "Shal 2017 R2 = ",signif(summary(lmShalQ10_2017)$adj.r.squared, 2),
  #                   " Shal 2017 Slope =",signif(lmShalQ10_2017$coef[[2]], 2)))+
  theme_bw()
#Q10 = 10^10b, where b=slope
ggsave("flux_Q10s.tiff", path="//AA.AD.EPA.GOV/ORD/CIN/USERS/MAIN/Q-Z/swaldo/Net MyDocuments/conference_materials/agu2018/figures",
       width=6, height=5)

dailyMassEb$year<-as.factor(dailyMassEb$year)

########################################################
###facet plot with 2DKS thresholds#####
ggplot(filter(dailyMassEb, year>2016), aes(sedT, dailyEbCh4mgM2h))+
  geom_point(alpha=0.3)+#, aes(color=as.factor(year)), show.legend=FALSE)+
  facet_grid(as.factor(site)~year)+#, scales = "free")+
  #stat_smooth(method="lm")+
  #xlim(10, 30)+
  labs(x="Sediment Temperature (deg C)", y=expression(CH[4]~emission~(mg~m^-2~hr^-1)))+
  #scale_color_manual(values=wes_palette(name="Darjeeling1", 2))+
  # labs(title=paste("Deep 2017 R2 = ",signif(summary(lmDeepQ10_2017)$adj.r.squared, 2),
  #                  "Deep 2017 Slope =",signif(lmDeepQ10_2017$coef[[2]],2 ),
  #                  "Shal 2017 R2 = ",signif(summary(lmShalQ10_2017)$adj.r.squared, 2),
  #                   " Shal 2017 Slope =",signif(lmShalQ10_2017$coef[[2]], 2)))+
  theme_bw()+
  guides(fill=FALSE)+
  scale_fill_discrete(guide=FALSE)+
  geom_hline(data=twoDKS, aes(yintercept=yintercept), alpha=0.5, linetype=2)+
  geom_vline(data=twoDKS, aes(xintercept=hintercept), alpha=0.5, linetype=2)

ggsave("flux_sedTthresholds2DKS.tiff", path="C:/R_Projects/actonFluxProject/figures",
       width=6, height=5)

Q10.deep17<-10^(10*lmDeepQ10_2017$coef[[2]])
Q10.shal17<-10^(10*lmShalQ10_2017$coef[[2]])
Q10.ec2017<-10^(10*lmEC.Q10_2017$coef[[2]])
Q10.deep18<-10^(10*lmDeepQ10_2018$coef[[2]])
Q10.shal18<-10^(10*lmShalQ10_2018$coef[[2]])
Q10.ec2018<-10^(10*lmEC.Q10_2018$coef[[2]])

paste("Deep 2017 R2 = ",signif(summary(lmDeepQ10_2017)$adj.r.squared, 3),
      "Deep 2017 Slope =",signif(lmDeepQ10_2017$coef[[2]],3 ),
      "Shal 2017 R2 = ",signif(summary(lmShalQ10_2017)$adj.r.squared, 3),
      "Shal 2017 Slope =",signif(lmShalQ10_2017$coef[[2]], 3))
paste("Deep 2018 R2 = ",signif(summary(lmDeepQ10_2018)$adj.r.squared, 3),
      "Deep 2018 Slope =",signif(lmDeepQ10_2018$coef[[2]],3),
      "Shal 2018 R2 = ",signif(summary(lmShalQ10_2018)$adj.r.squared, 3),
      "Shal 2018 Slope =",signif(lmShalQ10_2018$coef[[2]],3))
      

10^(10*0.07)

#meteorological drivers plots: rainfall, wind speed

ggplot(vanni30min, aes(RDateTime, dailyRa))+
  geom_point(alpha=0.3)

ggplot(filter(DailyVWS, RDateTime>"2018-05-01"),
       aes(RDateTime, totRain))+
  geom_line()+
  scale_x_date(breaks=date_breaks("1 month"),
                   labels=date_format("%b"))


#combined daily flux and daily sediment T plot for AGU poster:
dailyAGU<-ggplot(dailyFluxRbr, aes(monthday, meanCH4Flux))+
  geom_pointrange(mapping=aes(x=monthday, y=meanCH4Flux*24, 
                              ymin=(meanCH4Flux*24-(randErrCh4Prop/sqrt(nCH4Flux))*24),
                              ymax=(meanCH4Flux*24+(randErrCh4Prop/sqrt(nCH4Flux))*24), color=as.factor(year)),
                  shape=16, size=0.4, alpha=0.3)+
  geom_smooth(aes(monthday, meanCH4Flux*24, color=as.factor(year)), 
              alpha=0.5, span=0.3, se=FALSE)+
  ylab(expression(Daily~Mean~CH[4]~Flux~(mg~m^-2~d^-1)))+
  xlab("")+
  scale_color_manual(values=wes_palette(name="Darjeeling1", 2))+
  #facet_grid(year~.)+
  scale_x_date(breaks=date_breaks("1 month"),
               labels=date_format("%d %b"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

dailyAGUtmpr<-ggplot(dailyFluxRbr, aes(monthday, rbrMeanT_1.6))+
  geom_point(aes(color=as.factor(year)), shape=16, size=1, alpha=0.6)+
  geom_smooth(aes(monthday, meanAirT, color=as.factor(year)),
              alpha=0.5, span=0.3, se=FALSE)+
  ylab(expression(Daily~Mean~T~(deg~C)))+
  #ylim(0,30)+
  xlab("")+
  #facet_grid(year~.)+
  scale_x_date(breaks=date_breaks("1 month"),
               labels=date_format("%d %b"))+
  scale_color_manual(values=wes_palette(name="Darjeeling1", 2))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

dailyAGUtmpr+ylim(0, 30)
