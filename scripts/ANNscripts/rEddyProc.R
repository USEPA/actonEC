
#Step 1: make a fully continuous time series
#make a model time column that we'll join the epOut data to:
###hoboU12$date.timeHH[4] = "2017-05-10 12:00:00 UTC"
###hoboU12$date.timeHH[71997] = "2017-12-14 09:00:00 UTC"

#EddyData.S<-fLoadTXTIntoDataframe('rEddyFile.txt',
 #                                 Dir.s = "C:/R_Projects/actonFluxProject/output")
    #prepped rEddyFile in Excel to match the format of the ecample file in 
    #the rEddyProc vignette
EddyDataWithPosix.S <- fConvertTimeToPosix(epREddy, 'YDH',Year.s = 'Year'
                                           ,Day.s = 'DoY',Hour.s = 'Hour')
#sum(is.na(epREddy$DoY))
range(EddyDataWithPosix.S$DateTime)
#[1] "2017-01-26 05:30:00 UTC" "2018-11-13 17:00:00 UTC"
# nrow(epOutSub)
# #[1] 30648
# timeframe30<-seq.POSIXt(from = EddyDataWithPosix.S$DateTime[1],
#                          to = EddyDataWithPosix.S$DateTime[nrow(EddyDataWithPosix.S)-(17*2)],by = "30 min")
# 
# epTest<-as.data.frame(timeframe30)
# epTest$DateTime<-epTest$timeframe30
# epTest <- subset(epTest, !duplicated(DateTime, fromLast=TRUE))
# epTest<-left_join(epTest, EddyDataWithPosix.S, by="DateTime")
# #remove duplicate time rows
# epTest <- subset(epTest, !duplicated(DateTime, fromLast=TRUE))
# 
# #%>%
#   mutate(Ustar = ustar,
#          NEE = co2_flux)

epREddy$DateTime<-epREddy$RDateTime
EddyProc.C <- sEddyProc$new('Acton', EddyDataWithPosix.S, 
                            c('co2_flux', 'ch4_flux',
                              'LE', 'H', 'wind_dir',
                              'air_temperature','VPD', 'ustar', 'daytime'))

#gap-filling with MDC:
#####DAYTIME for gap distribution
flux.var='daytime'
EddyProc.C$sFillInit(Var.s=flux.var, 
                     QFVar.s="none",
                     FillAll.b=TRUE) #2519 real gaps of 35040 values, with two full years (Jan 1 - Jan 1)
EddyProc.C$sFillMDC(WinDays.i = 14,
                    Verbose.b=TRUE)
#EddyProc.C$sMDSGapFill(Var.s='ch4_flux', FillAll.b = TRUE
FilledEddyData.S <- EddyProc.C$sExportResults()
EddyDataWithPosix.S$daytime_filled<-FilledEddyData.S$VAR_f
#epOutSubFiltGaps<-select(epOutSubFiltGaps, -epOutSubFiltGaps)
epOutSubFiltGaps<-left_join(epOutSubFiltGaps, 
                             select(EddyDataWithPosix.S, RDateTime, daytime_filled), 
                             by="RDateTime")

#####USTAR#####
flux.var='ustar'
EddyProc.C$sFillInit(Var.s=flux.var, 
                     QFVar.s="none",
                     FillAll.b=TRUE) #25046 real gaps of 35040 values, with two full years (Jan 1 - Jan 1)
EddyProc.C$sFillMDC(WinDays.i = 10,
                    Verbose.b=TRUE)
#EddyProc.C$sMDSGapFill(Var.s='ch4_flux', FillAll.b = TRUE
FilledEddyData.S <- EddyProc.C$sExportResults()
EddyDataWithPosix.S$ustar_filled<-FilledEddyData.S$VAR_f

###LATENT HEAT#####
rm(EddyProc.C)
EddyProc.C <- sEddyProc$new('Acton', EddyDataWithPosix.S, 
                            c('co2_flux', 'ch4_flux',
                              'LE', 'H', 'wind_dir',
                              'air_temperature','VPD', 'ustar'))
flux.var='LE'
EddyProc.C$sFillInit(Var.s=flux.var, 
                     QFVar.s="none",
                     FillAll.b=TRUE) #25046 real gaps of 35040 values, with two full years (Jan 1 - Jan 1)
EddyProc.C$sFillMDC(WinDays.i = 10,
                    Verbose.b=TRUE)
#EddyProc.C$sMDSGapFill(Var.s='ch4_flux', FillAll.b = TRUE
FilledEddyData.S <- EddyProc.C$sExportResults()
EddyDataWithPosix.S$LE_filled<-FilledEddyData.S$VAR_f

###SENSIBLE HEAT#####
rm(EddyProc.C)
EddyProc.C <- sEddyProc$new('Acton', EddyDataWithPosix.S, 
                            c('co2_flux', 'ch4_flux',
                              'LE', 'H', 'wind_dir',
                              'air_temperature','VPD', 'ustar'))
flux.var='H'
EddyProc.C$sFillInit(Var.s=flux.var, 
                     QFVar.s="none",
                     FillAll.b=TRUE) #25046 real gaps of 35040 values, with two full years (Jan 1 - Jan 1)
EddyProc.C$sFillMDC(WinDays.i = 10,
                    Verbose.b=TRUE)
#EddyProc.C$sMDSGapFill(Var.s='ch4_flux', FillAll.b = TRUE
FilledEddyData.S <- EddyProc.C$sExportResults()
EddyDataWithPosix.S$H_filled<-FilledEddyData.S$VAR_f








#pre gap-filled plots:
EddyProc.C$sPlotFingerprintY(flux.var, Year.i = 2017)
EddyProc.C$sPlotFingerprintY(flux.var, Year.i = 2018)
EddyProc.C$sPlotHHFluxesY(flux.var, Year.i=2017)
EddyProc.C$sPlotHHFluxesY(flux.var, Year.i=2018)

epREddy_17<-filter(epREddy, year < 2018)
sum(is.na(epREddy_17$H)) #11775
sum(is.na(epREddy_17$ch4_flux))/nrow(epREddy_17)
epREddy_18<-filter(epREddy, year > 2017)
sum(is.na(epREddy_18$ch4_flux)) #11471
sum(is.na(epREddy_18$ch4_flux))/nrow(epREddy_18)
epREddy_18<-filter(epREddy, RDateTime > "2018-05-26 00:00", 
                   RDateTime< "2018-11-13 00:00")
sum(is.na(epREddy_18$ch4_flux)) #11433
sum(is.na(epREddy_18$ch4_flux))/nrow(epREddy_18)

EddyProc.C$sFillInit(Var.s=flux.var, 
                     QFVar.s="none",
                     FillAll.b=TRUE) #25046 real gaps of 35040 values, with two full years (Jan 1 - Jan 1)
EddyProc.C$sFillMDC(WinDays.i = 10,
                    Verbose.b=TRUE)
#EddyProc.C$sMDSGapFill(Var.s='ch4_flux', FillAll.b = TRUE
FilledEddyData.S <- EddyProc.C$sExportResults()
EddyDataWithPosix.S$ustar_filled<-FilledEddyData.S$VAR_f


EddyProc.C$sPlotFingerprintY('VAR_f', Year.i = 2017)
EddyProc.C$sPlotHHFluxesY('VAR_f', Year.i=2017) #sometimes saves as "VAR_f", sometimes as "ch4_flux_f"
EddyProc.C$sPlotFingerprintY('VAR_f', Year.i = 2018)
EddyProc.C$sPlotHHFluxesY('VAR_f', Year.i=2018)

#daily sums -- cumulative daily fluxes
EddyProc.C$sPlotDailySumsY('VAR_f', Year.i=2017,
                           timeFactor.n=3600*24,
                           massFactor.n=(16/1e+03)*(12/16),
                           unit.s = "mgC m-2 day-1")
EddyProc.C$sPlotDailySumsY('VAR_f', Year.i=2018,
                           timeFactor.n=3600*24,
                           massFactor.n=(16/1e+03)*(12/16),
                           unit.s = "mgC m-2 day-1")
     

#CombinedData.F <- cbind(EddyDataWithPosix.S, FilledEddyData.S)
#CombindedData.F2<-cbind(CombinedData.F, FilledEddyData.S)
#CombinedData.F$ustar_f<-FilledEddyData.S$VAR_f

sum(is.na(CombinedData.F$H))

str(CombinedData.F)
p1<-ggplot(filter(CombinedData.F, !is.na(year)), aes(monthday, ch4_flux_f*3600/1000*16))+
  geom_line(alpha=0.3, color="red")+
  ylim(-1*3.6*16, 2.5*3.6*16)+
  geom_line(data=filter(CombinedData.F, !is.na(year)), aes(monthday, ch4_flux*3.6*16), alpha=0.3)
p1+facet_grid(year~.)+ylab("CH4 Flux (mg m-2 hr-1)")


#wintertime baseline
wintertime<-filter(CombinedData.F, monthday<"2019-04-01 00:00:00"| monthday>"2019-11-01 00:00:00")
  mean(wintertime$ch4_flux_f*3.6*16, na.rm=TRUE) #0.454 mg m-2 hr-1
  sd(wintertime$ch4_flux_f*3.6*16, na.rm=TRUE) #0.81
  quantile(wintertime$ch4_flux_f*3.6*16, na.rm=TRUE) #25%: 0.09, median: 0.33, 75%: 0.67
  
CombinedData.F$ch4_filled<-CombinedData.F$ch4_flux_f  
  
sum(is.na(CombinedData.F$ch4_filled)) #3279

CombinedData.F<-CombinedData.F%>%
  mutate(ch4_filled = replace(ch4_filled, DateTime<"2017-04-01 00:00:00"&   
                                is.na(ch4_filled), 
                              mean(wintertime$VAR_f, na.rm=TRUE)),
         ch4_filled = replace(ch4_filled, DateTime>"2018-11-01 00:00:00"&                                 
                                is.na(ch4_filled), 
                              mean(wintertime$VAR_f, na.rm=TRUE)),
         ch4_filled = replace(ch4_filled, DateTime>"2017-11-01 00:00:00"&
                                DateTime<"2018-04-01 00:00:00"&
                                is.na(ch4_filled), 
                              mean(wintertime$VAR_f, na.rm=TRUE)))
sum(is.na(CombinedData.F$ch4_filled)) #35
ggplot(CombinedData.F, aes(DateTime, ch4_filled))+
  geom_line(alpha=0.8)

p2<-ggplot(filter(CombinedData.F, !is.na(year)), aes(monthday, ch4_filled*3600/1000*16))+
  geom_line(alpha=0.3, color="grey")+
  ylim(-1*3.6*16, 2.5*3.6*16)+
  geom_point(data=filter(CombinedData.F, !is.na(year)), aes(monthday, ch4_flux*3.6*16), alpha=0.6)
p2+facet_grid(year~.)+ylab("CH4 Flux (mg m-2 hr-1)")

plotGaps(CombinedData.F, "ch4_filled")

#no linear interpolation needed
# df14.gc<-df14.gc %>% mutate(meanCH4interp = na.approx(meanCH4, rule=2),
#                               meanCO2interp = na.approx(meanCO2, rule=2),
#                               meanN2Ointerp = na.approx(meanN2O, rule=2),

DailyEcFluxes.F<-CombinedData.F %>%
  group_by(DateTime = cut(DateTime, breaks = "24 hour")) %>%
  dplyr::summarize(meanCH4Flux = (mean(ch4_filled, na.rm=TRUE)/1000*16*60*60),
            sdCH4Flux = (sd(ch4_filled, na.rm=TRUE)/1000*16*60*60),
            randErrCh4Prop = sqrt(sum((rand_err_ch4_flux/1000*16*60*60)^2, 
                                      na.rm=TRUE)),
            nCH4Flux = n_distinct(ch4_flux, na.rm=TRUE),
            meanAirT = (mean(air_temperature, na.rm=TRUE)-273.15))
DailyEcFluxes.F<-DailyEcFluxes.F%>%
  mutate(DateTime=as.Date(DailyEcFluxes.F$DateTime),
         year = year(DateTime),
         monthday = format(DateTime, format="%m-%d %H:%M"))# %>%
DailyEcFluxes.F$monthday<-as.Date(DailyEcFluxes.F$monthday, format="%m-%d %H:%M")

ggplot(filter(DailyEcFluxes.F,year<2019),
       aes(monthday, meanCH4Flux))+
  geom_line(alpha=0.8, color="red")+
  # geom_pointrange(mapping=aes(x=monthday, y=meanCH4Flux, 
  #                             ymin=(meanCH4Flux-(randErrCh4Prop/sqrt(nCH4Flux))),
  #                             ymax=(meanCH4Flux+(randErrCh4Prop/sqrt(nCH4Flux)))),
  #                 color="grey", shape=21, fill="black", size=0.4, alpha=0.7)+
  geom_point(data=DailyEcFluxes, aes(monthday, meanCH4Flux), alpha=0.3)+
  facet_grid(year~.)+
  ylab("Daily Mean CH4 Flux (mg m-2 hr-1)")+
  xlab("")+
  scale_x_date(breaks=date_breaks("1 month"),
               labels=date_format("%d %b"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))


p2<-ggplot(filter(CombinedData.F, !is.na(year)), aes(monthday, ch4_filled*3600/1000*16))+
  geom_line(alpha=0.3, color="grey")+
  ylim(-1*3.6*16, 2.5*3.6*16)+
  geom_point(data=filter(CombinedData.F, !is.na(year)), 
             aes(monthday, ch4_flux*3.6*16), alpha=0.6)+
  geom_line(data=DailyEcFluxes.F, (aes(monthday, meanCH4Flux)), color="red")
p2+facet_grid(year~.)+ylab("CH4 Flux (mg m-2 hr-1)")+
  theme_bw()

DailyEcFluxes.F$ch4.cumu<-cumsum(DailyEcFluxes.F$meanCH4Flux*24/1000) 

summer2017<-filter(DailyEcFluxes.F, DateTime>"2017-05-01 00:00:00", DateTime<"2017-11-01 00:00:00")
summer2017val<-sum(summer2017$meanCH4Flux*24/1000)

summer2018<-filter(DailyEcFluxes.F, DateTime>"2018-05-01 00:00:00", DateTime<"2018-11-01 00:00:00")
summer2018val<-sum(summer2018$meanCH4Flux*24/1000)
#convert from mg/m2/hr to g/m2/d
#adding up over days, so days get integrated away into g CH4 / m2


ggplot(DailyEcFluxes.F, aes(DateTime, ch4.cumu))+
  geom_line()+
  ylab("g CH4 m-2")
max(DailyEcFluxes.F$ch4.cumu) #108

#annual
DailyEcFluxes.F$DateTime[365]
val2017<-DailyEcFluxes.F$ch4.cumu[365]
DailyEcFluxes.F$ch4.cumu.a<-DailyEcFluxes.F$ch4.cumu

DailyEcFluxes.F$ch4.cumu.a[366:730]<-DailyEcFluxes.F$ch4.cumu.a[366:730]-val2017
ggplot(DailyEcFluxes.F, aes(DateTime, ch4.cumu.a))+
  geom_line()+
  ylab("g CH4 m-2")

ggplot(filter(DailyEcFluxes.F, year<2019),
       aes(monthday, ch4.cumu.a))+
  geom_line(aes(color=as.factor(year)))+
  ylab("g CH4 m-2")+
  scale_x_date(labels=date_format("%b", tz="UTC"), 
               breaks=date_breaks("1 month"))+  
  theme_bw()
  facet_grid(year~.)
    scale_x_date(labels=date_format("%b %d", tz="UTC"), 
                 breaks=date_breaks("1 month"))+  

select(DailyEcFluxes.F, DateTime, meanCH4Flux, ch4.cumu)
    
#### Manuscript Figure 3 ######  
    
    #shallow trap ebullition
    ggplot(df14.gc, aes(date.timeHH, ebCh4mgM2h))+
      geom_point(alpha=0.3, size=0.5)+
      ylim(-50, 150)
  
    #eddy covariance CH4
    F3<-ggplot(filter(CombinedData.F, !is.na(year)), 
               aes(RDateTime, ch4_flux_f*3600/1000*16))+
      geom_point(alpha=0.2, color="red", size=0.25)+
      ylim(-1*3.6*16, 2.5*3.6*16)+
      geom_point(data=filter(CombinedData.F, !is.na(year)), 
                 aes(RDateTime, ch4_flux*3.6*16), alpha=0.3, size=0.5)+
      # geom_smooth(data=filter(CombinedData.F, !is.na(year)),
      #             aes(RDateTime, ch4_flux_f*3600/1000*16),
      #              alpha=0.3, span=0.01, se=FALSE)+
      #geom_line(data=DailyEcFluxes.F, aes(DateTime, meanCH4Flux))+
    ylab("CH4 Flux (mg m-2 hr-1)")
    CombinedData.F$RDate<-as.Date(CombinedData.F$RDateTime)
    CombinedData.F$RDate<-as.POSIXct(CombinedData.F$RDate,
                                     tz="UTC")
    F4<-ggplot(filter(CombinedData.F, !is.na(year)),
               aes(RDateTime, ch4_flux_f*3600/1000*16))+
      geom_line(alpha=0.2, color="red", size=0.25)
    F4+stat_summary(data=filter(CombinedData.F, !is.na(year)),
                   aes(RDate, ch4_flux_f*3600/1000*16),
                   fun.y="mean", geom="line")
    
    
    
    
    p1+facet_grid(year~.)+ylab("CH4 Flux (mg m-2 hr-1)")
    
    
# 
# 
# uStarTh <- EddyProc.C$sEstUstarThresholdDistribution(
#   UstarColName=EddyProc.C$sDATA$ustar, NEEColName = EddyProc.C$sDATA$co2_flux, 
#   nSample = 100L, probs = c(0.05, 0.5, 0.95)) 
# 
# help(REddyProc)
# 
# EddyProc.C$sPlotNEEVersusUStarForSeason()
# 
# #####From vignette https://cran.rstudio.com/web/packages/REddyProc/vignettes/useCase.html
# EddyData.F<-Example_DETha98
# DEtha_test<-EddyData.F
# DEtha_test%>%
#   set_(NULL)
# attr(DEtha_test, "varnames")<-NULL
# str(DEtha_test)
# str(EddyData.F)
# attributes(DEtha_test)<-NULL
# EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH',Year.s = 'Year'
#                                            ,Day.s = 'DoY',Hour.s = 'Hour')
# #+++ Initalize R5 reference class sEddyProc for post-processing of eddy data
# #+++ with the variables needed for post-processing later
# EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, 
#                             c('NEE','Rg','Tair','VPD', 'Ustar'))
# EddyProc.C$sPlotFingerprintY('NEE', Year.i = 1998)
# 
# 
# uStarTh <- EddyProc.C$sEstUstarThresholdDistribution(
#   nSample = 100L, probs = c(0.05, 0.5, 0.95)) 
# #filter(uStarTh, aggregationMode == "year")
# select(uStarTh, -seasonYear)
