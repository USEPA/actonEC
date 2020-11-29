
## this script uses the REddyProc library to gap-fill USTAR, LE, and H. 
## after these variables are gap-filled, the can be used as drivers of the ANN gap-filling for FCH4

### Step 1: Prep input for the rEddyProc code that uses the MDC, 
### which has very specific formatting requirements
### to gap fill LE, H, and ustar for input into the ANN gapfilling for CH4

timeframe30<-seq.POSIXt(from = as.POSIXct("2017-01-01 00:00:00",
                                          format="%Y-%m-%d %H:%M:%S",
                                          tz = "UTC"),
                        to = as.POSIXct("2018-12-31 23:30:00",
                                        format="%Y-%m-%d %H:%M:%S",
                                        tz = "UTC"),
                        by = "30 min")

epTest<-as.data.frame(timeframe30)
epTest$RDateTime<-epTest$timeframe30
epTest <- subset(epTest, !duplicated(RDateTime, fromLast=TRUE))
epOut.R<-select(epOutSubFilt, -monthday, -year)
epREddy<-left_join(epTest, epOut.R, by="RDateTime")
#remove duplicate time rows
epREddy <- subset(epREddy, !duplicated(RDateTime, fromLast=TRUE))

epREddy<-epREddy%>%
  mutate(Year = year(RDateTime),
         Hour = as.numeric(times(strftime(RDateTime, format="%T", tz="UTC")))*24,
         DoY = as.numeric(strftime(RDateTime, format="%j", tz="UTC")),
         year = year(RDateTime),
         daytime = replace(daytime, is.na(daytime) & Hour>21, 0),
         daytime = replace(daytime, is.na(daytime) & Hour<6, 0),
         daytime = replace(daytime, is.na(daytime) & Hour>8 & Hour<18, 1),
         monthday = format(RDateTime, format="%m-%d %H:%M"))
epREddy$monthday<-as.POSIXct(epREddy$monthday, format="%m-%d %H:%M", tz="UTC")

#add units as attributes for prep to use with REddyProc

units.match<-c(	RDateTime="-", date = "-", time =	"-", DOY =	"-", daytime = 	"1=daytime",
                Tau = "kgm-1s-2",qc_Tau =	"-",	H ="Wm-2", qc_H =	"-", rand_err_H = "Wm-2",	LE = "Wm-2", qc_LE =	"-", rand_err_LE=	"Wm-2",	
                co2_flux = "µmolm-2s-1", qc_co2_flux = "-",rand_err_co2_flux = "µmolm-2s-1",	
                ch4_flux = "µmolm-2s-1",qc_ch4_flux = "-",rand_err_ch4_flux = "µmolm-2s-1",	
                co2_mixing_ratio = "ppm",h2o_mixing_ratio = "mmol mol-1", ch4_mixing_ratio = "ppm", 
                air_temperature = "K",	air_pressure = "Pa", air_density = 	"kgm-3",	air_heat_capacity = "Jkg-1K-1",
                ET = "mm",	water_vapor_density = "kgm-3",	e= "Pa",	es="Pa",	specific_humidity = "kgkg-1", RH=	"%", VPD=	"Pa",	
                Tdew= "K",	u_rot = "ms-1",v_rot =	"ms-1",	w_rot = "ms-1",
                wind_speed = "ms-1",	max_wind_speed = "ms-1",	wind_dir = "deg_from_north",
                ustar = "ms-1", TKE =	"m+2s-2",	L = "m",	zL = "-",	bowen_ratio = "-",	Tstar = "K",	model = "0=KJ/1=KM/2=HS",	#footprint model
                x_peak = "m", x_offset = "m",x_10 =	"m", x_30 =	"m",	x_50 = "m",x_70 =	"m", x_90=	"m",	w_unrot = "ms-1", 
                Year = "-", Hour = "-", DoY = "-"
)

#epREddy<-Hmisc::upData(epREddy, labels = varnames.match)
epREddy<-Hmisc::upData(epREddy, units = units.match)

epREddy$DateTime<-epREddy$RDateTime

EddyDataWithPosix.S <- fConvertTimeToPosix(epREddy, 'YDH',Year = 'Year'
                                           ,Day = 'DoY',Hour = 'Hour')
# EddyDataWithPosix.S <- fConvertTimeToPosix(EddyData.S, 'YDH',Year = 'Year'
#                                            ,Day = 'DoY',Hour = 'Hour')

EddyProc.C <- sEddyProc$new('Acton', epREddy, 
                            c('co2_flux', 'ch4_flux',
                              'LE', 'H', 'wind_dir',
                              'air_temperature','VPD', 'ustar', 'daytime'))

### Step 2: gap-filling with MDC:
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


#####USTAR#####
flux.var='ustar'
rm(EddyProc.C)
EddyProc.C <- sEddyProc$new('Acton', EddyDataWithPosix.S, 
                            c('co2_flux', 'ch4_flux',
                              'LE', 'H', 'wind_dir',
                              'air_temperature','VPD', 'ustar'))
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







## Optional intermediate data visualization:
#pre gap-filled plots:

# EddyProc.C$sPlotFingerprintY(flux.var, Year = 2017)
# EddyProc.C$sPlotFingerprintY(flux.var, Year = 2018)
# EddyProc.C$sPlotHHFluxesY(flux.var, Year=2017)
# EddyProc.C$sPlotHHFluxesY(flux.var, Year=2018)
# 
# epREddy_17<-filter(epREddy, year < 2018)
# sum(is.na(epREddy_17$H)) #11775
# sum(is.na(epREddy_17$ch4_flux))/nrow(epREddy_17)
# epREddy_18<-filter(epREddy, year > 2017)
# sum(is.na(epREddy_18$ch4_flux)) #11471
# sum(is.na(epREddy_18$ch4_flux))/nrow(epREddy_18)
# epREddy_18<-filter(epREddy, RDateTime > "2018-05-26 00:00", 
#                    RDateTime< "2018-11-13 00:00")
# sum(is.na(epREddy_18$ch4_flux)) #11433
# sum(is.na(epREddy_18$ch4_flux))/nrow(epREddy_18)
# 
# EddyProc.C$sFillInit(Var.s=flux.var, 
#                      QFVar.s="none",
#                      FillAll.b=TRUE) #25046 real gaps of 35040 values, with two full years (Jan 1 - Jan 1)
# EddyProc.C$sFillMDC(WinDays.i = 10,
#                     Verbose.b=TRUE)
# #EddyProc.C$sMDSGapFill(Var.s='ch4_flux', FillAll.b = TRUE
# FilledEddyData.S <- EddyProc.C$sExportResults()
# EddyDataWithPosix.S$ustar_filled<-FilledEddyData.S$VAR_f
# 
# 
# EddyProc.C$sPlotFingerprintY('VAR_f', Year.i = 2017)
# EddyProc.C$sPlotHHFluxesY('VAR_f', Year.i=2017) #sometimes saves as "VAR_f", sometimes as "ch4_flux_f"
# EddyProc.C$sPlotFingerprintY('VAR_f', Year.i = 2018)
# EddyProc.C$sPlotHHFluxesY('VAR_f', Year.i=2018)
# 
# #daily sums -- cumulative daily fluxes
# EddyProc.C$sPlotDailySumsY('VAR_f', Year.i=2017,
#                            timeFactor.n=3600*24,
#                            massFactor.n=(16/1e+03)*(12/16),
#                            unit.s = "mgC m-2 day-1")
# EddyProc.C$sPlotDailySumsY('VAR_f', Year.i=2018,
#                            timeFactor.n=3600*24,
#                            massFactor.n=(16/1e+03)*(12/16),
#                            unit.s = "mgC m-2 day-1")
#      

#CombinedData.F <- cbind(EddyDataWithPosix.S, FilledEddyData.S)
#CombindedData.F2<-cbind(CombinedData.F, FilledEddyData.S)
#CombinedData.F$ustar_f<-FilledEddyData.S$VAR_f

# sum(is.na(CombinedData.F$H))
# 
# str(CombinedData.F)
# p1<-ggplot(filter(CombinedData.F, !is.na(year)), aes(monthday, ch4_flux_f*3600/1000*16))+
#   geom_line(alpha=0.3, color="red")+
#   ylim(-1*3.6*16, 2.5*3.6*16)+
#   geom_line(data=filter(CombinedData.F, !is.na(year)), aes(monthday, ch4_flux*3.6*16), alpha=0.3)
# p1+facet_grid(year~.)+ylab("CH4 Flux (mg m-2 hr-1)")
# 
# 
# #wintertime baseline
# wintertime<-filter(CombinedData.F, monthday<"2019-04-01 00:00:00"| monthday>"2019-11-01 00:00:00")
#   mean(wintertime$ch4_flux_f*3.6*16, na.rm=TRUE) #0.454 mg m-2 hr-1
#   sd(wintertime$ch4_flux_f*3.6*16, na.rm=TRUE) #0.81
#   quantile(wintertime$ch4_flux_f*3.6*16, na.rm=TRUE) #25%: 0.09, median: 0.33, 75%: 0.67
#   
# CombinedData.F$ch4_filled<-CombinedData.F$ch4_flux_f  
#   
# sum(is.na(CombinedData.F$ch4_filled)) #3279
# 
# CombinedData.F<-CombinedData.F%>%
#   mutate(ch4_filled = replace(ch4_filled, DateTime<"2017-04-01 00:00:00"&   
#                                 is.na(ch4_filled), 
#                               mean(wintertime$VAR_f, na.rm=TRUE)),
#          ch4_filled = replace(ch4_filled, DateTime>"2018-11-01 00:00:00"&                                 
#                                 is.na(ch4_filled), 
#                               mean(wintertime$VAR_f, na.rm=TRUE)),
#          ch4_filled = replace(ch4_filled, DateTime>"2017-11-01 00:00:00"&
#                                 DateTime<"2018-04-01 00:00:00"&
#                                 is.na(ch4_filled), 
#                               mean(wintertime$VAR_f, na.rm=TRUE)))
# sum(is.na(CombinedData.F$ch4_filled)) #35
# ggplot(CombinedData.F, aes(DateTime, ch4_filled))+
#   geom_line(alpha=0.8)
# 
# p2<-ggplot(filter(CombinedData.F, !is.na(year)), aes(monthday, ch4_filled*3600/1000*16))+
#   geom_line(alpha=0.3, color="grey")+
#   ylim(-1*3.6*16, 2.5*3.6*16)+
#   geom_point(data=filter(CombinedData.F, !is.na(year)), aes(monthday, ch4_flux*3.6*16), alpha=0.6)
# p2+facet_grid(year~.)+ylab("CH4 Flux (mg m-2 hr-1)")
# 
# plotGaps(CombinedData.F, "ch4_filled")
# 
# #no linear interpolation needed
# # df14.gc<-df14.gc %>% mutate(meanCH4interp = na.approx(meanCH4, rule=2),
# #                               meanCO2interp = na.approx(meanCO2, rule=2),
# #                               meanN2Ointerp = na.approx(meanN2O, rule=2),
# 
# DailyEcFluxes.F<-CombinedData.F %>%
#   group_by(DateTime = cut(DateTime, breaks = "24 hour")) %>%
#   dplyr::summarize(meanCH4Flux = (mean(ch4_filled, na.rm=TRUE)/1000*16*60*60),
#             sdCH4Flux = (sd(ch4_filled, na.rm=TRUE)/1000*16*60*60),
#             randErrCh4Prop = sqrt(sum((rand_err_ch4_flux/1000*16*60*60)^2, 
#                                       na.rm=TRUE)),
#             nCH4Flux = n_distinct(ch4_flux, na.rm=TRUE),
#             meanAirT = (mean(air_temperature, na.rm=TRUE)-273.15))
# DailyEcFluxes.F<-DailyEcFluxes.F%>%
#   mutate(DateTime=as.Date(DailyEcFluxes.F$DateTime),
#          year = year(DateTime),
#          monthday = format(DateTime, format="%m-%d %H:%M"))# %>%
# DailyEcFluxes.F$monthday<-as.Date(DailyEcFluxes.F$monthday, format="%m-%d %H:%M")
# 
# ggplot(filter(DailyEcFluxes.F,year<2019),
#        aes(monthday, meanCH4Flux))+
#   geom_line(alpha=0.8, color="red")+
#   # geom_pointrange(mapping=aes(x=monthday, y=meanCH4Flux, 
#   #                             ymin=(meanCH4Flux-(randErrCh4Prop/sqrt(nCH4Flux))),
#   #                             ymax=(meanCH4Flux+(randErrCh4Prop/sqrt(nCH4Flux)))),
#   #                 color="grey", shape=21, fill="black", size=0.4, alpha=0.7)+
#   geom_point(data=DailyEcFluxes, aes(monthday, meanCH4Flux), alpha=0.3)+
#   facet_grid(year~.)+
#   ylab("Daily Mean CH4 Flux (mg m-2 hr-1)")+
#   xlab("")+
#   scale_x_date(breaks=date_breaks("1 month"),
#                labels=date_format("%d %b"))+
#   theme_bw()+
#   theme(axis.text.x=element_text(angle=45, hjust=1))
# 
# 
# p2<-ggplot(filter(CombinedData.F, !is.na(year)), aes(monthday, ch4_filled*3600/1000*16))+
#   geom_line(alpha=0.3, color="grey")+
#   ylim(-1*3.6*16, 2.5*3.6*16)+
#   geom_point(data=filter(CombinedData.F, !is.na(year)), 
#              aes(monthday, ch4_flux*3.6*16), alpha=0.6)+
#   geom_line(data=DailyEcFluxes.F, (aes(monthday, meanCH4Flux)), color="red")
# p2+facet_grid(year~.)+ylab("CH4 Flux (mg m-2 hr-1)")+
#   theme_bw()

# DailyEcFluxes.F$ch4.cumu<-cumsum(DailyEcFluxes.F$meanCH4Flux*24/1000) 
# 
# summer2017<-filter(DailyEcFluxes.F, DateTime>"2017-05-01 00:00:00", DateTime<"2017-11-01 00:00:00")
# summer2017val<-sum(summer2017$meanCH4Flux*24/1000)
# 
# summer2018<-filter(DailyEcFluxes.F, DateTime>"2018-05-01 00:00:00", DateTime<"2018-11-01 00:00:00")
# summer2018val<-sum(summer2018$meanCH4Flux*24/1000)
# #convert from mg/m2/hr to g/m2/d
# #adding up over days, so days get integrated away into g CH4 / m2
# 
# 
# ggplot(DailyEcFluxes.F, aes(DateTime, ch4.cumu))+
#   geom_line()+
#   ylab("g CH4 m-2")
# max(DailyEcFluxes.F$ch4.cumu) #108
# 
# #annual
# DailyEcFluxes.F$DateTime[365]
# val2017<-DailyEcFluxes.F$ch4.cumu[365]
# DailyEcFluxes.F$ch4.cumu.a<-DailyEcFluxes.F$ch4.cumu
# 
# DailyEcFluxes.F$ch4.cumu.a[366:730]<-DailyEcFluxes.F$ch4.cumu.a[366:730]-val2017
# ggplot(DailyEcFluxes.F, aes(DateTime, ch4.cumu.a))+
#   geom_line()+
#   ylab("g CH4 m-2")
# 
# ggplot(filter(DailyEcFluxes.F, year<2019),
#        aes(monthday, ch4.cumu.a))+
#   geom_line(aes(color=as.factor(year)))+
#   ylab("g CH4 m-2")+
#   scale_x_date(labels=date_format("%b", tz="UTC"), 
#                breaks=date_breaks("1 month"))+  
#   theme_bw()
#   facet_grid(year~.)
#     scale_x_date(labels=date_format("%b %d", tz="UTC"), 
#                  breaks=date_breaks("1 month"))+  
# 
# select(DailyEcFluxes.F, DateTime, meanCH4Flux, ch4.cumu)
#     
