
### raw 10Hz time series of CH4 mixing ratios and 3D wind speeds 
### were processed into 30-0min fluxes using EddyPro.
### the EP output files were loaded and concatenated and resaved into one file, 
### "epOutOrder.csv", which is saved in the data folder and read in by the 
### readData.R script

### This script filters the 30-minute time series for periods that do not meet the 
### QC parameters and for outliers
### Then takes daily averages

### FILTER AND SELECT THE EDDY PRO OUTPUT ----

##Select the variables we want:
epOutSub<-select(epOutOrder, RDateTime, date,	time, DOY, daytime, Tau,qc_Tau,H,	qc_H,	rand_err_H, LE,	qc_LE, rand_err_LE,
                 co2_flux,	qc_co2_flux, rand_err_co2_flux, ch4_flux,	qc_ch4_flux, rand_err_ch4_flux,
                 co2_mixing_ratio,	h2o_mixing_ratio, ch4_mixing_ratio,	
                 air_temperature,	air_pressure, air_density,	air_heat_capacity,
                 ET,	water_vapor_density,	e,	es,	specific_humidity,	RH,	VPD,	Tdew,
                 u_rot,	v_rot,	w_rot,	wind_speed, max_wind_speed,	wind_dir,	ustar,	TKE,	L,	zL,	
                 bowen_ratio,	Tstar,	model,	x_peak,	x_offset,	x_10,	x_30,	x_50,	x_70,
                 x_90, w_unrot)


epOutSub$qc_ch4_factor<-as.factor(epOutSub$qc_ch4_flux)

##Filter fluxes for QAQC parameters and replace with NAs using mutate: 
epOutSubFilt<-epOutSub %>% 
  #epOutFilt.test<-ep.test2%>%
  mutate(
    #QC level 2
    ch4_flux=replace(ch4_flux, qc_ch4_flux==2, NA),
    co2_flux=replace(co2_flux, qc_co2_flux==2, NA),
    LE=replace(LE, qc_LE==2, NA),
    H=replace(H, qc_H==2, NA),
    #winds from the shore before tower relocation
    ch4_flux=replace(ch4_flux, RDateTime < "2018-04-30 00:00:00" & 
                       wind_dir>195 & wind_dir<330, NA),
    co2_flux=replace(co2_flux, RDateTime < "2018-04-30 00:00:00" &
                       wind_dir>195 & wind_dir<330, NA),
    H=replace(H, RDateTime < "2018-04-30 00:00:00" &
                wind_dir>195 & wind_dir<330, NA),
    LE=replace(LE, RDateTime < "2018-04-30 00:00:00" &
                 wind_dir>195 & wind_dir<330, NA),
    #ustar filter for CO2 and CH4 after tower relocation
    ch4_flux=replace(ch4_flux, ustar<0.07 & RDateTime>"2018-05-01 00:00:00", NA),
    co2_flux=replace(co2_flux, ustar<0.07 & RDateTime>"2018-05-01 00:00:00", NA),
    #co2_flux=replace(co2_flux, abs(co2_flux)>20, NA),
    rand_err_ch4_flux=replace(rand_err_ch4_flux, qc_ch4_flux==2, NA),
    rand_err_co2_flux=replace(rand_err_co2_flux, qc_co2_flux==2, NA),
    rand_err_H=replace(rand_err_H, qc_H==2 | wind_dir>195 & wind_dir<330, NA),
    rand_err_LE=replace(rand_err_LE, qc_LE==2 | wind_dir>195 & wind_dir<330, NA),
    #absolute limit:
    ch4_flux=replace(ch4_flux, abs(ch4_flux)>500, NA),
    co2_flux=replace(co2_flux, abs(co2_flux)>15000, NA),
    LE = replace(LE, abs(LE)>1000, NA),
    H = replace(H, abs(H)>200, NA),
    rand_err_ch4_flux=replace(rand_err_ch4_flux, wind_dir>195 & wind_dir<330, NA),
    rand_err_ch4_flux=replace(rand_err_ch4_flux, abs(ch4_flux)>500, NA),
    year = year(RDateTime),
    monthday = format(RDateTime, format="%m-%d %H:%M")%>%
      as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC"))

#outlier filter
epOutSubFilt<-epOutSubFilt %>% 
  #epOutFilt.test<-epOutFilt.test%>%  
  mutate(ch4_flux=replace(ch4_flux, ch4_flux<0 & qc_co2_flux==2 & qc_ch4_flux==1, NA),
         ch4_flux=replace(ch4_flux, ch4_flux<0 & qc_LE==2 & qc_ch4_flux==1, NA),
         ch4_flux=replace(ch4_flux, ch4_flux<0 & qc_H==2 & qc_ch4_flux==1, NA),
         ch4_flux=replace(ch4_flux, RDateTime>"2018-08-01" & ch4_flux>1 & qc_LE==2 & qc_ch4_flux==1, NA),
         ch4_flux=replace(ch4_flux, RDateTime>"2018-08-01" & ch4_flux>1 & qc_co2_flux==2 & qc_ch4_flux==1, NA),
         ch4_flux=replace(ch4_flux, RDateTime>"2018-08-01" & ch4_flux>1 & qc_H==2 & qc_ch4_flux==1, NA),
         ch4_flux=replace(ch4_flux, RDateTime<"2018-01-01" & ch4_flux>1 & qc_LE==2 & qc_ch4_flux==1, NA),
         ch4_flux=replace(ch4_flux, RDateTime<"2018-01-01" & ch4_flux>1 & qc_H==2 & qc_ch4_flux==1, NA),
         ch4_flux=replace(ch4_flux, RDateTime<"2018-01-01" & ch4_flux>1 & qc_co2_flux==2 & qc_ch4_flux==1, NA))

#additional CO2, LE, and H outliers
epOutSubFilt<-epOutSubFilt%>%
  #epOutFilt.test<-epOutFilt.test%>%
  mutate(ch4_flux = replace(ch4_flux, as.chron(RDateTime) == 
                              as.chron("2017-05-24 02:00:00 UTC"), NA),#, #ch4_flux = -2.4 umol/m2/s, LE & CO2 qc = 2, CH4 qc = 1
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2017-06-21 06:00:00 UTC"), NA), #LE qc = 2, ch4 qc = 1
         co2_flux = replace(co2_flux, as.chron(RDateTime) ==
                              as.chron("2017-06-21 06:00:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2017-07-03 20:30:00 UTC"), NA),#H, 
         co2_flux = replace(co2_flux, as.chron(RDateTime) ==
                              as.chron("2017-07-03 20:30:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2017-10-20 18:00:00 UTC"), NA),# qc for H, co2 = 2, ch4 qc = 1, LE isd negative outlier
         LE = replace(LE, as.chron(RDateTime) ==
                        as.chron("2017-10-20 18:00:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2018-06-28 14:30:00 UTC"), NA), #ch4_flux = -0.6, H qc = 2, CH4 qc = 1
         co2_flux = replace(co2_flux, as.chron(RDateTime) ==
                              as.chron("2018-06-28 14:30:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2017-07-07 06:30:00 UTC"), NA), #ch4_flux = -0.6, H &LE qc = 2, CH4 qc = 1, CO2 outlier
         co2_flux = replace(co2_flux, as.chron(RDateTime) ==
                              as.chron("2017-07-07 06:30:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2017-09-21 16:30:00 UTC"), NA), #ch4_flux = -1.8, CO2 qc = 2, LE is a negative outlier
         LE = replace(LE, as.chron(RDateTime) ==
                        as.chron("2017-09-21 16:30:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2017-07-26 19:30:00 UTC"), NA), #qcLE = 2, qc ch4 = 1
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2017-09-17 18:30:00 UTC"), NA), #co2 QC = 2, ch4 qc = 1, LE is a negative outlier
         LE = replace(LE, as.chron(RDateTime) ==
                        as.chron("2017-09-17 18:30:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2017-09-15 18:00:00 UTC"), NA), #qcLE = 2, qcch4, co2 H = 1, H, co2 are outliers
         co2_flux = replace(co2_flux, as.chron(RDateTime) ==
                              as.chron("2017-09-15 18:00:00 UTC"), NA),
         H = replace(H, as.chron(RDateTime) ==
                       as.chron("2017-09-15 18:00:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) == 
                              as.chron("2017-09-18 12:00:00 UTC"), NA),#LE qc = 2, both CO2 and CH4 are negative outliers
         co2_flux = replace(co2_flux, as.chron(RDateTime) == 
                              as.chron("2017-09-18 12:00:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) == 
                              as.chron("2017-07-05 21:30:00 UTC"), NA), #LE qc=2, co2 and ch4 are negative outliers, ch4 qc = 1
         co2_flux = replace(co2_flux, as.chron(RDateTime) == 
                              as.chron("2017-07-05 21:30:00 UTC"), NA),
         H = replace(H, as.chron(RDateTime) == 
                       as.chron("2017-07-05 21:30:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) == 
                              as.chron("2017-05-22 20:00:00 UTC"), NA), #co2 qc = 2, ch4 qc = 1, LE, H outliers
         H = replace(H, as.chron(RDateTime) == 
                       as.chron("2017-05-22 20:00:00 UTC"), NA),
         LE = replace(LE, as.chron(RDateTime) == 
                        as.chron("2017-05-22 20:00:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) == 
                              as.chron("2017-07-21 23:00:00 UTC"), NA))#, #qc CO2, LE = 2, qc ch4 = 1


# write.table(epOutSubFilt, 
#             file=("C:/R_Projects/actonFluxProject/output/acton30minFluxes.csv"),
#             sep=",",
#             row.names=FALSE)
# 
# write.table(select(epOutSubFilt, -qc_ch4_factor, -year, -monthday),
#             file=("C:/R_Projects/actonFluxProject/output/acton30minFluxes_uwe.csv"),
#             sep=",",
#             row.names=FALSE)

##Daily Averages, convert from umol m-2 s-1 to mg m-2 HOUR-1:
DailyEcFluxes<-epOutSubFilt %>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
  dplyr::summarize(meanCH4Flux = (mean(ch4_flux, na.rm=TRUE)/1000*16*60*60),
                   sdCH4Flux = (sd(ch4_flux, na.rm=TRUE)/1000*16*60*60),
                   randErrCh4Prop = sqrt(sum((rand_err_ch4_flux/1000*16*60*60)^2, 
                                             na.rm=TRUE)),
                   meanCO2Flux = (mean(co2_flux, na.rm=TRUE)/1000*44*60*60),
                   sdCO2Flux = (sd(co2_flux, na.rm=TRUE)/1000*44*60*60),
                   nCH4Flux = n_distinct(ch4_flux, na.rm=TRUE),
                   nCO2Flux =  n_distinct(co2_flux, na.rm=TRUE),
                   meanH = (mean(H, na.rm=TRUE)),
                   meanLE = (mean(LE, na.rm=TRUE)),
                   meanAirT = (mean(air_temperature, na.rm=TRUE)-273.15),
                   meanWnd = mean(wind_speed), na.rm=TRUE)
DailyEcFluxes<-DailyEcFluxes%>%
  mutate(RDateTime=as.Date(DailyEcFluxes$RDateTime),
         year = year(RDateTime),
         monthday = format(RDateTime, format="%m-%d %H:%M"))# %>%
DailyEcFluxes$monthday<-as.Date(DailyEcFluxes$monthday, format="%m-%d %H:%M")


# # range(epOutSubFilt$RDateTime)
# # #[1] "2017-01-26 00:30:00 EST" "2018-11-13 17:00:00 EST"
# ggplot(filter(epOutSubFilt, monthday>"2019-01-01", monthday<"2019-12-01"), 
#        aes(monthday, ch4_flux))+
#   annotate("rect", xmin=as.POSIXct(as.Date("2019-05-24")),
#            xmax=as.POSIXct(as.Date("2019-06-04")),
#            ymin=-Inf, ymax=Inf, alpha=0.5)+
#   geom_line(alpha=0.5)+
#   geom_point(alpha=0.1, size=1)+
#   scale_x_datetime(date_breaks = "1 month",
#                    labels=date_format("%b"))+
#   ylim(-0.25, 3.5)+
#   #ylim(-10, 10)+
#   facet_grid(year~.)+
#   ylab(expression(CH[4]~Flux~(umol~m^-2~s^-1)))+
#   xlab("")+
#   theme_bw()
# 
# ggplot(filter(DailyEcFluxes, monthday>"2020-05-15", monthday<"2020-07-15"),
#        aes(monthday, meanCH4Flux))+
#   geom_line()+
#   geom_point(alpha=0.5)+
#   scale_x_date(date_breaks = "1 week",
#                labels=date_format("%b %d"))+
#   facet_grid(year~.)
# 
# ggplot(DailyEcFluxes,
#        aes(monthday, meanCH4Flux))+
#   geom_point(alpha=0.5)+
#   #ylim(-500, 1000)+
#   ylab(expression(CH[4]~Flux~(mg~CH[4]~m^-2~hr^-1)))+
#   #stat_smooth(se =FALSE)+
#   scale_x_date(labels = date_format("%b"))+
#   facet_grid(year~.)      
