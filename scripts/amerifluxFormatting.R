
#This script prepares a file in the format needed to submit to AmeriFlux
#before running, load the following data frames (you can use loadPrelimOutputs.R):
#epOutOrder, rbrTsub, campMet, vanni30min

#file.edit("scriptsAndRmd/loadPrelimOutputs.R")



USact<-mutate(epOutOrder,
              RDateTime_START = RDateTime,
              RDateTime_END = (RDateTime+30*60),
              FC_SSITC_TEST = qc_co2_flux,
              FCH4_SSITC_TEST = qc_ch4_flux,
              FETCH_70 = x_70,
              FETCH_90 = x_90,
              FETCH_FILTER = -9999, # 0 and 1 flag indicating direction that should be discarded and kept, respectively
              FETCH_MAX = x_peak,
              CH4 = ch4_mixing_ratio*1000, #nmolCH4 per mol
              CO2 = co2_mixing_ratio, #umol CO2 per mol
              CO2_SIGMA = sqrt(co2_var),
              FC = co2_flux, #umolCO2 m-2 s-1
              FCH4 = ch4_flux*1000, #nmolCH4 m-2 s-1
              H2O = h2o_mixing_ratio, #mmol mol-1
              H2O_SIGMA = sqrt(h2o_var/1000), #h2o_var doesn't have units, and an 
              # investigation (see co2FluxDiagnostics) reveals that
              # it is generally ~1000x the variance calculated from the raw 
              # dataset, so probably in umol/mol, while h2o mixing ratio is
              # in mmol/mol. Likely not precisely a factor of 1000 due to processing steps.  
              SC = co2_strg,
              SCH4 = ch4_strg*1000, #nmol/m2/s
              H = H,
              H_SSITC_TEST = qc_H,
              LE = LE,
              LE_SSITC_TEST = qc_LE,
              SH = H_strg,
              SLE = LE_strg,
              PA = air_pressure/1000, #kPa -- air_p_mean was being loaded in incorrectly
              RH = RH,
              T_SONIC = sonic_temperature-273.15, #C
              TA = air_temperature-273.15, #C
              VPD = VPD/100, #hPa
              P=-9999, #precipitation
              P_RAIN = -9999, #rainfall, from VWS
              NETRAD = -9999, #net radiation, W/m2, from our net radiometer
              PPFD_IN = -9999, #PPFD, incoming
              TS = -9999, #soil temperature, sed t?, from RBRs, ~1.6m
              TW_1 = -9999, #water T, from RBRs -0.1
              TW_2 = -9999, #water T, from RBRs -0.25
              TW_3 = -9999, #water T, from RBRs -0.5
              TW_4 = -9999, #water T, from RBRs -0.75
              TW_5 = -9999, #water T, from RBRs -1.0 
              TW_6 = -9999, #water T, from RBRs -1.25
              WTD = -9999,
              MO_LENGTH = L,
              TAU = -Tau, #ameriflux sign convention: negative value of Tau indicates a downward transport of momentum flux
              TAU_SSITC_TEST = qc_Tau,
              U_SIGMA = sqrt(u_var), #rotated?
              USTAR = ustar,
              V_SIGMA = sqrt(v_var),
              W_SIGMA = sqrt(w_var),
              WD = wind_dir,
              WS = wind_speed,
              WS_MAX = max_wind_speed,
              ZL=zL)%>%
  select(RDateTime_START, RDateTime_END, FC_SSITC_TEST, FCH4_SSITC_TEST, FETCH_70, FETCH_90,
         FETCH_FILTER, FETCH_MAX, CH4, CO2, CO2_SIGMA, FC, FCH4, H2O, H2O_SIGMA, SC, SCH4,
         H, H_SSITC_TEST, LE, LE_SSITC_TEST, SH, SLE, PA, RH, T_SONIC, TA, VPD, P, P_RAIN, 
         NETRAD, PPFD_IN, TS, TW_1, TW_2, TW_3, TW_4, TW_5, TW_6, WTD, MO_LENGTH, TAU, 
         TAU_SSITC_TEST, U_SIGMA, USTAR, V_SIGMA, W_SIGMA, WD, WS, WS_MAX, ZL)

### filter values outside of plausible range (per email from Ameriflux Team):

ggplot(USact, aes(RDateTime_START, NETRAD))+
  geom_point()+
  ylim(-9000, 2000)

USact<-USact%>%
  mutate(CH4=replace(CH4, CH4< (-750), NA),
         CO2=replace(CO2, CO2>1570, NA),
         FC=replace(FC, abs(FC)>110, NA),
         FCH4=replace(FCH4, FCH4>5275, NA),
         H2O=replace(H2O, H2O>105, NA))


#merge RBRs
amerifluxTime<-select(USact, RDateTime_START)
amerifluxTime$RDateTime<-amerifluxTime$RDateTime_START

amerifluxRBR<-left_join(amerifluxTime, rbrTsub, by = "RDateTime")    
amerifluxRBR2<-subset(amerifluxRBR, !duplicated(RDateTime)) #30693
USact<-subset(USact, !duplicated(RDateTime_START)) #30693

#give USact RBR values for TS, TW 1 thru 6 where available, -9999s where not avail
USact<-mutate(USact,
              TW_1 = amerifluxRBR2$RBRmeanT_0.1,
              TW_2 = amerifluxRBR2$RBRmeanT_0.25,
              TW_3 = amerifluxRBR2$RBRmeanT_0.5,
              TW_4 = amerifluxRBR2$RBRmeanT_0.75,
              TW_5 = amerifluxRBR2$RBRmeanT_1,
              TW_6 = amerifluxRBR2$RBRmeanT_1.25,
              TS = amerifluxRBR2$RBRmeanT_1.6)
# ggplot(campMet, aes(RDateTime, Rain_mm_tot))+
#   geom_point(alpha=0.3)
# ggplot(vanni30min, aes(RDateTime, dailyRain.vws))+
#   geom_point()
# ggplot(filter(vanni30min, RDateTime>"2017-10-01"), aes(RDateTime, rain30min))+
#   geom_point()




#merge precip & PAR & water level from VWS
# also need to change precip from daily cumulative to 30-min


# P=-9999, #precipitation
# P_RAIN = -9999, #rainfall, from VWS
# NETRAD = -9999, #net radiation, W/m2, from our net radiometer
# PPFD_BC_IN = -9999, #PPFD, below canopy, incoming
amerifluxVWS<-left_join(amerifluxTime, vanni30min, by="RDateTime") %>% #30709
  subset(!duplicated(RDateTime)) #30693

#merge net radiation and precip from campbell suite   
amerifluxCampVWS<-left_join(amerifluxVWS, campMet, by="RDateTime")%>%
  subset(!duplicated(RDateTime)) #30693

# ggplot(amerifluxCampVWS, aes(rain30min, Rain_mm_tot))+
#   geom_point(alpha=0.2)# 
# ggplot(filter(amerifluxCampVWS, RDateTime>"2018-04-15", 
#               RDateTime<"2018-07-01"))+
#   geom_point(aes(RDateTime, rain30min, color="VWS"), alpha=0.3)+
#   geom_point(aes(RDateTime, Rain_mm_tot, color="tower"), alpha=0.3)+
#   ylim(0, 5)


ggplot(amerifluxCampVWS, aes(RDateTime, NR_Wm2_avg))+
  geom_line()



#give USact VWS values for PAR, precip, and water level (WTD) where avail, -9999s where not avail
#also fill in fetch filter values here
for(i in 1:length(USact$TS)){
  USact$P[i] = if(!is.na(amerifluxCampVWS$rain30min[i])) amerifluxCampVWS$rain30min[i] else
    if(!is.na(amerifluxCampVWS$Rain_mm_tot[i])) amerifluxCampVWS$Rain_mm_tot[i] else
      -9999
  USact$P_RAIN[i] = USact$P[i]
  USact$PPFD_IN[i] = ifelse(!is.na(amerifluxCampVWS$par.vws[i]), 
                            amerifluxCampVWS$par.vws[i], -9999)
  USact$WTD[i] = ifelse(!is.na(amerifluxCampVWS$levelAdj.vws[i]), #level adjust has the offset for the step change, plus 1 m to account for the depth difference between the flux footprint and the msmt site
                        amerifluxCampVWS$waterLevel.vws[i], -9999)
  USact$NETRAD[i] = ifelse(!is.na(amerifluxCampVWS$NR_Wm2_avg[i]), 
                           amerifluxCampVWS$NR_Wm2_avg[i], -9999)
  USact$FETCH_FILTER[i]=ifelse(USact$RDateTime_START<"2018-05-01 00:00:00" & USact$WD>195 & USact$WD<330, 0, #value if winds are from the W at the dock
                               1) #value if aquatic tower -- no fetch filter 
}

# ggplot(USact, aes(RDateTime_START, P))+
#   geom_point(alpha=0.2)+
#   ylim(0, 10)



#change all na's to -9999's
USact[is.na(USact)]<- -9999
USact[is.nan(USact)]<- -9999
USactNA<-USact

USactNA[USact== -9999]<- NA

sum(is.na(USactNA$FETCH_FILTER))

#check on outliers
# ggplot(USactNA, aes(RDateTime_START, CO2))+
#   geom_point()
# ggplot(USactNA, aes(RDateTime_START, CH4))+
#   geom_point()
# ggplot(USactNA, aes(RDateTime_START, FC))+
#   geom_point()
# ggplot(USactNA, aes(RDateTime_START, FCH4))+
#   geom_point()
# ggplot(USactNA, aes(RDateTime_START, H2O))+
#   geom_point()
# ggplot(USactNA, aes(RDateTime_START, SC))+
#   geom_point()
# ggplot(USactNA, aes(RDateTime_START, SCH4))+
#   geom_point()
# ggplot(USactNA, aes(RDateTime_START, NETRAD))+
#   geom_point()

USactNA<-USactNA%>%
  mutate(NETRAD=replace(NETRAD, NETRAD< (-390), NA),
         SC = replace(SC, abs(SC)>200, NA),
         SCH4=replace(SCH4, abs(SCH4)>200, NA))

USactOF<-USactNA
USactOF[is.na(USactOF)]<- -9999 #OF for outlier filtered

sum(is.na(USactOF$NETRAD))


#2017
USactSub17<-filter(USactOF, RDateTime_START>"2017-01-25 18:30",
                 RDateTime_START<"2017-12-31 19:00")   
#2018
USactSub18<-filter(USactOF, RDateTime_START>"2018-01-01 00:00",
                 RDateTime_START<"2018-12-31 23:30")    
head(USactSub17$RDateTime_START)
tail(USactSub17$RDateTime_END)
head(USactSub18$RDateTime_START)
tail(USactSub18$RDateTime_END)

#check for missing HH periods:
USactSub17$check<-c(1800, diff(as.numeric(USactSub17$RDateTime_START), 1))
summary(USactSub17$check)
USactSub18$check<-c(1800, diff(as.numeric(USactSub18$RDateTime_START), 1))
summary(USactSub18$check)
ggplot(filter(USactSub, RDateTime_START>"2017-01-01", RDateTime_START<"2017-02-01"),
       aes(RDateTime_START, check))+
  geom_point()




#change timestampts to YYYYMMDDHHMM format
#strptime(USactSub$RDateTime_START, "%Y-%m-%d %H:%M:%S")
USactSub17<-mutate(USactSub17,
                 TIMESTAMP_START=format(strptime(RDateTime_START,
                                                 "%Y-%m-%d %H:%M:%S"), 
                                        "%Y%m%d%H%M"),
                 TIMESTAMP_END=format(strptime(RDateTime_END, 
                                               "%Y-%m-%d %H:%M:%S"), 
                                      "%Y%m%d%H%M"))%>%
  select(TIMESTAMP_START, TIMESTAMP_END, FC_SSITC_TEST, FCH4_SSITC_TEST, FETCH_70, FETCH_90,
         FETCH_FILTER, FETCH_MAX, CH4, CO2, CO2_SIGMA, FC, FCH4, H2O, H2O_SIGMA, SC, SCH4,
         H, H_SSITC_TEST, LE, LE_SSITC_TEST, SH, SLE, PA, RH, T_SONIC, TA, VPD, P, P_RAIN, 
         NETRAD, PPFD_IN, TS, TW_1, TW_2, TW_3, TW_4, TW_5, TW_6, WTD, MO_LENGTH, TAU, 
         TAU_SSITC_TEST, U_SIGMA, USTAR, V_SIGMA, W_SIGMA, WD, WS, WS_MAX, ZL)


head(USactSub17)

USactSub18<-mutate(USactSub18,
                   TIMESTAMP_START=format(strptime(RDateTime_START,
                                                   "%Y-%m-%d %H:%M:%S"), 
                                          "%Y%m%d%H%M"),
                   TIMESTAMP_END=format(strptime(RDateTime_END, 
                                                 "%Y-%m-%d %H:%M:%S"), 
                                        "%Y%m%d%H%M"))%>%
  select(TIMESTAMP_START, TIMESTAMP_END, FC_SSITC_TEST, FCH4_SSITC_TEST, FETCH_70, FETCH_90,
         FETCH_FILTER, FETCH_MAX, CH4, CO2, CO2_SIGMA, FC, FCH4, H2O, H2O_SIGMA, SC, SCH4,
         H, H_SSITC_TEST, LE, LE_SSITC_TEST, SH, SLE, PA, RH, T_SONIC, TA, VPD, P, P_RAIN, 
         NETRAD, PPFD_IN, TS, TW_1, TW_2, TW_3, TW_4, TW_5, TW_6, WTD, MO_LENGTH, TAU, 
         TAU_SSITC_TEST, U_SIGMA, USTAR, V_SIGMA, W_SIGMA, WD, WS, WS_MAX, ZL)


head(USactSub18)

#2017
# write.table(USactSub, 
#             file=("output/US-Act_HH_201701260000_201712311800.csv"),
#             sep=",",
#             row.names=FALSE)
write.table(USactSub17, 
            file=("output/US-Act_HH_201701260300_201712311800.csv"),
            sep=",",
            row.names=FALSE)
#2018
# write.table(USactSub, 
#             file=("C_R_Projects/actonFluxProject/output/US-Act_HH_201801011130_201811131230.csv"),
#             sep=",",
#             row.names=FALSE)
write.table(USactSub18, 
            file=("output/US-Act_HH_201801011130_201811131230.csv"),
            sep=",",
            row.names=FALSE)
