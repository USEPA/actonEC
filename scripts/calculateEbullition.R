
####Steps in getting from voltage to ebullitive emissions:-----
# 0. Parse data from hobo data frame into shallow (U14) and deep (U12) sites
#    then filter time periods of bad readings -- circut not working, trap otherwise compromised. 
# 1. Convert from voltage to height using the calibration coefficients
#    measured in the lab
#    includes adding slope error column (hMultErr, deltaM) 
# 2. Convert from height to volume using the cross-sectional area of
#    inverted funnel tube. Different for each site, and maybe over time.
# 3. Smooth with 12-point moving average following Varadharajan, 2012 
#     - use zoo::rollapply, which can deal with NAs

# 4.Decide what timestep for dVol/dt to use. Too short 
#    will introduce noise, too long we lose temporal resolution.
#    The hobo's log on a 5-min timestep. 
#    >>30-min dt matchs the EC fluxes; calculate 30-min, 1-hr, 2, 6, 12, 24, and 48 hr time bins 
# From Varadharajan, 2012: 
#   - ebullitive gas fluxes were calculated by dividing smoothed cumulative gas 
#     volumes by the trap cross-sectional area and a time-bin width (2, 6, 12, 24
#     or 48 h), starting with the first data point of each signal. 

# 5. Filter instances of negative dVol/dt
# From Varadharajan, 2012: 
#   - small negative fluxes, which occurred ~15% of the time due to the 3-6 mL error 
#     in recorded volumes, were treated as zero fluxes

# 6. Calculate volumetric ebullitive fluxes by dividing dVol/dt by the
#    cross-sectional area of the funnels themselves then normalize for time: vol gas m-2 hr-1
#    funnel diameter: 54 cm (0.54 m)

# 7. Combine the vol flux with the GC gas concentration msmts --> mg GAS m-2 hr-1
#    And convert from volumetric flux in mL m^-2 h^-1 to to mass flux in mg GAS m-2 hr-1
#    using the GC msmts. Need:
#      >volumetric flux in units of mL m^-2 hr-1, named ebMlHrM2
#      >gas temperature, named "Tmp_C_S"
#      >gas concentrations named: "trap_gas.ppm"
#      >barometric pressure in units of atm (assuming, because default is 1)
###-------


### 0. Parse data and filter for bad readings ---- 
## "hobo" dataframe was loaded by readData.R; includes  files from acton and harsha
#hoboActon<-filter(hobo, lake.name=="acton lake")
hoboU12<-filter(hobo, lake.name=="acton lake", site=="u12") #deep site
hoboU14<-filter(hobo, lake.name=="acton lake", site=="u14") #shallow site

#metaDataTrapAct has been loaded as part of compileGcData.R script. This
#data frame includes trap.size, circuit, gas.vol.ml, and exetainer.code, site.visit.dateTime

######Deep Site aka U12:
## On 6/9/17 field visit, the trap was missing at the deep site (U-12). 
## a trap was re-deployed on 6/12/17. The trap was deployed on 5/26 came un-moored 
## the afternoon on 6/3 at 14:30
## Zero voltage from 6/26 14:00 thru 7/14 at 11:00
## Bad readings after 10/31 thru end of monitoring season
hoboU12<-hoboU12%>%
  mutate(volt = replace(volt, date.time>"2017-06-03 14:30:00" & date.time<"2017-06-12 00:00:00", NA),
         volt = replace(volt, date.time>"2017-06-12" & date.time<"2017-06-27" & volt<0.5, NA), #strange period with lots of voltage dropouts
         volt = replace(volt, date.time>"2017-06-26 14:00:00" & date.time<"2017-07-14 11:00:00", NA),
         volt = replace(volt, date.time>"2017-10-31 12:00:00" & date.time<"2017-12-14 11:00:00", NA),
         volt = replace(volt, date.time>"2018-04-01 12:00:00" & date.time<"2018-05-25 00:00:00", NA),
         volt = replace(volt, date.time>"2018-09-20 16:00:00" & date.time<"2018-10-03 01:00:00", NA),
         volt = replace(volt, date.time>"2018-11-10 00:00:00", NA)) 

######Shallow site aka U14:
## Some strange drop-outs between May 20-22: filter if voltage is <0.5
## Spike on 5/26 thats causing a spike in the final eb calc
## Zero voltage from 6/26 at 13:00 thru 7/14 at 13:30
## After 10/5, the signal looks very bad

hoboU14<-hoboU14%>%
  mutate(volt = replace(volt, date.time>"2017-05-10 01:00:00" & date.time<"2017-05-17 05:00:00" & volt >0.15, NA),
         volt = replace(volt, date.time>"2017-05-26 01:00:00" & date.time<"2017-05-27 05:00:00" & volt >1.75, NA),
         volt = replace(volt, date.time>"2017-05-20 01:00:00" & date.time<"2017-05-22 05:00:00" & volt <0.5, NA),
         volt = replace(volt, date.time>"2017-06-26 13:00:00" & date.time<"2017-07-14 13:30:00", NA),
         volt = replace(volt, date.time>"2017-10-05 00:00:00" & date.time<"2017-12-14 11:00:00", NA),
         volt = replace(volt, date.time>"2018-04-01 00:00:00" & date.time<"2018-06-07 12:00:00", NA)) 

#round the hobo data logger time to be on the 5-min
hoboU12$date.timeHH<-lubridate::round_date(hoboU12$date.time, "5 minutes")
hoboU14$date.timeHH<-lubridate::round_date(hoboU14$date.time, "5 minutes")


### Raw data to Volumetric Fluxes: ----
###1: Convert from voltage (V) to height (cm) using the calibration coefficients:----

#U12 (deep site) had the following circuits during the following dates:
# 2017 10 May - 9 June: #9. height = 31.603*volt - 13.475
# 2017 12 June - 14 July: #19. height = 28.742*volt - 4.8 (from 2015 calibration)
# 2017 14 July - 11 Dec: # 9
# 2018 all season: #9

hoboU12$hMult<-ifelse(hoboU12$date.time>"2017-06-10 12:00:00" 
                      & hoboU12$date.time<"2017-07-14 13:00:00",
                      28.742, #value if TRUE -- period with circuit #19
                      31.903)#value if FALSE -- 2017 period with circuit #9
hoboU12$hMult<-ifelse(hoboU12$date.time<"2018-05-01 12:00:00",
                      hoboU12$hMult,#value if TRUE -- values from above,
                      32.24) #value if FALSE -- 2018 circuit #9 post-cal
hoboU12$hMultErr<-ifelse(hoboU12$date.time>"2017-06-10 12:00:00" 
                         & hoboU12$date.time<"2017-07-14 13:00:00",
                         0.198, #value if TRUE -- period with circuit #19
                         0.65)#value if FALSE -- 2017 period with circuit #9
hoboU12$hMultErr<-ifelse(hoboU12$date.time<"2018-05-01 12:00:00",
                         hoboU12$hMultErr,#value if TRUE -- values from above,
                         0.098) #value if FALSE -- 2018 circuit #9 post-cal

hoboU12$hOffset<-ifelse(hoboU12$date.time>"2017-06-10 12:00:00" 
                        & hoboU12$date.time<"2017-07-14 13:00:00",
                        -4.8, #value if TRUE -- period with circuit #19
                        -13.475) #value if FALSE -- 2017 period with circuit #9
hoboU12$hOffset<-ifelse(hoboU12$date.time>"2018-05-01 12:00:00",
                        hoboU12$hOffset,
                        -13.565) #value for 2018 circuit #9 post-cal
hoboU12$height<-hoboU12$volt*hoboU12$hMult+hoboU12$hOffset 

#U14 (shallow site) had the following circuits during the following dates:
# 2017 10 May - 14 July: #1. height = 32.565 * volt + 0.54 (from 2015 calibration)
# 2017 14 July - 11 Dec: #19. height = 28.742*volt - 4.8 (from 2015 calibration)
# 2018 all season: #11 as of 9/10, no info on circut calibration

hoboU14$hMult<-ifelse(hoboU14$date.time<"2017-07-14 13:00:00", 
                      32.565, #value if TRUE -- period when circuit #1
                      28.742) #value if FALSE -- period when circuit #19
hoboU14$hMult<-ifelse(hoboU14$date.time>"2018-05-01 12:00:00",
                      hoboU14$hMult,
                      31.67)#value for 2018 circuit #11 post-cal

### slope errors from circuit calibrations
### typical values from Varadharajan: ~0.5 V
hoboU14$hMultErr<-ifelse(hoboU14$date.time<"2017-07-14 13:00:00", 
                         0.1, #value if TRUE -- period when circuit #1
                         0.198) #value if FALSE -- period when circuit #19
hoboU14$hMultErr<-ifelse(hoboU14$date.time>"2018-05-01 12:00:00",
                         hoboU14$hMultErr,
                         0.02955)#value for 2018 circuit #11 post-cal

hoboU14$hOffset<-ifelse(hoboU14$date.time<"2017-07-14 13:00:00",
                        0.54, #value if TRUE -- period when circuit #1
                        -4.8) #value if FALSE -- period when circuit #19
hoboU14$hOffset<-ifelse(hoboU14$date.time>"2018-05-01 12:00:00",
                        hoboU14$hOffset,
                        -19.264) #value for 2018 circuit #11 post-cal

hoboU14$height<-hoboU14$volt*hoboU14$hMult+hoboU14$hOffset 
hoboU12$Site<-"Deep"
hoboU14$Site<-"Shallow"

#######For SI figure S1#######################

# hoboList<-list()
# hoboList[[1]]<-hoboU12
# hoboList[[2]]<-hoboU14
# hoboShalDeep<-do.call("rbind", hoboList)
# 
# hoboShalDeep<-mutate(hoboShalDeep,
#                      year=year(date.time),
#                      monthday = format(date.time, format="%m-%d %H:%M"))
# 
# hoboShalDeep$monthday<-as.POSIXct(hoboShalDeep$monthday, format="%m-%d %H:%M", tz="UTC")
# 
# ggplot(filter(hoboShalDeep, monthday>"2020-08-01", monthday<"2020-09-01"), #update to current year
#        aes(monthday, height))+
#   facet_grid(Site~year)+
#   geom_line()+
#   ylab("Height (cm)")+
#   xlab("Date")

###### 2: smooth with rolling average----

zwat<-zoo::rollapply(hoboU12$height, width = 12,FUN = mean)
hoboU12$hgSmth<-c(rep(NA, 5), zwat, rep(NA, 6))

zwat<-zoo::rollapply(hoboU14$height, width = 12,FUN = mean)
hoboU14$hgSmth<-c(rep(NA, 5), zwat, rep(NA, 6))

rm(zwat)
## 0.5: Determining terms for error calculation:  
#delta_hg = sqrt(delta_m^2[V_out^2+mean(V_zero^2)]+mean(m)^2[delta_V_out^2+delta_V_zero^2])

library(pracma)
library(caTools)

##### V_out (electonic noise)
##### detrend a linear period of ebulltion, then calculate sd

# vOutTest<-filter(filter(hoboU12, date.time>"2018-06-06 20:00:00", date.time<"2018-06-07 08:00"))%>%
#   select(volt) 
# vOutTest<-as.vector(vOutTest$volt, mode="numeric")#vector of voltage
# vOutTest.dT<-detrend(vOutTest, tt='linear')
# vOutTest.dT.diff<-diff(vOutTest.dT)
# mean(vOutTest.dT.diff, na.rm=TRUE) #0.4*10^-5 let's use Jake's 0.0004 V


#deltaVout<-0.0004
deltaVout<-0.003 #Varadharajan typical value for 1-hr moving average filter; up to 0.012 V for single period

# vOutTest<-lm(volt~date.time, data=filter(hoboU12, date.time>"2018-06-06 20:00:00", date.time<"2018-06-07 08:00"))  
# summary(vOutTest)

##### Offset error (deltaVzero):
##### Includes effects of short term voltage drift, T fluxtuations, 
##### random V errors due to incomplete gas flushing, or gas remaining in the chamber after sampling
##### The offset error at any point in time is calculated as the sd of two nearest empty trap voltages
##### Typical values from Varadharajan: 0.15 +/- 0.015

Vzero<-0.15
deltaVzero<-0.015

# 
#vMinTest12<-unique(U12_Vzero$minV)
vMinTest12<-c(0.36569, 0.36, 0.34676, 0.39, 0.34, 0.38706, 0.46886, 0.42857, # thru sept
              0.40659, 0.47350, 0.35, 0.40, 0.37, 0.45, #thru July 2018
              0.40232, 0.31807, 0.33700, 0.34, 0.55, 0.48, 0.56)
vZeroBar12<-mean(vMinTest12) #0.41
vMinRollSd12<-runsd(vMinTest12, k=3)
deltaVzero12<-mean(vMinRollSd12) #0.045
# 
#vMinTest14<-unique(U14_Vzero$minV)
vMinTest14<-c(0.20, 0.73, 0.21429, 0.56349, 0.20940, 0.61905, 0.72, 0.63309,
              0.51, 0.42430, 0.38645, 0.32173, 0.33, 0.38,  #thru end of sept 2017
              0.56166, 0.38400, 0.33944, 0.49939, 0.52, 0.6227, 0.52076,
              0.617,0.66,0.6, 0.75, 0.76, 0.7)
vZeroBar14<-mean(vMinTest14) #0.51
vMinRollSd14<-runsd(vMinTest14, k=3)
deltaVzero14<-mean(vMinRollSd14[7:27]) #0.071
# 
# deltaVout<-0.0004

# for(i in 1:nrow(hoboU12)){
#   hoboU12$deltaHg12<-sqrt()
# }



### Varadharajan typical value: deltaHg14<-1.4
#shallow site: 3.69 cm

# for(i in 1:nrow(hoboU12)){
#   hoboU12$deltaHg[i]<-sqrt((hoboU12$hMultErr[i]^2*(hoboU12$volt[i]^2+vZeroBar12^2))+(hoboU12$hMult[i]^2*(deltaVout^2+deltaVzero12^2)))
# }
# 
# for(i in 1:nrow(hoboU14)){
#   hoboU14$deltaHg[i]<-sqrt(hoboU14$hMultErr[i]^2*(hoboU14$volt[i]^2+vZeroBar14^2)+hoboU14$hMult[i]^2*(deltaVout^2+deltaVzero14^2))
# }


#using the general slope error term: 0.5
deltaM = 0.5
for(i in 1:nrow(hoboU12)){
  hoboU12$deltaHg[i]<-sqrt(deltaM^2*(hoboU12$volt[i]^2+vZeroBar12^2)+hoboU12$hMult[i]^2*(deltaVout^2+deltaVzero12^2))
}

for(i in 1:nrow(hoboU14)){
  hoboU14$deltaHg[i]<-sqrt(deltaM^2*(hoboU14$volt[i]^2+vZeroBar14^2)+hoboU14$hMult[i]^2*(deltaVout^2+deltaVzero14^2))
}

#summary(hoboU12$deltaHg) #mean = 1.58
#summary(hoboU14$deltaHg) #mean = 2.15



#calculate fractional errors for propagation
hoboU12$heightFE<-hoboU12$deltaHg/hoboU12$hgSmth #0.73
hoboU14$heightFE<-hoboU14$deltaHg/hoboU14$hgSmth #1.2

summary(hoboU14$heightFE) #mean: 6%; max = 17900%
summary(hoboU12$heightFE) #mean: 6%, max = 14300%

#filter out outlier points that disproportionately contribute to the error
qc_thresh12<-as.numeric(quantile(abs(hoboU12$heightFE), 0.99, na.rm=TRUE))
qc_thresh14<-as.numeric(quantile(abs(hoboU14$heightFE), 0.99, na.rm=TRUE))

# ggplot(hoboU12, aes(date.timeHH, heightFE))+
#   geom_line()
#qcCheck<-filter(hoboU14qc, abs(heightFE)>0.5)

hoboU12qc<-hoboU12%>%
  mutate(hgSmth = replace(hgSmth, abs(heightFE)>qc_thresh12, NA),
         heightFE = replace(heightFE, abs(heightFE)>qc_thresh12, NA))

ggplot(hoboU12qc, aes(date.timeHH, heightFE))+
  geom_line()

hoboU14qc<-hoboU14%>%
  mutate(hgSmth = replace(hgSmth, abs(heightFE)>qc_thresh14, NA),
         heightFE = replace(heightFE, abs(heightFE)>qc_thresh14, NA))

ggplot(hoboU14qc, aes(date.timeHH, heightFE))+
  geom_line()


## why does the shallow site 2017 have much higher error than other site/years? Could be due to concentration error

summary(filter(hoboU14qc, date.timeHH<"2018-01-01"))
summary(filter(hoboU14qc, date.timeHH>"2018-01-01"))

###3: Convert from smooth height to volume (cm3) ----
##U12 had a large-diameter tube until 6/12/2017 at 12:00, when a small diameter tube was deployed as the replacement for the missing trap
#hoboU12$date.time[8000] #this is 6/7. Large diam. tube became unmoored on 6/3 
#nrow(hoboU12)
hoboU12qc$diameter<-ifelse(hoboU12qc$date.time<"2017-06-12 12:00:00",
                         2.5, #value if true, before 6/12 (cm)
                         1.5) #value if false, after 6/12/2017 (cm)
#c(rep(2.5, 8000), rep(1.5, nrow(hoboU12qc)-8000))
hoboU14qc$diameter<-ifelse(hoboU14qc$date.time<"2018-01-01 00:00:00",
                         2.5, #large diameter tube in 2017 
                         1.5) #small diameter tube in 2018


hoboU12qc$volSmth<- hoboU12qc$hgSmth*(hoboU12qc$diameter/2)^2*pi  #large then small diameter tube
hoboU14qc$volSmth<-hoboU14qc$hgSmth*(hoboU14qc$diameter/2)^2*pi  #large diameter tube in 2017, small in 2018

#Ac is the cross-sectional area of the collection chamber
hoboU12qc$Ac<-(hoboU12qc$diameter/2)^2*pi #cm^2
hoboU14qc$Ac<-(hoboU14qc$diameter/2)^2*pi #cm^2




###4: Make dV/dt column on a 30-min (6*5-min measurements) time step. ----
###   Units are cm^3/30min
#just take the measurements from the round half-hour time points:
#create a data frame with a column that gives the time period that the
#active traps were deployed (2017-05-10 11:30 thru 2017-12-20 12:00),
#then we can left_join the hobo data to that data frame


#make a model time column that we'll match the hobo data to:
#be sure to change the volumetric flux time conversion dt under #4 below
###hoboU12qc$date.timeHH[4] = "2017-05-10 12:00:00 UTC"
###hoboU12qc$date.timeHH[135326] = "2018-12-20 12:00:00 UTC"
#range(hoboU12qc$date.timeHH)
#nrow(hoboU12qc)
timeframe0.5<-seq.POSIXt(from = hoboU12qc$date.timeHH[4],
                         to = hoboU12qc$date.timeHH[135326],by = "30 min")

#head(timeframe)
df12<-as.data.frame(timeframe0.5)
df12$date.timeHH<-df12$timeframe0.5
df12<-left_join(df12, hoboU12qc, by="date.timeHH")

df14<-as.data.frame(timeframe0.5)
df14$date.timeHH<-df14$timeframe0.5
df14<-left_join(df14, hoboU14qc, by="date.timeHH")


df12<-df12%>%
  mutate(dVolSmth0.5=c(rep(NA, 1), diff(df12$volSmth, 1)),
         dVolSmth1=c(rep(NA, 2), diff(df12$volSmth, 2)),
         dVolSmth2=c(rep(NA, 4), diff(df12$volSmth, 4)))

df14<-df14%>%
  mutate(dVolSmth0.5=c(rep(NA, 1), diff(df14$volSmth, 1)),
         dVolSmth1=c(rep(NA, 2), diff(df14$volSmth, 2)),
         dVolSmth2=c(rep(NA, 4), diff(df14$volSmth, 4)))


#filter negative dVol periods that are likely reflecting siphon purges
df12<-df12%>%
  mutate(dVolSmth0.5 = replace(dVolSmth0.5, dVolSmth0.5<(-1), NA),
         dVolSmth1 = replace(dVolSmth1, dVolSmth1<(-1), NA),
         flushFlag = ifelse(dVolSmth2<(-2),
                            1,
                            0),
         dVolSmth2 = replace(dVolSmth2, dVolSmth2<(-1), NA)
  )

# ggplot(filter(df12, date.timeHH<"2017-08-07", date.timeHH>"2017-08-01"),
#        aes(date.timeHH, height))+
#   geom_line()+
#   geom_point(data=filter(df12, date.timeHH<"2017-08-07", date.timeHH>"2017-08-01"),
#              aes(date.timeHH, flushFlag), alpha=0.3)
### These flush flags are not just one point
### but we can use when they change from 0 to 1 to indicate a flush
df14<-df14%>%
  mutate(dVolSmth0.5 = replace(dVolSmth0.5, dVolSmth0.5<(-1), NA),
         dVolSmth1 = replace(dVolSmth1, dVolSmth1<(-1), NA),
         flushFlag = ifelse(dVolSmth2<(-2),
                            1,
                            0),
         dVolSmth2 = replace(dVolSmth2, dVolSmth2<(-1), NA)
  )

df14$deadSpaceError<-0
df12$deadSpaceError<-0

for(i in 1:nrow(df14)){
  ifelse((df14$flushFlag[i+1] - df14$flushFlag[i]) == 1,
     df14$deadSpaceError[i+1] <- 2,
     df14$deadSpaceError[i+1] <- 0)
}

for(i in 1:nrow(df12)){
  ifelse((df12$flushFlag[i+1] - df12$flushFlag[i]) == 1,
         df12$deadSpaceError[i+1] <- 2,
         df12$deadSpaceError[i+1] <- 0)
}



###4: Convert to a volumetric flux (mL/m2/hr):
funnelArea<-pi*(0.54/2)^2  #in m^2
df12<-df12%>%
  mutate(volEb0.5 = dVolSmth0.5/funnelArea*2,#cm^3 = mL, funnelArea in m^2, convert from timeframe to hr 
         volEb1 = dVolSmth1/funnelArea,
         volEb2 = dVolSmth2/funnelArea/2,
         volFE = heightFE, #since the fractional error of Ac is negligable, the vol FE is the same as the height FE
         volErr = (volEb2*volFE)+deadSpaceError, #Var eqn 3
         year=year(date.time),
         monthday = format(date.time, format="%m-%d %H:%M")%>%
           as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC"))

df14<-df14%>%
  mutate(volEb0.5 = dVolSmth0.5/funnelArea*2,#cm^3 = mL, funnelArea in m^2, convert from timeframe to hr 
         volEb1 = dVolSmth1/funnelArea,
         volEb2 = dVolSmth2/funnelArea/2,
         volFE = heightFE, #since the fractional error of Ac is negligable, the vol FE is the same as the height FE
         volErr = volEb2*volFE+deadSpaceError,
         year=year(date.time),
         monthday = format(date.time, format="%m-%d %H:%M")%>%
           as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC"))

dailyEb12<-df12 %>%
  dplyr::group_by(date.time= cut(date.time, breaks="24 hour")) %>%
  dplyr::summarize(dailyVolEb0.5 = (mean(volEb0.5, na.rm=TRUE)),
                   dailyVolEb2 = (mean(volEb2, na.rm=TRUE)),
                   dailyVolEb2Err = sqrt(sum(volErr^2, na.rm=TRUE)), 
                   sdVolEb0.5 = (sd(volEb0.5, na.rm=TRUE)),
                   sdVolEb2 = (sd(volEb2, na.rm=TRUE)))
dailyEb12<-mutate(dailyEb12,
                  date=as.Date(date.time),
                  site="deep",
                  year=year(date),
                  monthday = format(date, format="%m-%d %H:%M")%>%
                    as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC"))


dailyEb14<-df14 %>%
  dplyr::group_by(date.time= cut(date.time, breaks="24 hour")) %>%
  dplyr::summarize(dailyVolEb0.5 = (mean(volEb0.5, na.rm=TRUE)), 
                   sdVolEb0.5 = (sd(volEb0.5, na.rm=TRUE)),
                   dailyVolEb2 = (mean(volEb2, na.rm=TRUE)),
                   dailyVolEb2Err = sqrt(sum(volErr^2, na.rm=TRUE)),
                   sdVolEb2 = (sd(volEb2, na.rm=TRUE)))
dailyEb14<-mutate(dailyEb14,
                  date=as.Date(date.time),
                  site="shallow",
                  year=year(date),
                  monthday = format(date, format="%m-%d %H:%M")%>%
                    as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC"))


#combine so that they can be plotted in a facet plot:
dfFiltList<-list()
dfFiltList[[1]]<-df12
dfFiltList[[2]]<-df14
filteredEb<-do.call("rbind", dfFiltList)

dailyEb<-list()
dailyEb[[1]]<-dailyEb14
dailyEb[[2]]<-dailyEb12
dailyEb<-do.call("rbind", dailyEb)



#GRTS results for volumetric ebullition at u12 (deep) and u14 (shallow) for reference:
g.volEb<-c(15.4, 38.9, 33.7, 7.4, 7.4, 0.85)
g.date<-as.Date(c("2017-07-10", "2017-08-31", "2017-10-04", "2017-07-10", "2017-08-31", "2017-10-04"))
g.datetime<-as.POSIXct(c("2017-07-10 12:00", "2017-08-31 12:00", "2017-10-04 12:00", "2017-07-10 12:00", "2017-08-31 12:00", "2017-10-04 12:00"), tz="UTC")
g.site<-c("deep", "deep", "deep", "shallow", "shallow", "shallow")
grts.df<-data.frame(g.volEb, g.date, g.datetime, g.site)

dailyEbP1<-ggplot(filter(dailyEb, date>"2018-04-15"&date<"2018-11-01"),
                  aes(date, dailyVolEb2))+
  geom_point(alpha=0.5)+
  geom_errorbar(aes(ymin=dailyVolEb2-dailyVolEb2Err,
                    ymax=dailyVolEb2+dailyVolEb2Err))+
  facet_grid(site~.)+
  ylim(-10, 100)+
  #geom_hline(aes(yintercept=24,color="red"))+
  scale_x_date(labels=date_format("%b %d", tz="UTC"),
               breaks=date_breaks("1 month"))
# 
# dailyEbP2<-ggplot(filter(dailyEb, year!= "NA"),
#                   aes(monthday, dailyVolEb2))+
#   geom_line(alpha=0.5)+
#   facet_grid(year~site)+
#   #geom_hline(aes(yintercept=24,color="red"))+
#   scale_x_datetime(labels=date_format("%b %d", tz="UTC"), 
#                breaks=date_breaks("1 month"))+
#   labs(y="Daily Volumetric Ebullition (mL/m2/hr)")+
#   theme_bw()

# FilteredEbP1<-ggplot(filter(filteredEb, date>"2017-04-15"&date<"2017-11-01"),
#                   aes(date.time, volEb))+
#   geom_point(alpha=0.1)+
#   facet_grid(site~.)+
#   geom_hline(aes(yintercept=0,color="red"))+
#   theme(legend.position = "none")
# FilteredEbP1+ylim(-200, 500)+ylab("Volumetric Ebullition (mL/m2/hr)")


# dailyEbP2<-ggplot(dailyEb, aes(date, dailyVolEb))+
#   geom_point(alpha=0.5)+
#   facet_grid(site~.)+
#   #geom_hline(yintercept=24, aes(color="red"))+
#   scale_x_date(labels=date_format("%b %d", tz="UTC"), 
#                    breaks=date_breaks("1 month"))


# dailyEbP2+geom_point(data=grts.df, aes(g.date, g.volEb, color=g.site))


#FilteredEbP1+geom_point(data=grts.df, aes(g.date, g.volEb, color=g.site))

### Volumetric Fluxes to Gas Fluxes: ----

###5. Combine the volumetric flux with the GC gas concentration
###   start with the df12Filt and df14Filt data frames
###   ActonTrapAgg is the data frame created as part of "compileGcDataNonGrts.R"
###   that has Rdate, site, mean and sd of the three GHGs in ppm
###   Make this into a continuous file, extrapolate between points using the 
###   trapezoidal method (aka linear interpolation)

#### On 2017-08-31, gas was collected from two traps: the AFT and a passive trap. I believe we 
#### were testing the aluminum funnels?
#### The AFT [CH4] were 69% and 70%, while the passive trap [CH4] were 30%, 31%, and 49%.
#### It's tempting to throw out the passive trap results, but there are two reasons this is not a good thing to do:
#### 1 -- the two traps were co-located, and there is no reason to believe that the passive trap 
#### would be somehow biased
#### 2 -- the AFT results from 2017-08-24 also spread this range: 62%, 45%, 40%
#### let's reduce the sd by pooling these two dates -- will do in the compileGCdata.R script

actonTrapAgg2$date<-actonTrapAgg2$Rdate
actonTrapAgg14<-as.data.frame(filter(actonTrapAgg2, site=="u14"))
actonTrapAgg14<-select(actonTrapAgg14, -site)
df14$date<-as.Date(df14$date.timeHH, format="%m/%d/%y")
df14.gc<-left_join(df14, actonTrapAgg14, by="date")

actonTrapAgg12<-as.data.frame(filter(actonTrapAgg2, site=="u12"))
actonTrapAgg12<-select(actonTrapAgg12, -site)
df12$date<-as.Date(df12$date.timeHH, format="%m/%d/%y")
df12.gc<-left_join(df12, actonTrapAgg12, by="date")

#### investigate instances of highest uncertainty in the CH4 concentration results
#### deep site (U12) from 2017-08-24 and 2017-08-31

#qcGC<-filter(actonTrapJoin, site.visit.date>"2017-08-01", site.visit.date<"2017-10-01", site=="u12")
#### On 2017-08-31, gas was collected from two traps: the AFT and a passive trap. I believe we 
#### were testing the aluminum funnels?
#### The AFT [CH4] were 69% and 70%, while the passive trap [CH4] were 30%, 31%, and 49%.
#### It's tempting to throw out the passive trap results, but there are two reasons this is not a good thing to do:
#### 1 -- the two traps were co-located, and there is no reason to believe that the passive trap 
#### would be somehow biased
#### 2 -- the AFT results from 2017-08-24 also spread this range: 62%, 45%, 40%
#### let's reduce the sd by pooling these two dates -- will do in the compileGCdata.R script

#linear interpolation
df14.gc<-df14.gc %>% mutate(meanCH4interp = na.approx(meanCH4, rule=2),
                            meanCO2interp = na.approx(meanCO2, rule=2),
                            meanN2Ointerp = na.approx(meanN2O, rule=2),
                            sdCH4interp = na.approx(sdCH4, rule=2),
                            sdCO2interp = na.approx(sdCO2, rule=2),
                            sdN2Ointerp = na.approx(sdN2O, rule=2),
                            ch4FE = sdCH4interp/meanCH4interp)
df12.gc<-df12.gc %>% mutate(meanCH4interp = na.approx(meanCH4, rule=2),
                            meanCO2interp = na.approx(meanCO2, rule=2),
                            meanN2Ointerp = na.approx(meanN2O, rule=2),
                            sdCH4interp = na.approx(sdCH4, rule=2),
                            sdCO2interp = na.approx(sdCO2, rule=2),
                            sdN2Ointerp = na.approx(sdN2O, rule=2),
                            ch4FE = sdCH4interp/meanCH4interp)

ggplot(filter(df12.gc, !is.na(year.x)), aes(monthday.x, ch4FE))+
  geom_line()+
  geom_point(aes(monthday.x, volFE), alpha=0.1)+
  facet_grid(year.x~.)

testP1<-ggplot(df14.gc, aes(date.time, meanCH4/10^4))+
  geom_errorbar(ymin=(df14.gc$meanCH4-df14.gc$sdCH4)/10^4,
                ymax=(df14.gc$meanCH4+df14.gc$sdCH4)/10^4,
                alpha=0.3)
testP1+geom_line(data=df14.gc, aes(date.time, meanCH4interp/10^4), alpha=0.3)+
  theme_bw()+
  scale_x_datetime(breaks=date_breaks("2 month"),
                   labels=date_format("%b %Y"))+
  ylab("Shallow Site % CH4")
  facet_grid(year.x~.)
testP2<-ggplot(df12.gc, aes(date.time, meanCH4/10^4))+
  geom_errorbar(ymin=(df12.gc$meanCH4-df12.gc$sdCH4)/10^4,
                ymax=(df12.gc$meanCH4+df12.gc$sdCH4)/10^4,
                alpha=0.3)
testP2+geom_line(data=df12.gc, aes(date.time, meanCH4interp/10^4), alpha=0.7)+
  #geom_line(aes(date.time, (meanCH4interp+sdCH4interp)/10^4), alpha=0.2)+
  #geom_line(aes(date.time, (meanCH4interp-sdCH4interp)/10^4), alpha=0.2)+
  theme_bw()+
  scale_x_datetime(breaks=date_breaks("2 month"),
                   labels=date_format("%b %Y"))+
  ylab("Deep Site % CH4")

###----
#Declare Constants:
gasConst<-0.082058 #units of L atm mol^-1 K^-1
df14.gc<-mutate(df14.gc,
                ebMlHrM2 = volEb2,
                Tmp_C_S = temp.c,
                trap_ch4.fraction = meanCH4interp/10^6,  #converting ppm to the fraction of gas in the funnel that is CH4
                trap_co2.fraction = meanCO2interp/10^6,
                trap_n2o.fraction = meanN2Ointerp/10^6,
                BrPrssr = 1,
                date=date.timeHH)
df12.gc<-mutate(df12.gc,
                ebMlHrM2 = volEb2,
                Tmp_C_S = temp.c,
                trap_ch4.fraction = meanCH4interp/10^6,  #converting ppm to the fraction of gas in the funnel that is CH4
                trap_co2.fraction = meanCO2interp/10^6,
                trap_n2o.fraction = meanN2Ointerp/10^6,
                BrPrssr = 1,
                date = date.timeHH)
df14.gc<-mutate(df14.gc,
                ebCh4mgM2h = ebMlHrM2*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_ch4.fraction*16,
                ebCo2mgM2h = ebMlHrM2*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_co2.fraction*44,
                ebN2omgM2h = ebMlHrM2*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_n2o.fraction*44)
df12.gc<-mutate(df12.gc,
                ebCh4mgM2h = ebMlHrM2*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_ch4.fraction*16,
                ebCo2mgM2h = ebMlHrM2*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_co2.fraction*44,
                ebN2omgM2h = ebMlHrM2*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_n2o.fraction*44)


## propogate error by adding the fractional errors of 
## the volumetric ebullition and the concentration msmt in quadrature
df14.gc<-mutate(df14.gc,
                ebCh4volErr = ebCh4mgM2h*volFE,
                ebCh4mgM2hFE=sqrt(volFE^2 + ch4FE^2), #volFE includes dead space err
                ebCh4mgM2hErr=ebCh4mgM2h*ebCh4mgM2hFE)
df12.gc<-mutate(df12.gc,
                ebCh4volErr = ebCh4mgM2h*volFE,
                ebCh4mgM2hFE=sqrt(volFE^2+ ch4FE^2),
                ebCh4mgM2hErr=ebCh4mgM2h*ebCh4mgM2hFE)




ggplot(df14.gc, aes(ebCh4mgM2hFE))+
  geom_histogram()+
  xlim(-0.1, 1.5)

summary(df14.gc$ebCh4mgM2hFE)
summary(df14.gc$ch4FE^2)
summary(df14.gc$volFE^2)

#ggplot(df14.gc, aes(date.timeHH, ebCo2mgM2h))+

#  geom_point(alpha=0.3)

# myEbTsList <- list()
# myEbTsList[[1]]<-df14.gc
# myEbTsList[[2]]<-df12.gc
# massEbFlux<-do.call("rbind", myEbTsList)
# 
# grts.df$g.ch4.eb<-c(5.9, 9.0, 16.3,2.56,2.54,0.41)
# 
# massP1<-
#   ggplot(filter(massEbFlux, site=="u12", date.timeHH>"2017-10-01"),
#          aes(date.timeHH, ebCh4mgM2h))+
#   geom_point(alpha=0.2)+
#   # geom_errorbar(aes(ymin = ebCh4mgM2h-ebCh4mgM2hErr,
#   #                   ymax = ebCh4mgM2h+ebCh4mgM2hErr))+
#   ylim(-10, 50)
# #facet_grid(site~.)
# massP1


####Calculate daily Eb and propagate error by summing in quadrature

df12.gc$date<-as.Date(df12.gc$date)
df14.gc$date<-as.Date(df14.gc$date)      

DailyShalFluxes<-df14.gc %>%
  group_by(date) %>%
  dplyr::summarize(meanCH4Flux = (mean(ebCh4mgM2h, na.rm=TRUE)),
                   dailyCH4VolErr = sqrt(sum(ebCh4volErr^2, na.rm=TRUE)),
                   dailyCH4TErr = (sqrt(sum(ebCh4mgM2hErr^2, na.rm=TRUE))))
DailyShalFluxes<-DailyShalFluxes%>%
  mutate(date=as.POSIXct(DailyShalFluxes$date, tz="etc/GMT+5"),
         monthday = format(date, format="%m-%d %H:%M"),
         year = year(date))# %>%
DailyShalFluxes$monthday<-as.Date(DailyShalFluxes$monthday, format="%m-%d %H:%M")

DailyShalFluxes$Rdate <- as.Date(DailyShalFluxes$date)

ggplot(filter(DailyShalFluxes, monthday>"2020-05-01"), aes(monthday, meanCH4Flux))+
  geom_line()+
  geom_line(aes(monthday, meanCH4Flux+dailyCH4VolErr), alpha=0.3)+
  geom_line(aes(monthday, meanCH4Flux-dailyCH4VolErr), alpha=0.3)+
  geom_line(aes(monthday, meanCH4Flux+dailyCH4TErr), alpha=0.3, color="#FF6699")+
  geom_line(aes(monthday, meanCH4Flux-dailyCH4TErr), alpha=0.3, color="#FF6699")+
  facet_grid(year~.)+
  #ylim(-40, 70)+
  ylab("Shallow Site Daily Fluxes (mg Ch4 m-2 h-1)")+
  theme_bw()


DailyDeepFluxes<-df12.gc %>%
  group_by(date) %>%
  dplyr::summarize(meanCH4Flux = (mean(ebCh4mgM2h, na.rm=TRUE)),
                   dailyCH4VolErr = sqrt(sum(ebCh4volErr^2, na.rm=TRUE)),
                   dailyCH4TErr = (sqrt(sum(ebCh4mgM2hErr^2, na.rm=TRUE))))
DailyDeepFluxes<-DailyDeepFluxes%>%
  mutate(date=as.POSIXct(DailyDeepFluxes$date, tz="etc/GMT+5"),
         monthday = format(date, format="%m-%d %H:%M"),
         year = year(date))# %>%
DailyDeepFluxes$monthday<-as.Date(DailyDeepFluxes$monthday, format="%m-%d %H:%M")

ggplot(filter(DailyDeepFluxes, monthday>"2020-05-01"), aes(monthday, meanCH4Flux))+
  geom_line()+
  geom_line(aes(monthday, meanCH4Flux+dailyCH4VolErr), alpha=0.3)+
  geom_line(aes(monthday, meanCH4Flux-dailyCH4VolErr), alpha=0.3)+
  geom_line(aes(monthday, meanCH4Flux+dailyCH4TErr), alpha=0.3, color="#FF6699")+
  geom_line(aes(monthday, meanCH4Flux-dailyCH4TErr), alpha=0.3, color="#FF6699")+
  facet_grid(year~.)+
  ylim(-20, 40)+
  ylab("Deep Site Daily Fluxes (mg Ch4 m-2 h-1)")+
  theme_bw()


rm(vMinTest12, vMinTest14, vZeroBar12, vZeroBar14, deltaVzero12, deltaVzero14, vMinRollSd12, vMinRollSd14)
rm(testP1, testP2)

############## Comparison with Jake's calcs

# jake.eb<-read_delim(file="data/activeTrapForCloudCopy.txt",
#                             delim=" ")
# jake.eb<-jake.eb%>%
#   mutate(site = replace(site, site == "u12", "deep"),
#          site = replace(site, site == "u14", "shallow"),
#          dataset = "JB")
# dailyEb$dataset<-"SW"
# dailyEb$Rdate<-dailyEb$date
# 
# ggplot(filter(jake.eb, lake == "acton"), aes(Rdate, ebMlDM2))+
#   geom_line(aes(color=dataset))+
#   geom_line(data=filter(dailyEb, date<"2018-01-01"),  aes(date, dailyVolEb2*24, color=dataset))+
#   facet_grid(site~.)
# 
# jake.eb.acton<-filter(jake.eb, lake=="acton")%>%
#   select(Rdate, site, ebMlDM2, dataset)%>%
#   spread(key=site, value=ebMlDM2)
# 
# sarah.eb<-filter(dailyEb, Rdate<"2018-01-01")%>%
#   select(Rdate, site, dailyVolEb2, dataset)%>%
#   spread(key=site, value=dailyVolEb2)
# 
# sarah.eb$deep_SW<-sarah.eb$deep*24
# sarah.eb$shallow_SW<-sarah.eb$shallow*24  
# 
# compare.eb<-left_join(jake.eb.acton,
#                        select(sarah.eb, -shallow, -deep),
#                        by="Rdate")
# 
# #https://rpkgs.datanovia.com/ggpubr/reference/stat_regline_equation.html
# library(ggpubr)
# ggscatter(compare.eb, x="deep", y="deep_SW", add="reg.line")+
#   stat_cor(label.x.npc=0.2, label.y.npc=0.9)+
#   stat_regline_equation(label.x.npc = 0.2, label.y.npc=0.85)+
#   xlim(-100, 1500)+
#   ylim(-100, 1500)
# 
# ggscatter(compare.eb, x="shallow", y="shallow_SW", add="reg.line")+
#   stat_cor(label.x.npc=0.2, label.y.npc=0.9)+
#   stat_regline_equation(label.x.npc = 0.2, label.y.npc=0.85)+
#   xlim(-100, 1500)+
#   ylim(-100, 1500)
# 
# shallow.lm<-
# 
# ggplot(compare.eb, aes(shallow, shallow_SW))+
#   geom_point(alpha=0.3)+
#   stat_smooth(method="lm")+
#   stat_regline_equation(
#     aes(label=paste(..eq.label.., ..adj.rr.label.., sep="~~~")),
#     formula=(shallow_SW~shallow))+
#   theme_bw()
#   )
# 
# ggplot(compare.eb, aes(deep, deep_SW))+
#   geom_point(alpha=0.3)
# 
# #cumulative emissions:
# 
# compare.eb.cml<-compare.eb%>%
#   mutate(deep = replace(deep, is.na(deep), 0),
#          deep_SW = replace(deep_SW, is.na(deep_SW), 0),
#          shallow = replace(shallow, is.na(shallow), 0),
#          shallow_SW = replace(shallow_SW, is.na(shallow_SW), 0),
#          deep_cml = cumsum(deep),
#          deep_SW_cml = cumsum(deep_SW),
#          shallow_cml = cumsum(shallow),
#          shallow_SW_cml = cumsum(shallow_SW))
# 
# ggplot(compare.eb.cml, aes(Rdate, deep_cml))+
#   geom_line(color="#FF6699")+
#   geom_line(aes(Rdate, deep_SW_cml), color="#00CC99")
# 
# ggplot(compare.eb.cml)+
#   geom_line(aes(Rdate, shallow_cml), color="#FF6699")+
#   geom_line(aes(Rdate, shallow_SW_cml), color="#00CC99")
# 
# difference=(compare.eb.cml$deep_cml[nrow(compare.eb.cml)]-compare.eb.cml$deep_SW_cml[nrow(compare.eb.cml)])
# bias = difference/compare.eb.cml$deep_SW_cml[nrow(compare.eb.cml)]
# bias*100
# 
# difference=(compare.eb.cml$shallow_cml[nrow(compare.eb.cml)]-compare.eb.cml$shallow_SW_cml[nrow(compare.eb.cml)])
# bias = difference/compare.eb.cml$shallow_cml[nrow(compare.eb.cml)]
# bias*100
