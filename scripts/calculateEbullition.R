

##Run this script after running actonTimeSeriesMasterScript.R

####Steps in getting from voltage to ebullitive emissions:-----

# 0. Filter time periods of bad readings -- circut not working, trap otherwise compromised. 

# 1. Convert from voltage to height using the calibration coefficients
#    measured in the lab 
#    >>Circut 9: y=31.603x - 13.475 (u12, deep site)
# 2. Convert from height to volume using the cross-sectional area of
#    inverted funnel tube. Different for each site, and maybe over time.
#         large diameter: 2.5 cm 
#         small diameter: 1.5 cm (u12, deep site)

# 3. Smooth with 12-point moving average 
#     - use zoo::rollapply, which can deal with NAs
#went back to original paper to look at their approach for choosing averaging period, 
#executing averaging:
# From Varadharajan, 2012: 
#   - a 12-point moving average filter was applied to smooth the gas volumes
#     and minimize noise

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


###Parse data -- "hobo" dataframe was loaded by readhobo.R; includes  files from acton and harsha----
hoboActon<-filter(hobo, lake.name=="acton lake")
hoboU12<-filter(hoboActon, site=="u12") #deep site
hoboU14<-filter(hoboActon, site=="u14") #shallow site

#metaDataTrapAct has been loaded as part of compileGcData.R script. This
#data frame includes trap.size, circuit, gas.vol.ml, and exetainer.code, site.visit.dateTime

###0. Filter datsets for bad readings----
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

scale_x_datetime(labels=date_format("%H:%M", tz="UTC"),
                 breaks=date_breaks("2 hour"))


## calculating vOut without a calibration period    
library(pracma)
library(caTools)

## detrend a linear period of ebulltion, then use that to calculate vOut

vOutTest<-filter(filter(hoboU12, date.time>"2018-06-06 20:00:00", date.time<"2018-06-07 08:00"))%>%
  select(volt) 
vOutTest<-as.vector(vOutTest$volt, mode="numeric")#vector of voltage
vOutTest.dT<-detrend(vOutTest, tt='linear')
vOutTest.dT.diff<-diff(vOutTest.dT)
mean(vOutTest.dT.diff) #0.4*10^-5 let's use Jake's 0.0004 V


deltaVout<-0.0004

vOutTest<-lm(volt~date.time, data=filter(hoboU12, date.time>"2018-06-06 20:00:00", date.time<"2018-06-07 08:00"))  
summary(vOutTest)

U12_Vzero<-filter(hoboU12, volt<0.6, volt>0.2)
U14_Vzero<-filter(hoboU14, volt<0.8, volt>0.2)
U12_Vzero$date<-as.POSIXct(U12_Vzero$date, format="%m/%d/%y", tz="UTC")

ggplot(filter(U12_Vzero, date.time>"2017-01-01"), aes(date.time, volt))+
  geom_point(alpha=0.4)+
  scale_x_datetime(labels=date_format("%b %d", tz="UTC"),
                   breaks=date_breaks("6 week"))
ggplot(filter(U14_Vzero, date.time>"2017-01-01"), aes(date.time, volt))+
  geom_point(alpha=0.4)+
  scale_x_datetime(labels=date_format("%b %d", tz="UTC"),
                   breaks=date_breaks("6 week"))
U12_Vzero<-mutate(U12_Vzero,
                  diff = c(NA, diff(volt)),
                  minV = as.numeric(NA),
                  volt = replace(volt, volt == 0, NA))
U14_Vzero<-mutate(U14_Vzero,
                  diff = c(NA, diff(volt)),
                  minV = as.numeric(NA),
                  volt = replace(volt, volt == 0, NA))
ggplot(U12_Vzero, aes(date.timeHH, diff))+
  geom_point(alpha=0.3)
ggplot(U14_Vzero, aes(date.timeHH, diff))+
  geom_point(alpha=0.3)

minThresh<- -0.13

for(i in 1:nrow(U12_Vzero)){
  U12_Vzero$minV[i]<-ifelse(U12_Vzero$diff[i]< minThresh,
                            U12_Vzero$volt[i],
                            #min(subset(U12_Vzero[i-1:i+1,])$volt, na.rm=TRUE),
                            NA)
}
for(i in 1:nrow(U14_Vzero)){
  U14_Vzero$minV[i]<-ifelse(U14_Vzero$diff[i]< minThresh,
                            U14_Vzero$volt[i],
                            #min(subset(U12_Vzero[i-1:i+1,])$volt, na.rm=TRUE),
                            NA)
}

ggplot(filter(U12_Vzero, date.timeHH>"2018-01-01", date.timeHH<"2018-12-01"), 
       aes(date.timeHH, volt))+
  geom_point(alpha=0.4)+
  geom_point(data=filter(U12_Vzero, date.timeHH>"2017-01-01", date.timeHH<"2018-12-01"),
             aes(date.timeHH, minV), color="red", alpha=0.7)
ggplot(filter(U14_Vzero, date.timeHH>"2018-01-01", date.timeHH<"2018-12-01"), 
       aes(date.timeHH, volt))+
  geom_point(alpha=0.4)+
  geom_point(data=filter(U14_Vzero, date.timeHH>"2018-01-01", date.timeHH<"2018-12-01"),
             aes(date.timeHH, minV), color="red", alpha=0.7)

ggplot(U12_V0_2wk, aes(date.time, minV))+
  geom_point()+
  geom_point(data=U12_Vzero, aes(date.timeHH, volt))

vMinTest12<-unique(U12_Vzero$minV)
vMinTest12<-c(0.36569, 0.36, 0.34676, 0.39, 0.34, 0.38706, 0.46886, 0.42857, # thru sept
              0.40659, 0.47350, 0.35, 0.40, 0.37, 0.45, #thru July 2018
              0.40232, 0.31807, 0.33700, 0.34, 0.55, 0.48, 0.56)
vZeroBar12<-mean(vMinTest12) #0.41
vMinRollSd12<-runsd(vMinTest12, k=3)
deltaVzero12<-mean(vMinRollSd12) #0.045

vMinTest14<-unique(U14_Vzero$minV)
vMinTest14<-c(0.20, 0.73, 0.21429, 0.56349, 0.20940, 0.61905, 0.72, 0.63309,
              0.51, 0.42430, 0.38645, 0.32173, 0.33, 0.38,  #thru end of sept 2017
              0.56166, 0.38400, 0.33944, 0.49939, 0.52, 0.6227, 0.52076,
              0.617,0.66,0.6, 0.75, 0.76, 0.7)
vZeroBar14<-mean(vMinTest14) #0.51
vMinRollSd14<-runsd(vMinTest14, k=3)
deltaVzero14<-mean(vMinRollSd14[7:27]) #0.071

deltaVout<-0.0004

# for(i in 1:nrow(hoboU12)){
#   hoboU12$deltaHg12<-sqrt()
# }
hoboU12<-mutate(hoboU12,
                deltaHg12 = sqrt(deltaM^2*(volt^2+vZeroBar12^2)+mBar^2*(deltaVout^2+deltaVzero12^2)))

deltaHg12<-sqrt(deltaM^2*(vOut^2+vZeroBar12^2)+mBar^2*(deltaVout^2+deltaVzero12^2))
#deep site: 1.60 cm
deltaHg14<-sqrt(deltaM^2*(vOut^2+vZeroBar14^2)+mBar^2*(deltaVout^2+deltaVzero14^2))
deltaHg14<-1.4
#shallow site: 3.69 cm
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

# ggplot(hoboU12, aes(date.time, hMult))+
#   geom_point()


#this would need to be inside a for loop
# hoboU12$hMult<-if(hoboU12$date.time>"2017-06-10 12:00:00" 
#                       & hoboU12$date.time<"2017-07-14 13:00:00"){
#                       28.742 #value if TRUE -- period with circuit #19
#                       } else if (hoboU12$date.time>"2017-07-14 13:00:00" &
#                                  hoboU12$date.time<"2018-05-01 12:00:00"){
#                         31.903 #value if TRUE -- 2017 period with circuit #9
#                       } else {
#                         32.24 #value for 2018 circuit #9 post-cal
#                       }
hoboU12$hOffset<-ifelse(hoboU12$date.time>"2017-06-10 12:00:00" 
                        & hoboU12$date.time<"2017-07-14 13:00:00",
                        -4.8, #value if TRUE -- period with circuit #19
                        -13.475) #value if FALSE -- 2017 period with circuit #9
hoboU12$hOffset<-ifelse(hoboU12$date.time>"2018-05-01 12:00:00",
                        hoboU12$hOffset,
                        -13.565) #value for 2018 circuit #9 post-cal

hoboU12$height<-hoboU12$volt*hoboU12$hMult+hoboU12$hOffset

# ggplot(filter(hoboU12, date.time>"2017-05-01"&date.time<"2017-05-20"),
#        aes(date.time, height))+
#   geom_line()

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

deltaVout<-0.0004

for(i in 1:nrow(hoboU12)){
  hoboU12$deltaHg12[i]<-sqrt(hoboU12$hMultErr[i]^2*(hoboU12$volt[i]^2+vZeroBar12^2)+hoboU12$hMult[i]^2*(deltaVout^2+deltaVzero12^2))
}

for(i in 1:nrow(hoboU14)){
  hoboU14$deltaHg14[i]<-sqrt(hoboU14$hMultErr[i]^2*(hoboU14$volt[i]^2+vZeroBar14^2)+hoboU14$hMult[i]^2*(deltaVout^2+deltaVzero14^2))
}

summary(hoboU12$deltaHg12)
# Was this an attempt that didn't end up working?
# hoboU12<-mutate(hoboU12,
#                 deltaHg12 = sqrt(deltaM^2*(1^2+vZeroBar12^2)+mBar^2*(deltaVout^2+deltaVzero12^2)))

###*****Same here****or this may be it -- just one value for each site
# deltaHg12<-sqrt(deltaM^2*(vOut^2+vZeroBar12^2)+mBar^2*(deltaVout^2+deltaVzero12^2))
# #deep site: 1.60 cm
# deltaHg14<-sqrt(deltaM^2*(vOut^2+vZeroBar14^2)+mBar^2*(deltaVout^2+deltaVzero14^2))
# deltaHg14<-1.4

## From Eleanor's circuit calibration RMD document: ###########################################
## L:\Priv\Cin\NRMRL\ReservoirEbullitionStudy\ebullition2017\scriptsAndRmd\harshaEbullition
# circuits<-c("nine_17", "nine_18", "nineteen", "eleven")
# deltaM<-mean(c(0.098, 0.65, 0.198, 0.2955))
# mBar<-mean(c(32.24, 31.86, 28.42, 31.67))
# deltaVzero<-c(0.194, 1.19, 0.348, 0.636)
# vZeroBar<-c(13.565, 4.42, 6.7, 19.26)
# #vOut<-mean(c(max(hoboU14$volt, na.rm=TRUE), max(hoboU12$volt, na.rm=TRUE)))
# deltaVout<-c(0.005, 0.4, 0.05, 0.49, 0.06, 0.03, 0.27, 0.6, 0.33,
#              0.25, 1.15, 0.6, 0.648, 0.89, 0.94, 0.79, 0.99, 0.668,
#              1.03, 0.92, 1.18, 0.63, 0.99, 1.2, 0.047, 1.25, 0.79,
#              1.42, 0.79, 0.59)
# del.h<-sqrt(0.31^2*(2.45^2+10.98^2)+31^2*(0.66^2+0.592^2))
############################################
#######For SI figure S1#######################
hoboList<-list()
hoboList[[1]]<-hoboU12 #select(hoboU12)#, -deltaHg12)
hoboList[[2]]<-hoboU14
hoboShalDeep<-do.call("rbind", hoboList)

hoboShalDeep<-mutate(hoboShalDeep,
                     year=year(date.time),
                     monthday = format(date.time, format="%m-%d %H:%M"))

hoboShalDeep$monthday<-as.POSIXct(hoboShalDeep$monthday, format="%m-%d %H:%M", tz="UTC")

ggplot(filter(hoboShalDeep, monthday>"2020-06-01", monthday<"2020-08-01"), 
       aes(monthday, height))+
  facet_grid(rows=Site~.)+
  geom_line()+
  ylab("Height (cm)")+
  xlab("Date")

###2: Convert from height to volume (cm3) ----
##U12 had a large-diameter tube until 6/12/2017 at 12:00, when a small diameter tube was deployed as the replacement for the missing trap
#hoboU12$date.time[8000] #this is 6/7. Large diam. tube became unmoored on 6/3 
#nrow(hoboU12)
hoboU12$diameter<-ifelse(hoboU12$date.time<"2017-06-12 12:00:00",
                         2.5, #value if true, before 6/12 (cm)
                         1.5) #value if false, after 6/12/2017 (cm)
#c(rep(2.5, 8000), rep(1.5, nrow(hoboU12)-8000))
hoboU14$diameter<-ifelse(hoboU14$date.time<"2018-01-01 00:00:00",
                         2.5, #large diameter tube in 2017 
                         1.5) #small diameter tube in 2018
#ggplot(hoboU12, aes(date.time, diameter))+
#  geom_line()

hoboU12$Vol<- hoboU12$height*(hoboU12$diameter/2)^2*pi  #large then small diameter tube
hoboU14$Vol<-hoboU14$height*(hoboU14$diameter/2)^2*pi  #large diameter tube in 2017, small in 2018

hoboU12$Ac<-(hoboU12$diameter/2)^2*pi #cm^2
hoboU14$Ac<-(hoboU14$diameter/2)^2*pi #cm^2

# hoboU12$VolNorm<-((1+((96.8)*(1+(0.01*hoboU12$height))))*(hoboU12$Ac*hoboU12$height*(293/(hoboU12$temp.c+273.15))))/100
# hoboU14$VolNorm<-((1+((96.8)*(1+(0.01*hoboU14$height))))*(hoboU14$Ac*hoboU14$height*(293/(hoboU14$temp.c+273.15))))/100


# ggplot(hoboU12, aes(Vol, VolNorm))+
#   geom_point(alpha=0.3)
# 
# ggplot(hoboU14, aes(date.timeHH, Vol))+
#   geom_line()+
#   geom_line(aes(date.timeHH, VolNorm), color="red", alpha=0.3)

###3: smooth with rolling average----

zwat<-zoo::rollapply(hoboU12$Vol, width = 12,FUN = mean)
hoboU12$volSmth<-c(rep(NA, 5), zwat, rep(NA, 6))

zwat<-zoo::rollapply(hoboU14$Vol, width = 12,FUN = mean)
hoboU14$volSmth<-c(rep(NA, 5), zwat, rep(NA, 6))

#ggplot(filter(hoboU14, date.time>"2017-05-15", date.time<"2017-07-19"),
#              aes(date.time, Vol))+
#  geom_point()+
#  geom_line(aes(date.time, volSmth), color="red")

###4: Make dV/dt column on a 30-min (6*5-min measurements) time step. ----
###   Units are cm^3/30min
#just take the measurements from the round half-hour time points:
#try creating a data frame with a column that gives the time period that the
#active traps were deployed (2017-05-10 11:30 thru 2017-12-14 10:00),
#then we can left_join the hobo data to that data frame

#round the hobo data logger time to be on the 5-min
hoboU12$date.timeHH<-lubridate::round_date(hoboU12$date.time, "5 minutes")
hoboU14$date.timeHH<-lubridate::round_date(hoboU14$date.time, "5 minutes")
#head(hoboU12$date.timeHH)
#tail(hoboU12$date.timeHH)
#summary(hoboU12$dH)
#summary(hoboU14$dH)

#make a model time column that we'll match the hobo data to:
#be sure to change the volumetric flux time conversion dt under #4 below
###hoboU12$date.timeHH[4] = "2017-05-10 12:00:00 EST"
###hoboU12$date.timeHH[71997] = "2017-12-14 09:00:00 EST"

range(hoboU12$date.timeHH)
nrow(hoboU12)
timeframe0.5<-seq.POSIXt(from = hoboU12$date.timeHH[4],
                         to = hoboU12$date.timeHH[125274],by = "30 min")

#head(timeframe)
df12<-as.data.frame(timeframe0.5)
df12$date.timeHH<-df12$timeframe0.5
df12<-left_join(df12, hoboU12, by="date.timeHH")

df14<-as.data.frame(timeframe0.5)
df14$date.timeHH<-df14$timeframe0.5
df14<-left_join(df14, hoboU14, by="date.timeHH")


df12<-df12%>%
  mutate(dVolSmth0.5=c(rep(NA, 1), diff(df12$volSmth, 1)),
         dVolSmth1=c(rep(NA, 2), diff(df12$volSmth, 2)),
         dVolSmth2=c(rep(NA, 4), diff(df12$volSmth, 4)),
         dVolSmth6=c(rep(NA, 12), diff(df12$volSmth, 12)),
         dVolSmth12=c(rep(NA, 24), diff(df12$volSmth, 24)),
         dVolSmth24=c(rep(NA, 48), diff(df12$volSmth, 48)),
         dVolSmth48=c(rep(NA, 96), diff(df12$volSmth, 96)))

df14<-df14%>%
  mutate(dVolSmth0.5=c(rep(NA, 1), diff(df14$volSmth, 1)),
         dVolSmth1=c(rep(NA, 2), diff(df14$volSmth, 2)),
         dVolSmth2=c(rep(NA, 4), diff(df14$volSmth, 4)),
         dVolSmth6=c(rep(NA, 12), diff(df14$volSmth, 12)),
         dVolSmth12=c(rep(NA, 24), diff(df14$volSmth, 24)),
         dVolSmth24=c(rep(NA, 48), diff(df14$volSmth, 48)),
         dVolSmth48=c(rep(NA, 96), diff(df14$volSmth, 96)))

#black is 30 min timestep; red is 2-hr timestep
ggplot(filter(df12, date.time>"2018-06-15"&date.time<"2018-7-01"),
       aes(date.time, dVolSmth2))+
  geom_point(alpha=0.3, color="red")
#geom_point(aes(date.time, dVolSmth2), alpha = 0.3)
ylim(-30, 10)

#df14 <- hoboU14[seq(5,nrow(hoboU14),6),]

# ggplot(filter(df14, date.time>"2017-07-15"&date.time<"2017-08-01"),
#        aes(date.time, dH))+
#   geom_line()

################Attempt to calculate error per purge #######
#   library(quantmod)
#   
#   peaks12<-findPeaks(df12$volSmth, thresh=3)
#   peakTime<-df12$date.timeHH[(peaks12-3)]
#   peakVol<-df12$volSmth[(peaks12-3)]
#   peaks12df<-data.frame(peakTime, peakVol)
#   
#   val12<-findPeaks(df12$volSmth*-1, thresh=3)
#   valTime<-df12$date.timeHH[(peaks12+3)]
#   valVol<-df12$volSmth[(peaks12+3)]
#   val12df<-data.frame(valTime, valVol)
#   
# ggplot(filter(df12, date.timeHH>"2017-07-25", date.timeHH<"2017-09-1"), 
#        aes(date.timeHH, volSmth))+
#   geom_line()+
#   geom_point(data=filter(peaks12df, peakTime> "2017-07-25",
#                          peakTime< "2017-08-1"),
#              aes(peakTime, peakVol), color="red")+
#   geom_point(data=filter(val12df, valTime> "2017-07-25",
#                         valTime< "2017-08-1"),
#             aes(valTime, valVol), color="blue")
# 
# peaks14<-findPeaks(df14$volSmth, thresh=3)
# peakTime<-df14$date.timeHH[(peaks14-3)]
# peakVol<-df14$volSmth[(peaks14-3)]
# peaks14df<-data.frame(peakTime, peakVol)
# 
# val14<-findPeaks(df14$volSmth*-1, thresh=3)
# valTime<-df14$date.timeHH[(peaks14+3)]
# valVol<-df14$volSmth[(peaks14+3)]
# val14df<-data.frame(valTime, valVol)
# peaks14df$valVol<-val14df$valVol 
# peaks14df<-mutate(peaks14df,
#                   volDiff = peakVol - valVol,
#                   volDiff = replace(volDiff, volDiff<50, NA),
#                   date.timeHH = peakTime)
# peaks14df<-filter(peaks14df, peakTime>"2017-06-01")
# df14<-left_join(df14, peaks14df)
# df14<-mutate(df14,
#              volErr = volDiff/(volDiff/Ac)*deltaHg14)
# 
# ggplot(filter(df14, date.timeHH>"2017-05-15", date.timeHH<"2018-09-10"), 
#        aes(date.timeHH, volSmth), alpha=0.2)+
#   geom_line()+
#   geom_point(data=filter(peaks14df, peakTime> "2017-05-15",
#                          peakTime< "2018-09-10"),
#              aes(peakTime, peakVol), color="red")+
#   geom_point(data=filter(val14df, valTime> "2017-05-15",
#                          valTime< "2018-09-10"),
#              aes(valTime, valVol), color="blue")+
#   geom_point(data=filter(df14, date.timeHH<"2018-09-10"), 
#              aes(date.timeHH, volErr*10), alpha=0.3)
# 
# 
# peaks12df$valVol<-val12df$valVol 
# peaks12df<-mutate(peaks12df,
#                   volDiff = peakVol - valVol,
#                   volDiff = replace(volDiff, volDiff<50, NA),
#                   date.timeHH = peakTime)
# df12<-left_join(df12, peaks12df)
# df12<-mutate(df12,
#              volErr = volDiff/(volDiff/Ac)*deltaHg12)
#   
###########################

#filter negative dVol periods that are likely reflecting siphon purges



df12<-df12%>%
  mutate(dVolSmth0.5 = replace(dVolSmth0.5, dVolSmth0.5<(-1), NA),
         dVolSmth1 = replace(dVolSmth1, dVolSmth1<(-1), NA),
         dVolSmth2 = replace(dVolSmth2, dVolSmth2<(-1), NA),
         dVolSmth6 = replace(dVolSmth6, dVolSmth6<(-1), NA),
         dVolSmth12 = replace(dVolSmth12, dVolSmth12<(-1), NA),
         dVolSmth24 = replace(dVolSmth24, dVolSmth24<(-1), NA),
         dVolSmth48 = replace(dVolSmth48, dVolSmth48<(-1), NA),
         #volErr = deltaHg12*Ac
  )

df14<-df14%>%
  mutate(dVolSmth0.5 = replace(dVolSmth0.5, dVolSmth0.5<(-1), NA),
         dVolSmth1 = replace(dVolSmth1, dVolSmth1<(-1), NA),
         dVolSmth2 = replace(dVolSmth2, dVolSmth2<(-1), NA),
         dVolSmth6 = replace(dVolSmth6, dVolSmth6<(-1), NA),
         dVolSmth12 = replace(dVolSmth12, dVolSmth12<(-1), NA),
         dVolSmth24 = replace(dVolSmth24, dVolSmth24<(-1), NA),
         dVolSmth48 = replace(dVolSmth48, dVolSmth48<(-1), NA),
         #volErr = deltaHg14*Ac
  )

###4: Convert to a volumetric flux (mL/m2/hr):
funnelArea<-pi*(0.54/2)^2  #in m^2
df12<-df12%>%
  mutate(volEb0.5 = dVolSmth0.5/funnelArea*2,#cm^3 = mL, funnelArea in m^2, convert from timeframe to hr 
         volEb1 = dVolSmth1/funnelArea,
         volEb2 = dVolSmth2/funnelArea/2,
         volEb6 = dVolSmth6/funnelArea/6,
         volEb12 = dVolSmth12/funnelArea/12,
         volEb24 = dVolSmth24/funnelArea/24,
         volEb48 = dVolSmth48/funnelArea/48,
         #Eb2FE = volErr/volEb2,
         year=year(date.time),
         monthday = format(date.time, format="%m-%d %H:%M")%>%
           as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC"))

df14<-df14%>%
  mutate(volEb0.5 = dVolSmth0.5/funnelArea*2,#cm^3 = mL, funnelArea in m^2, convert from timeframe to hr 
         volEb1 = dVolSmth1/funnelArea,
         volEb2 = dVolSmth2/funnelArea/2,
         volEb6 = dVolSmth6/funnelArea/6,
         volEb12 = dVolSmth12/funnelArea/12,
         volEb24 = dVolSmth24/funnelArea/24,
         volEb48 = dVolSmth48/funnelArea/48,
         #Eb2FE = volErr/volEb2,
         year=year(date.time),
         monthday = format(date.time, format="%m-%d %H:%M")%>%
           as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC"))

ggplot(filter(df12, abs(volEb2FE)<10), aes(volEb2FE))+
  geom_histogram() 
#ggplot(filter(df12, date.time>"2017-07-15"&date.time<"2017-8-20"),
#        aes(date.time, volEb0.5))+
#   geom_point(alpha=0.3)+
#    geom_point(aes(date.time, volEb0.5), color="red", alpha=0.3)
#   #ylim(-250, 500)
# ggplot(filter(df14, date.time>"2017-06-15"&date.time<"2017-10-01"),
#        aes(date.time, volEb0.5))+
#   geom_point(alpha=0.3)
# 
# ggplot(filter(df14, date.timeHH>"2018-05-01 00:00:00"), 
#        aes(monthday, volEb2))+
#   geom_point(alpha=0.5)
#facet_grid(year~.)

###diurnal plots:---- 
# df12$date<-df12$date.timeHH
# df14$date<-df14$date.timeHH
# 
# df14_2017<-filter(df14, date<"2018-04-01 00:00:00")
# df12_2017<-filter(df12, date<"2018-04-01 00:00:00")
# # %>%
# #   mutate(volEb0.5, replace(volEb0.5, date>"2017-07-01 00:00:00" & 
# #                              date< "2017-08-01 00:00:00"), NA)
# 
# 
# shallowDiurnalPlot17<-timeVariation(df14_2017, pollutant="volEb1", 
#                                   type="month", statistic="mean",
#                                   normalise=FALSE)
# plot(shallowDiurnalPlot17, subset="hour")
# 
# deepDiurnalPlot17<-timeVariation(df12_2017, pollutant="volEb1", 
#                                type="month", statistic="mean",
#                                normalise=FALSE)
# plot(deepDiurnalPlot17, subset="hour")
# 
# df14_2018<-filter(df14, date>"2018-04-01 00:00:00")
# df12_2018<-filter(df12, date>"2018-04-01 00:00:00")
# 
# shallowDiurnalPlot<-timeVariation(df14_2018, pollutant="volEb1", 
#                                        type="month", statistic="mean",
#                                        normalise=FALSE)
# plot(shallowDiurnalPlot, subset="hour")
# 
# deepDiurnalPlot<-timeVariation(df12_2018, pollutant="volEb1", 
#                                   type="month", statistic="mean",
#                                   normalise=FALSE)
# plot(deepDiurnalPlot, subset="hour")

###end diurnal plot section----

#Why is the 2017 july volEb0.5 super high? It's like it didn't get divided or something
#volEb1 for the same period looks ok.

##   #ylim(-250, 500) ----

#filter the time points measuring siphon purges:
#df12Filt<-filter(df12, volEb>-100)         #filtering threshold of 50, small diameter funnel
#df12Filt$date<-as.Date(df12Filt$date.time)

#df14Filt<-filter(df14, volEb>-500, volEb<500)        #filtering threshold of 500, large diamter funnel
#df14Filt$date<-as.Date(df14Filt$date.time)

dailyEb12<-df12 %>%
  dplyr::group_by(date.time= cut(date.time, breaks="24 hour")) %>%
  dplyr::summarize(dailyVolEb0.5 = (mean(volEb0.5, na.rm=TRUE)),
                   dailyVolEb2 = (mean(volEb2, na.rm=TRUE)),
                   #dailyVolEb2Err = sqrt(sum(volErr^2, na.rm=TRUE)), 
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
                   #dailyVolEb2Err = sqrt(sum(volErr^2, na.rm=TRUE)),
                   sdVolEb2 = (sd(volEb2, na.rm=TRUE)))
dailyEb14<-mutate(dailyEb14,
                  date=as.Date(date.time),
                  site="shallow",
                  year=year(date),
                  monthday = format(date, format="%m-%d %H:%M")%>%
                    as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC"))

# dailyEb14$date<-as.Date(dailyEb14$date.time)
# dailyEb14$site<-"shallow"
# dailyEb14$year=year(dailyEb14$date)
# dailyEb14$monthday = format(dailyEb14$date, format="%m-%d %H:%M")%>%
#   as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC")

# ggplot(filter(dailyEb12, date>"2017-05-15"&date<"2017-12-01"),
#        aes(date, dailyVolEb0.5))+
#   geom_line(alpha=0.3)
# 
# 
# ggplot(filter(dailyEb14, date>"2017-01-15"&date<"2017-12-01"),
#        aes(date, dailyVolEb0.5))+
#   geom_line(alpha=0.3)
#ylim(-0, 250)

#combine so that they can be plotted in a facet plot:
dfFiltList<-list()
dfFiltList[[1]]<-df12
dfFiltList[[2]]<-df14
filteredEb<-do.call("rbind", dfFiltList)

dailyEb<-list()
dailyEb[[1]]<-dailyEb14
dailyEb[[2]]<-dailyEb12
dailyEb<-do.call("rbind", dailyEb)



#GRTS results for volumetric ebullition at u12 (deep) and u14 (shallow):
g.volEb<-c(15.4, 38.9, 33.7, 7.4, 7.4, 0.85)
g.date<-as.Date(c("2017-07-10", "2017-08-31", "2017-10-04", "2017-07-10", "2017-08-31", "2017-10-04"))
g.datetime<-as.POSIXct(c("2017-07-10 12:00", "2017-08-31 12:00", "2017-10-04 12:00", "2017-07-10 12:00", "2017-08-31 12:00", "2017-10-04 12:00"), tz="UTC")
g.site<-c("deep", "deep", "deep", "shallow", "shallow", "shallow")
grts.df<-data.frame(g.volEb, g.date, g.datetime, g.site)

dailyEbP1<-ggplot(filter(dailyEb, date>"2017-04-15"&date<"2017-11-01"),
                  aes(date, dailyVolEb2))+
  geom_point(alpha=0.5)+
  geom_errorbar(aes(ymin=dailyVolEb2-dailyVolEb2Err,
                    ymax=dailyVolEb2+dailyVolEb2Err))+
  facet_grid(site~.)+
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

actonTrapAgg$date<-actonTrapAgg$Rdate
actonTrapAgg14<-as.data.frame(filter(actonTrapAgg, site=="u14"))
actonTrapAgg14<-select(actonTrapAgg14, -site)
df14$date<-as.Date(df14$date.timeHH, format="%m/%d/%y")
df14.gc<-left_join(df14, actonTrapAgg14, by="date")

actonTrapAgg12<-as.data.frame(filter(actonTrapAgg, site=="u12"))
actonTrapAgg12<-select(actonTrapAgg12, -site)
df12$date<-as.Date(df12$date.timeHH, format="%m/%d/%y")
df12.gc<-left_join(df12, actonTrapAgg12, by="date")

#linear interpolation
df14.gc<-df14.gc %>% mutate(meanCH4interp = na.approx(meanCH4, rule=2),
                            meanCO2interp = na.approx(meanCO2, rule=2),
                            meanN2Ointerp = na.approx(meanN2O, rule=2),
                            sdCH4interp = na.approx(sdCH4, rule=2),
                            sdCO2interp = na.approx(sdCO2, rule=2),
                            sdN2Ointerp = na.approx(sdN2O, rule=2))
df12.gc<-df12.gc %>% mutate(meanCH4interp = na.approx(meanCH4, rule=2),
                            meanCO2interp = na.approx(meanCO2, rule=2),
                            meanN2Ointerp = na.approx(meanN2O, rule=2),
                            sdCH4interp = na.approx(sdCH4, rule=2),
                            sdCO2interp = na.approx(sdCO2, rule=2),
                            sdN2Ointerp = na.approx(sdN2O, rule=2))

#replace missing 2018 GC data with 50% CH4
# df14.gc<-df14.gc%>%
#   mutate(meanCH4interp = replace(meanCH4interp, date.time>"2018-01-01 00:00:00" & meanCH4interp <100000, 500000)) 
# df12.gc<-df12.gc%>%
#   mutate(meanCH4interp = replace(meanCH4interp, date.time>"2018-01-01 00:00:00" & meanCH4interp <100000, 500000)) 


testP1<-ggplot(df14.gc, aes(date.time, meanCH4/10^4))+
  geom_point(alpha=0.3)
testP1+geom_line(data=df14.gc, aes(date.time, meanCH4interp/10^4), alpha=0.3)+
  theme_bw()+
  scale_x_datetime(breaks=date_breaks("2 month"),
                   labels=date_format("%b %Y"))
testP2<-ggplot(df12.gc, aes(date.time, meanCH4/10^4))+
  geom_point(alpha=0.3)
testP2+geom_line(data=df12.gc, aes(date.time, meanCH4interp/10^4), alpha=0.3)+
  theme_bw()+
  scale_x_datetime(breaks=date_breaks("2 month"),
                   labels=date_format("%b %Y"))
#there's a strange repeat of June 12-26th at u12.
#check for duplicate dates:
#filter(df12.gc, duplicated(date.time,fromLast = TRUE) | duplicated(date.time,fromLast = FALSE)) %>% arrange(date.time)
#no duplicates. Upon further inspection, it is a matter of one section being 12:10, another 12:22
#need to improve the way we get from 5-min to 30-min time series
#improved from sampling every 6 to matching the hobo data up with a 30-min 
#time column. 
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



#ggplot(df14.gc, aes(date.timeHH, ebCo2mgM2h))+

#  geom_point(alpha=0.3)

myEbTsList <- list()
myEbTsList[[1]]<-df14.gc
myEbTsList[[2]]<-df12.gc
massEbFlux<-do.call("rbind", myEbTsList)

grts.df$g.ch4.eb<-c(5.9, 9.0, 16.3,2.56,2.54,0.41)

massP1<-
  ggplot(filter(massEbFlux, site=="u12", date.timeHH>"2017-10-01"),
         aes(date.timeHH, ebCh4mgM2h))+
  geom_point(alpha=0.2)+
  # geom_errorbar(aes(ymin = ebCh4mgM2h-ebCh4mgM2hErr,
  #                   ymax = ebCh4mgM2h+ebCh4mgM2hErr))+
  ylim(-10, 50)
#facet_grid(site~.)
massP1
massP1+geom_point(data=grts.df, aes(g.datetime, g.ch4.eb, color=g.site))



#dailyEbP1+geom_point(data=grts.df, aes(g.date, g.ch4.eb, color=g.site))+ylim(-25, 200)

# #TRIED TO GET THIS TO WORK BUT THE VALUES CALCULATED WERE UNREASONABLY SMALL
# the function "mass.rate" in the "masterLibraryActonGRTS.R", used for
# the 32-reservoir survey mass ebullition calcs, can be used here.
# function of X1 and choice1, where X1 is data.i and choice1 is the GHG
# data.i is by lake? must be by lake and by site, but I don't get it.
# df14.gc<-mutate(df14.gc,
#                 ebMlHrM2 = volEb,
#                 Tmp_C_S = temp.c,
#                 trap_ch4.ppm = meanCH4interp,
#                 trap_co2.ppm = meanCO2interp,
#                 trap_n2o.ppm = meanN2Ointerp,
#                 BrPrssr = 1)
# df12.gc<-mutate(df12.gc,
#                 ebMlHrM2 = volEb,
#                 Tmp_C_S = temp.c,
#                 trap_ch4.ppm = meanCH4interp,
#                 trap_co2.ppm = meanCO2interp,
#                 trap_n2o.ppm = meanN2Ointerp,
#                 BrPrssr = 1)
# 
# out14.ch4 <-mass.rate(df14.gc, choice1= "ch4")
# out12.ch4 <-mass.rate(df12.gc, choice1= "ch4")
# out14.co2 <-mass.rate(df14.gc, choice1= "co2")
# out12.co2 <-mass.rate(df12.gc, choice1= "co2")
# out14.n2o <-mass.rate(df14.gc, choice1= "n2o")
# out12.n2o <-mass.rate(df12.gc, choice1= "n2o")
# 
# myEbTsList <- list()
# 
# myEbTsList[[1]]<-data.frame(ebCh4mgM2h = out12.ch4,
#                             ebCo2mgM2h = out12.co2,
#                             ebN2omgM2h = out12.n2o,
#                             date.time = df12.gc$date.timeHH,
#                             site = "u12")
# myEbTsList[[2]]<-data.frame(ebCh4mgM2h = out14.ch4,
#                             ebCo2mgM2h = out14.co2,
#                             ebN2omgM2h = out14.n2o,
#                             date.time = df14.gc$date.timeHH,
#                             site = "u14")
# ebResults <- do.call("rbind", myEbTsList)
# 
# ggplot(ebResults, aes(date.time, ebCh4mgM2h))+
#   geom_point(alpha=0.3)+
#   facet_grid(site~.)+
#   ylim(-0.5, 1)



