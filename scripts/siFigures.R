

#diurnal plots

library(openair)

#SI of monthly diurnal patterns in 2017:
diurnal.17<-filter(epOutSubFilt, RDateTime>"2017-03-01", RDateTime<"2017-12-01")
diurnal.17$date<-diurnal.17$RDateTime

diurnalPec.17<-timeVariation(diurnal.17, pollutant="ch4_flux",
                             type="month", statistic="mean", 
                             # xlab=c("hour", "hour of day, 2018",
                             #        "month", "weekday"),
                             normalise=FALSE)
plot(diurnalPec.17, subset="hour")

#SI figure of monthly diurnal patterns in 2018:
diurnal.18<-filter(epOutSubFilt, RDateTime>"2018-03-01", RDateTime<"2018-12-01")
diurnal.18$date<-diurnal.18$RDateTime

diurnalPec.18<-timeVariation(diurnal.18, pollutant="ch4_flux",
                             type="month", statistic="mean", 
                             # xlab=c("hour", "hour of day, 2018",
                             #        "month", "weekday"),
                             normalise=FALSE)
plot(diurnalPec.18, subset="hour")

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

epOutSubFilt$air_pressure_kPa<-epOutSubFilt$air_pressure/1000

yesDlow<-filter(epOutSubFilt, RDateTime>"2018-08-26 12:00", RDateTime<"2018-08-31") 
yesDlow$date<-yesDlow$RDateTime
mean(yesDlow$air_pressure_kPa, na.rm=TRUE)
yesDlow$air_pressure_kPa_off<-yesDlow$air_pressure/1000-98.3
mean(yesDlow$ch4_flux, na.rm=TRUE)
yesDlow$ch4_flux_off<-yesDlow$ch4_flux-0.3977
yesDlow.p<-timeVariation(yesDlow, pollutant=c("ch4_flux_off", "air_pressure_kPa_off"),
                         statistic="mean", 
                         # xlab=c("hour", "hour of day, 2018",
                         #        "month", "weekday"),
                         normalise=FALSE,
                         cols=c("blue", "green"))
plot(yesDlow.p, subset="hour")


ggplot(yesDlow, aes(air_pressure_kPa, ch4_flux/1000*60*60*16))+
  geom_point()+
  geom_smooth(method="lm")+
  ggtitle("R^2 = 0.34")+
  ylab(expression(F[CH4]~(mg~m^-2~hr^-1)))+
  xlab(expression(Air~Pressure~(kPa)))+
  theme_bw()

ggsave(filename="figures/SI_ch4vsP.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

lm_press<-lm(ch4_flux~air_pressure_kPa, data=yesDlow)  
summary(lm_press)

