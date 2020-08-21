

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


######### other analyses -----

### Comparison of Acton observed monthly bowen ratio to that in Liu et al (Missouri reservoir):

MonthlyFluxDat$liu<-c(0.44, 0.28, 0.21, 0.19, 0.11, 0.11, 0.09, 0.17, 0.14, 0.26, 0.32, 0.38,
                      0.44, 0.28, 0.21, 0.19, 0.11, 0.11, 0.09, 0.17, 0.14, 0.26, 0.32)

MonthlyFluxDat$abs.bowR<-ifelse(MonthlyFluxDat$bowR<0,
                                abs(MonthlyFluxDat$bowR),
                                MonthlyFluxDat$bowR)


ggplot(MonthlyFluxDat, aes(Rdate, meanLE))+
  geom_line()+
  geom_point()+
  geom_line(aes(Rdate, meanH, color="red"))+
  geom_point(aes(Rdate, meanH, color="red"))

ggplot(MonthlyFluxDat, aes(monthday, abs.bowR))+
  geom_point(alpha=0.7, aes(color=as.factor(year)))+
  geom_point(aes(monthday, liu))



## plot of sed T and air T from May thru Oct:

currentYear<-year(Sys.time())

ggplot(filter(DailyFluxDat, monthday>paste0(currentYear,"-05-01"), monthday<paste0(currentYear, "-11-01")),
       aes(monthday, meanAirT))+
  annotate("rect", xmin=as.Date(paste0(currentYear, "-05-24")),
           xmax=as.Date(paste0(currentYear,"-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line(color="red", size=1, linetype=2)+
  geom_line(data=filter(DailyFluxDat, monthday>paste0(currentYear,"-05-01"), monthday<paste0(currentYear, "-11-01")), 
            aes(monthday, meanSedT), alpha=0.8, size=1)+
  # scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
  #              breaks=date_breaks("2 month"),
  #              limits = c(as.Date("2017-01-01"), as.Date("2018-11-20")))+
  ylab(expression(T~(deg~C)))+
  xlab("")+
  facet_grid(year~.)+
  #geom_hline(yintercept=0, linetype = 2, alpha=0.2)+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme_bw()

######## Daily PAR #######
ggplot(DailyFluxDat, aes(date, meanPAR))+
  annotate("rect", xmin=as.Date("2017-05-24"),
           xmax=as.Date("2017-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.Date("2018-05-24"),
           xmax=as.Date("2018-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line(alpha=0.5)+
  theme_bw()+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
               breaks=date_breaks("2 month"),
               limits = c(as.Date("2017-01-01"), as.Date("2018-11-20")))+
  ylab(expression(PAR~(mol~m^-2~d^-1)))+
  xlab("")
