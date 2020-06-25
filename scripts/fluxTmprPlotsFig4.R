#### Script to make plots showing continuous FCH4 vs. sedT
#### on a log scale, showing ecoQ10
#### on a linear scale, with 2DKS threshold

### Looking at AFT vs. sedT measured by Miami/Vanni buoy thermister at the deep site (U12)
### Looking at AFT vs. sedT measured by RBR thermister at shallow site (U14)
### Looking at EC fluxes vs. sedT measured by RBR at shallow site (U14)
### For analysis and visualization, all T and FCH4 measurements need to be on a daily timestep

## Pull together all water and sediment temperature measurements 
## RBRs measure continuous sed T at shallow site (U14)
## Vanni weather stations measures water T near U14 -- use to gap-fill RBRs where needed
## Vanni buoy thermisters measure continuous sedT at deep site (U12)
## EPA team used sonde to measure sed T 2x/month at deep site (U12) -- use to gap-fill buoy thermister where needed

vanni30min<-vanni30min%>%
  mutate(year = year(RDateTime),
         monthday = format(RDateTime, format="%m-%d %H:%M"))# %>%
vanni30min$monthday<-as.POSIXct(vanni30min$monthday, format="%m-%d %H:%M", tz="UTC")

vanni30min$date<-as.Date(vanni30min$RDateTime)
DailyVWS<-vanni30min%>%
  group_by(date)%>%
  dplyr::summarise(
    meanWaterT = mean(waterT.vws, na.rm=TRUE))

DailyVWS<-DailyVWS%>%
  mutate(
    RDateTime = date,
    year = year(date),
    monthday = format(date, format="%m-%d %H:%M")%>%
      as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC")
  )

#### Deep site thermistor
## buoyTdaily was loaded as part of the readData.R script
buoyTdaily<-buoyTdaily%>%
  mutate(year = year(date),
         monthday = format(date, format="%m-%d %H:%M"))# %>%
buoyTdaily$monthday<-as.Date(buoyTdaily$monthday, format="%m-%d %H:%M")
buoyTdaily$sedTbuoy<-buoyTdaily$buoyMeanT_08

### AFT ebullitive FCH4 
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

dailyMassFlux12<-left_join(dailyMassFlux12, select(buoyTdaily, sedTbuoy, date), by="date")
dailyMassFlux12<-left_join(dailyMassFlux12, select(U12sonde, sondeTmpr, date), by="date")

## Sonde measurements were physical samples taken bi-weekly by the EPA crew
## This is a QC step
ggplot(U12sonde, aes(date, sondeTmpr))+
  geom_point(aes(color=sondeDepth))

lmSondeBuoy<-lm(sedTbuoy ~ sondeTmpr, data=dailyMassFlux12)
summary(lmSondeBuoy) #R2 = 0.94, slope = 0.99
ggplot(dailyMassFlux12, aes(sondeTmpr, sedTbuoy))+
  geom_point()+
  stat_smooth(method="lm")+
  labs(title=paste("Adj R2 = ",signif(summary(lmSondeBuoy)$adj.r.squared, 2),
                   "Intercept =",signif(lmSondeBuoy$coef[[1]],2 ),
                   " Slope =",signif(lmSondeBuoy$coef[[2]], 2),
                   " P =",signif(summary(lmSondeBuoy)$coef[2,4], 2)))
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


### making a sedT column from the sedTsonde and sedTbuoy columns 
## gap-filling the periods where we're missing buoy measurements with 
## linearly interpolated sonde data
## we took sonde measurements later in the season past when the buoys were pulled
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

###Gap Fill in missing RBR observations with corrected water T from VWS
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

ggplot(waterTcompare,
       aes(monthday, sedT))+
  geom_line(aes(color=as.factor(year)), alpha=0.7)+
  #scale_x_date(date_breaks=("3 month"), date_minor_breaks=("1 month"))+
  geom_vline(aes(xintercept=as.numeric(as.Date("2019-05-10"))))+
  geom_vline(aes(xintercept=as.numeric(as.Date("2019-06-06"))))
#spring burst 2019: May 10 - June 6
epBurst<-filter(epOutSubFilt, RDateTime>"2018-05-10", RDateTime<"2018-06-06")
mean(epBurst$ch4_flux/1000*16*60*60, na.rm=TRUE)

dailyECfluxSedT<-left_join(DailyEcFluxes, select(waterTcompare, -siteT, -date, -year, -monthday), by="RDateTime")
dailyECfluxSedT<-as.data.frame(dailyECfluxSedT)

dailyMassFlux14<-left_join(dailyMassFlux14, select(waterTcompare, sedT, date), by="date") 
dailyMassFlux12$siteT<-"(e) Deep"

## Plots of AFT shallow site, AFT deep site, EC fluxes as a f(filled sed T)
# ggplot(dailyMassFlux14, aes(sedT, dailyEbCh4mgM2h))+
#   geom_point()
# ggplot(dailyMassFlux12, aes(sedT, dailyEbCh4mgM2h))+
#   geom_point()
# dailyECfluxSedT$year<-year(dailyECfluxSedT$RDateTime)
# ggplot(dailyECfluxSedT, aes(sedT, meanCH4Flux))+
#   geom_point(alpha=0.4)+
#   facet_grid(year~.)

### Put together one data frame with all information for facet plots:

dailyECfluxSedT<-dailyECfluxSedT%>%
  mutate(date = RDateTime,
         dailyEbCh4mgM2h = meanCH4Flux,
         sdEbCh4mgM2h = NA, #sdCH4Flux,
         site ="(a) Eddy Covariance",
         siteT = NA
  )

dailyECfluxSedT<-as.data.frame(dailyECfluxSedT)
dailyMassFlux12$site<-"(c) Deep Site"
dailyMassFlux14$site<-"(b) Shallow Site"
dailyMassFlux14$siteT<-NA

dailyMassEbList<-list()
dailyMassEbList[[1]]<-dailyMassFlux14
dailyMassEbList[[2]]<-select(dailyMassFlux12, -TmprAdj, -sedTbuoy, -sedTsonde, -sondeTmpr)
dailyMassEbList[[3]]<-select(dailyECfluxSedT, date, dailyEbCh4mgM2h, sdEbCh4mgM2h, site, year, monthday, sedT, siteT)
dailyMassEb<-do.call("rbind", dailyMassEbList)

### calculate ecoQ10 regressions for Figure 4 a:
dailyMassEb$log10eb<-log((dailyMassEb$dailyEbCh4mgM2h*24), base=10) #convert to mg m-2 d-1

lmDeepQ10_2017<-lm(log10eb ~ sedT, 
                   data=filter(dailyMassEb, site=="(c) Deep Site", year=="2017"))
lmDeepQ10_2018<-lm(log10eb ~ sedT, 
                   data=filter(dailyMassEb, site=="(c) Deep Site", year=="2018"))
lmShalQ10_2017<-lm(log10eb ~ sedT, 
                   data=filter(dailyMassEb, site=="(b) Shallow Site", year == "2017"))
lmShalQ10_2018<-lm(log10eb ~ sedT, 
                   data=filter(dailyMassEb, site=="(b) Shallow Site", year == "2018"))
lmEC.Q10_2017<-lm(log10eb ~ sedT, 
                  data=filter(dailyMassEb, site=="(a) Eddy Covariance", year == "2017"))
lmEC.Q10_2018<-lm(log10eb ~ sedT, 
                  data=filter(dailyMassEb, site=="(a) Eddy Covariance", year == "2018"))

######### Figure 4 a (left panel) ##########
ggplot(filter(dailyMassEb, year>2016, year<2019), aes(sedT, log10eb))+
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
ggsave("flux_Q10s.tiff", path=paste0(projectWD, "/figures"),
       width=6, height=5)



############# ecoQ10 coefficient calculations: ###########################################

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


#####2DKS Threshold test, aka "peacock test":
peaEC17<-filter(dailyECfluxSedT, !is.na(meanCH4Flux), !is.na(sedT), RDateTime<"2018-01-01")%>%
  select(sedT, meanCH4Flux)

### Used Two-dimensional K-S test software from 
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

peaEC18<-filter(dailyECfluxSedT, !is.na(meanCH4Flux), !is.na(sedT), RDateTime>"2018-01-01")%>%
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
# write.table(peaDeep17, 
#             file="C:/R_Projects/actonFluxProject/Threshold test/deep17.prn",
#             sep=" ",
#             row.names=FALSE)
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
# write.table(peaDeep18, 
#             file="C:/R_Projects/actonFluxProject/Threshold test/deep18.prn",
#             sep=" ",
#             row.names=FALSE)
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

xthresholds<-c(xEC17, xEC18, xShal17, xShal18, xDeep17, xDeep18)
ythresholds<-c(yEC17, yEC18, yShal17, yShal18, yDeep17, yDeep18)

twoDKS<-data.frame(site=c(rep("(a) Eddy Covariance", 2), rep("(b) Shallow Site", 2),
                          rep("(c) Deep Site", 2)),
                   year=c("2017", "2018","2017", "2018","2017", "2018"),
                   hintercept=xthresholds,
                   yintercept=ythresholds)
twoDKS<-twoDKS%>%
  mutate(site=as.character(site),
         year=as.character(year))

###Figure 4 right panel: facet plot with 2DKS thresholds#####
ggplot(filter(dailyMassEb, year>2016, year<2019), aes(sedT, dailyEbCh4mgM2h))+
  geom_point(alpha=0.3)+#, aes(color=as.factor(year)), show.legend=FALSE)+
  facet_grid(as.factor(site)~year)+#, scales = "free")+
  #stat_smooth(method="lm")+
  #xlim(10, 30)+
  labs(x="Sediment Temperature (deg C)", y=expression(F[CH4]~(mg~CH[4]~m^-2~hr^-1)))+
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

ggsave("flux_sedTthresholds2DKS.tiff", path=paste0(projectWD, "/figures"),
       width=6, height=5)


write_csv(dailyMassEb,
          path=paste0(projectWD, "/dataL2/Fig4data.csv"))




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

