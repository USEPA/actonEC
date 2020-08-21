

#source("scriptsAndRmd/masterLibraryActon.R")
#library(OceanView)
library(rLakeAnalyzer)
library(chron)
library(tidyverse)



rLakeBuoyT<-buoyT30min
rLakeRbrT<-rbrTsub

rLakeBuoyT<-rLakeBuoyT%>%
  dplyr::rename(datetime = RDateTime,
        wtrT_0 = buoyMeanT_0.1,
         wtrT_1 = buoyMeanT_01,
         wtrT_2 = buoyMeanT_02,
         wtrT_3 = buoyMeanT_03,
         wtrT_4 = buoyMeanT_04,
         wtrT_5 = buoyMeanT_05,
         wtrT_6 = buoyMeanT_06,
         wtrT_7 = buoyMeanT_07,
         wtrT_8 = buoyMeanT_08)

rLakeRbrT<-rLakeRbrT%>%
  dplyr::rename(datetime = RDateTime,
                wtrT_0 = RBRmeanT_0.1,
                wtrT_0.25 = RBRmeanT_0.25,
                wtrT_0.5 = RBRmeanT_0.5,
                wtrT_0.75 = RBRmeanT_0.75,
                wtrT_1.0 = RBRmeanT_1,
                wtrT_1.25 = RBRmeanT_1.25,
                wtrT_1.6 = RBRmeanT_1.6)

bvf<-ts.buoyancy.freq(rLakeRbrT, at.thermo=FALSE, na.rm=TRUE)
bvf$year<-year(bvf$datetime)
bvf$monthday<- format(bvf$datetime, format="%m-%d %H:%M")%>%
  as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC")

currentYear<-year(Sys.Date())
ggplot(filter(bvf, monthday>paste0(currentYear, "-07-01"), 
              monthday<paste0(currentYear, "-07-10")),
       aes(monthday, N2_0.625))+
  geom_line(alpha=1)+
  facet_grid(year~.)+
  geom_vline(xintercept=as.POSIXct("2019-05-30 00:00:00", 
                                   format="%Y-%m-%d %H:%M:%S", tz="UTC"))+
  ylim(-0.01, 0.03)

dailyBVF<-bvf%>%
  group_by(datetime=cut(datetime, breaks="24 hour"))%>%
  dplyr::summarize(maxBV = max(N2_1.125, na.rm=TRUE),
                   maxBVm_s = maxBV*1000)
dailyBVF$RDateTime<-as.Date(dailyBVF$datetime)


#####FIGURE 2F: BV Frequency#########
ggplot(dailyBVF, aes(RDateTime, maxBVm_s))+
  annotate("rect", xmin=as.Date("2017-05-24"),
           xmax=as.Date("2017-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.Date("2018-05-24"),
           xmax=as.Date("2018-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line(alpha=0.5)+
  geom_point(alpha=0.4, size=1)+
  theme_bw()+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
                   breaks=date_breaks("2 month"),
                   limits = c(as.Date("2017-01-01"),
                              as.Date("2018-11-20")))+
  ylab(expression(BV~Freq~(ms^-1)))+
  xlab("")
###############################  


write_csv(select(dailyBVF, RDateTime, maxBVm_s),
          path = paste0(projectWD, "/dataL2/Fig3f_data.csv"))




DailyEcFluxes<-left_join(DailyEcFluxes, dailyBVF, by="RDateTime")

### Figure S5: FCH4 as a function of BV Freq:
ggplot(filter(DailyEcFluxes, monthday>paste0(currentYear,"-05-01"), 
              monthday<paste0(currentYear,"-09-30"), year<2019),
       aes(maxBV, meanCH4Flux))+
  geom_point(alpha=0.5, aes(color=as.factor(year)))+
  geom_smooth()+
  ylab(expression(Daily~F[CH4]~(mg~CH[4]~m^-2~hr^-1)))+
  xlab(expression(BV~Freq~(s^-1)))+
  labs(color="Year")

### Figure S6:
ggplot(filter(DailyEcFluxes, monthday>paste0(currentYear,"-04-01"), 
              monthday<paste0(currentYear,"-10-30"), year<2019),
       aes(monthday, meanCH4Flux))+
  geom_line()+
  geom_point(alpha=0.5)+
  geom_line(data=filter(DailyEcFluxes, monthday>paste0(currentYear,"-04-01"), 
                        monthday<paste0(currentYear,"-10-30"), year<2019),
             aes(monthday, maxBV*1000), color="red", alpha=0.3)+
  geom_point(data=filter(DailyEcFluxes, monthday>paste0(currentYear,"-04-01"), 
                         monthday<paste0(currentYear,"-10-30"), year<2019),
             aes(monthday, maxBV*1000), color="red", alpha=0.3)+
  facet_grid(year~.)+
  ylab(expression(Daily~F[CH4]~(mg~CH[4]~m^-2~hr^-1)~and~BV~Freq~(ms^-1)))+
  xlab("")

bvf$RDateTime<-bvf$datetime
epOutSubFilt<-left_join(epOutSubFilt, bvf, by="RDateTime")

ggplot(epOutSubFilt, aes(N2_1.125, ch4_flux))+
  geom_point(alpha=0.3)

rLakeBuoyT<-select(rLakeBuoyT, -buoyMeanT_0.5, -buoyMeanT_01.5)

rLakeBuoyDaily<-rLakeBuoyT %>%
  group_by(datetime=cut(datetime, breaks="24 hour")) %>%
  summarize(wtrT_0 = mean(wtrT_0, na.rm=TRUE),
            wtrT_1 = mean(wtrT_1, na.rm=TRUE),
            wtrT_2 = mean(wtrT_2, na.rm=TRUE),
            wtrT_3 = mean(wtrT_3, na.rm=TRUE),
            wtrT_4 = mean(wtrT_4, na.rm=TRUE),
            wtrT_5 = mean(wtrT_5, na.rm=TRUE),
            wtrT_6 = mean(wtrT_6, na.rm=TRUE),
            wtrT_7 = mean(wtrT_7, na.rm=TRUE),
            wtrT_8 = mean(wtrT_8, na.rm=TRUE))
rLakeBuoyDaily$datetime<-as.POSIXct(as.character(rLakeBuoyDaily$datetime),
                                    format = "%Y-%m-%d",
                                    tz="UTC")


#Gapfilling
sum(is.na(rLakeBuoyDaily$wtrT_8))
#13, only gaps for deepest measurement
rLakeBuoyDaily<-rLakeBuoyDaily %>% 
  mutate(wtrT_8 = na.approx(wtrT_8, rule=2))

datetime.plus<-c(as.character(rLakeBuoyDaily$datetime), "2017-10-20 10:00:00", "2017-10-31 10:00:00", "2017-11-14 10:00:00", 
                 "2017-12-11 10:00:00")
tail(datetime.plus)

U12sondePro<-filter(metaDataSonde, Site == "u12")%>%
  select(Site, Lake, Sample.depth.m, Temp.C, Sample.Date)
U12sondePro<-U12sondePro%>%
  mutate(Sample.depth.m = replace(Sample.depth.m, Sample.depth.m<0.9, 0.1),
         Sample.depth.m = replace(Sample.depth.m, Sample.depth.m>=0.9 & Sample.depth.m<1.8, 1.0),
         Sample.depth.m = replace(Sample.depth.m, Sample.depth.m>=1.8 & Sample.depth.m<2.8, 2.0),
         Sample.depth.m = replace(Sample.depth.m, Sample.depth.m>=2.8 & Sample.depth.m<3.7, 3.0),
         Sample.depth.m = replace(Sample.depth.m, Sample.depth.m>=3.7 & Sample.depth.m<4.6, 4.0),
         Sample.depth.m = replace(Sample.depth.m, Sample.depth.m>=4.6 & Sample.depth.m<5.6, 5.0),
         Sample.depth.m = replace(Sample.depth.m, Sample.depth.m>=5.6 & Sample.depth.m<6.6, 6.0),
         Sample.depth.m = replace(Sample.depth.m, Sample.depth.m>=6.6 & Sample.depth.m<7.6, 7.0),
         Sample.depth.m = replace(Sample.depth.m, Sample.depth.m>=7.6, 8.0))

#U12sondePro.d<-dcast(U12sondePro, Temp.C ~ Sample.depth.m)

U12sondePro<-filter(U12sondePro, Temp.C!=6.71, Temp.C!=16.06, Temp.C !=14.24,
                    Temp.C!=13.85, Temp.C!=13.01)
U12sondePro<-U12sondePro[1:289,]
sum(U12sondePro$Temp.C==13.01)
 
#wahoo, this works!!
U12SondePro.s<-spread(U12sondePro, key=Sample.depth.m, value = Temp.C)

U12SondePro.s<-U12SondePro.s%>%
  dplyr::rename(datetime = Sample.Date,
                wtrT_0 = "0.1",
                wtrT_1 = "1",
                wtrT_2 = "2",
                wtrT_3 = "3",
                wtrT_4 = "4",
                wtrT_5 = "5",
                wtrT_6 = "6",
                wtrT_7 = "7",
                wtrT_8 = "8")
U12SondePro.rLake<-select(U12SondePro.s, -Site, -Lake)

par(fg=NA, col="black", axisTicks(usr=c(5, 35), log=FALSE, nint = 14)) #border="black",#this gets rid of the lines on the legend


#missing sonde measurements for 1, 3, and 5 m depths in fall of 2017: 
# 10/31, 11/14, 11/28 & 12/11
#want to fill these so rLakeAnalyzer can use them:
for (i in 1:nrow(U12SondePro.rLake)) {
  U12SondePro.rLake$wtrT_1[i]<-ifelse(is.na(U12SondePro.rLake$wtrT_1[i]),
                    sum(U12SondePro.rLake$wtrT_0[i], U12SondePro.rLake$wtrT_2[i])/2,
                    U12SondePro.rLake$wtrT_1[i])
  U12SondePro.rLake$wtrT_3[i]<-ifelse(is.na(U12SondePro.rLake$wtrT_3[i]),
                                      sum(U12SondePro.rLake$wtrT_2[i], U12SondePro.rLake$wtrT_4[i])/2,
                                      U12SondePro.rLake$wtrT_3[i])
  U12SondePro.rLake$wtrT_5[i]<-ifelse(is.na(U12SondePro.rLake$wtrT_5[i]),
                                      sum(U12SondePro.rLake$wtrT_4[i], U12SondePro.rLake$wtrT_6[i])/2,
                                      U12SondePro.rLake$wtrT_5[i])}

### adding in blank filler on x-axis to match wtr.heat.map with other Fig 2 panels,
### for the winter when the sondes were out of the water
timeframeDaily<-seq.POSIXt(from = as.POSIXct("2016-12-01 00:00:00",
                                          format="%Y-%m-%d %H:%M:%S",
                                          tz = "UTC"),
                        to = as.POSIXct("2017-04-24 23:30:00",
                                        format="%Y-%m-%d %H:%M:%S",
                                        tz = "UTC"),by = "1 day")
timeframeDaily2<-seq.POSIXt(from = as.POSIXct("2017-12-12 00:00:00",
                                              format="%Y-%m-%d %H:%M:%S",
                                              tz = "UTC"),
                            to = as.POSIXct("2018-04-11 00:00:00",
                                            format="%Y-%m-%d %H:%M:%S",
                                            tz = "UTC"),by = "1 day")
fillerDF<-as.data.frame(timeframeDaily)
fillerDFwinter<-as.data.frame(timeframeDaily2)
#temp<-nrow(timeframeDaily)
fillerDF<-fillerDF%>%
  mutate(datetime=as.Date(timeframeDaily),
         wtrT_0=NaN,
         wrtT_1=NaN,
         wtrT_2=NaN,
         wtrT_3=NaN,
         wtrT_4=NaN,
         wtrT_5=NaN,
         wtrT_6=NaN,
         wtrT_7=NaN,
         wtrT_8=NaN)
fillerDFwinter<-fillerDFwinter%>%
  mutate(datetime=as.Date(timeframeDaily2),
         wtrT_0=NaN,
         wrtT_1=NaN,
         wtrT_2=NaN,
         wtrT_3=NaN,
         wtrT_4=NaN,
         wtrT_5=NaN,
         wtrT_6=NaN,
         wtrT_7=NaN,
         wtrT_8=NaN)


myDeepTList <- list()
myDeepTList[[1]]<-filter(U12SondePro.rLake, datetime>"2017-10-15", datetime<"2018-01-01")
myDeepTList[[2]]<-filter(U12SondePro.rLake, datetime>"2018-10-15")
myDeepTList[[3]]<-rLakeBuoyDaily
myDeepTList[[4]]<-select(fillerDF, -timeframeDaily)
names(myDeepTList[[4]])<-names(myDeepTList[[1]])
myDeepTList[[5]]<-select(fillerDFwinter, -timeframeDaily2)
names(myDeepTList[[5]])<-names(myDeepTList[[1]])
rLakeBuoySonde<-do.call("rbind", myDeepTList)
rLakeBuoySonde<-rLakeBuoySonde[order(rLakeBuoySonde$datetime),]

rLakeBuoySonde$datetime[32]
rLakeBuoySonde$datetime<-as.POSIXct(rLakeBuoySonde$datetime, tz="UTC")
rLakeBuoySonde2<-as.data.frame(rLakeBuoySonde)
rLakeBuoySonde2$datetime<-rLakeBuoySonde$datetime

plotTicks<-seq(from=as.Date(rLakeBuoySonde$datetime[32]),
               to = as.Date(rLakeBuoySonde$datetime[nrow(rLakeBuoySonde)]),
               by = "1 month")
par(fg="black", col="black", axisTicks(usr=c(5, 35), log=FALSE, nint = 14)) #this gets rid of the lines on the legend


######## Figure 3 g ########
rLakeAnalyzer::wtr.heat.map(rLakeBuoySonde2,
             zlim=c(2, 32),
             key.title = title(main = "Celsius", cex.main = 1, line=1),
             plot.title = title(ylab = "Depth (m)",
                                main="Deep Site T Profile"),
             plot.axes = {axis.Date(side = 1, 
                                    x=rLakeBuoySonde2$datetime,
                                    at=plotTicks,
                                    format="%b %Y");
               axis(2)},
             borders="black")

write_csv(rLakeBuoySonde,
          path=paste0(projectWD, "/dataL2/Fig3g_heatmap.csv"))

profileTest<-filter(rLakeBuoySonde, datetime>"2018-04-01", datetime<"2018-11-01")

# wtr.heatmap.layers(profileTest,
#                    zlim=c(2, 32),
#                    key.title = title(main = "Celsius", cex.main = 1, line=1),
#                    plot.title = title(ylab = "Depth (m)",
#                                       main="Deep Site T Profile"),
#                    plot.axes = {axis.Date(side = 1, 
#                                           x=rLakeBuoySonde$datetime,
#                                           at=plotTicks,
#                                           format="%b %Y");
#                      axis(2)},
#                    borders="black")
# 
# turnOver<-ts.thermo.depth(rLakeBuoySonde, Smin=0.1, na.rm=FALSE)
# 
# 
# ggplot(filter(turnOver, datetime>"2018-09-01", datetime<"2018-10-16"),
#        aes(datetime, thermo.depth*-1))+
#   geom_line()+
#   scale_x_datetime(labels=date_format("%b %d", tz="UTC"), 
#                breaks=date_breaks("2 days"),
#                limits = c(as.POSIXct(as.Date("2018-09-01")),
#                             as.POSIXct(as.Date("2018-10-16"))))+
#   ylim(-8, 0)
#turnover occurred in 2017

#https://stackoverflow.com/questions/41186998/controlling-x-axis-time-stamp-on-filled-contour-plot-r


