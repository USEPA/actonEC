
######## Prep the EC Data: gapfilled, upper and lower confidence intervals #####################

ggplot(DailyANNFluxes, aes(date, ch4.t_cumu))+
  geom_line()



rowNum<-which(grepl("2018-01-01", DailyANNFluxes$date))
value17<-DailyANNFluxes$ch4.t_cumu[rowNum]
value17L<-DailyANNFluxes$ch4.t_cumuL95[rowNum]
value17U<-DailyANNFluxes$ch4.t_cumuU95[rowNum]

#break it up by year: subtract max 2017 cumulative value from 2018 to start at 0 
for(i in 1:nrow(DailyANNFluxes)){
  DailyANNFluxes$ch4.t_cumu_yr[i]<-ifelse(DailyANNFluxes$year[i]<2018,
                                          DailyANNFluxes$ch4.t_cumu[i],
                                          DailyANNFluxes$ch4.t_cumu[i] - value17)
  DailyANNFluxes$ch4.t_cumu_yr_U95[i]<-ifelse(DailyANNFluxes$year[i]<2018,
                                              DailyANNFluxes$ch4.t_cumuU95[i],
                                              DailyANNFluxes$ch4.t_cumuU95[i] - value17U)
  DailyANNFluxes$ch4.t_cumu_yr_L95[i]<-ifelse(DailyANNFluxes$year[i]<2018,
                                              DailyANNFluxes$ch4.t_cumuL95[i],
                                              DailyANNFluxes$ch4.t_cumuL95[i] - value17L)
}


######### AFT cumulative areal emissions and error -----
### The AFT uncertainty is the standard deviation, not confidence interval
### but the script names it CI so it can be coerced into a long data frame with the 
### EC and GRTS data for plotting

### Need to sum daily errors in quadrature:
DailyDeepTfluxes$ch4.cumuErr<-DailyDeepTfluxes$dailyCH4TErr

for(i in 2:nrow(DailyDeepTfluxes)){
  DailyDeepTfluxes$ch4.cumuErr[i]<-ifelse(!is.na(DailyDeepTfluxes$dailyCH4TErr[i]),
                                          (sqrt(DailyDeepTfluxes$ch4.cumuErr[i-1]^2+
                                                  DailyDeepTfluxes$dailyCH4TErr[i]^2)),
                                          DailyDeepTfluxes$ch4.cumuErr[i-1])
}
DailyDeepTfluxes$ch4.cumuErr<-DailyDeepTfluxes$ch4.cumuErr*24/1000

DailyDeepTfluxes<-mutate(DailyDeepTfluxes,
                         ch4.trate_zero = replace(ch4.trate, is.na(ch4.trate), 0), #zero means that NAs have been replaced with zeros
                         ch4.t.cumu = cumsum(ch4.trate_zero)*24/1000, #convert from mg/m2/hr to g/m2/d
                         #ch4.cumuErr = cumsum(dailyCH4TErr)*24/1000, need to add in quadrature
                         ch4.t.cumuUCI = ch4.t.cumu+ch4.cumuErr, 
                         ch4.t.cumuLCI = ch4.t.cumu-ch4.cumuErr)

DailyDeepTfluxes<-DailyDeepTfluxes%>%
  mutate(ch4.t_cumu_yr = ch4.t.cumu,
         ch4.t_cumu_yr = replace(ch4.t_cumu_yr, monthday<"2020-05-01", NA),
         ch4.t_cumuL95=ch4.t_cumu_yr-ch4.cumuErr,
         ch4.t_cumuU95=ch4.t_cumu_yr+ch4.cumuErr
  )
#take care of subtracting 2017 from 2018
rowNum<-which(grepl("2018-01-01", DailyDeepTfluxes$date))
value17<-DailyDeepTfluxes$ch4.t.cumu[rowNum]
value17L<-DailyDeepTfluxes$ch4.t.cumuLCI[rowNum]
value17U<-DailyDeepTfluxes$ch4.t.cumuUCI[rowNum]

for(i in 1:nrow(DailyDeepTfluxes)){
  DailyDeepTfluxes$ch4.t_cumu_yr[i]<-ifelse(DailyDeepTfluxes$year[i]<2018,
                                            DailyDeepTfluxes$ch4.t.cumu[i],
                                            DailyDeepTfluxes$ch4.t.cumu[i] - value17)
  DailyDeepTfluxes$ch4.t_cumu_yr_U95[i]<-ifelse(DailyDeepTfluxes$year[i]<2018,
                                              DailyDeepTfluxes$ch4.t.cumuUCI[i],
                                              DailyDeepTfluxes$ch4.t.cumuUCI[i] - value17U)
  DailyDeepTfluxes$ch4.t_cumu_yr_L95[i]<-ifelse(DailyDeepTfluxes$year[i]<2018,
                                              DailyDeepTfluxes$ch4.t.cumuLCI[i],
                                              DailyDeepTfluxes$ch4.t.cumuLCI[i] - value17L)
}

ggplot(DailyDeepTfluxes, aes(Rdate, ch4.t_cumu_yr))+
  geom_line()+
  geom_ribbon(data=DailyDeepTfluxes,
              aes(x=Rdate, ymin=ch4.t_cumu_yr_L95,
              ymax=ch4.t_cumu_yr_U95), alpha=0.3)

######### shallow AFT + Cham site -----
### Need to sum daily errors in quadrature:
DailyShalTfluxes$ch4.cumuErr<-DailyShalTfluxes$dailyCH4TErr

for(i in 2:nrow(DailyShalTfluxes)){
  DailyShalTfluxes$ch4.cumuErr[i]<-ifelse(!is.na(DailyShalTfluxes$dailyCH4TErr[i]),
                                          (sqrt(DailyShalTfluxes$ch4.cumuErr[i-1]^2+
                                                 DailyShalTfluxes$dailyCH4TErr[i]^2)),
                                          DailyShalTfluxes$ch4.cumuErr[i-1])
}
DailyShalTfluxes$ch4.cumuErr<-DailyShalTfluxes$ch4.cumuErr*24/1000

DailyShalTfluxes<-mutate(DailyShalTfluxes,
                         ch4.trate_zero = replace(ch4.trate, is.na(ch4.trate), 0), #zero means that NAs have been replaced with zeros
                         ch4.t.cumu = cumsum(ch4.trate_zero)*24/1000, #convert from mg/m2/hr to g/m2/d
                         #ch4.cumuErr = cumsum(dailyCH4TErr)*24/1000, need to add in quadrature
                         ch4.t.cumuUCI = ch4.t.cumu+ch4.cumuErr, 
                         ch4.t.cumuLCI = ch4.t.cumu-ch4.cumuErr)

DailyShalTfluxes<-DailyShalTfluxes%>%
  mutate(ch4.t_cumu_yr = ch4.t.cumu,
         ch4.t_cumu_yr = replace(ch4.t_cumu_yr, monthday<"2020-05-01", NA),
         ch4.t_cumuL95=ch4.t_cumu_yr-ch4.cumuErr,
         ch4.t_cumuU95=ch4.t_cumu_yr+ch4.cumuErr
  )
#take care of subracting 2017 from 2018
rowNum<-which(grepl("2018-01-01", DailyShalTfluxes$date))
value17<-DailyShalTfluxes$ch4.t.cumu[rowNum]
value17L<-DailyShalTfluxes$ch4.t.cumuLCI[rowNum]
value17U<-DailyShalTfluxes$ch4.t.cumuUCI[rowNum]

for(i in 1:nrow(DailyShalTfluxes)){
  DailyShalTfluxes$ch4.t_cumu_yr[i]<-ifelse(DailyShalTfluxes$year[i]<2018,
                                            DailyShalTfluxes$ch4.t.cumu[i],
                                            DailyShalTfluxes$ch4.t.cumu[i] - value17)
  DailyShalTfluxes$ch4.t_cumu_yr_U95[i]<-ifelse(DailyShalTfluxes$year[i]<2018,
                                                DailyShalTfluxes$ch4.t.cumuUCI[i],
                                                DailyShalTfluxes$ch4.t.cumuUCI[i] - value17U)
  DailyShalTfluxes$ch4.t_cumu_yr_L95[i]<-ifelse(DailyShalTfluxes$year[i]<2018,
                                                DailyShalTfluxes$ch4.t.cumuLCI[i],
                                                DailyShalTfluxes$ch4.t.cumuLCI[i] - value17L)
}

# ggplot(DailyShalTfluxes, aes(Rdate, ch4.t_cumu_yr))+
#   geom_line()+
#   geom_ribbon(data=DailyShalTfluxes,
#               aes(x=Rdate, ymin=ch4.t_cumu_yr_L95,
#               ymax=ch4.t_cumu_yr_U95, alpha=0.2))

############# GRTS data ####

#total
grts_ts<-grts_ts%>%
  mutate(ch4.t_gf = replace(ch4.t_gf, date>"2017-05-01" & date<"2017-07-10", grts_ts$ch4.t_gf[which(grepl("2017-07-10", grts_ts$date))]),
         ch4.t_gf = replace(ch4.t_gf, date>"2018-05-01" & date<"2018-07-10", grts_ts$ch4.t_gf[which(grepl("2018-07-10", grts_ts$date))]),
         ch4.t_gf = replace(ch4.t_gf, date>"2018-09-19" & date<"2018-10-01", grts_ts$ch4.t_gf[which(grepl("2018-09-19", grts_ts$date))]),
         ch4.t_gf = replace(ch4.t_gf, date<"2017-05-01", 0),
         ch4.t_gf = replace(ch4.t_gf, date>"2017-10-04" & date<"2018-05-01", 0),
         ch4.t_gf = replace(ch4.t_gf, date>"2018-10-01", 0),
         ch4.t_gf = na.approx(ch4.t_gf, rule=2),
         ch4.t_cumu = cumsum(ch4.t_gf)/1000*24,
         ch4.t_cumuPt = as.numeric(NA),
         ch4.t_cumuErr = as.numeric(NA))

for(i in 1:nrow(grts_ts)){
  grts_ts$ch4.t_cumuPt[i] <- ifelse(is.na(grts_ts$ch4.trate.FE[i]),
                                    grts_ts$ch4.t_cumuPt[i],
                                    grts_ts$ch4.t_cumu[i])
  grts_ts$ch4.t_cumuErr[i]<- ifelse(is.na(grts_ts$ch4.trate.FE[i]),
                                    grts_ts$ch4.t_cumuErr[i],
                                    grts_ts$ch4.t_cumu[i]*grts_ts$ch4.trate.FE[i])
}

grts_ts<-grts_ts%>%
  mutate(year = year(date),
         monthday = format(date, format="%m-%d %H:%M"))


#####Test plots, cuml calcs#
ggplot(grts_ts, aes(date, ch4.trate.mg.h_Estimate))+
  geom_point()+
  geom_point(aes(date, ch4.erate.mg.h_Estimate), color="red")+
  geom_point(aes(date, ch4.drate.mg.m2.h_Estimate), color = "blue")+
  geom_line(aes(date, ch4.t_gf))+
  geom_line(aes(date, ch4.e_gf), color="red")+
  geom_line(aes(date, ch4.d_gf), color="blue")+
  ylim(0, 50)

ggplot(grts_ts, aes(date, ch4.t_cumu))+
  geom_line()+
  geom_errorbar(data=grts_ts, aes(date, grts_ts$ch4.t_cumuPt), ymin=ch4.t_cumuPt-ch4.t_cumuErr,
                ymax=ch4.t_cumuPt+ch4.t_cumuErr)

ggplot(grts_ts, aes(date, ch4.t_cumuPt))+
  geom_point()+
  geom_errorbar(data = grts_ts,
                aes(ymin=(ch4.t_cumuPt-ch4.t_cumuErr),
                    ymax=(ch4.t_cumuPt+ch4.t_cumuErr)))

GRTSF8<-grts_ts%>%
  mutate(ch4.t_gf = replace(ch4.t_gf, date<"2017-05-01", NA),
         ch4.t_gf = replace(ch4.t_gf, date>"2017-10-04" & date<"2018-05-01", NA),
         ch4.t_gf = replace(ch4.t_gf, date>"2018-10-01", NA),
         ch4.trate = ch4.t_gf,
         ch4.tPt = ch4.t_cumuPt,
         ch4.FE = ch4.trate.FE,
         Label = "d) Lake-Wide Surveys",
         year = year(date),
         monthday = format(date, format="%m-%d %H:%M"))# %>%
GRTSF8$monthday<-as.Date(GRTSF8$monthday, format="%m-%d %H:%M")

rowNum<-which(grepl("2018-01-01", GRTSF8$date))
value17<-GRTSF8$ch4.t_cumu[rowNum]

for(i in 1:nrow(GRTSF8)){
  GRTSF8$ch4.t_cumu_yr[i]<-ifelse(GRTSF8$year[i]<2018,
                                  GRTSF8$ch4.t_cumu[i],
                                  GRTSF8$ch4.t_cumu[i] - value17)
  GRTSF8$ch4.tPt_yr[i]<-ifelse(GRTSF8$year[i]<2018,
                               GRTSF8$ch4.tPt[i],
                               GRTSF8$ch4.tPt[i] - value17)
  GRTSF8$ch4.tPt_err[i]<-GRTSF8$ch4.tPt_yr[i]*GRTSF8$ch4.FE[i]
  
}

GRTSF8<-GRTSF8%>%
  mutate(ch4.t_cumu_yr = replace(ch4.t_cumu_yr, monthday<"2020-05-01", NA),
         ch4.t_cumu_yr = replace(ch4.t_cumu_yr, monthday>"2020-10-04", NA),
         ch4.t_cumu_yr_L95 = ch4.tPt_yr-ch4.tPt_err,
         ch4.t_cumu_yr_U95 = ch4.tPt_yr+ch4.tPt_err
  )

DailyANNF8<-select(DailyANNFluxes, date, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)
ShalSiteF8<-select(DailyShalTfluxes, date, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)
DeepSiteF8<-select(DailyDeepTfluxes, date, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)
GRTSF8<-select(GRTSF8, date, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)

cumulativeFluxF8<-list()
cumulativeFluxF8[[1]]<-DailyANNF8
cumulativeFluxF8[[2]]<-ShalSiteF8
cumulativeFluxF8[[3]]<-DeepSiteF8
cumulativeFluxF8[[4]]<-GRTSF8

cmlDailyFluxF8<-do.call("rbind", cumulativeFluxF8)

#Figure7<-
ggplot(filter(cmlDailyFluxF8, year>2016), aes(monthday, ch4.t_cumu_yr))+
  geom_line(aes(color=Label, linetype=Label), size=1)+
  geom_ribbon(data=filter(cmlDailyFluxF8, year>2016), 
              aes(x = monthday, 
                  ymin = ch4.t_cumu_yr_L95,
                  ymax = ch4.t_cumu_yr_U95,
                  color = Label, fill=Label),alpha=0.2,
              show.legend=FALSE)+
  facet_grid(.~year)+
  scale_x_date(date_minor_breaks = "1 month", 
               labels = date_format("%b"))+
  theme(legend.position = "top",
        legend.title=element_blank())+
  #theme_bw()+
  ylab(expression(Cumulative~CH[4]~Areal~Emissions~(g~m^-2)))+
  xlab("")+
  labs(fill = "Method")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank())

ggsave()

#####Test plots, cuml calcs####################

figureS10<-ggplot(grts_ts, aes(date, ch4.trate.mg.h_Estimate))+
  geom_point()+
  geom_point(aes(date, ch4.erate.mg.h_Estimate), color="red")+
  geom_point(aes(date, ch4.drate.mg.m2.h_Estimate), color = "blue")+
  geom_line(aes(date, ch4.t_gf))+
  geom_line(aes(date, ch4.e_gf), color="red")+
  geom_line(aes(date, ch4.d_gf), color="blue")+
  ylim(0, 50)

# figTest<-ggplot(dailyFluxF3, aes(date, meanCH4Flux))+
#   annotate("rect", xmin=as.POSIXct(as.Date("2017-05-24")),
#            xmax=as.POSIXct(as.Date("2017-06-04")),
#            ymin=-Inf, ymax=Inf, alpha=0.5)+
#   geom_line()+
#   facet_grid(Label~.)+
#   theme_bw()
# 
# Figure3+
#   annotate("rect", xmin = as.POSIXct(as.Date("2017-05-24")),
#            xmax = as.POSIXct(as.Date("2017-06-04")),
#            ymin = -25, ymax = 100, 
#            alpha = .5)
#   geom_vline(xintercept=as.POSIXct(as.Date(c("2017-05-24", "2017-06-04", "2018-05-24", "2018-06-04"))))





# ggplot(filter(epDataFilled, datetime>"2018-05-21", datetime<"2018-06-15"),
#        aes(datetime, ch4_preds6.01*60*60*16/1000))+
#   geom_line(alpha=0.5)+
#   geom_point(data=filter(epDataFilled, datetime>"2018-05-21", datetime<"2018-06-15"),
#              aes(datetime, ch4_flux*60*60*16/1000), color="red", alpha=0.1)+
#   ylim(1*60*60*16/1000, 2*60*60*16/1000)+
#   ylab("CH4 Flux (mg m-2 hr-1)")
# 
# max(epDataFilled$ch4_flux, na.rm=TRUE)
# select(epDataFilled, datetime, ch4_flux)%>%
#   filter(datetime>"2018-05-21", datetime<"2018-06-15")
# 
# ggplot

#### CUMULATIVE FUNNEL FLUXES ###########
DailyShalFluxes<-DailyShalFluxes %>% 
  mutate(ch4_flux_filled.f3 = na.approx(meanCH4Flux, rule=2),
         cumu_ch4 = cumsum(ch4_flux_filled.f3*24/1000)) #convert from mg ch4 m-2 hr-1 to g ch4 m-2

DailyDeepFluxes$ch4_filtered<-DailyDeepFluxes$meanCH4Flux
DailyDeepFluxes$ch4_filtered[66]<-NaN #spike in measurements

DailyDeepFluxes<-DailyDeepFluxes %>% 
  mutate(ch4_flux_filled.f3 = na.approx(ch4_filtered, rule=2),
         cumu_ch4 = cumsum(ch4_flux_filled.f3*24/1000)) #convert from mg ch4 m-2 hr-1 (per day for daily fluxes) to g ch4 m-2)                           

ggplot(DailyDeepFluxes, aes(date, ch4_flux_filled.f3))+
  geom_line()
ggplot(DailyShalFluxes, aes(date, ch4_flux_filled.f3))+
  geom_line()
ggplot(DailyDeepFluxes, aes(date, cumu_ch4))+
  geom_line()
ggplot(DailyShalFluxes, aes(date, cumu_ch4))+
  geom_line()

maydf17<-DailyDeepFluxes$cumu_ch4[1] #2017-05-09 20:00:00        0.0119
octdf17<-DailyDeepFluxes$cumu_ch4[145] #2017-09-30 20:00:00      19.2
maysf17<-DailyShalFluxes$cumu_ch4[1] #2017-05-09 20:00:00       0.00315
octsf17<-DailyShalFluxes$cumu_ch4[145] #2017-09-30 20:00:00       15.2

octdf17-maydf17 #summer 2017 deep site AFT
octsf17-maysf17 #summer 2017 shallow site AFT

novdf17<-DailyDeepFluxes$cumu_ch4[176] #2017-10-30 20:00:00       22.9     

novdf17-octdf17

#start of 2018 active funnel fluxes: 5/24 for deep site, June 6th for shallow site
offset.df18<-DailyDeepFluxes$cumu_ch4[380] #2018-05-23 20:00:00          25.9 
octdf18<-DailyDeepFluxes$cumu_ch4[510] #2018-09-30 20:00:00            48.3
novdf18<-DailyDeepFluxes$cumu_ch4[541] #2018-10-31 20:00:00            50.4
nov14.df18<-DailyDeepFluxes$cumu_ch4[555] # 2018-11-14 19:00:00     50.4

offset.sf18<-DailyShalFluxes$cumu_ch4[393] #2018-06-05 20:00:00       21.9
octsf18<-DailyShalFluxes$cumu_ch4[510] # 2018-09-30 20:00:00          42.3
novsf18<-DailyShalFluxes$cumu_ch4[541] #2018-10-31 20:00:00     43
nov14.df18<-DailyShalFluxes$cumu_ch4[555] #2018-11-14 19:00:00   43.0

#may 23 - sept 30th cumulative areal methane emissions (g ch4 m-2):
(octdf18-offset.df18+(octsf18-offset.sf18))/2

#shoulder (oct 1 - 31)
(novdf18-octdf18+(novsf18-octsf18))/2





#adding GRTS data -- need to run GRTS code
file.edit('scriptsAndRmd/GRTS/masterScriptActonGRTS.R')
meanVariance.c<-meanVariance.c%>%
  mutate(date = as.POSIXct(meanVariance.c$deplyDt, format="%m/%d/%Y", tz="UTC"),
         Label = "d) Lake Surveys",
         ch4.trate.FE = ch4.trate.mg.h_StdError/ch4.trate.mg.h_Estimate)

ggplot(meanVariance.c,
       aes(Lake_Name, ch4.erate.mg.h_Estimate))+
  geom_point()+
  geom_errorbar(aes(ymax = ch4.erate.mg.h_UCB95Pct,
                    ymin = ch4.erate.mg.h_LCB95Pct))+
  #ylim(0, 15)+
  #xlim("Acton Lake 07", "Acton Lake 08")+
  labs(x="", y=expression(CH[4]~Ebullition~(mg~CH[4]~m^{-2}~hr^{-1})))

grts_ts<-left_join(select(DailyANNF4, date), 
                   select(meanVariance.c, date, Label, ch4.drate.mg.m2.h_Estimate, ch4.trate.mg.h_Estimate, ch4.erate.mg.h_Estimate,
                          ch4.trate.FE),
                   by="date")
rowNum<-which(grepl("2017-07-10", grts_ts$date))

#setup
grts_ts<-grts_ts%>%
  mutate(ch4.d_gf = ch4.drate.mg.m2.h_Estimate,
         ch4.e_gf = ch4.erate.mg.h_Estimate,
         ch4.t_gf = ch4.trate.mg.h_Estimate)

#diffusive
grts_ts<-grts_ts%>%
  mutate(ch4.d_gf = replace(ch4.d_gf, date>"2017-05-01" & date<"2017-07-10", grts_ts$ch4.d_gf[which(grepl("2017-07-10", grts_ts$date))]),
         ch4.d_gf = replace(ch4.d_gf, date>"2018-05-01" & date<"2018-07-10", grts_ts$ch4.d_gf[which(grepl("2018-07-10", grts_ts$date))]),
         ch4.d_gf = replace(ch4.d_gf, date>"2018-09-19" & date<"2018-10-01", grts_ts$ch4.d_gf[which(grepl("2018-09-19", grts_ts$date))]),
         ch4.d_gf = replace(ch4.d_gf, date<"2017-05-01", 0),
         ch4.d_gf = replace(ch4.d_gf, date>"2017-10-04" & date<"2018-05-01", 0),
         ch4.d_gf = replace(ch4.d_gf, date>"2018-10-01", 0),
         ch4.d_gf = na.approx(ch4.d_gf, rule=2),
         ch4.d_gf_grts_cumu = cumsum(ch4.d_gf)/1000*24)

#ebullitive
grts_ts<-grts_ts%>%
  mutate(ch4.e_gf = replace(ch4.e_gf, date>"2017-05-01" & date<"2017-07-10", grts_ts$ch4.e_gf[which(grepl("2017-07-10", grts_ts$date))]),
         ch4.e_gf = replace(ch4.e_gf, date>"2018-05-01" & date<"2018-07-10", grts_ts$ch4.e_gf[which(grepl("2018-07-10", grts_ts$date))]),
         ch4.e_gf = replace(ch4.e_gf, date>"2018-09-19" & date<"2018-10-01", grts_ts$ch4.e_gf[which(grepl("2018-09-19", grts_ts$date))]),
         ch4.e_gf = replace(ch4.e_gf, date<"2017-05-01", 0),
         ch4.e_gf = replace(ch4.e_gf, date>"2017-10-04" & date<"2018-05-01", 0),
         ch4.e_gf = replace(ch4.e_gf, date>"2018-10-01", 0),
         ch4.e_gf = na.approx(ch4.e_gf, rule=2),
         ch4.e_gf_grts_cumu = cumsum(ch4.e_gf)/1000*24)



###################
GRTSF8<-grts_ts%>%
  mutate(ch4.t_gf = replace(ch4.t_gf, date<"2017-05-01", NA),
         ch4.t_gf = replace(ch4.t_gf, date>"2017-10-04" & date<"2018-05-01", NA),
         ch4.t_gf = replace(ch4.t_gf, date>"2018-10-01", NA),
         ch4.trate = ch4.t_gf,
         ch4.tPt = ch4.t_cumuPt,
         ch4.FE = ch4.trate.FE,
         Label = "d) Lake-Wide Surveys",
         year = year(date),
         monthday = format(date, format="%m-%d %H:%M"))# %>%
GRTSF8$monthday<-as.Date(GRTSF8$monthday, format="%m-%d %H:%M")

rowNum<-which(grepl("2018-01-01", GRTSF8$date))
value17<-GRTSF8$ch4.t_cumu[rowNum]

for(i in 1:nrow(GRTSF8)){
  GRTSF8$ch4.t_cumu_yr[i]<-ifelse(GRTSF8$year[i]<2018,
                                  GRTSF8$ch4.t_cumu[i],
                                  GRTSF8$ch4.t_cumu[i] - value17)
  GRTSF8$ch4.tPt_yr[i]<-ifelse(GRTSF8$year[i]<2018,
                               GRTSF8$ch4.tPt[i],
                               GRTSF8$ch4.tPt[i] - value17)
  GRTSF8$ch4.tPt_err[i]<-GRTSF8$ch4.tPt_yr[i]*GRTSF8$ch4.FE[i]
  
}

GRTSF8<-GRTSF8%>%
  mutate(ch4.t_cumu_yr = replace(ch4.t_cumu_yr, monthday<"2019-05-01", NA),
         ch4.t_cumu_yr = replace(ch4.t_cumu_yr, monthday>"2019-10-04", NA),
         ch4.t_cumu_yr_L95 = ch4.tPt_yr-ch4.tPt_err,
         ch4.t_cumu_yr_U95 = ch4.tPt_yr+ch4.tPt_err
  )

GRTSf3<-meanVariance.c%>%
  mutate(datetime = date,
         Label = "d) Lake-Wide Surveys",
         ch4_flux.f3 = ch4.trate.mg.h_Estimate,
         ch4_flux_filled.f3 = ch4.trate.mg.h_StdError)
#########################


DailyANNF8<-select(DailyANNFluxes, date, ch4.trate, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)
#ShalSiteF8<-select(df14Cham, date, ch4.trate, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)
ShalSiteF4<-select(DailyShalTfluxes, date, ch4.trate, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)
#DeeptSiteF8<-select(df12Cham, date, ch4.trate, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)
DeepSiteF4<-select(DailyDeepTfluxes, date, ch4.trate, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)

dailyF8list<-list()
dailyF8list[[1]]<-as.data.frame(DailyANNF8)
dailyF8list[[2]]<-as.data.frame(ShalSiteF8)
dailyF8list[[3]]<-as.data.frame(DeepSiteF8)
dailyF8list[[4]]<-as.data.frame(select(GRTSF8, date, ch4.trate, Label, year, ch4.t_cumu, 
                                       monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_L95, ch4.t_cumu_yr_U95))

########### Figure8: Cumulative areal emissions  ------
ggplot(filter(dailyFluxF4, year>2016), aes(monthday, ch4.t_cumu_yr))+
  geom_line(aes(color=Label, linetype=Label), size=1)+
  geom_ribbon(data=filter(dailyFluxF4, year>2016), 
              aes(x = monthday, 
                  ymin = ch4.t_cumu_yr_L95,
                  ymax = ch4.t_cumu_yr_U95,
                  color = Label, fill=Label),alpha=0.2)+
  facet_grid(.~year)+
  scale_x_date(date_minor_breaks = "1 month", 
               labels = date_format("%b"))+
  theme(legend.position = "top",
        legend.title=element_blank())+
  #theme_bw()+
  ylab(expression(Cumulative~CH[4]~Areal~Emissions~(g~m^-2)))+
  xlab("")+
  labs(fill = "Method")+
  theme_bw()





# ggplot(filter(epDataFilled, datetime>"2018-05-21", datetime<"2018-06-15"),
#        aes(datetime, ch4_preds6.01*60*60*16/1000))+
#   geom_line(alpha=0.5)+
#   geom_point(data=filter(epDataFilled, datetime>"2018-05-21", datetime<"2018-06-15"),
#              aes(datetime, ch4_flux*60*60*16/1000), color="red", alpha=0.1)+
#   ylim(1*60*60*16/1000, 2*60*60*16/1000)+
#   ylab("CH4 Flux (mg m-2 hr-1)")
# 
# max(epDataFilled$ch4_flux, na.rm=TRUE)
# select(epDataFilled, datetime, ch4_flux)%>%
#   filter(datetime>"2018-05-21", datetime<"2018-06-15")
# 
# ggplot

#### CUMULATIVE FUNNEL FLUXES ###########
DailyShalFluxes<-DailyShalFluxes %>% 
  mutate(ch4_flux_filled.f3 = na.approx(meanCH4Flux, rule=2),
         cumu_ch4 = cumsum(ch4_flux_filled.f3*24/1000)) #convert from mg ch4 m-2 hr-1 to g ch4 m-2

DailyDeepFluxes$ch4_filtered<-DailyDeepFluxes$meanCH4Flux
DailyDeepFluxes$ch4_filtered[66]<-NaN #spike in measurements

DailyDeepFluxes<-DailyDeepFluxes %>% 
  mutate(ch4_flux_filled.f3 = na.approx(ch4_filtered, rule=2),
         cumu_ch4 = cumsum(ch4_flux_filled.f3*24/1000)) #convert from mg ch4 m-2 hr-1 (per day for daily fluxes) to g ch4 m-2)                           

ggplot(DailyDeepFluxes, aes(date, ch4_flux_filled.f3))+
  geom_line()
ggplot(DailyShalFluxes, aes(date, ch4_flux_filled.f3))+
  geom_line()
ggplot(DailyDeepFluxes, aes(date, cumu_ch4))+
  geom_line()
ggplot(DailyShalFluxes, aes(date, cumu_ch4))+
  geom_line()

maydf17<-DailyDeepFluxes$cumu_ch4[1] #2017-05-09 20:00:00        0.0119
octdf17<-DailyDeepFluxes$cumu_ch4[145] #2017-09-30 20:00:00      19.2
maysf17<-DailyShalFluxes$cumu_ch4[1] #2017-05-09 20:00:00       0.00315
octsf17<-DailyShalFluxes$cumu_ch4[145] #2017-09-30 20:00:00       15.2

octdf17-maydf17 #summer 2017 deep site AFT
octsf17-maysf17 #summer 2017 shallow site AFT

novdf17<-DailyDeepFluxes$cumu_ch4[176] #2017-10-30 20:00:00       22.9     

novdf17-octdf17

#start of 2018 active funnel fluxes: 5/24 for deep site, June 6th for shallow site
offset.df18<-DailyDeepFluxes$cumu_ch4[380] #2018-05-23 20:00:00          25.9 
octdf18<-DailyDeepFluxes$cumu_ch4[510] #2018-09-30 20:00:00            48.3
novdf18<-DailyDeepFluxes$cumu_ch4[541] #2018-10-31 20:00:00            50.4
nov14.df18<-DailyDeepFluxes$cumu_ch4[555] # 2018-11-14 19:00:00     50.4

offset.sf18<-DailyShalFluxes$cumu_ch4[393] #2018-06-05 20:00:00       21.9
octsf18<-DailyShalFluxes$cumu_ch4[510] # 2018-09-30 20:00:00          42.3
novsf18<-DailyShalFluxes$cumu_ch4[541] #2018-10-31 20:00:00     43
nov14.df18<-DailyShalFluxes$cumu_ch4[555] #2018-11-14 19:00:00   43.0

#may 23 - sept 30th cumulative areal methane emissions (g ch4 m-2):
(octdf18-offset.df18+(octsf18-offset.sf18))/2

#shoulder (oct 1 - 31)
(novdf18-octdf18+(novsf18-octsf18))/2

