
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

### SI Figure 
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

GRTSF9<-grts_ts%>%
  mutate(ch4.t_gf = replace(ch4.t_gf, date<"2017-05-01", NA),
         ch4.t_gf = replace(ch4.t_gf, date>"2017-10-04" & date<"2018-05-01", NA),
         ch4.t_gf = replace(ch4.t_gf, date>"2018-10-01", NA),
         ch4.trate = ch4.t_gf,
         ch4.tPt = ch4.t_cumuPt,
         ch4.FE = ch4.trate.FE,
         Label = "d) GRTS Surveys",
         year = year(date),
         monthday = format(date, format="%m-%d %H:%M"))# %>%
GRTSF9$monthday<-as.Date(GRTSF9$monthday, format="%m-%d %H:%M")

rowNum<-which(grepl("2018-01-01", GRTSF9$date))
value17<-GRTSF9$ch4.t_cumu[rowNum]

for(i in 1:nrow(GRTSF9)){
  GRTSF9$ch4.t_cumu_yr[i]<-ifelse(GRTSF9$year[i]<2018,
                                  GRTSF9$ch4.t_cumu[i],
                                  GRTSF9$ch4.t_cumu[i] - value17)
  GRTSF9$ch4.tPt_yr[i]<-ifelse(GRTSF9$year[i]<2018,
                               GRTSF9$ch4.tPt[i],
                               GRTSF9$ch4.tPt[i] - value17)
  GRTSF9$ch4.tPt_err[i]<-GRTSF9$ch4.tPt_yr[i]*GRTSF9$ch4.FE[i]
  
}

GRTSF9<-GRTSF9%>%
  mutate(ch4.t_cumu_yr = replace(ch4.t_cumu_yr, monthday<"2020-05-01", NA),
         ch4.t_cumu_yr = replace(ch4.t_cumu_yr, monthday>"2020-10-04", NA),
         ch4.t_cumu_yr_L95 = ch4.tPt_yr-ch4.tPt_err,
         ch4.t_cumu_yr_U95 = ch4.tPt_yr+ch4.tPt_err
  )

DailyANNF9<-select(DailyANNFluxes, date, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)
ShalSiteF9<-select(DailyShalTfluxes, date, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)
DeepSiteF9<-select(DailyDeepTfluxes, date, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)
GRTSF9<-select(GRTSF9, date, Label, year, ch4.t_cumu, monthday, ch4.t_cumu_yr, ch4.t_cumu_yr_U95, ch4.t_cumu_yr_L95)

cumulativeFluxF9<-list()
cumulativeFluxF9[[1]]<-DailyANNF9
cumulativeFluxF9[[2]]<-ShalSiteF9
cumulativeFluxF9[[3]]<-DeepSiteF9
cumulativeFluxF9[[4]]<-GRTSF9

cmlDailyFluxF9<-do.call("rbind", cumulativeFluxF9)

write.csv(cmlDailyFluxF9, paste0(projectWD, "/dataL2/Fig9data.csv"),
          row.names =FALSE)

#Figure 9:
ggplot(filter(cmlDailyFluxF9, year>2016), aes(monthday, ch4.t_cumu_yr))+
  geom_line(aes(color=Label, linetype=Label), size=1)+
  geom_ribbon(data=filter(cmlDailyFluxF9, year>2016), 
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
  ylab(expression(Cumulative~CH[4]~Areal~Emissions~(g~CH[4]~m^-2)))+
  xlab("")+
  labs(fill = "Method")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank())

