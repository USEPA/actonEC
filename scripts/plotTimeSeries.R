####Plotting time series plots from continuous flux measurements, plus chamber fluxes######

######## CHAMBER DIFFUSIVE FLUXES ############
#do this first so that the total shallow and deep site daily fluxes (eb + dif)
#can be plotted in Figure 2: time series fluxes of CH4

chamData.d<-filter(chamData, siteID=="deep")%>%
  mutate(Rdate=as.Date(chmDeplyDtTm))
chamData.s<-filter(chamData, siteID=="shallow W")%>%
  mutate(Rdate=as.Date(chmDeplyDtTm)) #in 2019 we used a 2nd shallow site east of the original site, hence Shallow West and shallow east

## average instances of >1 chamber measurement on the same day:
## for the deep site:
chamData.d<-chamData.d%>%
  group_by(Rdate)%>%
  mutate(co2.drate.mg.h.best=mean(co2.drate.mg.h.best),
         ch4.drate.mg.h.best=mean(ch4.drate.mg.h.best),
         chmDeplyDtTm=min(chmDeplyDtTm))

## and the shallow site:
chamData.s<-chamData.s%>%
  group_by(Rdate)%>%
  mutate(co2.drate.mg.h.best=mean(co2.drate.mg.h.best),
         ch4.drate.mg.h.best=mean(ch4.drate.mg.h.best),
         chmDeplyDtTm=min(chmDeplyDtTm))


## want to join chamber data with funnel data,
## gap-fill chamber data using linear interpolation
## then calculate a "total" emission for the U12 and U14 sites,
## eb+diff emissions
## want to do this with daily average eb data for time series figure
## 2-hr eb data for cumulative emissions figure -- why not daily?

## Assume uncertainty for chamber measurements is negligable compared to ebullition

DailyShalFluxes$Label<-"b) Shallow Site"
sum(is.na(DailyShalFluxes$meanCH4Flux)) #262

DailyDeepFluxes$Label<-"c) Deep Site"
DailyDeepFluxes$Rdate<-as.Date(DailyDeepFluxes$date)

DailyDeepTfluxes<-left_join(DailyDeepFluxes, 
                            select(chamData.d, ch4.drate.mg.h.best, Rdate), by="Rdate")
DailyDeepTfluxes<-DailyDeepTfluxes %>%
  mutate(ch4.drate.interp = na.approx(ch4.drate.mg.h.best, rule=2),
         ch4.drate.interp = replace(ch4.drate.interp, Rdate>"2017-12-11" & Rdate<"2018-06-07", 0),
         meanCH4flux.interp = na.approx(meanCH4Flux, rule=2),
         ch4.trate = ch4.drate.interp+meanCH4flux.interp,
         ch4.trate = replace(ch4.trate, Rdate>"2018-01-01" & Rdate <"2018-05-01", 0),
         ch4.t_cumu = cumsum(ch4.trate)*24/1000,
         ch4.trate = replace(ch4.trate, Rdate>"2018-01-01" & Rdate <"2018-05-01", NA),
         #ch4.dailyErr = meanCH4Flux*dailyFracErr,
         year = year(Rdate),
         monthday = format(Rdate, format="%m-%d %H:%M"))# %>%
DailyDeepTfluxes$monthday<-as.Date(DailyDeepTfluxes$monthday, format="%m-%d %H:%M")


####Shallow Site
DailyShalFluxes$Rdate<-as.Date(DailyShalFluxes$date)

DailyShalTfluxes<-left_join(DailyShalFluxes, 
                            select(chamData.s, ch4.drate.mg.h.best, Rdate), by="Rdate")
DailyShalTfluxes<-DailyShalTfluxes %>%
  mutate(ch4.drate.interp = na.approx(ch4.drate.mg.h.best, rule=2),
         ch4.drate.interp = replace(ch4.drate.interp, Rdate>"2017-12-11" & Rdate<"2018-06-07", 0),
         meanCH4flux.interp=na.approx(meanCH4Flux, rule=2), 
         meanCH4flux.interp = replace(meanCH4flux.interp, Rdate>"2017-10-03" & Rdate<"2018-06-06", 0),
         ch4.trate = ch4.drate.interp+meanCH4flux.interp,
         ch4.t_cumu = cumsum(ch4.trate)*24/1000,
         ch4.trate = replace(ch4.trate, Rdate>"2018-01-01" & Rdate<"2018-05-01", NA),
         ch4.ecum = cumsum(meanCH4flux.interp)*24/1000, 
         #ch4.dailyErr = meanCH4Flux*dailyFracErr,
         year = year(Rdate),
         monthday = format(Rdate, format="%m-%d %H:%M"))# %>%
DailyShalTfluxes$monthday<-as.Date(DailyShalTfluxes$monthday, format="%m-%d %H:%M")


#######ANN filled EP Info#########################################

epDataFilled<-read.csv("dataL2/gapFilledEC_results.csv") # results from ANN gapfilling
epDataFilled$datetime<-as.POSIXct(epDataFilled$datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")

epDataFilled<-epDataFilled%>%
  mutate(date=as.Date(epDataFilled$datetime),
         year = year(datetime),
         monthday = format(datetime, format="%m-%d %H:%M"),
         iceInd = 0,
         iceInd = replace(iceInd, datetime>="2017-12-27" & datetime<="2018-01-10", 1),
         iceInd = replace(iceInd, datetime>="2018-01-13" & datetime<="2018-01-21", 1),
         iceInd = replace(iceInd, datetime>="2018-02-05" & datetime<="2018-02-15", 1))# %>%
epDataFilled$monthday<-as.Date(epDataFilled$monthday, format="%m-%d %H:%M")

#For time series figure, Fig 2:
epDataFilled$ch4_flux.f2<-epDataFilled$ch4_flux*60*60*16/1000 #convert to mg/m2/hr from umol/m2/s (EC units)
epDataFilled$ch4_flux_filled.f2<-epDataFilled$ch4_filled*60*60*16/1000
epDataFilled$Label<-"a) Eddy Covaraince"

epDataFilledF2<-select(epDataFilled, datetime, Label, ch4_flux.f2, ch4_flux_filled.f2)

# epDataFilled$seasonInd<-if_else(epDataFilled$monthday<"2019-05-01" | epDataFilled$monthday>"2019-10-01",
#                                 1,
#                                 0)

DailyANNFluxes<-epDataFilled %>%
  group_by(date) %>%
  dplyr::summarize(ch4.trate = (mean(ch4_filled, na.rm=TRUE))/1000*16*60*60,
                   ch4.trateU95 = (mean(ch4_U95, na.rm=TRUE))/1000*16*60*60,
                   ch4.trateL95 = (mean(ch4_L95, na.rm=TRUE))/1000*16*60*60)
#year = year(datetime),

#sdCH4Flux = (sd(ch4_flux, na.rm=TRUE)/1000*16*60*60),
#meanANNch4Flux = (mean(ch4_preds6.01, na.rm=TRUE)/1000*16*60*60))
DailyANNFluxes<-DailyANNFluxes%>%
  mutate(date=as.POSIXct(DailyANNFluxes$date, tz="etc/GMT+5"),
         year = year(date),
         ch4.t_cumu = cumsum(ch4.trate)*24/1000,
         ch4.t_cumuU95 = cumsum(ch4.trateU95)*24/1000,
         ch4.t_cumuL95 = cumsum(ch4.trateL95)*24/1000,
         monthday = format(date, format="%m-%d %H:%M"))# %>%
DailyANNFluxes$monthday<-as.Date(DailyANNFluxes$monthday, format="%m-%d %H:%M")


##########################
DailyANNFluxes$Label<-"a) Eddy Covaraince" #label will be used for faceting


##### AFT info for deep and shallow sites

df12.gc$ch4_flux.f2<-df12.gc$ebCh4mgM2h #hourly data for the background points
df12.gc$ch4_flux_filled.f2<-df12.gc$ebCh4mgM2h
df12.gc$datetime<-df12.gc$date.timeHH
df12.gc$Label<-"c) Deep Site"

deepAFTf2<-select(df12.gc, datetime, Label, ch4_flux.f2, ch4_flux_filled.f2)

df14.gc$ch4_flux.f2<-df14.gc$ebCh4mgM2h   #hourly scale data for the background points
df14.gc$ch4_flux_filled.f2<-df14.gc$ebCh4mgM2h
df14.gc$datetime<-df14.gc$date.timeHH
df14.gc$Label<-"b) Shallow Site"

shalAFTf2<-select(df14.gc, datetime, Label, ch4_flux.f2, ch4_flux_filled.f2)


########GRTS info#############
#adding GRTS data -- need to have run GRTS code
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

grts_ts<-left_join(select(DailyANNF2, date), 
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

GRTSf2<-meanVariance.c%>%
  mutate(datetime = date,
         Label = "d) Lake-Wide Surveys",
         ch4_flux.f2 = ch4.trate.mg.h_Estimate,
         ch4_flux_filled.f2 = ch4.trate.mg.h_StdError,
         ch4.trate = ch4.trate.mg.h_Estimate,
         year = year(date))
#########################
DailyANNF2<-select(DailyANNFluxes, date, ch4.trate, Label, year)
ShalSiteF2<-select(DailyShalTfluxes, date, ch4.trate, Label, year)
DeepSiteF2<-select(DailyDeepTfluxes, date, ch4.trate, Label, year)


#format: date as POSIXct, ch4.trate as num, label as chr
dailyF2list<-list()
dailyF2list[[1]]<-as.data.frame(DailyANNF2)
dailyF2list[[2]]<-as.data.frame(ShalSiteF2)
dailyF2list[[3]]<-as.data.frame(DeepSiteF2)
dailyF2list[[4]]<-as.data.frame(select(GRTSf2, date, ch4.trate, Label, year))

dailyFluxF2<-do.call("rbind", dailyF2list)

#format: datetime as POSIXct, ch4_flux.f3 and ch4_flux_filled.f3 as num, Label as chr
F2list<-list()
F2list[[1]]<-epDataFilledF2
F2list[[2]]<-deepAFTf2
F2list[[3]]<-shalAFTf2
F2list[[4]]<-select(GRTSf2, datetime, Label, ch4_flux.f2, ch4_flux_filled.f2)

flux30min_tsF2<-do.call("rbind", F2list)

########################
#Figure 2: time series of EC, shallow and deep AFT pseudo-continuous flux measurements
Figure2<-ggplot(flux30min_tsF2, aes(datetime, ch4_flux_filled.f2))+
  annotate("rect", xmin=as.POSIXct(as.Date("2017-05-24")), #spring burst rectangles
            xmax=as.POSIXct(as.Date("2017-06-04")),
            ymin=-Inf, ymax=Inf, alpha=0.5)+
   annotate("rect", xmin=as.POSIXct(as.Date("2018-05-24")),
            xmax=as.POSIXct(as.Date("2018-06-04")),
            ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_point(alpha=0.1, size=1, color="gray")+ #gray for the gapfilled data
  geom_point(data=flux30min_tsF2, aes(datetime, ch4_flux.f2), size=1, alpha=0.2)+ #black points for the 30-min EC data
  geom_line(data=dailyFluxF2, aes(date, ch4.trate), color="red", size=1)+
  geom_errorbar(data = filter(flux30min_tsF2, Label == "d) Lake-Wide Surveys"),
                aes(ymax = ch4_flux.f2+ch4_flux_filled.f2*1.96,
                    ymin = ch4_flux.f2-ch4_flux_filled.f2*1.96))+
  facet_grid(Label~.)+
  ylab(expression(CH[4]~Flux~(mg~m^-2~hr^-1)))+
  xlab("")+
  scale_x_datetime(date_breaks="3 months", date_minor_breaks = "1 month", 
                   labels=date_format("%b %Y"))+
  ylim(-20, 100)+
  theme_bw()

Figure2

