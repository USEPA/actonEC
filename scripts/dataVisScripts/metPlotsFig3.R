
#### Need to make data frame of daily values for air T, sediment T, LE, H


DailyFluxDatF<-left_join(epDataFilled, select(vanni30min, RDateTime, dailyRain.vws), by=c("datetime" = "RDateTime"))%>%
  group_by(date)%>%
  dplyr::summarize(meanPAR=mean(FilledPAR*60*60*24/10^6), #convert from umolm-2s-1 to mol m-2 d-1
                   meanAirT = mean(FilledAirT-273.15), #convert from K to C
                   meanSedT = mean(FilledSedT), #in C
                   precip = max(dailyRain.vws), #in mm
                   meanLE = mean(FilledLE, na.rm=TRUE), #W m-2
                   meanH = mean(FilledH, na.rm=TRUE),
                   mean_ch4 = mean(ch4_flux, na.rm=TRUE)) 

DailyFluxDatF$year<-year(DailyFluxDatF$date)
DailyFluxDatF$monthday<- format(DailyFluxDatF$date, format="%m-%d")
DailyFluxDatF$monthday<-as.Date(DailyFluxDatF$monthday, format="%m-%d")

#########Figure 2A: Daily mean air and sediment T #######  
#ggplot(filter(DailyFluxDat, date>"2017-12-30", date<"2018-02-15"), aes(date, meanAirT))+
#ggplot(DailyFluxDat, aes(date, meanAirT))+
ggplot(DailyFluxDatF, aes(date, meanAirT))+
  annotate("rect", xmin=as.Date("2017-05-24"),
           xmax=as.Date("2017-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.Date("2018-05-24"),
           xmax=as.Date("2018-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line(color="red")+
  geom_line(data=DailyFluxDatF, aes(date, meanSedT), alpha=0.8)+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"),
               breaks=date_breaks("2 month"),
               limits = c(as.Date("2017-01-01"), as.Date("2018-11-20")))+
  ylab(expression(T~(deg~C)))+
  xlab("")+
  geom_hline(yintercept=0, linetype = 2, alpha=0.2)+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme_bw()
###export by copying to clipboard, height = 200, width = 900


########FIGURE 2B.a: Daily LE and H #######
ggplot(DailyFluxDatF, aes(date, meanLE))+
  annotate("rect", xmin=as.Date("2017-05-24"),
           xmax=as.Date("2017-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.Date("2018-05-24"),
           xmax=as.Date("2018-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line(alpha=0.5)+
  geom_line(data=DailyFluxDatF, aes(date, meanH, color="red"), alpha=0.5)+
  theme_bw()+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
               breaks=date_breaks("2 month"),
               limits = c(as.Date("2017-01-01"), as.Date("2018-11-20")))+
  ylab(expression(LE~and~H~(W~m^-2)))+
  xlab("")+
  theme(legend.position="none")

### Warm season stats:
# mean(subset(DailyFluxDatF, date>"2017-05-01", date<"2017-10-01")$meanAirT)
# sd(subset(DailyFluxDatF, date>"2017-05-01", date<"2017-10-01")$meanAirT)
# mean(subset(DailyFluxDatF, date>"2018-05-01", date<"2018-10-01")$meanAirT)
# sd(subset(DailyFluxDatF, date>"2018-05-01", date<"2018-10-01")$meanAirT)
# 
# mean(subset(DailyFluxDatF, date>"2017-05-01", date<"2017-10-01")$meanSedT)
# mean(subset(DailyFluxDatF, date>"2018-05-01", date<"2018-10-01")$meanSedT)
# 
# mean(subset(DailyFluxDatF, date>"2017-05-01", date<"2017-10-01")$meanLE)
# mean(subset(DailyFluxDatF, date>"2018-05-01", date<"2018-10-01")$meanLE)
# 
# mean(subset(DailyFluxDatF, date>"2017-05-01", date<"2017-10-01")$meanH)
# mean(subset(DailyFluxDatF, date>"2018-05-01", date<"2018-10-01")$meanH)



################FIGURE 2C: Precip##################

## DailyFluxDat precip has gaps:
# ggplot(DailyFluxDatF, aes(date, precip))+
#   geom_bar(stat="identity")+
#   theme_bw()+
#   scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
#                breaks=date_breaks("2 month"))+
#   ylab(expression(Precip~(mm)))+
#   xlab("")

campMet$date<-as.Date(campMet$RDateTime)

DailyCamp<-campMet%>%
  group_by(date)%>%
  dplyr::summarize(loggerT = mean(PTemp_C_Avg, na.rm=TRUE),
                   totRain = sum(Rain_mm_tot, na.rm=TRUE),
                   meanNR = mean(NR_Wm2_avg, na.rm=TRUE),
                   meanWS = mean(WS_ms_Avg, na.rm=TRUE))

# ggplot(DailyCamp, aes(date, totRain))+
#   geom_bar(stat="identity")+
#   theme_bw()+
#   scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
#                breaks=date_breaks("2 month"))+
#   ylab(expression(Precip~(mm)))+
#   xlab("")
campMet$datetime<-campMet$RDateTime
rain_test<-inner_join(select(vanni30min, RDateTime, rain30min), 
                      select(campMet, datetime, Rain_mm_tot),
                      by=c("RDateTime" = "datetime"))
# ggplot(rain_test, aes(rain30min, Rain_mm_tot))+
#   geom_point(alpha=0.3)
# ggplot(rain_test, aes(RDateTime, rain30min))+
#   geom_line(alpha=0.5)+
#   geom_line(data=rain_test, aes(RDateTime, Rain_mm_tot), color="red", alpha=0.5)

## load precip data from nearby National Atmospheric Deposition Program site
## http://nadp.slh.wisc.edu/data/sites/siteDetails.aspx?net=NTN&id=OH09

ntn<-read.csv(file = paste0(projectWD, "/data/NTN-OH09-d.csv"))%>%
  mutate(starttime = as.POSIXct(as.character(starttime), format="%m/%d/%Y %H:%M:%S", tz="UTC"),
         endtime = as.POSIXct(as.character(endtime), format="%m/%d/%Y %H:%M:%S", tz="UTC"),
         date = as.Date(starttime))%>%
  filter(starttime>"2017-01-01")

## Combine NADP, Vanni met station, campbell met station rain data:

DailyFluxDatR<-left_join(DailyFluxDatF, select(DailyCamp, date, totRain), 
                        by="date")
DailyFluxDatR2<-left_join(DailyFluxDatR, select(ntn, amount, date), by="date")
DailyFluxDatR2$FilledPrecip<-DailyFluxDatR2$precip

DailyFluxDatR2<-DailyFluxDatR2%>%
  mutate(FilledPrecip = replace(FilledPrecip, FilledPrecip==-Inf, NA))
for(i in 1:nrow(DailyFluxDatR2)){
  DailyFluxDatR2$FilledPrecip[i] <- ifelse(is.na(DailyFluxDatR2$FilledPrecip[i]),
                                         DailyFluxDatR2$totRain[i], 
                                         DailyFluxDatR2$precip[i])}
for(i in 1:nrow(DailyFluxDatR2)){
  DailyFluxDatR2$FilledPrecip[i]<-ifelse(is.na(DailyFluxDatR2$FilledPrecip[i]),
                                       DailyFluxDatR2$amount[i]*2.54*10, #from NADP
                                       DailyFluxDatR2$FilledPrecip[i])}

####FIGURE 2c: PRECIP
ggplot(DailyFluxDatR2, aes(date, FilledPrecip))+
  annotate("rect", xmin=as.Date("2017-05-24"),
           xmax=as.Date("2017-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.Date("2018-05-24"),
           xmax=as.Date("2018-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_bar(stat="identity")+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"),
               breaks=date_breaks("2 month"),
               limits = c(as.Date("2017-01-01"), as.Date("2018-11-20")))+
  theme_bw()+
  ylab(expression(Precip~(mm)))+
  xlab("")

## For SciHub:
write_csv(DailyFluxDatR2, path=paste0(projectWD, "/dataL2/Fig3abc.csv"))

#########Still missing a chunk in May, December#######

#####FIGURE 2E: Lake Level#########

# #the gap-filled driver dataset for ANN:
temp<-read.csv("data/annDataset_MDC.csv") #has rain
temp$datetime<-as.POSIXct(as.character(temp$datetime),
                          format="%Y-%m-%d %H:%M:%S")
temp$date<-as.Date(temp$datetime)
# 
# fluxDatFilled<-read.csv("output/fluxDataFilled6.01.csv")
# fluxDatFilled$datetime<-as.POSIXct(as.character(fluxDatFilled$datetime),
#                                    format="%Y-%m-%d %H:%M:%S")
# fluxDatFilled$date<-as.Date(fluxDatFilled$datetime)
# 

waterLevel<- select(temp, datetime, rain30min, dailyRain.vws, levelAdj.vws)
rm(temp)

## Figure 2e:
ggplot(waterLevel, aes(datetime, levelAdj.vws))+
  annotate("rect", xmin=as.POSIXct(as.Date("2017-05-24")),
           xmax=as.POSIXct(as.Date("2017-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.POSIXct(as.Date("2018-05-24")),
           xmax=as.POSIXct(as.Date("2018-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line()+
  theme_bw()+
  scale_x_datetime(labels=date_format("%b %Y", tz="UTC"), 
                   breaks=date_breaks("2 month"),
                   limits = c(as.POSIXct(as.Date("2017-01-01")),
                              as.POSIXct(as.Date("2018-11-20"))))+
  ylab(expression(Water~Level~(m)))+
  xlab("")

write_csv(select(waterLevel, datetime, levelAdj.vws),
          path = paste0(projectWD, "/dataL2/Fig3e_data.csv"))

#####FIGURE 2D: Stream Discharge/Inflow#########
##see edi.256.1.r

### Load, analyze, and plot Miami University's stream flow and chla monitoring data
## some of this data is available online via the Envirnmental Data Initiative (EDI) site. 
## at the time of manuscript prep, the 2018 data had not yet been uploaded

# load streamflow data from EDI:
infile3 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/256/1/7bc5a642e46c2b1abfed0c300da69d09") 
infile3 <-sub("^https","http",infile3)
# This creates a tibble named: dt3 
dt3 <-read_delim(infile3,delim=",",skip=1,
                 col_names=c("Site", "DateTime","DischargeHourly"), 
                 col_types=list( 
                   col_character(),  
                   col_datetime("%Y-%m-%d %H:%M"), 
                   col_number() ), 
                 na=c( " ",".","NA")  )

# Convert Missing Values to NA for individual vectors 
dt3$DischargeHourly <- ifelse((trimws(as.character(dt3$DischargeHourly))==trimws("NA")),NA,dt3$DischargeHourly)

# Here is the structure of the input data tibble: 
#glimpse(dt3) 
# And some statistical summaries of the data 
summary(dt3) 

# load streamflow data for 2018: 
dt3_2018<-read.xls(paste0(projectWD, "/data/discharge2018.xlsx"), sheet=2)

dt3_2017<-filter(dt3, DateTime>"2017-01-01")

dt3_2018.g<-gather(dt3_2018, key="Site", value = DischargeHourly, 
                   -Date.Time, -Day, -Month)
dt3_2018.g<-dt3_2018.g%>%
  mutate(Site = replace(Site, Site=="FourMileCreek", "Four Mile Stream"),
         Site = replace(Site, Site=="LittleFourMileCreek", "Little Four Mile Stream"),
         Site = replace(Site, Site=="MarshallsBranch", "Marshall's Branch Stream"),
         DateTime = as.POSIXct(Date.Time, format="%m/%d/%y %H:%M", tz="UTC"))

dt3.bind<-rbind(dt3, select(dt3_2018.g, Site, DateTime, DischargeHourly))

dt3.s<-spread(data=dt3.bind, key="Site", value="DischargeHourly")
#dt3.s<-spread(data=dt3_2018, key="Site", value="DischargeHourly")
#dt3.s<-filter(dt3.s, DateTime>"2017-01-01")
dt3.s$EntireWatershed<-rowSums(dt3.s[,2:4], na.rm=TRUE)


#####FIGURE 2D: Stream Discharge#########
ggplot(dt3.s, aes(DateTime, EntireWatershed))+
  annotate("rect", xmin=as.POSIXct(as.Date("2017-05-24")),
           xmax=as.POSIXct(as.Date("2017-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.POSIXct(as.Date("2018-05-24")),
           xmax=as.POSIXct(as.Date("2018-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_point(alpha=0.1, size=0.1)+
  scale_x_datetime(labels=date_format("%b %Y", tz="UTC"), 
                   breaks=date_breaks("2 month"),
                   limits = c(as.POSIXct(as.Date("2017-01-01")),
                              as.POSIXct(as.Date("2018-11-20"))))+
  ylim(0, 40)+
  theme_bw()+
  ylab(expression(Inflow~(m^3~s^-1)))

select(dt3.s, DateTime, EntireWatershed)%>%
  dplyr::filter(DateTime>"2017-01-01")%>%
  write_csv(path=paste0(projectWD, "/dataL2/Fig3d_data.csv"))

dt3.s<-filter(dt3.s, DateTime>"2017-01-01")
dt3.s$cumlInflow<-cumsum(dt3.s$EntireWatershed*60*60) #m3/s to integated m3 over hours
dt3.s$DateTime[3619] #2017-06-01
dt3.s$DateTime[2875] #2017-05-01

dt3.s$DateTime[12380] #2018-06-01
dt3.s$DateTime[11636]

dt3.s$cumlInflow[3619]-dt3.s$cumlInflow[2875]
dt3.s$cumlInflow[12380]-dt3.s$cumlInflow[11636]






