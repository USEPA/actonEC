

# #the gap-filled driver dataset for ANN:
temp<-read.csv("data/annDataset_MDC.csv") #has rain
temp$datetime<-as.POSIXct(as.character(temp$datetime),
                          format="%Y-%m-%d %H:%M:%S")
temp$date<-as.Date(temp$datetime)

currentYear<-year(Sys.Date())

ggplot(filter(DailyFluxDatF, monthday<paste0(currentYear,"-10-01"), monthday>paste0(currentYear,"-05-01")), 
       aes(monthday, meanAirT))+
  annotate("rect", xmin=as.Date(paste0(currentYear, "-05-24")),
           xmax=as.Date(paste0(currentYear, "-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
         geom_line(color="red", linetype=2)+
  geom_line(data=filter(DailyFluxDatF, monthday>paste0(currentYear,"-05-01"),
                        monthday<paste0(currentYear,"-10-01")), 
            aes(monthday, meanSedT), alpha=0.8)+
  scale_x_date(labels=date_format("%b", tz="UTC"),
              breaks=date_breaks("1 month"))+
  #             #limits = c(as.Date(paste0(currentYear,"-01-01")), as.Date(paste0(currentYear,"-11-20"))))+
  ylab(expression(T~(deg~C)))+
  xlab("")+
  facet_grid(year~.)+
  geom_hline(yintercept=c(21.0, 24.8))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme_bw()


filter(DailyFluxDatF, date>"2017-05-01", date<"2017-10-01")%>%
  summarize(mean(meanAirT, na.rm=TRUE))

filter(DailyFluxDatF, date>"2018-05-01", date<"2018-10-01")%>%
  summarize(mean(meanAirT, na.rm=TRUE))

filter(DailyFluxDatF, date>"2017-05-01", date<"2017-10-01")%>%
  summarize(mean(meanSedT, na.rm=TRUE))

filter(DailyFluxDatF, date>"2018-05-01", date<"2018-10-01")%>%
  summarize(mean(meanSedT, na.rm=TRUE))

filter(DailyFluxDatF, date>"2017-05-01", date<"2017-10-01")%>%
  summarize(mean(meanLE, na.rm=TRUE))

filter(DailyFluxDatF, date>"2018-05-01", date<"2018-10-01")%>%
  summarize(mean(meanLE, na.rm=TRUE))

### Load chl data measured by Vanni group at Miami University


chl2017<-read.xls(paste(projectWD, "/data/Chl2017.xlsx", sep=""), sheet=3)
chl2017$RDate<-as.Date(chl2017$Date, format="%Y-%m-%d")

chl2018<-read.xls(paste(projectWD, "/data/Chl2018.xlsx", sep=""), sheet=3)
chl2018$RDate<-as.Date(chl2018$Date, format="%Y-%m-%d")

chlVanni<-rbind(chl2017, chl2018)
chlVanni$year<-year(chlVanni$RDate)
chlVanni$monthday<-format(chlVanni$RDate, format="%m-%d")
chlVanni$monthday<-as.Date(chlVanni$monthday, format="%m-%d")

currentYear<-year(Sys.time())

### Figure 11 b
ggplot(chlVanni, aes(monthday, Inflow_chl))+
  annotate("rect", xmin=as.Date(paste0(currentYear, "-05-24")),
           xmax=as.Date(paste0(currentYear,"-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line(color="green")+
  geom_point(color="green")+
  facet_grid(year~.)+
  ylab("Chlorophyll a (ug/L)")+
  xlab("")+
  theme_bw()

write_csv(select(chlVanni, Date, Inflow_chl), 
          path= paste0(projectWD, "/dataL2/Fig11b_data.csv"))
