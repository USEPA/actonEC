
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


# ggplot(chl2018, aes(RDate, Inflow_chl))+
#   annotate("rect", xmin=as.Date("2018-05-24"),
#            xmax=as.Date("2018-06-04"),
#            ymin=-Inf, ymax=Inf, alpha=0.5)+
#   geom_line()+
#   
#   geom_point()
# 
# metaDataSonde$sample.date<-as.Date(metaDataSonde$Sample.Date)
# 
# #Bi-weekly site visit sonde chla measurements + vanni chla
# ggplot(filter(metaDataSonde, Site=="u14"), aes(sample.date, chl.a))+
#   annotate("rect", xmin=as.Date("2017-05-24"),
#            xmax=as.Date("2017-06-04"),
#            ymin=-Inf, ymax=Inf, alpha=0.5)+
#   annotate("rect", xmin=as.Date("2018-05-24"),
#            xmax=as.Date("2018-06-04"),
#            ymin=-Inf, ymax=Inf, alpha=0.5)+
#   annotate("rect", xmin=as.Date("2019-05-24"),
#            xmax=as.Date("2019-06-04"),
#            ymin=-Inf, ymax=Inf, alpha=0.5)+
#   geom_point(alpha=0.5)+
#   geom_point(data=chl2018, aes(RDate, Inflow_chl), color="green")+
#   geom_line(data=chl2018, aes(RDate, Inflow_chl), color="green")+
#   geom_point(data=chl2017, aes(RDate, Inflow_chl), color="green")+
#   geom_line(data=chl2017, aes(RDate, Inflow_chl), color="green")+
#   scale_x_date(labels=date_format("%b '%y", tz="UTC"),
#                breaks=date_breaks("2 month"),
#                limits = c(as.Date("2017-04-01"), as.Date("2019-07-20")))+
#   ylab("Chlorophyll a (ug/L)")+
#   xlab("")+
#   theme_light()
# 
# 
# 
# ggplot(filter(dt3.bind, DateTime>"2017-01-01", Site!="EntireWatershed"), 
#        aes(DateTime, DischargeHourly))+
#   annotate("rect", xmin=as.POSIXct(as.Date("2017-05-24")),
#            xmax=as.POSIXct(as.Date("2017-06-04")),
#            ymin=-Inf, ymax=Inf, alpha=0.5)+
#   annotate("rect", xmin=as.POSIXct(as.Date("2018-05-24")),
#            xmax=as.POSIXct(as.Date("2018-06-04")),
#            ymin=-Inf, ymax=Inf, alpha=0.5)+
#   geom_line(alpha=0.7, aes(color=Site))+
#   
#   ylim(0, 25)+
#   theme_bw()
# 
# 
# 
# metaDataSonde<-rename(metaDataSonde, DO.mgL = "DO.mg/L")
# metaDataSonde$year<-year(metaDataSonde$sample.date)
# metaDataSonde$monthday<-format(metaDataSonde$sample.date, format="%m-%d")
# metaDataSonde$monthday<-as.Date(metaDataSonde$monthday, format="%m-%d")
# 
# 
# 

