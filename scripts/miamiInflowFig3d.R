
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
glimpse(dt3) 
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
dt3.s<-spread(data=dt3_2018, key="Site", value="DischargeHourly")
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
  filter(DateTime>"2017-01-01")%>%
  write_csv(path=paste0(projectWD, "/dataL2/Fig3d_data.csv"))

dt3.s$cumlInflow<-cumsum(dt3.s$EntireWatershed*60*60) #m3/s to integated m3 over hours
dt3.s$DateTime[3619] #2017-06-01
dt3.s$DateTime[2875] #2017-05-01

dt3.s$DateTime[12380] #2018-06-01
dt3.s$DateTime[11636]

dt3.s$cumlInflow[3619]-dt3.s$cumlInflow[2875]
dt3.s$cumlInflow[12380]-dt3.s$cumlInflow[11636]


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


ggplot(chl2018, aes(RDate, Inflow_chl))+
  annotate("rect", xmin=as.Date("2018-05-24"),
           xmax=as.Date("2018-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line()+
  
  geom_point()

metaDataSonde$sample.date<-as.Date(metaDataSonde$Sample.Date)

#Bi-weekly site visit sonde chla measurements + vanni chla
ggplot(filter(metaDataSonde, Site=="u14"), aes(sample.date, chl.a))+
  annotate("rect", xmin=as.Date("2017-05-24"),
           xmax=as.Date("2017-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.Date("2018-05-24"),
           xmax=as.Date("2018-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.Date("2019-05-24"),
           xmax=as.Date("2019-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_point(alpha=0.5)+
  geom_point(data=chl2018, aes(RDate, Inflow_chl), color="green")+
  geom_line(data=chl2018, aes(RDate, Inflow_chl), color="green")+
  geom_point(data=chl2017, aes(RDate, Inflow_chl), color="green")+
  geom_line(data=chl2017, aes(RDate, Inflow_chl), color="green")+
  scale_x_date(labels=date_format("%b '%y", tz="UTC"),
               breaks=date_breaks("2 month"),
               limits = c(as.Date("2017-04-01"), as.Date("2019-07-20")))+
  ylab("Chlorophyll a (ug/L)")+
  xlab("")+
  theme_light()



ggplot(filter(dt3.bind, DateTime>"2017-01-01", Site!="EntireWatershed"), 
       aes(DateTime, DischargeHourly))+
  annotate("rect", xmin=as.POSIXct(as.Date("2017-05-24")),
           xmax=as.POSIXct(as.Date("2017-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.POSIXct(as.Date("2018-05-24")),
           xmax=as.POSIXct(as.Date("2018-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line(alpha=0.7, aes(color=Site))+
  
  ylim(0, 25)+
  theme_bw()



metaDataSonde<-rename(metaDataSonde, DO.mgL = "DO.mg/L")
metaDataSonde$year<-year(metaDataSonde$sample.date)
metaDataSonde$monthday<-format(metaDataSonde$sample.date, format="%m-%d")
metaDataSonde$monthday<-as.Date(metaDataSonde$monthday, format="%m-%d")



#####Creating weighted total inflow nutrient concentration from dt3 and dt4

#1. Spread dt3 and dt4 by site
#2. Aggregate both to daily timesteps:
    # 2.1 Integrate dt3 to total volume of inflow per day
    # 2.2 Average dt4 to daily averages. 
          #Mostly there seems to be one nutrient measurement per day,
          #but sometimes there are two, and some days have 0. Interpolate over missing days
#3. Join the two dataframes
#4. Save this dataframe
#5. Use it in the diunralAnalysis.R script to plot the diurnal pdf

plotGaps <- function(d, resp){
  
  # d <- fluxDat; resp = "RBRmeanT_1.6"
  dayGaps <- ddply(d, .(date), function(x){
    # x <- subset(fluxDat, date == "2017-02-01")
    return(data.frame("Gaps"=sum(is.na(x[,resp]))))
  })
  p <- ggplot(dayGaps, aes_string(x = "date", y = "Gaps")) + geom_bar(stat = "identity") +
    xlab("Date") + ylab("Number of 30-min Gaps per Day") + 
    ggtitle(paste("Daily Gap Plot for: ", resp, sep=""))
  return(p)
}

# spreadEDI<-function(edi.df, val) {
#   edi.df.s<-spread(edi.df, key='Site', value='val')
#   edi.df.s$FourMileStream<-edi.df.s$`Four Mile Stream`
#   edi.df.s$LittleFourMile<-edi.df.s$`Little Four Mile Stream`
#   edi.df.s$MarshallBranch<-edi.df.s$`Marshall's Branch Stream`
#   
#   edi.df.s<-select(dt3.s, DateTime, FourMileStream, LittleFourMile, MarshallBranch)
# }
# 
# spreadEDI(dt3, "DischargeHourly")

#dt3 by site:
dt3.s<-spread(dt3, key='Site', value='DischargeHourly')
dt3.s$FourMileStream<-dt3.s$`Four Mile Stream`
dt3.s$LittleFourMile<-dt3.s$`Little Four Mile Stream`
dt3.s$MarshallBranch<-dt3.s$`Marshall's Branch Stream`

dt3.s<-select(dt3.s, DateTime, FourMileStream, LittleFourMile, MarshallBranch)

#dt4 by site and nutrient:
dt4NH3<-select(dt4, Site, DateTime, Ammonia)
dt4NH3.s<-spread(dt4NH3, key='Site', value='Ammonia')
dt4NH3.s$FourMileStream<-dt4NH3.s$`Four Mile Stream`
dt4NH3.s$LittleFourMile<-dt4NH3.s$`Little Four Mile Stream`
dt4NH3.s$MarshallBranch<-dt4NH3.s$`Marshall's Branch Stream`
dt4NH3.s<-select(dt4NH3.s, DateTime, FourMileStream, LittleFourMile, MarshallBranch)
dt4NH3.s$var<-"NH3"
dt4NH3.s<-dt4NH3.s%>% mutate(FourMileStream = na.approx(FourMileStream, rule=2),
                             LittleFourMile = na.approx(LittleFourMile, rule=2),
                             MarshallBranch = na.approx(MarshallBranch, rule=2))

dt4NO3<-select(dt4, Site, DateTime, Nitrate)
dt4NO3.s<-spread(dt4NO3, key='Site', value='Nitrate')
dt4NO3.s$FourMileStream<-dt4NO3.s$`Four Mile Stream`
dt4NO3.s$LittleFourMile<-dt4NO3.s$`Little Four Mile Stream`
dt4NO3.s$MarshallBranch<-dt4NO3.s$`Marshall's Branch Stream`
dt4NO3.s<-select(dt4NO3.s, DateTime, FourMileStream, LittleFourMile, MarshallBranch)
dt4NO3.s$var<-"NO3"
dt4NO3.s<-dt4NO3.s%>% mutate(FourMileStream = na.approx(FourMileStream, rule=2),
                             LittleFourMile = na.approx(LittleFourMile, rule=2),
                             MarshallBranch = na.approx(MarshallBranch, rule=2))

dt4SP<-select(dt4, Site, DateTime, SolubleReactivePhosphorus)
dt4SP.s<-spread(dt4SP, key='Site', value='SolubleReactivePhosphorus')
dt4SP.s$FourMileStream<-dt4SP.s$`Four Mile Stream`
dt4SP.s$LittleFourMile<-dt4SP.s$`Little Four Mile Stream`
dt4SP.s$MarshallBranch<-dt4SP.s$`Marshall's Branch Stream`
dt4SP.s<-select(dt4SP.s, DateTime, FourMileStream, LittleFourMile, MarshallBranch)
dt4SP.s$var<-"SolP"
dt4SP.s<-dt4SP.s%>% mutate(FourMileStream = na.approx(FourMileStream, rule=2),
                             LittleFourMile = na.approx(LittleFourMile, rule=2),
                             MarshallBranch = na.approx(MarshallBranch, rule=2))

dt4SS<-select(dt4, Site, DateTime, SuspendedSolids)
dt4SS.s<-spread(dt4SS, key='Site', value='SuspendedSolids')
dt4SS.s$FourMileStream<-dt4SS.s$`Four Mile Stream`
dt4SS.s$LittleFourMile<-dt4SS.s$`Little Four Mile Stream`
dt4SS.s$MarshallBranch<-dt4SS.s$`Marshall's Branch Stream`
dt4SS.s<-select(dt4SS.s, DateTime, FourMileStream, LittleFourMile, MarshallBranch)
dt4SS.s$var<-"SuspSol"
dt4SS.s<-dt4SS.s%>% mutate(FourMileStream = na.approx(FourMileStream, rule=2),
                             LittleFourMile = na.approx(LittleFourMile, rule=2),
                             MarshallBranch = na.approx(MarshallBranch, rule=2))

dt4List<-list()
dt4List[[1]]<-dt4NH3.s
dt4List[[2]]<-dt4NO3.s  
dt4List[[3]]<-dt4SP.s  
dt4List[[4]]<-dt4SS.s

nutrients<-do.call("rbind", dt4List)

nutrientsSub<-filter(nutrients, DateTime>"2017-01-01")

ggplot(filter(dt3.s, DateTime>"2017-10-01"), aes(DateTime, FourMileStream))+
  geom_point(alpha=0.3)

####2: integrating flows
dt3.s2017<-filter(dt3.s, DateTime>"2017-01-01")

dt3.s2017<-dt3.s2017%>%
  mutate(FourMileStream.m3h = FourMileStream*60*60, #converting from m3/s to m3/hr
         LittleFourMile.m3h = LittleFourMile*60*60,
         MarshallBranch.m3h = MarshallBranch*60*60)
dt3.s2017$date<-as.Date(dt3.s2017$DateTime)
DailyFlow<-dt3.s2017%>%
  group_by(date) %>%
  dplyr::summarize(totFlowFM = (sum(FourMileStream.m3h, na.rm=TRUE)),
                   totFlowLFM = (sum(LittleFourMile.m3h, na.rm=TRUE)),
                   #LFM.obs = n(LittleFourMile.m3h, na.rm=TRUE),
                   totFlowMB = (sum(MarshallBranch.m3h, na.rm=TRUE)),
                   obs = n())
                   #MB.obs = n(MarshallBranch.m3h, na.rm=TRUE))

##### want daily nutrient values
nutrientsSub$date<-as.Date(nutrientsSub$DateTime)
DailyNutrients<-nutrientsSub%>%
  group_by(date, var) %>%
  dplyr::summarize(meanFM = mean(FourMileStream, na.rm=TRUE), #units of ug/L
                   meanLFM = mean(LittleFourMile, na.rm=TRUE),
                   meanMB = mean(MarshallBranch, na.rm=TRUE))
  
ggplot(DailyNutrients, aes(date, meanFM))+
  geom_point(alpha=0.5)+
  facet_grid(var~.,
             scales="free")
ggplot(DailyNutrients, aes(date, meanLFM))+
  geom_point(alpha=0.5)+
  facet_grid(var~.,
             scales="free")
ggplot(filter(DailyNutrients, date>"2017-01-01", date<"2017-12-01"), aes(date, meanMB))+
  geom_point(alpha=0.5)+
  facet_grid(var~.,
             scales="free")
DailyNutrients$meanMB<-ifelse(DailyNutrients$date>"2017-10-27",
                              NA,
                              DailyNutrients$meanMB)

DailyMassDelivery<-left_join(DailyNutrients, DailyFlow, by="date")

DailyMassDelivery<-DailyMassDelivery%>%
  mutate(massFM = meanFM*totFlowFM*1000/10^6, #1000 to convert from m3 to L, 10^6 for ug to g; units of g
         massLFM = meanLFM*totFlowLFM*1000/10^6,
         massMB = meanMB*totFlowMB*1000/10^6) 

DailyMassDelivery[,10:12] #check that these columns are massFM, massLFM, and massMB
DailyMassDelivery$nutrientSums<-apply(DailyMassDelivery[,10:12], 1, sum, na.rm=TRUE) #units of g

DailyMassDelivery[,6:8] #checfk that these are totFlow...FM, LFM, and MB
DailyMassDelivery$sumTotFlow<-apply(DailyMassDelivery[,6:8], 1, sum, na.rm=TRUE) #units of m3

# ggplot(DailyMassDelivery, aes(date, nutrientSums))+
#   geom_point(alpha=0.5)+
#   facet_grid(var~.,
#              scales="free")

DailyMassDelivery<-DailyMassDelivery%>%
  mutate(inletNutrients = nutrientSums/sumTotFlow*10^6/1000) #converting back to ug/L

ggplot(DailyMassDelivery, aes(date, inletNutrients))+
  geom_point(alpha=0.5)+
  facet_grid(var~.,
             scales="free")

ggplot(filter(dt2, Date>"2015-01-01"), aes(Date, Chlorophyll_a))+
  geom_point()
ggplot(filter(dt2, Date>"2014-01-01"), aes(Date, ParticulateC))+
  geom_line()+
  geom_point(data=filter(dt2, Date>"2014-01-01"), aes(Date, Chlorophyll_a*50))

ggplot(dt2, aes(ParticulateC, Chlorophyll_a))+
  geom_point()

ggplot(filter(dt2, Date>"2005-01-01"), aes(ParticulateC, SuspendedSolids))+
  geom_point()


tail(dt2)

