
gga<-read.csv("data/gga.csv")%>%
  mutate(RDate = as.Date(RDate),
         RDateTime = as.POSIXct(RDateTime, tz="UTC"))

epOutOrder<-read.csv("data/epOutOrder.csv")%>%
  mutate(date = as.Date(date),
         time = as.character(time))
epOutOrder$RDateTime<-as.POSIXct(epOutOrder$RDateTime,
                                 format="%Y-%m-%d %H:%M:%S", tz="UTC")

#MET AND WATER T: vanni30min, rbrTsub, rbrDaily, buoyT30min, buoyTdaily, U12sonde, campMet
## data from Mike Vanni's met station (Miami University of Ohio)
vanni30min<-read.csv("data/vanni30min.csv")
vanni30min$RDateTime<-as.POSIXct(vanni30min$RDateTime,
                                 format="%Y-%m-%d %H:%M:%S", tz="UTC")

## data from RBR thermistor string located at shallow site (U14):
rbrTsub<-read.csv("data/rbrTsub.csv")
rbrTsub$RDateTime<-as.POSIXct(rbrTsub$RDateTime,
                              format="%Y-%m-%d %H:%M:%S", tz="UTC")
rbrDaily<-read.csv("data/rbrDaily.csv")%>%
  mutate(RDateTime = as.Date(RDateTime),
         monthday = as.POSIXct(monthday, tz="UTC"),
         year = as.numeric(year))

## data from thermistors managed by Miami U at the deep site (U12):
buoyT30min<-read.csv("data/buoyT30min.csv")%>%
  mutate(RDateTime = as.POSIXct(RDateTime, tz="UTC"))
buoyTdaily<-read.csv("data/buoyTdaily.csv")%>%
  mutate(RDateTime = as.POSIXct(RDateTime, tz="UTC"),
         date = as.Date(date))

## sonde data from Miami U at the deep site (U12):
U12sonde<-read.csv("data/U12sonde.csv")%>%
  mutate(date = as.Date(date))

## data from our auxiliary met measurements, logged on a 
## campbell sci datalogger
campMet<-read.csv("data/campMet.csv")%>%
  mutate(RDateTime = as.POSIXct(RDateTime, tz="UTC"))

# ACTIVE FUNNEL TRAPS
hobo<-read.csv("data/hobo.csv")%>%
  mutate(date.time = as.POSIXct(date.time, tz="UTC"),
         date=as.Date(date.time),
         lake.name=as.character(lake.name),
         site=as.character(site))
