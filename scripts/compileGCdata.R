## This script loads and compiles in the gas composition data from the GC
## need info for emission calculations for the AFT's and dissolved gas

## read files for 2017 and 2018
gc.all.NonGrts.2017<-read.table("data/gcMasterFile2017updated2018-03-15.txt",
                                col.names=c("sample", "n2o.ppm", "co2.ppm", "ch4.ppm", "flag.n2o",
                                            "flag.co2", "flag.ch4", "o2.ar.percent", "n2.perc", "o2.chk",
                                            "flag.n2", "flag.o2.ar"),
                                skip=1) 
gc.all.NonGrts.2018<-read.table("data/gcMasterFile2018updated2018-12-21.txt",
                                col.names=c("sample", "ch4.ppm", "co2.ppm", "n2o.ppm", "flag.n2o",
                                            "flag.co2", "flag.ch4"),
                                skip=1)

gc.all.NonGrts<-rbind(select(gc.all.NonGrts.2017, sample, n2o.ppm, co2.ppm, ch4.ppm, flag.n2o, flag.co2, flag.ch4),
                      select(gc.all.NonGrts.2018, sample, n2o.ppm, co2.ppm, ch4.ppm, flag.n2o, flag.co2, flag.ch4))

gc.all.NonGrts$sample<-as.character(gc.all.NonGrts$sample)
gc.all.NonGrts$sampleNum<-gsub("([0-9]*).*","\\1",gc.all.NonGrts$sample) #extract numbers 
gc.all.NonGrts$sampleNum<-substring(gc.all.NonGrts$sample, 4)

## filter the GC master file, which includes all samples run in 2017 and 2018, for Acton samples
gc.Acton<-dplyr::filter(gc.all.NonGrts, grepl("ACT",sample)) #346 obs
#864 obs 12/21/2018

## Check for duplicates.
filter(gc.Acton, duplicated(sample,fromLast = TRUE) | duplicated(sample,fromLast = FALSE)) %>% 
  arrange(sample)

#load multiple tabs of Excel spreadsheed with dissolved gas sample information 
#trap tab
metaDataTrap2017<-read_excel("data/masterDataSheetEbullition2017.xlsx",
                             sheet="trapData",
                             skip=0,
                             na=c("NA", ""),
                             trim_ws=TRUE,
                             col_types=c("date", rep("text", 2), rep("numeric", 4), 
                                         "text", "numeric", "numeric", "text", "date",
                                         "date", "text"))
metaDataTrap2018<-read_excel("data/masterDataSheetEbullition2018.xlsx",
                             sheet="trapData",
                             skip=0,
                             na=c("NA", "", "na"),
                             trim_ws=TRUE,
                             col_types=c("date", rep("text", 2), rep("numeric", 4), 
                                         "text", "numeric", "numeric", "text", "date",
                                         "date", "text"))
metaDataTrap<-rbind(metaDataTrap2017, metaDataTrap2018)%>%
  mutate(site.visit.date = as.Date(site.visit.date),
         trap.deply.time = as.character(substr(trap.deply.time, 12,16)),
         site.visit.dateTime = as.POSIXct(paste(trap.deply.date,
                                     trap.deply.time, sep=" "),
                                     format="%Y-%m-%d %H:%M", tz="UTC"))
rm(metaDataTrap2017, metaDataTrap2018)
#read_excel automatically formats the date as a POSIXct object, but we want it as a date
# metaDataTrap$site.visit.date<-as.Date(metaDataTrap$site.visit.date)
# metaDataTrap$trap.deply.time<-as.character(substr(metaDataTrap$trap.deply.time, 12,16))
# metaDataTrap$site.visit.dateTime<-paste(metaDataTrap$trap.deply.date, 
#                                         metaDataTrap$trap.deply.time, sep=" ")
# metaDataTrap$site.visit.dateTime<-as.POSIXct(metaDataTrap$site.visit.dateTime,
#                                              format="%Y-%m-%d %H:%M",
#                                              tz="UTC")

## dissolved gas tab -- added Acton dock samples to this on 3/19/2018
metaDataDG2017<-read_excel("data/masterDataSheetEbullition2017.xlsx",
                           sheet="dissGasData",
                           skip=1,
                           na=c("NA", ""),
                           trim_ws=TRUE)
metaDataDG2018<-read_excel("data/masterDataSheetEbullition2018.xlsx",
                           sheet="dissGasData",
                           skip=1,
                           na=c("NA", ""),
                           trim_ws=TRUE)
metaDataDG<-rbind(metaDataDG2017, select(metaDataDG2018, -...13))
rm(metaDataDG2017,metaDataDG2018)
metaDataDG$sample.date<-as.Date(metaDataDG$sample.date)
#original excel spreadsheet that Sarah put together that includes dock DG samples, doesn't include trap samples 
#metaData<-read_excel("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/gasTransferVelocity/dissolvedGasSampleCodes.xlsx", 
#                                        trim_ws=TRUE, skip=0,sheet="original"
#                   na="NA")
metaDataDG$sample<-metaDataDG$exetainer.code
metaDataDGact<-filter(metaDataDG, lake=="acton")

#sonde tab -- pH needed for dissolved gas calc
metaDataSonde2017<-read_excel("data/masterDataSheetEbullition2017.xlsx",
                              sheet="sondeData",
                              skip=0,
                              na=c("NA", ""),
                              trim_ws=TRUE,
                              col_types=c("text", "text", "date", rep("numeric", 10), rep("text", 7)))
metaDataSonde2018<-read_excel("data/masterDataSheetEbullition2018.xlsx",
                              sheet="sondeData",
                              skip=0,
                              na=c("NA", ""),
                              trim_ws=TRUE,
                              col_types=c("text", "text", "date", rep("numeric", 10), rep("text", 7)))
# metaDataSonde2019<-read_excel("data/masterDataSheetEbullition2019.xlsx",
#                               sheet="sondeData",
#                               skip=0,
#                               na=c("NA", ""),
#                               trim_ws=TRUE,
#                               col_types=c("text", "text", "date", rep("numeric", 10), rep("text", 8)))


metaDataSonde<-rbind(metaDataSonde2017, metaDataSonde2018)
rm(metaDataSonde2017, metaDataSonde2018)
#metaDataSonde$Sample.Date<-as.Date(metaDataDG$Sample.Date)
metaDataSonde$Lake<-ifelse(metaDataSonde$Lake == "Acton", "acton", metaDataSonde$Lake)
metaDataSonde<-filter(metaDataSonde, Lake=="acton")
metaDataSonde$Site<-ifelse(metaDataSonde$Site == "u4", "u04", metaDataSonde$Site)
metaDataSonde$Site<-ifelse(metaDataSonde$Site == "u5", "u05", metaDataSonde$Site)
metaDataSonde$Site<-ifelse(metaDataSonde$Site == "u6", "u06", metaDataSonde$Site)
metaDataSonde$Site<-ifelse(metaDataSonde$Site == "u7", "u07", metaDataSonde$Site)
metaDataSonde$Site<-ifelse(metaDataSonde$Site == "u8", "u08", metaDataSonde$Site)
metaDataSonde$Site<-ifelse(metaDataSonde$Site == "u9", "u09", metaDataSonde$Site)
metaDataSonde$Site<-ifelse(metaDataSonde$Site == "u0", "u09", metaDataSonde$Site) #checked in the data sheet
metaDataSonde$Site<-ifelse(metaDataSonde$Site == "u1", "u01", metaDataSonde$Site)
metaDataSonde$Site<-ifelse(metaDataSonde$Site == "U12", "u12", metaDataSonde$Site)
metaDataSonde$Site<-ifelse(metaDataSonde$Site == "U14", "u14", metaDataSonde$Site)
# ggplot(filter(metaDataSonde, Sample.depth.m<0.5), aes(Site, pH))+
#   geom_point(alpha=0.8, aes(color=Sample.Date))

metaDataSonde$salinity<-(metaDataSonde$`sp.Cond.us/cm`/1000)^1.0878*0.4665 #in ppt (?)
metaDataSonde$Temp.K<-metaDataSonde$Temp.C+273.15

write.table(metaDataSonde,
            file="dataL2/metaDataSonde.csv",
            sep=",",
            row.names=FALSE)
metaDataTrapAct<-filter(metaDataTrap, lake=="acton")
#the trap sheet has exetainer codes in comma delimited lists of up to three per cell
#need to parse these before we join with the GC results
# PREPARE EXETAINER CODES----------------------
# Extract from eqAreaData
xtrCodes2017 <- select(metaDataTrapAct, site, site.visit.date, exetainer.code)%>%
  filter(site.visit.date<"2018-01-01")
xtrCodes2018 <- select(metaDataTrapAct, site, site.visit.date, exetainer.code)%>%
  filter(site.visit.date>"2018-01-01")  

# Split codes into separate fields
xtrCodes2017 <- tidyr::separate(xtrCodes2017, exetainer.code, into = c("tp.xtr.1", "tp.xtr.2", "tp.xtr.3"), sep = ", ")
xtrCodes2018 <- tidyr::separate(xtrCodes2018, exetainer.code, into = c("tp.xtr.1", "tp.xtr.2", "tp.xtr.3"), sep = ",")
xtrCodes<-rbind(xtrCodes2017, xtrCodes2018)
xtrCodes$site.visit.date<-as.character(xtrCodes$site.visit.date)
#was getting an error using the melt command, turns out xtrCodes wasn't a dataframe:
#https://stackoverflow.com/questions/16941111/r-cannot-melt-data-frame/35500964
#coerce to dataframe:
xtrCodes<-as.data.frame(xtrCodes)
# Melt  
xtrCodes.m <- reshape2::melt(xtrCodes, id.vars = c("site.visit.date", "site")) %>% # melt, converts exetainer code to factor
  mutate(value = (as.character(value))) %>%  # Must got from factor -->character, not to integer because these have the "ACT" prefix
  mutate(variable = as.character(variable)) %>% # Must got from factor -->character
  filter(!is.na(value))  # remove NAs

# Simplify variable names 
xtrCodes.m[grepl(pattern = ".1|.2|.3|", x = xtrCodes.m$variable), "variable"] <- 
  gsub(pattern = ".1|.2|.3|", replacement = "", x = xtrCodes.m[grepl(pattern = ".1|.2|.3|", x = xtrCodes.m$variable), "variable"])


# Check for duplicates.  Should be none.
filter(xtrCodes.m, duplicated(value,fromLast = TRUE) | duplicated(value,fromLast = FALSE)) %>% arrange(value)
# site.visit.date site variable     value
# 1      2017-06-26  u14   tp.xtr ACT170109
# 2      2017-06-26  u14   tp.xtr ACT170109
# 3      2017-06-26  u14   tp.xtr ACT170110
# 4      2017-06-26  u14   tp.xtr ACT170110
# 5      2017-06-26  u14   tp.xtr ACT170111
# 6      2017-06-26  u14   tp.xtr ACT170111



##End exetainer code parsing

#gc.Acton$sample has underscores in the sample names, e.g. "ACT18_269"
gc.Acton$sample<-sub("_", "", gc.Acton$sample)

actonDgJoin<-left_join(metaDataDGact, gc.Acton, by="sample") #2/22/18, 312 observations
#3/19/18, 328 observations
#12/20/18, 580 obs
#04/09/19, 580 obs
#xtrCodes is the melted info from metaDataTrapAct

actonTrapJoin<-merge(xtrCodes.m, gc.Acton, by.x="value", by.y="sample")#3/19/18, 72 obs; after fixing trap data sheet: 80 obs
#12/20/18: 118 obs
#04/09/19: 153 obs
#site.visit.date is a character from using it in melt, create Rdate and change to a date
actonTrapJoin$Rdate<-as.Date(actonTrapJoin$site.visit.date)
actonTrapJoin<-actonTrapJoin%>%
  mutate(year = year(Rdate),
         monthday = format(Rdate, format="%m-%d"))
actonTrapJoin$monthday<-as.Date(actonTrapJoin$monthday, format="%m-%d")
actonTrapJoin$SiteDesc<-ifelse(actonTrapJoin$site == "u12", "deep", "shallow")

#Time series plot of Trap Gas %CH4 at the deep and shallow site faceted between 2017 and 2018
ggplot(actonTrapJoin, aes(monthday, ch4.ppm/10^4))+ #ppm/percent conversion 
  geom_point(aes(color=SiteDesc), alpha=0.5)+
  scale_x_date(date_breaks = "1 month", date_labels="%b-%d")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  facet_grid(year~.)+
  labs(x = "Date", y = "Trap Gas %CH4")

#Sample ACT18307 has a CH4 concentration of 17.4 ppm, and N2O of 2.64 ppm. 
#Need to check log book -- may be a typo. Out of order -- more likely ACT18037. 

actonTrapJoin<-actonTrapJoin%>%
  mutate(ch4.ppm = replace(ch4.ppm, value=="ACT170165", NA), #note in the log book: sample contaminated with atmospheric air
         ch4.ppm = replace(ch4.ppm, value=="ACT170166", NA), #note in the log book: sample contaminated with atmospheric air
         ch4.ppm = replace(ch4.ppm, value=="ACT170167", NA)) #note in the log book: sample contaminated with atmospheric air

write.table(actonTrapJoin,
            file="dataL2/actonTrapJoin.csv",
            sep=",",
            row.names=FALSE)
write.table(actonDgJoin,
            file="dataL2/actonDgJoin.csv",
            sep=",",
            row.names=FALSE)

#take mean and sd of duplicate and triplicate samples
actonTrapAgg<-actonTrapJoin %>%
  group_by(Rdate, site) %>%
  dplyr::summarize(meanCH4 = mean(ch4.ppm),
                   meanCO2 = mean(co2.ppm),
                   meanN2O = mean(n2o.ppm),
                   sdCH4 = sd(ch4.ppm),
                   sdCO2 = sd(co2.ppm),
                   sdN2O = sd(n2o.ppm))
actonTrapAgg$SiteDesc<-ifelse(actonTrapAgg$site == "u12", "deep", "shallow")
actonTrapAgg<-actonTrapAgg%>%
  mutate(year = year(Rdate),
         monthday = format(Rdate, format="%m-%d"))
actonTrapAgg$monthday<-as.Date(actonTrapAgg$monthday, format="%m-%d")

## Same as time series above, but with mean and sd values
ggplot(actonTrapAgg, aes(monthday, meanCH4/10000))+
  geom_jitter(aes(color=SiteDesc))+
  geom_errorbar(aes(color=SiteDesc, ymin=((meanCH4-sdCH4)/10000), ymax =((meanCH4+sdCH4)/10000)))+
  scale_x_date(date_breaks = "1 month", date_labels="%b-%d")+
  facet_grid(year~.)+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(x = "Date", y = "Trap Gas %CH4")+
  ylim(0, 100)+
  theme_bw()

write.table(actonTrapAgg,
            file="dataL2/actonTrapAgg.csv",
            sep=",",
            row.names=FALSE)
# 
# 
# ggplot(filter(actonDgJoin, sample.type=="dg", sample.depth.m==0.1, ch4.ppm<6*10^5), aes(sample.date, ch4.ppm))+
#   geom_point(aes(color=site))
# 
# ggplot(filter(actonDgJoin, sample.type=="dg", ch4.ppm<6*10^5), aes(sample.date, ch4.ppm))+
#   geom_point(aes(color=site))+
#   facet_grid(sample.depth.m~.,
#              scales="free")
# 
# ggplot(filter(actonDgJoin, sample.type=="air" & site =="dock"), aes(sample.date, ch4.ppm))+
#   geom_point(aes(color=site))


