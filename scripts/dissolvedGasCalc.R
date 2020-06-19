##run masterLibrary.R to initialize the def.calc.sdg.R script
##run compileGcDataNonGrts.R to load GC data -- update masterGCFile name
##need airCO2, airCH4, and airN2O, and want these to be the average of the three air
##samples taken at each site
##for each unique combination of site and sample.date, average

airMeans<-filter(actonDgJoin, sample.type=="air") %>%
  group_by(site, sample.date) %>%
  dplyr::summarize(meanN2O.ppm = mean(n2o.ppm, na.rm=TRUE),
                   sdN2O.ppm = sd(n2o.ppm, na.rm=TRUE),
                   meanCO2.ppm= mean(co2.ppm, na.rm=TRUE),
                   sdCO2.ppm = sd(co2.ppm, na.rm=TRUE),
                   meanCH4.ppm = mean(ch4.ppm, na.rm=TRUE),
                   sdCH4.ppm = sd(ch4.ppm, na.rm=TRUE))


#taking the mean of all "NA"s with na.rm = TRUE returns NaN. Turn those into NAs here
#still have the question of why there are periods with all NAs
airMeans$meanN2O.ppm<-replace(airMeans$meanN2O.ppm, is.nan(airMeans$meanN2O.ppm), NA)
airMeans$meanCO2.ppm<-replace(airMeans$meanCO2.ppm, is.nan(airMeans$meanCO2.ppm), NA)
airMeans$meanCH4.ppm<-replace(airMeans$meanCH4.ppm, is.nan(airMeans$meanCH4.ppm), NA)
airMeans$sdN2O.ppm<-replace(airMeans$sdN2O.ppm, is.nan(airMeans$sdN2O.ppm), NA)
airMeans$sdCO2.ppm<-replace(airMeans$sdCO2.ppm, is.nan(airMeans$sdCO2.ppm), NA)
airMeans$sdCH4.ppm<-replace(airMeans$sdCH4.ppm, is.nan(airMeans$sdCH4.ppm), NA)


# ggplot(filter(airMeans, site == "dock"), aes(sample.date, meanCH4.ppm))+
#   geom_point()+
#   geom_errorbar(aes(ymin=meanCH4.ppm-sdCH4.ppm, ymax=meanCH4.ppm+sdCH4.ppm))

###Create inputFile for def.calc.sdg from 
###actonDgJoin df created by "compileGcDataNonGrts.R":

#' @param inputFile Name of the data fram containing the information needed to calculate the dissolved gas concentrations. If the headers are named: "gasVolume", "waterVolume", "barometricPressure", "waterTemp", "concentrationCO2Gas", "concentrationCO2Air", "concentrationCH4Gas", "concentrationCH4Air", "concentrationN2OGas", "concentrationN2OAir", respectively, no other inputs are required. Otherwise, the names of the columns need to be input for the function to work.
#' @param volGas Volume of air equilibrated with water [mL]
#' @param volH2O Volume of water equilibrated with air [mL]
#' @param baro Barometric pressure at the time of equilibration [kPa]
#' @param waterTemp Temperature of the waterbody when sampled [celsius]
#' @param headspaceTemp Temperature of the water sample during the headspace equilibration [celsius]
#' @param eqCO2 Concentration of carbon dioxide in the equilibrated gas [ppmv]
#' @param airCO2 Concentration of carbon dioxide in atmosphere [ppmv]
#' @param sourceCO2 Concentration of carbon dioxide in headspace source gas [ppmv]
#' @param eqCH4 Concentration of methane in the equilibrated gas [ppmv]
#' @param airCH4 Concentration of methane in atmosphere [ppmv]
#' @param sourceCH4 Concentration methane in headspace source gas [ppmv]
#' @param eqN2O Concentration of nitrous oxide in the equilibrated gas [ppmv]
#' @param airN2O Concentration of nitrous oxide in atmosphere [ppmv]
#' @param sourceN2O Concentration of nitrous oxide in headspace source gas [ppmv]


actonDG<-filter(actonDgJoin, sample.type=="dg")
#join to populate the dissolved gas df with ambient gas concentrations for each 
#unique sampling date and site
actonDGair<-left_join(actonDG, airMeans, by=c("sample.date", "site"))

##Gap-filling barometric pressure: 

actonDGair$BPfilled<-actonDGair$bp.mm.hg*101.325/760 #convert from mmHg to kPa
actonDGair$BPfilled<-replace(actonDGair$BPfilled, is.na(actonDGair$BPfilled), 101.325)


#duplicate columns to have the right names to be input into GLEON code
actonDGair<- mutate(actonDGair,
                    gasVolume = headspace.volume.ml,
                    waterVolume = water.volume.ml,
                    barometricPressure = BPfilled,
                    waterTemp = as.numeric(headspace.equil.temp.c),
                    headspaceTemp = as.numeric(headspace.equil.temp.c),
                    concentrationCO2Gas = co2.ppm,
                    concentrationCO2Air = meanCO2.ppm,
                    concentrationCO2Source = 0,    #need to go back and change for any non-He samples
                    concentrationCH4Gas = ch4.ppm,
                    concentrationCH4Air = meanCH4.ppm,
                    concentrationCH4Source = 0,    #need to go back and change for any non-He samples
                    concentrationN2OGas = n2o.ppm,
                    concentrationN2OAir = meanN2O.ppm,
                    concentrationN2OSource = 0)


## Headspace source gas is usually helium, except for:
### 8/15/17 at the dock
### 

actonDGair$concentrationCH4Source <- with(actonDGair, 
                                          ifelse(site=="dock"&sample.date=="2017-08-15",
                                                 concentrationCH4Air,
                                                 concentrationCH4Source))
actonDGair$concentrationCO2Source <- with(actonDGair, 
                                          ifelse(site=="dock"&sample.date=="2017-08-15",
                                                 concentrationCO2Air,
                                                 concentrationCO2Source))
actonDGair$concentrationN2OSource <- with(actonDGair, 
                                          ifelse(site=="dock"&sample.date=="2017-08-15",
                                                 concentrationN2OAir,
                                                 concentrationN2OSource))



actonDGinput<-dplyr::select(actonDGair,sample.date, site, sample.depth.m,
                            gasVolume,waterVolume,barometricPressure,waterTemp,
                            headspaceTemp,concentrationCO2Gas,concentrationCO2Air, 
                            concentrationCO2Source,concentrationCH4Gas,
                            concentrationCH4Air,concentrationCH4Source,
                            concentrationN2OGas,concentrationN2OAir,concentrationN2OSource)



actonDGoutput<-def.calc.sdg(inputFile=actonDGinput)

#create deltaGHG columns that are the dissolved - sat values
actonDGoutput<-mutate(actonDGoutput,
                      deltaCO2=dissolvedCO2-satCO2,
                      deltaCH4=dissolvedCH4-satCH4,
                      deltaN2O=dissolvedN2O-satN2O)
#actonDGoutput$site2<-replace(actonDGoutput$site, "dock", "Udock")


ggplot(filter(actonDGoutput,sample.depth.m==0.1), aes(sample.date, deltaCO2))+
  geom_jitter(aes(color=site), alpha=0.8)+
  scale_x_date(breaks=date_breaks("2 months"),
               labels=date_format("%d %b"),
               name="Date")
#removed 52 rows, 14 are depth profiles from the dock w/out T info

actonDGoutput<-actonDGoutput%>%
  mutate(sampleDepthFactor = as.factor(sample.depth.m),
         sampleDepthFactor = replace(sampleDepthFactor, sampleDepthFactor == 0.8,
                                     0.75),
         sampleDepthFactor = replace(sampleDepthFactor, sampleDepthFactor == 1.25,
                                     1.3))

actonDGoutputAvg<-actonDGoutput%>%
  group_by(site, sample.date, sampleDepthFactor)%>%
  dplyr::summarize(meanDissCH4 = mean(dissolvedCH4, na.rm=TRUE),
                   sdDissCH4 = sd(dissolvedCH4, na.rm=TRUE))
#deep site: SI Figure S4
ggplot(filter(actonDGoutputAvg, site=="u12", meanDissCH4<0.0025), 
       aes(sample.date, meanDissCH4*10^6))+
  geom_point(alpha=0.5)+
  facet_grid(sampleDepthFactor~.,
             scales="free")+
  scale_x_date(breaks=date_breaks("2 months"),
               labels=date_format("%b %y"),
               name="Date")+
  ylab("dissolved CH4 (uM microMolar)")

#shallow site:
ggplot(filter(actonDGoutputAvg, site=="u14", meanDissCH4<0.0025), 
       aes(sample.date, meanDissCH4*10^6))+
  geom_point(alpha=0.5)+
  facet_grid(sampleDepthFactor~.,
             scales="free")+
  scale_x_date(breaks=date_breaks("2 months"),
               labels=date_format("%b %y"),
               name="Date")+
  ylab("dissolved CH4 (uM microMolar)")
#removed 52 rows, 14 are depth profiles from the dock w/out T info

ggplot(filter(actonDGoutput,sample.depth.m==0.1), aes(sample.date, deltaCH4))+
  geom_point(aes(color=site))

ggplot(filter(actonDGoutput,sample.depth.m==0.1), aes(sample.date, deltaN2O))+
  geom_point(aes(color=site), alpha=0.5)   

write.table(actonDGoutput,
            file="dataL2/actonDGoutput.csv",
            sep=",",
            row.names=FALSE)

rm(actonDG, actonDGair, actonDGinput, actonDgJoin, airMeans, gc.Acton)


