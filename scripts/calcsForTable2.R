####summer cumulative info for Table 2################


summer17DeepAFT<-cumsum(subset(DailyDeepTfluxes, Rdate>"2017-05-01"&
                                 Rdate<"2017-10-01")$meanCH4flux.interp)*24/1000 #units: g/m2
summer17DeepTot<-cumsum(subset(DailyDeepTfluxes, Rdate>"2017-05-01"&
                                 Rdate<"2017-10-01")$ch4.trate)*24/1000 #units: g/m2
summer17Err<-DailyDeepTfluxes$ch4.cumuErr_yr[which(grepl("2017-10-01",DailyDeepTfluxes$date))]-
  DailyDeepTfluxes$ch4.cumuErr_yr[1]
summer17DateDeep<-subset(DailyDeepTfluxes, Rdate>"2017-05-01"&
                           Rdate<"2017-10-01")$Rdate
summer18DeepAFT<-cumsum(subset(DailyDeepTfluxes, Rdate>"2018-05-01"&
                                 Rdate<"2018-10-01")$meanCH4flux.interp)*24/1000 #units: g/m2
summer18DeepTot<-cumsum(subset(DailyDeepTfluxes, Rdate>"2018-05-01"&
                                 Rdate<"2018-10-01")$ch4.trate)*24/1000 #units: g/m2
summer18DateDeep<-subset(DailyDeepTfluxes, Rdate>"2018-05-01"&
                           Rdate<"2018-10-01")$Rdate
print(paste("Deep AFT Summer 2017 Avg FCH4:", 
            round(summer17DeepAFT[length(summer17DeepAFT)]/length(summer17DeepAFT)*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))
print(paste("Deep Tot Summer 2017 Avg FCH4:", 
            round(summer17DeepTot[length(summer17DeepTot)]/length(summer17DeepTot)*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))
print(paste("Deep Summer 2017 Avg Err:", round(summer17Err/length(summer17DeepTot), 2), 2))
print(paste("Deep AFT Summer 2018 Avg FCH4:", 
            round(summer18DeepAFT[length(summer18DeepAFT)]/length(summer18DeepAFT)*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))
print(paste("Deep Tot Summer 2018 Avg FCH4:", 
            round(summer18DeepTot[length(summer18DeepTot)]/length(summer18DeepTot)*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))
# combinedDate<-c(summerDate, summerDate)
# combinedData<-c(summerDeepTot, summerDeepAFT)
# combinedIndex<-c(rep("total", 145), rep("AFT", 145))

# testDF<-data.frame(combinedDate, combinedData, combinedIndex)
# 
# ggplot(testDF, aes(combinedDate, combinedData))+
#   geom_line(aes(color=combinedIndex))
############################



#######summer cumulative info for Table 2#######

summer17ShalAFT<-cumsum(subset(DailyShalTfluxes, Rdate>"2017-05-01"&
                                 Rdate<"2017-10-01")$meanCH4flux.interp)*24/1000 #units: g/m2
summer17ShalTot<-cumsum(subset(DailyShalTfluxes, Rdate>"2017-05-01"&
                                 Rdate<"2017-10-01")$ch4.trate)*24/1000 #units: g/m2
summer17DateShal<-subset(DailyShalTfluxes, Rdate>"2017-05-01"&
                           Rdate<"2017-10-01")$Rdate
summer18ShalAFT<-cumsum(subset(DailyShalTfluxes, Rdate>"2018-05-01"&
                                 Rdate<"2018-10-01")$meanCH4flux.interp)*24/1000 #units: g/m2
summer18ShalTot<-cumsum(subset(DailyShalTfluxes, Rdate>"2018-05-01"&
                                 Rdate<"2018-10-01")$ch4.trate)*24/1000 #units: g/m2
summer18DateShal<-subset(DailyShalTfluxes, Rdate>"2018-05-01"&
                           Rdate<"2018-10-01")$Rdate

print(paste("Shal AFT Summer 2017 Avg FCH4:", 
            round(summer17ShalAFT[length(summer17ShalAFT)]/length(summer17ShalAFT)*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))
print(paste("Shal Tot Summer 2017 Avg FCH4:", 
            round(summer17ShalTot[length(summer17ShalTot)]/length(summer17ShalTot)*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))
print(paste("Shal AFT Summer 2018 Avg FCH4:", 
            round(summer18ShalAFT[length(summer18ShalAFT)]/length(summer18ShalAFT)*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))
print(paste("Shal Tot Summer 2018 Avg FCH4:", 
            round(summer18ShalTot[length(summer18ShalTot)]/length(summer18ShalTot)*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))

combinedDate<-c(rep(summer17DateDeep, 2), rep(summer17DateShal, 2), 
                rep(summer18DateDeep, 2), rep(summer18DateShal, 2))
combinedData<-c(summer17DeepTot, summer17DeepAFT, summer17ShalTot, summer17ShalAFT,
                summer18DeepTot, summer18DeepAFT, summer18ShalTot, summer18ShalAFT)
combinedSiteIndex<-c(rep("deep", 145), rep("deep", 145), 
                     rep("shal", 144), rep("shal", 144),
                     rep("deep", 152), rep("deep", 152),
                     rep("shal", 153), rep("shal", 153))
combinedMethodIndex<-c(rep("total", 145), rep("AFT", 145), 
                       rep("total", 144), rep("AFT", 144),
                       rep("total", 152), rep("AFT", 152),
                       rep("total", 153), rep("AFT", 153))


epSeaonal<-epDataFilled%>%
  group_by(seasonInd)%>%
  dplyr::summarise(meanCH4Flux = mean(ch4_filled, na.rm=TRUE),
                   sdCH4Flux = sd(ch4_filled, na.rm=TRUE),
                   nch4 = n(),
                   seCH4Flux = sdCH4Flux/sqrt(nch4))
##calculating mean flux during ice cover:
mean(subset(epDataFilled, iceInd==1)$ch4_flux, na.rm=TRUE)*60*60*16/1000
sd(subset(epDataFilled, iceInd==1)$ch4_flux, na.rm=TRUE)*60*60*16/1000

##warm season 2017:
mean(subset(epDataFilled, datetime>="2017-05-01" & datetime<"2017-10-01")$ch4_flux, na.rm=TRUE)*60*60*16/1000
sd(subset(epDataFilled, datetime>="2017-05-01" & datetime<"2017-10-01")$ch4_flux, na.rm=TRUE)*60*60*16/1000


##warm season 2018:
mean(subset(epDataFilled, datetime>="2018-05-01" & datetime<"2018-10-01")$ch4_flux, na.rm=TRUE)*60*60*16/1000
sd(subset(epDataFilled, datetime>="2018-05-01" & datetime<"2018-10-01")$ch4_flux, na.rm=TRUE)*60*60*16/1000


rowNum<-which(grepl("2018-01-01", DailyANNFluxes$date))
value17<-DailyANNFluxes$ch4.t_cumu[rowNum]
value17L<-DailyANNFluxes$ch4.t_cumuL95[rowNum]
value17U<-DailyANNFluxes$ch4.t_cumuU95[rowNum]

###spring burst info:
maxVal<-max(DailyANNFluxes$ch4.trate)
rowNum<-which(grepl(maxVal, DailyANNFluxes$ch4.trate))
DailyANNFluxes[515,]

sb2017<-DailyANNFluxes[145:156,]
mean(sb2017$ch4.trate)
sd(sb2017$ch4.trate)


total2017<-DailyANNFluxes$ch4.t_cumu[which(grepl("2017-12-31",DailyANNFluxes$date))]
ws2017<-DailyANNFluxes$ch4.t_cumu[which(grepl("2017-10-01",DailyANNFluxes$date))]-
  DailyANNFluxes$ch4.t_cumu[which(grepl("2017-05-01",DailyANNFluxes$date))]
warmSeasFrac<-ws2017/total2017
ws2017L<-DailyANNFluxes$ch4.t_cumuL95[which(grepl("2017-10-01",DailyANNFluxes$date))]-
  DailyANNFluxes$ch4.t_cumuL95[which(grepl("2017-05-01",DailyANNFluxes$date))]
ws2017U<-DailyANNFluxes$ch4.t_cumuU95[which(grepl("2017-10-01",DailyANNFluxes$date))]-
  DailyANNFluxes$ch4.t_cumuU95[which(grepl("2017-05-01",DailyANNFluxes$date))]
ndays<-as.numeric(DailyANNFluxes$date[which(grepl("2017-10-01",
                                                  DailyANNFluxes$date))]-
                    DailyANNFluxes$date[which(grepl("2017-05-01",
                                                    DailyANNFluxes$date))])
total2018<-max(DailyANNFluxes$ch4.t_cumu)-total2017
ws2018<-DailyANNFluxes$ch4.t_cumu[which(grepl("2018-10-01",DailyANNFluxes$date))]-
  DailyANNFluxes$ch4.t_cumu[which(grepl("2018-05-01",DailyANNFluxes$date))]
ws2018/total2018
ws2018L<-DailyANNFluxes$ch4.t_cumuL95[which(grepl("2018-10-01",DailyANNFluxes$date))]-
  DailyANNFluxes$ch4.t_cumuL95[which(grepl("2018-05-01",DailyANNFluxes$date))]
print(paste("Mean Warm Season Fluxes 2017", 
            (ws2018/ndays)*1000/24, sep=""))
print(paste("L95 Warm Season Fluxes 2017", 
            (ws2018L/ndays)*(1000/24), sep=""))


grts_cuml2017<-grts_ts[which(grepl("2017-10-05", grts_ts$date)), "ch4.t_cumu"]
grtsDays2017<-as.numeric(as.Date("2017-10-04")-as.Date("2017-05-01"))

grts_cuml2018<-grts_ts[which(grepl("2018-10-01", grts_ts$date)), "ch4.t_cumu"]-grts_cuml2017
grtsDays2018<-as.numeric(as.Date("2017-10-01")-as.Date("2017-05-01"))

print(paste("GRTS Summer 2017 Avg FCH4:", 
            round(grts_cuml2017/grtsDays2017*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))
print(paste("GRTS Summer 2018 Avg FCH4:", 
            round(grts_cuml2018/grtsDays2017*1000/24, 2), 
            "mg m-2 hr-1", sep=" "))