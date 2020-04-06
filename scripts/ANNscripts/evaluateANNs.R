
##Start here if you just ran an ANN Fit Model

library(rlist)
library(matrixStats)
library(merTools)

## Read in the ANN input data, version 7.0, which includes DOY, HOD, and
## excludes "fuzzyRAD"
annDat = read.csv("dataL2/annDat7.csv")
annDat <- subset(annDat, complete.cases(annDat[,2:ncol(annDat)]))
maxs <- apply(annDat, 2, max, na.rm=TRUE)
mins <- apply(annDat, 2, min, na.rm=TRUE)
# 
# ## Look at each errorFits object and pick out the 'best' models based on the 
# ## TESTING dataset by looking at the R^2 values first
# ## then save to the output/BestANNs subfolder
# ## don't need to do this if bestANNfiles is already populated -- skip to line 42
# fn = "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/virtualMachine/output"
# errorFiles = list.files(fn)
# #for testing code: load(file.path(fn, "BestANNsResample01.RData"))
# 
# for(f in 1:length(errorFiles)){
#   load(file.path(fn, errorFiles[[f]]))
#   r2Sims <- sapply(errorFits, function(x){x$r2})
#   minR2 <- sort(r2Sims, decreasing = TRUE)[100]
#   simKeep <- sapply(errorFits, function(x){ x$r2 >= minR2 } )
#   bestANNs <- errorFits[simKeep]
#   save(bestANNs, file = paste("output/BestANNs/BestANNs",
#                               #"L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/virtualMachine/BestANNs/BestANNs", 
#                               substring(errorFiles[[f]], 1, 10), ".RData", sep=""))
# }#one iteration takes ~10 min
# 
# ## for each iteration of the n=20 errorFits, the valIdx is the same across the 800 ANNs
# ## ergo, the validation data will be the same
# ## need to select the median of the 800 flux predictions for each of the 20 errorFits, 
# ## save medPreds and medR2

fnBest<-"dataL2/BestANNs"
bestANNfiles = list.files(fnBest)
#summaryInfo<-list()
## Make separate lists to hold the categories of information we need
summary.V.Info<-list() #list of validation and prediction data 
summary.VIF.Info<-list() # variable importance figure info
summary.Stat.Info<-list() # median R2 and median bias
summary.All<-list() # all predictions 

## for loop to load in each of the bestANN files, 
## then extract the info we need: validation, VIF, stats, and preds,
## and save into one of the four summary lists
for(g in 1:length(bestANNfiles)){
  load(file.path(paste(fnBest, bestANNfiles[g], sep="/")))
  validFlux = annDat$ch4_flux[bestANNs[[1]]$valIdx]
  bootRuns <- lapply(bestANNs, function(x){
    # x = errorList[[2]]
    ## This is crude, but works -- look for the intersection of sets (i.e., indices) where
    ## bestSL$seed and bestSL$layers are identical to the ones we care about. If there's a row match,
    ## we do stuff. If not, we skip it.
    #validFlux = annDat$ch4_flux[x$valIdx]
    #predFlux = x$preds[x$valIdx,1] # it's a matrix, so have to specify the column
    predFlux = x$preds[x$valIdx, 1] # not sure what the 1 is for, it adds the valIdx as a name
    # if(any(is.na(validFlux))){
    #   naInds = is.na(validFlux)
    #   validFlux = validFlux[!naInds]
    #   predFlux = predFlux[!naInds]
    # }
    tmpR2 <- 1 - (sum((validFlux-predFlux )^2)/sum((validFlux-mean(validFlux))^2))
    list("preds" = predFlux, "r2" = tmpR2, "valIdx" = x$valIdx )
  })
  validPreds <- do.call("cbind",lapply(bootRuns, function(x){ x$preds }))
  
  ## Median predictions
  validMedians <- apply(validPreds, 1, median)
  ## Overall R^2 value for the median predictions
  medR2 <- 1 - (sum((validFlux-validMedians)^2)/sum((validFlux-mean(validFlux))^2))
  
  d <- data.frame("Flux" = validFlux,
                  "Preds" = validMedians)
  lms <- c(-1,1.75)
  
  impVars <- do.call(rbind,lapply(bestANNs, function(x){ x$varImp}))
  #impVars <- do.call(rbind,lapply(bestANNs, function(x){ x$varimp}))
  impVarsMedians <- ddply(impVars, .(Variable), summarise, 
                          "MedianImportance" = median(Importance))
  
  d$diff_gCH4m2hh = d$Preds*60*30*16/10^6 - d$Flux*60*30*16/10^6
  BiasErr_gCH4m2hh<-sum(d$diff_gCH4m2hh)/nrow(d)
  
  #need the median of the predictions
  ANNpreds <- do.call("cbind",lapply(bestANNs, function(x){ x$preds }))
  ## Median predictions
  ANNmedians <- apply(ANNpreds, 1, median)#*(maxs[1] - mins[1]) + mins[1]
  
  
  summary.V.Info[[g]]<-list("measFlux" = validFlux, "predsV" = validMedians)
  summary.VIF.Info[[g]]<-list("varImport" = impVarsMedians)
  summary.Stat.Info[[g]]<-list("overallR2" = medR2, "biasErr_gCH4m2hh" = BiasErr_gCH4m2hh)
  summary.All[[g]]<-list("predsAll"=ANNmedians)
  
}
#takes ~3 min

## Now the summary stat list holds the R2 values, biases, median variable importance,
## validation set, and median prediction set for all n=20 runs

summaryR2<-sapply(summary.Stat.Info, function(x){x$overallR2})
summaryR2<-as.data.frame(summaryR2)
ggplot(summaryR2, aes(summaryR2))+
  geom_histogram(binwidth=0.005)

summaryBias<-sapply(summary.Stat.Info, function(x){x$biasErr_gCH4m2hh})
summaryBias<-as.data.frame(summaryBias)
ggplot(summaryBias, aes(summaryBias))+
  geom_histogram(binwidth=0.00005)

summary(summaryR2$summaryR2) #median = 0.7943
summary(summaryBias$summaryBias)
medBiasErr<-median(summaryBias$summaryBias)
maxBias<-max(summaryBias$summaryBias)
minBias<-min(summaryBias$summaryBias)

## need the measured dataset to know how many gaps:
## load in annIN, similar to annDat, but it has a datetime column
annIN<-read.csv("data/annDataset_20190610.csv")
annIN<-select(annIN, -fuzzyRAD)
annIN<-annIN%>%
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
         DOY = as.numeric(format(datetime, "%j")),
         HOD = as.numeric(hms::hms(second(datetime),minute(datetime),hour(datetime))))
annIN2<-subset(annIN, complete.cases(annIN[,4:ncol(annIN)]))

sum(is.na(annIN2$ch4_flux)) #22952 missing 30-minute periods
#bias error is in gCH4 per m2 per half hour
cmlBias<-round(medBiasErr*sum(is.na(annIN2$ch4_flux)), digits=3) #0.254 g CH4 per m2
cmlBias<-round(minBias*sum(is.na(annIN2$ch4_flux)), digits=3)

## Let's make a pdf file with the linear regression plots for each of the 20 runs
pdf(paper = "a4r", width = 20, file = "figures/ANN20xLinReg.pdf") # landscape orientation
lapply(summary.V.Info, function(x){
  df.temp<-data.frame(x$measFlux, x$predsV)
  colnames(df.temp)<-c("measFlux", "predsV")
  
  ggplot(df.temp, aes(measFlux*60*60*16/1000, predsV*60*60*16/1000))+
    geom_point(alpha=0.1)+
    geom_abline(slope = 1, intercept = 0, colour = "red") + 
    xlim(-50, 100) + ylim(-50, 100)+
    xlab(expression(Measured~CH[4]~Flux~(mg~CH[4]~m^{-2}~hr^{-1})))+
    ylab(expression(Predicted~CH[4]~Flux~(mg~CH[4]~m^{-2}~hr^{-1})))
  
})
dev.off() 

## Same thing for VIF plots
pdf(paper = "a4r", width = 20, file = "figures/ANN20xVIF.pdf") # landscape orientation
lapply(summary.VIF.Info, function(x){
  df.vif.temp<-data.frame(x$varImport)
  ggplot(df.vif.temp, 
         aes(x = reorder(Variable, MedianImportance), y = MedianImportance))+
    geom_bar(stat = "identity", width = 0.5) + coord_flip() +
    xlab("Variable") + ylab("Importance")
})
dev.off() 

## now, try to aggregate the 20 iterations of VI's
count<-(1:20)
for(i in 1:length(summary.VIF.Info)){
  colnames(summary.VIF.Info[[i]]$varImport)<-c(paste("Variable", count[i], sep=""), paste("MedianImportance", count[i], sep=""))
}
VIF.df<-do.call("cbind", summary.VIF.Info)

summary(VIF.df)
str(VIF.df)
VIF.df<-do.call("cbind", VIF.df)

#take the median of the medians
VIF.df<-VIF.df%>%
  rowwise()%>%
  mutate(CM = median(MedianImportance1, MedianImportance2, MedianImportance3, MedianImportance4, MedianImportance5,
                     MedianImportance5, MedianImportance7, MedianImportance8, MedianImportance9, MedianImportance10,
                     MedianImportance11, MedianImportance12, MedianImportance13, MedianImportance14, MedianImportance15, 
                     MedianImportance16, MedianImportance17, MedianImportance18, MedianImportance19, MedianImportance20))
## rename for figure-friendly labels
VIF.df$names<-c("DOY", "Air T", "H", "LE", "PAR", "Sed T", "Site", "Static P", "Delta Static P", "u Star",
                "Wind Dir", "Wind Speed", "HOD")

## FIGURE 8
ggplot(VIF.df, 
       aes(x = reorder(names, CM), y = CM))+
  geom_bar(stat = "identity", width = 0.5) + coord_flip() +
  xlab("Variable") + ylab("Importance (%)")+
  ggtitle("Median ANN Variable Importance")+
  theme_bw()

## aggregate and find the median of the median predictions
count<-(1:20)
#All.df<-do.call("cbind", summary.All)
for(i in 1:length(summary.All)){
  names(summary.All[[i]])<-(paste("predsAll", count[i], sep=""))
}

## this "rowwise" strategy only worked for the mean & median, not sd and se 
All.df<-data.frame(summary.All)
All.df<-All.df%>%
  rowwise()%>%
  mutate(
    all.median = median(predsAll1, predsAll2, predsAll3, predsAll4, predsAll5,
                        predsAll6, predsAll7, predsAll8, predsAll9, predsAll10,
                        predsAll11, predsAll12, predsAll13, predsAll14, predsAll15,
                        predsAll16, predsAll17, predsAll18, predsAll19, predsAll20),
    all.mean = mean(predsAll1, predsAll2, predsAll3, predsAll4, predsAll5,
                    predsAll6, predsAll7, predsAll8, predsAll9, predsAll10,
                    predsAll11, predsAll12, predsAll13, predsAll14, predsAll15,
                    predsAll16, predsAll17, predsAll18, predsAll19, predsAll20))
# all.sd = sd(predsAll1, predsAll2, predsAll3, predsAll4, predsAll5,
#                predsAll6, predsAll7, predsAll8, predsAll9, predsAll10,
#                predsAll11, predsAll12, predsAll13, predsAll14, predsAll15,
#                predsAll16, predsAll17, predsAll18, predsAll19, predsAll20),
# all.se = all.sd/sqrt(20),
# all.95CI = all.se*1.96)

## need to turn the data into a matrix to calculate the rowwise sd, se, and CI

All.matrix<-matrix(unlist(summary.All, use.names = FALSE), ncol=20, byrow=TRUE)
#All.matrix<-do.call(rbind,lapply(summary.All,matrix,ncol=20,byrow=TRUE))

#all.mean<-rowMeans2(All.matrix)
all.sd<-matrixStats::rowSds(All.matrix)
all.se = all.sd/sqrt(20)
all.95CI = all.se*1.96

All.df$all.95CI<-all.95CI
All.df$all.L95<-All.df$all.mean-All.df$all.95CI
All.df$all.U95<-All.df$all.mean+All.df$all.95CI

## Create gap-filled vectors by combining annDat observations and the ANN preds 
ch4ANN_med <- ifelse(is.na(annDat$ch4_flux), 
                     All.df$all.median,
                     annDat$ch4_flux)
ch4ANN_mean <- ifelse(is.na(annDat$ch4_flux), 
                      All.df$all.mean,
                      annDat$ch4_flux)
ch4ANN_L95<-ifelse(is.na(annDat$ch4_flux), 
                   All.df$all.L95,
                   annDat$ch4_flux)
ch4ANN_U95<-ifelse(is.na(annDat$ch4_flux), 
                   All.df$all.U95,
                   annDat$ch4_flux)

annDat$ch4_filled<-ch4ANN_mean
annDat$ch4_L95<-ch4ANN_L95
annDat$ch4_U95<-ch4ANN_U95

## this seems redundant, but I'm keeping it because it worked

annIN2$ch4_filled<-annDat$ch4_filled
annIN2$ch4_L95<-annDat$ch4_L95
annIN2$ch4_U95<-annDat$ch4_U95

## Calculate cumulative emissions, 95% CIs
annIN2<-annIN2%>%
  mutate(ch4_cumulative = cumsum(ch4_filled*60*30*16/10^6),#units of g CH4 m-2 per 30 min, summed over 30 min increments))
         ch4_cumulative_L95 = cumsum(ch4_L95*60*30*16/10^6),
         ch4_cumulative_U95 = cumsum(ch4_U95*60*30*16/10^6))

ggplot(annIN2, aes(datetime, ch4_flux))+
  geom_point(alpha=0.3)+
  geom_point(aes(datetime, ch4_filled), color="grey", alpha=0.3)

ggplot(annIN2, aes(datetime, ch4_cumulative))+
  geom_line()+
  geom_line(aes(datetime, ch4_cumulative_L95), alpha=0.5)+
  geom_line(aes(datetime, ch4_cumulative_U95), alpha=0.5)

annIN2$date<-as.Date(annIN2$datetime)
dailyFilled<-annIN2%>%
  group_by(date)%>%
  dplyr::summarize(meanFilledCH4 = mean(ch4_filled, na.rm=TRUE),
                   meanFilledCH4_U95 = mean(ch4_U95, na.rm=TRUE),
                   meanFilledCH4_L95 = mean(ch4_L95, na.rm=TRUE))

dailyFilled$date<-as.POSIXct(dailyFilled$date, format="%Y-%m-%d", tz="UTC")

ggplot(annIN2, aes(datetime, ch4_filled))+
  geom_point(alpha=0.3, color="gray")+
  geom_point(aes(datetime, ch4_flux), alpha=0.3)+
  geom_line(data=dailyFilled, aes(date, meanFilledCH4), color="red")+
  geom_line(data=dailyFilled, aes(date, meanFilledCH4_L95), color="red", alpha=0.5)+
  geom_line(data=dailyFilled, aes(date, meanFilledCH4_U95), color="red", alpha=0.5)

write.table(annIN2, 
            file="dataL2/gapFilledEC_results.csv",
            sep=",",
            row.names = FALSE)











## Previous code


# d <- data.frame("Flux" = validFlux,
#                     "Preds" = validMedians)
#     
#     validRuns <- lapply(bestANNs, function(x){
#        x <- bestANNs[[1]]
#       tmpPreds <- predict(x$ann, newdata = validSet[,annCols[-1]]) * 
#         (maxs[1] - mins[1]) + mins[1]
#       tmpR2 <- 1 - (sum((validFlux-tmpPreds )^2)/sum((validFlux-mean(validFlux))^2))
#       list("preds" = tmpPreds, "r2" = tmpR2)
#     })
#     
#     validPreds <- do.call("cbind",lapply(validRuns, function(x){ x$preds }))
#     ## Median predictions
#     validMedians <- apply(validPreds, 1, median)
#     ## Overall R^2 value for the median predictions
#     medR2 <- 1 - (sum((validFlux-validMedians)^2)/sum((validFlux-mean(validFlux))^2))
#     
#   
# })
# 
# 
# 
# ## Since all of the predictions are returned already, we only really need to send back the
# ## indices that represent the validation data set.
# ## Example of how to assemble the validation r2 values:
# fn = "output/ANNerrors"
# errorFiles = list.files(fn)
# errorFiles
# errorList = list()
# 
# for(f in errorFiles[1:2]){
#   load(file.path(fn, f))
#   errorList = append(errorList, errorFits)
# }
# 
# # for(f in errorFiles){
# #   load(file.path(fn, f))
# #   errorList = append(errorList, errorFits)
# # }
# 
# validRuns <- lapply(errorList, function(x){
#   # x = errorFits[[1]]
#   validFlux = annDat$ch4_flux[x$valIdx]
#   predFlux = x$preds[x$valIdx,1] # it's a matrix, so have to specify the column
#   if(any(is.na(validFlux))){
#     naInds = is.na(validFlux)
#     validFlux = validFlux[!naInds]
#     predFlux = predFlux[!naInds]
#   }
#   tmpR2 <- 1 - (sum((validFlux-predFlux )^2)/sum((validFlux-mean(validFlux))^2))
#   list("preds" = predFlux, "r2" = tmpR2, "valIdx" = x$valIdx )
# })
# 
# ## Each set of validation predictions are from different indices now.
# validPredsList = lapply(validRuns, function(x){
#   data.frame("Idx" = x$valIdx, "Preds" = x$preds)
# })
# 
# ## Cool function to do recursive joins (thanks StackOverflow)
# func <- function(...){
#   df1 = list(...)[[1]]
#   df2 = list(...)[[2]]
#   col1 = colnames(df1)[1]
#   col2 = colnames(df2)[1]
#   xxx = full_join(..., by = "Idx")
#   return(xxx)
# }
# 
# validPreds = Reduce( func, validPredsList)
# head(validPreds) # This should have every index in the union of validation sets
# 
# 
