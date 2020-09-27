#2019 03 05 ANN Runs to execute:
#1. Complete dataset (2017 - 2018), using LE, H, ustar, sedT, deltaStaticP, 
#   WS, WD, AirT, fuzzy radiation, static pressure, site location indicator, 
#   degree of stratification
#2. Same as above, minus site location indicator   
#3. Subset of data, truncated to match the active trap observations

#######################################
############# ANN Fitting #############
#######################################
plotGaps(fluxDat, "ch4_flux")
sum(is.na(fluxDat$ch4_flux)) / nrow(fluxDat) # 2018 dataset: 67% missing, vs 75% for 2017

fluxDatToUse<-subset(fluxDat, fluxDat$datetime>(startdate) & fluxDat$datetime<(enddate))
fluxDatToUse<-subset(fluxDatFilled, fluxDatFilled$datetime>(startdate) & fluxDatFilled$datetime<(enddate))

fluxDatToUse$index<-1:nrow(fluxDatToUse)

plotGaps(fluxDatToUse, "ch4_flux")
sum(is.na(fluxDatToUse$ch4_flux)) / nrow(fluxDatToUse) #28% missing, 38% with ustar filter of 0.07
range(fluxDatToUse$datetime)
covarFuzzy=TRUE
# covarWD=FALSE
# covarPAR=FALSE
## Data prep
annCols <- c("ch4_flux",
             "FilledSedT",
             "FilledAirT","FilledWindSpeed",
             "FilledStaticPress",
             "FilledStaticPressChg",
             "FilledUstar",
             "FilledLE",
             "FilledH", 
             "FilledSite",
             "FilledPAR",
             "index")
annDat <- fluxDatToUse[,annCols]
#annDat <- fluxDatFilled[,annCols]

fluxDatFilled2018<-select(fluxDat, datetime, co2_flux, annCols)
#test5.2$FilledPAR<-test5.2$par.vws
#annDat<-fluxDatEbFilled[,annCols]
# write.table(fluxDatFilled,
#             file=("C:/R_Projects/actonFluxProject/output/annDataset_20190403.csv"),
#             sep=",",
#             row.names=FALSE)
write.table(fluxDatToUse,
            file=("C:/R_Projects/actonFluxProject/output/annDataset_20190610.csv"),
            sep=",",
            row.names=FALSE)

annIN<-read.csv("C:/R_Projects/actonFluxProject/output/annDataset_20190610.csv")
annIN<-annIN%>%
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
         DOY = as.numeric(format(datetime, "%j")),
         HOD = as.numeric(hms::hms(second(datetime),minute(datetime),hour(datetime))))

###Realizing I need parameters from annDat to evaluate each run
# ###Should save here
# write.table(annDat,
#             file=(paste("C:/R_Projects/actonFluxProject/output/annDat", 
#                         runVer, ".csv", sep="")),
#                   sep=",",
#                   row.names=FALSE)

annIN<-select(annIN, -fuzzyRAD)
annDat<-subset(annIN2, complete.cases(annIN2[,4:ncol(annIN2)]))
annIN2<-subset(annIN, complete.cases(annIN[,4:ncol(annIN)]))

annDat<-select(annDat, -index, -co2_flux, -datetime)

#annDat <- subset(annDat, complete.cases(annDat[,2:ncol(annDat)]))

write.table(annDat,
            file=(paste("C:/R_Projects/actonFluxProject/output/annDat", 
                        runVer, ".csv", sep="")),
            sep=",",
            row.names=FALSE)

##########
### Start ANN setup
#########

maxs <- apply(annDat, 2, max, na.rm=TRUE)
mins <- apply(annDat, 2, min, na.rm=TRUE)
scaledDat <- as.data.frame(scale(annDat, center = mins, scale = maxs - mins))
summary(scaledDat)


## K-means clustering of data points, for training/testing/validation sets
set.seed(4321)
k <- 10
kClusters <- kmeans(scaledDat[,2:ncol(scaledDat)], centers = k)
df <- data.frame("Index" = 1:nrow(scaledDat),
                 "Cluster" = kClusters$cluster)

## Do training set first
set.seed(1111)
trainProp <- 0.5
sizeClust <- as.vector(table(df$Cluster))
nSampsClust <- ceiling(trainProp*sizeClust)
trainList <- dlply(df, .(Cluster), function(x){
  # x <- subset(df, Cluster == 1)
  sampInd <- unique(x$Cluster)
  sample(x$Index, nSampsClust[sampInd], replace = FALSE)
})
trainInds <- unlist(trainList)
trainDat <- scaledDat[trainInds,]
## Same routine for testing set
set.seed(2222)
testProp <- 0.5 # We're taking half of what's left, so 25% of the total.
# Take out the 'training' indices and sample from what's left.
dfTest <- df[-trainInds,]
sizeClust <- as.vector(table(dfTest$Cluster))
nSampsClust <- ceiling(testProp*sizeClust)
testList <- dlply(dfTest, .(Cluster), function(x){
  # x <- subset(df, Cluster == 1)
  sampInd <- unique(x$Cluster)
  sample(x$Index, nSampsClust[sampInd], replace = FALSE)
})
testInds <- unlist(testList)
testDat <- scaledDat[testInds,]
## Validation data is everything left.
validationDat <- scaledDat[-c(trainInds,testInds),]


## Testing activation functions and fitting algorithm.
## Morin (2014) used input-12-5-output hidden layer structure, both with the hyperbolic tangent sigmoid 
## transfer function. 
## Dengel (2013) used resilient backpropagation algorithm and the sigmoid function. They don't define it exactly.
## Papale and Valentinin used feed-forward back propagation algorithm, with 5 inputs, one hidden layer
## with 3 nodes, and 2 output variables. The sigmoid function is y = 1 / (1 + e^(-a/p)), where a is the weighted sum
## of the inputs to the node. The p coefficient determines the shape/steepness of the curve. Typically 1 (ignored).
## Apparently the tanh function provides better gradients in the tails, and is preferred
## over sigmoid.
## Several data science sources give the RELu function as 'better' -- but it's not 
## differentiable for the regression cases. So the softplus function is used, which
## is a close differentiable approximation.
# softplus <<- function(x) {log(1+exp(x))}
# custom <<- function(x) {x/(1+exp(-2*k*x))}

## ANN
## If fittinf with 'neuralnet', use the following few lines.
# set.seed(9876)
# n <- names(trainDat)
# f <- as.formula(paste("ch4_flux ~", paste(n[!n %in% "ch4_flux"], collapse = " + ")))
# nn <- neuralnet(f, data = trainDat, hidden=8, act.fct = "logistic", linear.output=T)
# plot(nn)
# linear.output specifies that we're doing 'regression', not 'classification'


## Set up a simulation with varying hidden layers and seeds.
seeds <- 101:150
layers <- 5:20
trainSet <- subset(trainDat, !is.na(ch4_flux))
testSet <- subset(testDat, !is.na(ch4_flux))
validSet <- subset(validationDat, !is.na(ch4_flux))
testFlux <- testSet$ch4_flux *(maxs[1] - mins[1]) + mins[1]
fitANN <- function(s,lyr){
  # s <- seeds[1]; lyr <- layers[1]
  set.seed(s);
  # Model
  tmpMod <- nnet::nnet(ch4_flux ~ ., data = trainSet, size = lyr,
                       maxit = 10000, entropy = TRUE)
  # Variable importance
  tmpVarImp <- varImp(tmpMod)
  idx <- order(tmpVarImp$Overall, decreasing = TRUE)
  varImp <- data.frame("Variable" = rownames(tmpVarImp)[idx],
                       "Importance" = tmpVarImp$Overall[idx])
  # R^2
  tmpPreds <- predict(tmpMod, newdata = testSet[,annCols[-1]]) * 
    (maxs[1] - mins[1]) + mins[1]
  tmpR2 <- 1 - (sum((testFlux-tmpPreds )^2)/sum((testFlux-mean(testFlux))^2))
  list("seed"=s, "layers"=lyr,
       "ann"=tmpMod, "varimp"=varImp, "r2"=tmpR2)
}

fitModels <- TRUE
if(fitModels){
  ## Make prediction grid, use apply fxn
  annGrid <- expand.grid(seeds,layers)
  ptm <- proc.time()
  simList <- apply(annGrid,1, function(x){
    fitANN(x[1],x[2])
  })
  proc.time() - ptm # 2762 seconds --> ~5.5 hours
  save(simList, file = paste("output/annSimulationList", runVer, ".RData", sep=""))
}


## Error fitting
## This calls a function that bootstraps the scaledDat object lots of times
annDat <- read.csv("output/annDat6.0.csv")
annDat <- subset(annDat, complete.cases(annDat[,2:ncol(annDat)]))
maxs <- apply(annDat, 2, max, na.rm=TRUE)
mins <- apply(annDat, 2, min, na.rm=TRUE)
scaledDat <- as.data.frame(scale(annDat, center = mins, scale = maxs - mins))
## K-means clustering of data points, for training/testing/validation sets
set.seed(4321)
k <- 10
kClusters <- kmeans(scaledDat[,2:ncol(scaledDat)], centers = k)
df <- data.frame("Index" = 1:nrow(scaledDat),
                 "Cluster" = kClusters$cluster)
errorFunction <- function(d, df, n, ptrain = 0.5, lyr = NULL){
  library(tidyverse)
  n = 250; d = scaledDat; df = df; ptrain = 0.5; lyr = NULL
  ## Assign number of layers
  if(is.null(lyr)) lyr = 19 # Max r2 value from running code on simList for ann6.0:
  #simList[[which.max(unlist(lapply(simList, function(x) x$r2)))]]$layers
  ## Create data partitions
  ## Use caret package to create train / test data sets
  ## Length n list of training sets
  trainIdx = caret::createDataPartition(y = df$Cluster, times = n, p = ptrain, list = TRUE)
  ## Fit model to each training set, predict values held out
  bootList = lapply(trainIdx, function(x){
    ## x = trainIdx[[2]]
    tmpDat = scaledDat[x,]
    tmpMod = nnet::nnet(ch4_flux ~ ., data = tmpDat, size = lyr,
                        maxit = 10000, entropy = TRUE)
    return(data.frame("Idx" = (1:nrow(scaledDat))[-x],
                      "Preds" = predict(tmpMod, newdata = scaledDat[-x,-1 ]) * (maxs[1] - mins[1]) + mins[1]))
  })
  predsDf = bootList %>% reduce(full_join, by = "Idx") %>% arrange(Idx)
  return(predsDf)
}

fitErrors <- TRUE
if(fitErrors){
  ## Set seed here if you want, so the errorFunction output is reproducible.
  predsDf = errorFunction(n = 20, d = scaledDat, df = df, ptrain = 0.5, lyr = NULL)
  ## On average, each record will have n*p predictions. So if we split the data in half to train,
  ## half the records will have predictions. Choose n accordingly.
  ## We can get percentiles with apply:
  interQuartRanges = t(apply(predsDf[,-1], 1, FUN = function(x){ quantile(x, c(0.05, 0.25, 0.75, 0.95), na.rm = TRUE)}))
}

predsMedian<-apply(predsDf, 1, median, na.rm=TRUE)
predsN<-apply(predsDf, 1, nobs)
interQR.df<-as.data.frame(cbind(predsMedian, predsN, interQuartRanges, df))
names(interQR.df)[names(interQR.df) == "5%"] <- "quant5"
names(interQR.df)[names(interQR.df) == "25%"] <- "quant25"
names(interQR.df)[names(interQR.df) == "75%"] <- "quant75"
names(interQR.df)[names(interQR.df) == "95%"] <- "quant95"

write.table(interQR.df, 
            file="C:/R_Projects/actonFluxProject/output/interQR20190523.csv",
            sep=",",
            row.names=FALSE)

write.table(predsDf,
            file="C:/R_Projects/actonFluxProject/output/predsError20190523.csv",
            sep=",",
            row.names=FALSE)

ggplot(interQR.df, aes(Index, predsMedian))+
  geom_line(alpha=0.5)+
  geom_line(data=interQR.df, aes(Index, quant25), color="red", alpha=0.2)+
  geom_line(data=interQR.df, aes(Index, quant75), color="red", alpha=0.2)
# head(predsDf)
# predsDf[1,]
# 
# gmodels::ci(predsDf[1,], confidence=0.95)
# load("output/annSimulationListAq2018.RData")
#3.1: aq tower dataset 5/6/2018 thru 8/6/2018 with ustar filter applied

# 
