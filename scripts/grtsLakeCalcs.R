# ADJUST WEIGHTS
# Need to define 4 inputs
# 1. sitesAdj:  TRUE/FALSE corresponding to "sampled" "notsampled"
# 2. wgtAdj: original weights
# 3. wgtCat: stratum if equal area (stratified or unstratified), mdcaty if unequal
# 4. framesizeAdj: named vector containing area of:
#       -if unstratified, then whole lake
#       -if unstratified-unequal (Charles Mill, Hocking), then section
#       -if stratified-equal, then each strata
#       -if stratified-unequal, then each section

myWgtList <- list() # empty list to catch adjusted weights

for (i in 1:length(unique(eqAreaData$Lake_Name))) {
  lake.i <- unique(eqAreaData$Lake_Name)[i]
  data.i <- filter(eqAreaData, Lake_Name == lake.i)
  
  # 1. sitesAdj:  TRUE/FALSE corresponding to "sampled" "notsampled"  
  sites.adj.i <- ifelse(data.i$EvalStatus == "sampled",
                        TRUE, FALSE)
  
  # 2. wgtAdj: original weights  
  wgtAdj.i <- data.i$wgt
  
  # 3. wgtCat: stratum if equal area (stratified or unstratified), section if unequal   
  if(length(unique(data.i$mdcaty)) == 1) { # one mdcaty means equal area
    wgtCat.i <- data.i$stratum
  } else {  # >1 mdcaty means unequal area
    wgtCat.i <- data.i$section
  }
  
  # 4. framesizeAdj: named vector containing area of:
  
  if(length(unique(data.i$stratum)) == 1 & length(unique(data.i$mdcaty) == 1)) { # unstratified and equal area
    framesizeAdj.i <- distinct(data.i, Area_km2) %>% select(Area_km2)
    framesizeAdj.i <- framesizeAdj.i[ , "Area_km2"]  
    attributes(framesizeAdj.i) <- NULL
    names(framesizeAdj.i) <- c("None")
  }
  
  if(length(unique(data.i$stratum)) > 1 & length(unique(data.i$mdcaty) == 1)) { # stratified, equal
    
    framesizeAdj.ow <- filter(data.i, stratum == "open_water") %>%
      distinct(Area_km2) %>% select(Area_km2)
    
    framesizeAdj.trib <- filter(data.i, stratum == "trib") %>%
      distinct(Area_km2) %>% select(Area_km2)
    
    framesizeAdj.i <- c(framesizeAdj.ow[1,1], framesizeAdj.trib[1,1])
    attributes(framesizeAdj.i) <- NULL
    names(framesizeAdj.i) <- c("open_water", "trib")
  }
  
  if(length(unique(data.i$stratum)) >= 1 & length(unique(data.i$mdcaty)) > 1) { # stratified or unstratified (CharlesM,Hocking), unequal
    nSection.i <- length(unique(data.i$section))
    section.i <- unique(data.i$section)
    
    framesizeList <- list()
    for (j in 1:nSection.i) {
      framesizeList[[j]] <- filter(data.i, section == section.i[j]) %>%
        distinct(Area_km2) %>% select(Area_km2)
    }
    
    framesizeAdj.i <- do.call("rbind", framesizeList)
    
    framesizeAdj.i <-  framesizeAdj.i[ , "Area_km2"]
    attributes(framesizeAdj.i) <- NULL
    names(framesizeAdj.i) <- section.i
  }
  
  # 5. Adjust weights
  adjustedWgt.i <- spsurvey::adjwgt(sites.adj.i, wgtAdj.i, wgtCat.i, framesizeAdj.i)
  
  myWgtList[[i]] <- data.frame(
    Lake_Name = data.i$Lake_Name,
    siteID = data.i$siteID,
    adjWgt = adjustedWgt.i
  )
}

# 6. Verify that all lakes have >=15 sites with non zero weights
lapply(myWgtList, function(x) summarize(x,n = sum(x$adj > 0)))


# 7. Incorporate adjusted weights into eqAreaData
wgtAdjDf <- do.call("rbind", myWgtList)  # Coerces list into dataframe.
wgtAdjDf[, c("Lake_Name", "siteID")] <- lapply(wgtAdjDf[, c("Lake_Name", "siteID")], 
                                               as.character) # convert to character

#str(eqAreaData) #105 observations  # may need to merge with something else after ebulition calcs finished
#str(wgtAdjDf)  # 105 observations
eqAreaData <- merge(eqAreaData, wgtAdjDf) #105 observations


###### CALCULATE LAKE MEAN AND VARIANCE FROM GRTS DESIGN


# Loop to apply grtsMeanVariance function to each lake.
myMeanVarianceList <- list() # empty list to catch mean and variance

for (i in 1:length(unique(eqAreaData$Lake_Name))) {
  lake.i <- unique(eqAreaData$Lake_Name)[i]
  data.i <- filter(eqAreaData, Lake_Name == lake.i)#, siteID !="U-06", siteID!="U-09")
  
  myMeanVarianceList[[i]] <- grtsMeanVariance(data.i)  # this function is sourced from masterLibrary.R
  myMeanVarianceList[[i]]$Pct$Lake_Name = lake.i  # add lake name to dataframe!
}


# Extract portion of interest from list components  
myMeanVarianceList <- lapply(myMeanVarianceList, function(x) {  # apply function to each list element
  filter(x$Pct, Statistic == "Mean") %>%  # Pct is the portion we want
    select(Lake_Name, Subpopulation, Indicator, Estimate, LCB95Pct, UCB95Pct, StdError) %>%
    mutate(StdError = as.numeric(StdError)) # Comes out as a char of class "Asis"?
})


# Coerce to df, format
meanVariance <- do.call("rbind", myMeanVarianceList)  # coerce to df
meanVariance[ , c("Subpopulation", "Indicator")] = apply(meanVariance[ , c("Subpopulation", "Indicator")], MARGIN = 2, FUN = as.character)


# Melt/dcast for plotting
meanVariance.m <- reshape2::melt(meanVariance)  # specify package.  reshape and reshape2 loaded
meanVariance.c <- dcast(meanVariance.m, formula = Lake_Name + Subpopulation ~ Indicator + variable) # cast

# Add sample date to meanVariance.c
sample.dates <- select(eqAreaData, Lake_Name, deplyDt) %>% 
  distinct(Lake_Name, deplyDt) %>% 
  filter(!is.na(deplyDt))
meanVariance.c <- merge(meanVariance.c, sample.dates)


rm(myWgtList, myMeanVarianceList)

