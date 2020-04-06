# LIBRARIES---------------
perl <- "C:/Strawberry/perl/bin/perl.exe"
library(readxl)  # For reading Excel files
library(gdata)   # Also for reading Excel files
library(ggplot2) # For plotting
library(gridExtra) # For plotting
library(scales)  # For plotting
library(reshape) # For merge_recurse function
library(reshape2) # For melt/dcast
library(tidyr)  # for separate

library(knitr)   # To knit rmarkdown document
library(ggmap)   # For ggmap plot of reservoirs
library(maptools) # for ggplot plotting of shapefile (fortify function)
library(minpack.lm) # for non linear diffusion model
# http://www.statmethods.net/stats/rdiagnostics.html)
library(car) # vif function
library(fmsb) # variance inflation factor 'VIF' function
library(relaimpo)  # dependent on MASS, which masks dplyr select
library(nlme) # for gls function
library(piecewiseSEM) # for rsquared of lme model

# Always load dplyr after plyr and relaimpo!  These packages mask
# dplyr functions.
library(plyr)  # for 'join' in ggplot plotting of shapefile
library(dplyr)   # For data manipulation
library(tidyverse)

library(chron)
library(plotly)
library(lubridate)
library(zoo)
library(REddyProc)
library(openair)
library(wesanderson)

#threshold test attempts
#library(Peacock.test)
#library(tree)

# TRIM FUNCTION--------------------------
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


# GRTS ANALYSIS FUNCTION------------------
# Analyze continuous variable from grts survey design.
grtsMeanVariance <- function(x) {
  
  # FIRST, DEFINE FRAMESIZE.  DEPENDS ON WHETHER STRATIFIED OR NOT.
  if(length(unique(x$stratum)) == 1) {  # if unstratified
    framesize.tmp = select(x, Area_km2) %>% distinct(Area_km2)
    framesize <- c("lake" = sum(framesize.tmp$Area_km2)) # sum needed to accomodate multiple mdcaty
  }
  
  if(length(unique(x$stratum)) > 1) {  # if stratified
    owFramesize.tmp <- filter(x, stratum == "open_water") %>%
      select(Area_km2) %>% distinct(Area_km2)
    owFramesize <- sum(owFramesize.tmp$Area_km2) # sum needed to accomodate multiple mdcaty
    
    tribFramesize.tmp <- filter(x, stratum == "trib") %>%
      select(Area_km2) %>% distinct(Area_km2)
    tribFramesize <- sum(tribFramesize.tmp$Area_km2) # sum needed to accomodate multiple mdcaty
    
    framesize <- c("open_water" = owFramesize, "trib" = tribFramesize)
  }
  
  
  # DEFINE NUMBER OF ROWS IN DATAFRAME
  nRows <- nrow(x)
  
  
  
  # CREATE SITES DATAFRAME
  sites <- data.frame(siteID=x$siteID,
                      Use=x$EvalStatus == "sampled")  # use sampled sites
  
  
  
  # SUBPOP DATAFRAME
  if(length(unique(x$stratum)) == 1) {  # if unstratified
    subpop <- data.frame(siteID=x$siteID,
                         lake=rep("lake", nRows))
  }
  
  if(length(unique(x$stratum)) > 1) {  # if stratified
    subpop <- data.frame(siteID=x$siteID,
                         lake=rep("lake", nRows),
                         stratum=x$stratum)
  }
  
  
  # DESIGN DATAFRAME
  design <- data.frame(siteID=x$siteID,
                       wgt=x$adjWgt,
                       xcoord=x$xcoord,
                       ycoord=x$ycoord)
  
  
  # DATA.CONF data frame.
  data.cont <- data.frame(siteID = x$siteID,
                          ebMlHrM2 = x$ebMlHrM2, # volume of gas in trap
                          #need to temporarily comment the following out; were breaking the code
                          #chla = x$chla_S,  
                          #tp = x$TP,
                          #tn = x$TN,
                          #tnh4 = x$TNH4,
                          #tno2 = x$TNO2,
                          trap_ch4.ppm = x$trap_ch4.ppm,
                          #tno2-3 = x$TNO-3, # this breaks code.  need to remove dash
                          ch4.drate.mg.m2.h = x$ch4.drate.mg.h.best,
                          co2.drate.mg.m2.h = x$co2.drate.mg.h.best,
                          ch4.erate.mg.h = x$ch4.erate.mg.h,
                          co2.erate.mg.h = x$co2.erate.mg.h,
                          n2o.erate.mg.h = x$n2o.erate.mg.h,
                          co2.trate.mg.h = x$co2.trate.mg.h,
                          ch4.trate.mg.h = x$ch4.trate.mg.h)
  
  
  # CALCULATE CDF ESTIMATES
  if(length(unique(x$stratum)) == 1) {  # if unstratified
    cdf.final <- spsurvey::cont.analysis(sites, subpop, design, data.cont,
                               popsize=list(lake=sum(framesize)))
  }
  
  if(length(unique(x$stratum)) > 1) {  # if stratified
    cdf.final <- spsurvey::cont.analysis(sites, subpop, design, data.cont,
                               popsize=list(lake=sum(framesize),
                                            stratum=as.list(framesize)))
  }
  cdf.final
}


# EBULLITION MASS FLUX FUNCTION------------------------

# Function for calculating mass flux rate--                  
mass.rate <- function(X1, choice1){
  # trap gas data to use if measured values aren't available
  trap_ch4.ppm <- ifelse(is.na(X1$trap_ch4.ppm), mean(X1$trap_ch4.ppm, na.rm=TRUE), X1$trap_ch4.ppm) 
  trap_co2.ppm <- ifelse(is.na(X1$trap_co2.ppm), mean(X1$trap_co2.ppm, na.rm=TRUE), X1$trap_co2.ppm)
  trap_n2o.ppm <- ifelse(is.na(X1$trap_n2o.ppm), mean(X1$trap_n2o.ppm, na.rm=TRUE), X1$trap_n2o.ppm)
  
  #trap_ch4.ppm <-  X1$trap_ch4.ppm
  #trap_co2.ppm <-  X1$trap_co2.ppm
  #trap_n2o.ppm <-  X1$trap_n2o.ppm
  
  # barometric pressure needed: n=PV/RT
  bp <- ifelse(is.na(mean(X1$BrPrssr, na.rm=TRUE)),
               1,
               mean(X1$BrPrssr, na.rm=TRUE)/760)
  
  # temperature needed
  gas.temp <- ifelse(is.na(X1$Tmp_C_S),
                     273.15 + 20, # assume 20C if not measured
                     273.15 + X1$Tmp_C_S)
  
  # convert 1mL to moles. 0.082058 is the gas constant in units of L atm mol^-1 K^-1
  mL.to.mmoles <- ((bp*0.001)/(0.082058 * gas.temp)) * 1000     #1mL = 0.001L; *1000 to convt to mmol       
  
  # convert mmoles to mg
  if(choice1 == "ch4") {mg.gas <- mL.to.mmoles * 16 * (trap_ch4.ppm/1000000)}  #16mg/mmole
  if(choice1 == "co2") {mg.gas <- mL.to.mmoles * 44 * (trap_co2.ppm/1000000)}  #44mg/mmole
  if(choice1 == "n2o") {mg.gas <- mL.to.mmoles * 44 * (trap_n2o.ppm/1000000)}  #44mg/mmole
  
  # calculate rate
  mass.flux.rate <- mg.gas * X1$ebMlHrM2 #bubble rate in mg ch4-co2-n2o /hr/m2 (NOT per day -- changed this comment on 4/11/2018 SW)
  
  # return mass flux rate in mg ch4-co2-n2o /HR/m2
  mass.flux.rate
}


# ORDER Lake_Name FUNCTION-------------------------
orderLake <- function(x, choice1) {
  if(choice1 == "ch4.d") {
    column <- "ch4.drate.mg.m2.h_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  } else if(choice1 == "ch4.e") {
    column <- "ch4.erate.mg.h_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  } else if(choice1 == "ch4.t") {
    column <- "ch4.trate.mg.h_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  } else if(choice1 == "co2.d") {
    column <- "co2.drate.mg.m2.h_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  } else if(choice1 == "co2.e") {
    column <- "co2.erate.mg.h_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  } else if(choice1 == "co2.t") {
    column <- "co2.trate.mg.h_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  } else if (choice1 == "vol") {
    column <- "ebMlHrM2_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  } else if (choice1 == "ch4.ppm.trap") {
    column <- "trap_ch4.ppm_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  } else if (choice1 == "chl") {
    column <- "chla_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  } else if (choice1 == "TP") {
    column <- "tp_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  } else if (choice1 == "TN") {
    column <- "tn_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)
  } 
}

# ORDER siteID FUNCTION-------------------------
orderSite <- function(x, choice1) {
  if(choice1 == "ch4.d") {
    column <- "ch4.drate.mg.h.best"
    orderList <- order(x[, column])
    siteLevels <- x[orderList, "siteID"]
    factor(x$siteID, levels = siteLevels)
  } else if(choice1 == "ch4.e") {
    column <- "ch4.erate.mg.h"
    orderList <- order(x[, column])
    siteLevels <- x[orderList, "siteID"]
    factor(x$siteID, levels = siteLevels)
  } 
  else if(choice1 == "co2.d") {
    column <- "co2.drate.mg.h.best"
    orderList <- order(x[, column])
    siteLevels <- x[orderList, "siteID"]
    factor(x$siteID, levels = siteLevels)
  } 
}


# Custom variance inflation factor from R-bloggers----------------------
# See blog at https://www.r-bloggers.com/collinearity-and-stepwise-vif-selection/
# Can source function from gist; but can't get to work
"https://gist.githubusercontent.com/fawda123/4717702/raw/a84b776b6145c9c8f2adf93de517eab97d42cfa9/vif_fun.r"
# Define function here
vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]))
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}


# VIF FUNCTION FROM HIGHLAND STATS---------------
#Library files for courses provided by: Highland Statistics Ltd.
#To cite these functions, use:
#Mixed effects models and extensions in ecology with R. (2009).
#Zuur, AF, Ieno, EN, Walker, N, Saveliev, AA, and Smith, GM. Springer.

#Copyright Highland Statistics LTD.

#####################################################################
#VIF FUNCTION.
#To use:  corvif(YourDataFile)
corvif <- function(dataz) {
  dataz <- as.data.frame(dataz)
  #correlation part
  #cat("Correlations of the variables\n\n")
  #tmp_cor <- cor(dataz,use="complete.obs")
  #print(tmp_cor)
  
  #vif part
  form    <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
  dataz   <- data.frame(fooy=1,dataz)
  lm_mod  <- lm(form,dataz)
  
  cat("\n\nVariance inflation factors\n\n")
  print(myvif(lm_mod))
}


#Support function for corvif. Will not be called by the user
myvif <- function(mod) {
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) stop("The model contains fewer than 2 terms")
  if (length(assign) > dim(v)[1] ) {
    diag(tmp_cor)<-0
    if (any(tmp_cor==1.0)){
      return("Sample size is too small, 100% collinearity is present")
    } else {
      return("Sample size is too small")
    }
  }
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- data.frame(GVIF=result[, 1])
  } else {
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  invisible(result)
}
#END VIF FUNCTIONS


# LAKE ABBREVIATIONS---------------------
# Unique ID for Chem samples includes a 3-letter code for Lake_Name.
# The following key can be used to translate.



###From Will Barnett's ANN prep script:
## Function to look at how many 30-minute gaps exist per day
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

