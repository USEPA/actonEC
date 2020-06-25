#load libraries:
source("scripts/masterLibrary.R")
projectWD<-"/Users/swaldo/Documents/epaComputer/C_R_Projects/actonEC"

source("scripts/def.calc.sdg.R")
#load GLEON dissolved & saturated gas concentration code 

####Parallel processing of both time series and GRTS survey data
####in approximate order of producing all of the manuscript figures

### Load in data
source("scripts/compileGCdata.R")  
  #loads GC result output & data sheet with info on 
  #dissolved gas and trap sample field data
  #DISSOLVED GAS: actonDgJoin, actonTrapJoin, actonTrapAgg
  #puts them together into five dataframes called 
  #actonDgJoin, actonTrapJoin, dockAmbientAir, metaDataDCact, metaDataTrapAct
  #also calculates actonTrapAgg with aggregated mean and sd trap GHG #s
source("scripts/readData.R") #takes ~1 min
  #reads in LGR CHAMBER: gga
  #EDDY COVARIANCE (eddyPro output): epOutOrder
  #ACTIVE FUNNEL TRAPS: hobo
  #METEOROLOGICAL: vanni30min (vanni met station data), campMet (aux met data from tower)
  #WATER T: rbrTsub, rbrDaily,(shallow site thermistor string data); buoyT30min, buoyTdaily (deep site thermistor string data)
  #SONDE DATA: U12sonde (sonde data from deep site)
source("scripts/grtsReadSiteData.R") #reads in GIS eqArea files with survey info

###DATA PROCESSING###
  #turn raw data into data products: dissolved/sat gas, chamber fluxes
### GRTS survey processing:
source("scripts/grtsLGRplotClean.R") #preps gga data files from surveys
source("scripts/grtsCalcEmissions.R") #calculates diffusive, ebullitive, and total emissions for each site,
                                      # makes pdf file: pdf("figures/curveFitsGRTS.pdf")
source("scripts/grtsLakeCalcs.R") # calculates mean and 95% CI for whole lake

#### Time series measurement processing:
source("scripts/dissolvedGasCalc.R")
  #takes actonDgJoin, reformats it and uses the GLEON code to 
  #calculate the dissolved/sat gas concentrations, stored
  #in data frame "actonDGoutput"
  #plots SI figure 4, time series of dissolved CH4 depth profile at the deep site
source("scripts/plotCleanLgr.R")
  #reads in the field data from the file "chamberBiweekly.xlsx"
  #optimizes start and end times for the chamber calcs
source("scripts/calculateChamberEmissions.R")
  #produces chamData with all columns,
  #and chamDataSub : "chmDeplyDtTm","siteID", "ch4.drate.mg.h.best" "year", monthday      
source("scripts/calculateEbullition.R")
  #calculates time series of ebullition emissions from the active trap data
  #characterizes uncertainty in volume calc (after Varadharjan et al), and due to uncertainty in [CH4]
source("scripts/qcECfluxes.R") 
  #makes epOutSub, filters data by QC parameters
  #makes DailyEcFluxes 

### ANN Gap Filling ##########
### In the initiall data processing, this is where the ANN runs occurred
### These don't need to be duplicated each time, but the scripts are 
### included here:
#file.edit('scripts/ANN/rEddyProc.R') #mean diurnal course gap-filling for LE, H, ustar 
## preps input into ANN


##### DATA VISUALIZATION SCRIPTS #####
file.edit("scripts/calcsForTable2.R")
file.edit("scripts/plotTimeSeriesFig2.R") #sets up and plots Figure 2: time series of FCH4 measured by EC, AFTs, and surveys
file.edit("scripts/metPlotsFig3.R") # sets up and plots Figure 3 a-e
#file.edit('scripts/miamiInflowFig3d.R') #Loads vanni stream gauge data plots Figure 3 d
file.edit('scripts/miamiHydroFig3.R') #Figure 3 f, g
file.edit('scripts/fluxTmprPlotsFig4.R') ## Multi-panel plots showing Q10 and 2DKS, Fig 4
file.edit("scripts/figure5_lollipopPlots.Rmd")  ## makes plots for figure 5
file.edit("scripts/ebPlotsFigs6_7.R")
file.edit("scripts/plotCumulativeTSFig8.R")
file.edit('scripts/ANNscripts/evaluateANNs.R') #make Figure 9 (VIF)
file.edit("scripts/plotFig10.R") 
file.edit("scripts/miamiChlFig11b.R") #load chl data and make Fig 11b

file.edit('scriptsAndRmd/diurnalAnalysis.R') #makes diurnal pdfs, Figure 12

## remove non longer needed data frames and lists:
rm(vanniMet, txtFilesSize, OUT, rbrT, ggaGRTS1, 
   gga.model,gga.i,ep.i, data.i.co2, data.i.ch4, data.i, 
   buoyT, adjDataDf)
rm(ch4.ex.pred, chmVol.L.i, co2.ex.pred, gga, 
   ggaList, dupes)
rm(metaDataTrap, metaDataDG)




