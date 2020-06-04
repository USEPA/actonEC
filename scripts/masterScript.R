#load libraries:
source("scripts/masterLibrary.R")

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

source("scripts/readData.R")
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
source("scripts/grtsCalcEmissions.R") #calculates diffusive, ebullitive, and total emissions for each site
source("scripts/grtsLakeCalcs.R") #
file.edit("scripts/lakeReportActon2017.Rmd")  ## readOGR can't find features

#### Time series measurement processing:
source("scripts/dissolvedGasCalc.R")
#takes actonDgJoin, reformats it and uses the GLEON code to 
#calculate the dissolved/sat gas concentrations, stored
#in data frame "actonDGoutput"

source("scripts/plotCleanLgr.R")
#reads in the field data from the file "chamberBiweekly.xlsx"
#optimizes start and end times for the chamber calcs

source("scripts/calculateChamberEmissions.R")
#produces chamData with all columns,
#and chamDataSub : "chmDeplyDtTm","siteID", "ch4.drate.mg.h.best" "year", monthday      

source("scripts/calculateEbullition.R")
#calculates time series of ebullition emissions
#from the active trap data
#characterizes uncertainty in volume calc (after Varadharjan et al), and due to uncertainty in [CH4]

source("scripts/qcECfluxes.R") 
#makes epOutSub, filters data by QC parameters
#makes DailyEcFluxes 


### In the initiall data processing, this is where the ANN runs occurred
### These don't need to be duplicated each time, but the scripts are 
### included here:
#file.edit('scripts/ANN/rEddyProc.R') #mean diurnal course gap-filling for LE, H, ustar 
## preps input into ANN
#file.edit('scripts/ANN/evaluateANNs.R') 
## get the gap-filled data
## make Figure 8 (VIF)

##### DATA VISUALIZATION SCRIPTS #####

file.edit("scripts/plotTimeSeries.R")
file.edit("scripts/metPlotsFig3.R")
file.edit("scripts/plotCumulativeTS.R")
file.edit('scriptsAndRmd/fluxTmprPlots.R')
  ## Multi-panel plots showing Q10 and 2DKS
file.edit("scripts/calcsForTable2.R")

#remove non longer needed data frames and lists:
rm(vanniMet, txtFilesSize, OUT, rbrT, ggaGRTS1, 
   gga.model,gga.i,ep.i, data.i.co2, data.i.ch4, data.i, 
   buoyT, adjDataDf)
rm(ch4.ex.pred, chmVol.L.i, co2.ex.pred, gga, 
   ggaList, dupes)
rm(metaDataTrap, metaDataDG)



## Figures:
file.edit('scriptsAndRmd/edi.256.1.r') #vanni stream gauge data, figure 2 d
file.edit('scriptsAndRmd/metPlotsFig2.R')#figure 2 a-c, d-e
file.edit('scriptsAndRmd/hydroDynamicsVanniBuoy.R') #Figure 2 e&f
file.edit('scriptsAndRmd/cumulativeTS.R')
## Multi-panel time series figure showing:
##  - 30 min measured, 30 min gap-filled, and daily avg EC results
##  - 2 hr measured AFT, daily avg AFT + interpolated chamber for U-14
##  - 2 hr measured AFT, daily avg AFT + interpolated chamber for U-12
##  - GRTS results
##  - AKA Figure 3
## Two-panel figure showing 2017 and 2018 cumulative FCH4 (Fig 7)




file.edit('scriptsAndRmd/ecFluxAnalysisPlots.R')

file.edit('scriptsAndRmd/diurnalAnalysis.R') #makes diurnal pdfs, Figure 11
file.edit('scriptsAndRmd/GRTS/exploratoryPlotsActonGRTS.R') 
## Makes linear regression of ebullition as f(depth) and
## ratio of ebullitive FCH4:total FCH4 (Figures 5 and 6)

#run fluxTmprPlots to make plots used in AGU poster
# cumulativeTS.R is the script used to transform time series flux data into 
##### cumulative emission estimates



#gapfilling scripts:

