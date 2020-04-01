#load libraries:
source("scripts/masterLibrary.R")

source("scripts/def.calc.sdg.R")
  #load GLEON dissolved & saturated gas concentration code 

source("scripts/compileGCdata.R")  
  #loads GC result output & data sheet with info on 
  #dissolved gas and trap sample field data
  #puts them together into five dataframes called 
  #actonDgJoin, actonTrapJoin, dockAmbientAir, metaDataDCact, metaDataTrapAct
  #also calculates actonTrapAgg with aggregated mean and sd trap GHG #s

source("scripts/readData.R")
  #reads in LGR data: gga
  #eddyPro data,
  #vanni met station data
  #shallow site thermistor string data (RBR)
  #deep site thermistor string data (buoy T)
  #sonde data from deep site
  #auxiliary met data from tower site
  #active funnel trap data (hobo)
#loads data into dataframes: 
#DISSOLVED GAS: actonDgJoin, actonTrapJoin, actonTrapAgg
#LGR CHAMBER: gga
#EDDY COVARIANCE: epOutOrder
#METEOROLOGICAL & WATER T: vanni30min, rbrTsub, rbrDaily, buoyT30min, buoyTdaily, U12sonde, campMet
#ACTIVE BUBBLE TRAPS: hobo


rm(epList, epFiles, txtFiles2017, txtFiles2018)  

###DATA PROCESSING###
#turn raw data into data products: dissolved/sat gas, chamber fluxes

source("scripts/dissolvedGasCalc.R")
#takes actonDgJoin, reformats it and uses the GLEON code to 
#calculate the dissolved/sat gas concentrations, stored
#in data frame "actonDGoutput"

source("scripts/plotCleanLgr.R")
#reads in the field data from the file "chamberBiweekly.xlsx"
#optimizes start and end times for the chamber calcs

source("scripts/calculateChamberEmissions.R")
#produces chamData and chamDataSub

source("scriptsAndRmd/calculateEbEmissions.R")
#calculates time series of ebullition emissions
#from the active trap data
#remove non longer needed data frames and lists:

source("scriptsAndRmd/qcEddyPro.R") 
#makes epOutSub, filters data by QC parameters
#makes DailyEcFluxes and MonthlyCh4

#ADDED FEB 2019:
#makes the qa-filtered, continuous, labeled data frame "epREddy" 
#to be used in the
#rEddyProc.R script to gap-fill with MDC

rm(vanniMet, txtFilesSize, OUT, rbrT, ggaGRTS1, 
   gga.model,gga.i,ep.i, data.i.co2, data.i.ch4, data.i, 
   buoyT, adjDataDf)
rm(ch4.ex.pred, chmVol.L.i, co2.ex.pred, gga, 
   ggaList, dupes)
rm(metaDataTrap, metaDataDG)

file.edit('scriptsAndRmd/rEddyProc.R') #mean diurnal course gap-filling for LE, H, ustar 
#preps input into ANN
file.edit('scriptsAndRmd/ANN/evaluateANN2019.R') 
## get the gap-filled data
## make Figure 8 (VIF)

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

file.edit('scriptsAndRmd/fluxTmprPlots.R')
## Multi-panel plots showing Q10 and 2DKS


file.edit('scriptsAndRmd/ecFluxAnalysisPlots.R')

file.edit('scriptsAndRmd/diurnalAnalysis.R') #makes diurnal pdfs, Figure 11
file.edit('scriptsAndRmd/GRTS/exploratoryPlotsActonGRTS.R') 
## Makes linear regression of ebullition as f(depth) and
## ratio of ebullitive FCH4:total FCH4 (Figures 5 and 6)

#run fluxTmprPlots to make plots used in AGU poster
# cumulativeTS.R is the script used to transform time series flux data into 
##### cumulative emission estimates



#gapfilling scripts:

