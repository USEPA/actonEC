

#set path to project directory:
projectWD<-"/Users/swaldo/Documents/epaComputer/C_R_Projects/actonEC"

#load libraries:
source("scripts/masterLibrary.R")

source("scripts/def.calc.sdg.R")
#load GLEON dissolved & saturated 
#gas concentration code 

####Parallel processing of both time series and GRTS survey data
####in approximate order of producing all of the manuscript figures

### Load in data -----
source("scripts/compileGCdata.R")  
  #loads GC result output & data sheet with info on 
  #dissolved gas and trap sample field data
  #DISSOLVED GAS: actonDgJoin, actonTrapJoin, actonTrapAgg
  #puts them together into five dataframes called 
  #actonDgJoin, actonTrapJoin, dockAmbientAir, metaDataDCact, metaDataTrapAct
  #also calculates actonTrapAgg with aggregated mean and sd trap GHG #s
source("scripts/readData.R") #takes ~1 min
  #reads in and formats data frames for:
  #LGR CHAMBER: gga
  #EDDY COVARIANCE (eddyPro output): epOutOrder
  #ACTIVE FUNNEL TRAPS: hobo
  #METEOROLOGICAL: vanni30min (vanni met station data), campMet (aux met data from tower)
  #WATER T: rbrTsub, rbrDaily,(shallow site thermistor string data); buoyT30min, buoyTdaily (deep site thermistor string data)
  #SONDE DATA: U12sonde (sonde data from deep site)
source("scripts/GRTSscripts/grtsReadSiteData.R") 
  #reads in GIS files with survey info
  #reformats and saves the GIS file info into "eqAreaData" dataframe

###DATA PROCESSING### ----
  #turn raw data into data products: dissolved/sat gas, chamber fluxes
### GRTS survey processing:
plotCO2CH4profiles <- "no" ## set to "yes" if you want to make the pdf file of the CO2 and CH4 
                           ## time series for inspection: figures/ggaProfileGRTS.pdf  
source("scripts/GRTSscripts/grtsLGRplotClean.R") #preps gga data files from surveys

source("scripts/GRTSscripts/grtsCalcEmissions.R") #calculates diffusive, ebullitive, and total emissions for each site,
                                      # option to make pdf file: pdf("figures/curveFitsGRTS.pdf")
source("scripts/GRTSscripts/grtsLakeCalcs.R") #calculates mean and 95% CI for whole lake

#### Time series measurement processing:
source("scripts/dissolvedGasCalc.R")
  #takes actonDgJoin, reformats it and uses the GLEON code to 
  #calculate the dissolved/sat gas concentrations, stored
  #in data frame "actonDGoutput"
  #plots SI figure 4, time series of dissolved CH4 depth profile at the deep site

plotCO2CH4profilesBiweekly <- "no" ## set to "yes" if you want to make the pdf file of the CO2 and CH4 
## time series for inspection: figures/ggaBiweeklyProfile.pdf  
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

### The F_CH4 time series was gap filled using an Artificial Neural Network (ANN).
### You can run the code in this section to run the gap-filling, 
### OR you can proceed to the plotTimeSeriesFig2.R script, which loads the file "dataL2/gapFilledEC_results.csv"

### The ANN uses the drivers: overlying static pressure, change in static pressure, sediment temperature (sedT), 
### air temperature, latent heat flux (LE), sensible heat (H), wind speed, 
### ustar (friction velocity, a measure of turbulence), and photosynthetically active radiation.

### The driver time series must not have any gaps when running the ANN
### We used mean diurnal course to gap fill LE, H, and ustar:
file.edit('scripts/ANNscripts/mdc_gapfilling.R')  #mean diurnal course gap-filling for LE, H, ustar 
### and linear interpolation for P, deltaP, T, WS, and PAR:
file.edit('scripts/ANNscripts/linearInterpolation.R')

## preps input into ANN


##### DATA VISUALIZATION SCRIPTS #####

file.edit("scripts/dataVisScripts/plotTimeSeriesFig2.R") #sets up and plots Figure 2: reads in ANN gapfilled data, plots time series of FCH4 measured by EC, AFTs, and surveys
file.edit("scripts/dataVisScripts/metPlotsFig3.R") # sets up and plots Figure 3 a-e: temperature, H & LE, precip, inflow, water level  
file.edit('scripts/dataVisScripts/miamiHydroFig3.R') #Figure 3 f, g
file.edit('scripts/dataVisScripts/fluxTmprPlotsFig4.R') ## Multi-panel plots showing Q10 and 2DKS, Fig 4
file.edit("scripts/dataVisScripts/figure5_lollipopPlots.Rmd")  ## makes plots for figure 5
file.edit("scripts/dataVisScripts/ebPlotsFigs6_7.R")
file.edit("scripts/dataVisScripts/plotCumulativeTSFig9.R")
file.edit('scripts/ANNscripts/evaluateANNs.R') #make Figure 9 (VIF)
file.edit("scripts/dataVisScripts/plotFig10.R") 
file.edit("scripts/dataVisScripts/miamiChlFig11b.R") #load chl data and make Fig 11b

file.edit('scriptsAndRmd/dataVisScripts/diurnalAnalysisFig12.R') #makes diurnal pdfs, Figure 12



## remove non longer needed data frames and lists:
rm(vanniMet, txtFilesSize, OUT, rbrT, ggaGRTS1, 
   gga.model,gga.i,ep.i, data.i.co2, data.i.ch4, data.i, 
   buoyT, adjDataDf)
rm(ch4.ex.pred, chmVol.L.i, co2.ex.pred, gga, 
   ggaList, dupes)
rm(metaDataTrap, metaDataDG)




