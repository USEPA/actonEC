#Adapted from the code Jake Beaulieu developed for the 32 reservoir survey. 

# THIS CODE WILL BE USED TO READ THE POPULATED SHAPEFILES
# WE REALLY DON'T NEED THE FULL SHAPEFILE FOR THE ANALYSIS, WE WILL
# JUST WORK WITH THE .dbf FILE.

# READ IN .dbf FILES-------------------

# 1. Create a list of files to read in.  The completed data files all
# contain the pattern ...SitesEqAreaData.dbf and are saved in data/grtsEqArea

rootDir<-"data/grtsEqArea/"

fileNames <- list.files(path = rootDir, 
                        pattern = "SitesEqAreaDataEC.dbf", # file names containing this pattern
                        recursive = TRUE) # look in all subdirectories


# 2.  Read in files
mylist <- list()  # Create an empty list to hold data

for (i in 1:length(fileNames)){  # for each file
  # read.dbf is in foreign and spsurvey.  The foreign version allows for
  # use of as.is argument.
  data.i <- foreign::read.dbf(paste(rootDir, fileNames[i], sep = ""),
                              as.is=TRUE)
  mylist[[i]] <- data.i
}


# 3. Strip column names that are not consisent (or necessary) across different .dbf
# files.  Inconsistency related to source of original GIS shapefile.

# Vector of columns to remove
columnsToRemove <- c("OBJECTID|Permanent_|FDate|Resolution|GNIS_ID|Elevation|ReachCode|FType|FCode|Connectivi|Issue_Type|Lake_Name_|Reservoir_|QC")

# remove columns from all dfs in list
mylist1 <- lapply(mylist, function(x) dplyr::select(x, -matches(columnsToRemove))) # matches allows for multiple terms

# Some dfs in list have column name "GNIS_Name".  Rename to Lake_Name where it appears. 
# ;x needed to have function report whole df.
mylist2 <- lapply(mylist1, function(x) {names(x) <- sub("GNIS_Name", "Lake_Name", names(x));x})

# Add 'section' as column, if not already present.  This happens in equal area designs
mylist3 <- lapply(mylist2, function(x){
  if("section" %in% names(x))  # if 'section' already exists
    x  # then report original df
  else 
    cbind(x, section = NA) # if 'section' doesn't exist, report new column of NAs
})

# 4.  Arrange columns in identical way to facilitate rbind
mylist4 <- lapply(mylist3, function(x) {
  dplyr::select(x, noquote(order(colnames(x))))} # sort colnames alphabetically
) 

# 5.  Coerce list into dataframe via rbind
eqAreaData <- do.call("rbind", mylist4)  # Coerces list into dataframe.

rm(mylist, mylist1, mylist2, mylist3, mylist4)

#the chamber msmts were taken on the retreival day during the 
#2018 July and Sept GRTS surveys. Program this in:
eqAreaData$chamDay<-ifelse(eqAreaData$Lake_Name=="Acton Lake 2018 July" | 
                             eqAreaData$Lake_Name == "Acton Lake 2018 Sept",
                           "day 2", #value if true
                           "day 1" ) #value if false

#head(filter(eqAreaData, Lake_Name=="Acton Lake 2018 Sept"))

# FORMAT DATAFRAME-----------
eqAreaData <- mutate(eqAreaData,
                     chmDeplyDtTm = as.POSIXct(paste(trim(deplyDt), # trim removes white space
                                                     trim(chmStTm), sep=""),
                                               format = "%m/%d/%Y%H:%M",
                                               tz="UTC"), # set tz!
                     trapDeplyDtTm = as.POSIXct(paste(trim(deplyDt), # trim removes white space
                                                      trim(deplyTm), sep=""),
                                                format = "%m/%d/%Y%H:%M",
                                                tz="UTC"),
                     trapRtrvDtTm = as.POSIXct(paste(trim(RtrvDat), # trim removes white space
                                                     trim(RtrvTim), sep=""),
                                               format = "%m/%d/%Y%H:%M",
                                               tz="UTC"))  
for(i in 1:nrow(eqAreaData)){
  if(eqAreaData$chamDay[i]=="day 2"){
    eqAreaData$chmDeplyDtTm[i]=as.POSIXct(paste(trim(eqAreaData$RtrvDat[i]), # trim removes white space
                                                trim(eqAreaData$chmStTm[i]), sep=""),
                                          format = "%m/%d/%Y%H:%M",
                                          tz="UTC")}
  # set tz!)
}

# Columns that should be converted to numeric
cols <- c("chm_vol", "wtrDpth", "smDpthS", "Tmp_C_S", "DOPrc_S", "DO__L_S",   
          "SpCn__S", "pH_S", "ORP_S", "TrNTU_S", "chla_S", "smDpthD", "Tmp_C_D", "DOPrc_D", "DO__L_D",   
          "SpCn__D", "pH_D", "ORP_D", "TrNTU_D", "chla_D", "BrPrssr", "TtTrpVl", "LatSamp", "LongSmp")

eqAreaData[, cols] <- lapply(eqAreaData[, cols], as.numeric) # convert to numeric

# NA in character fields (i.e. TrapExtn) shapefile are being read as character values.
# Convert to NA.
eqAreaData[, "TrapExtn"] <- ifelse(eqAreaData[, "TrapExtn"] == "NA", 
                                   NA, 
                                   eqAreaData[, "TrapExtn"])

eqAreaData[, "ArExtnrs"] <- ifelse(eqAreaData[, "ArExtnrs"] == "NA", 
                                   NA, 
                                   eqAreaData[, "ArExtnrs"])

eqAreaData[, "DG_Extn"] <- ifelse(eqAreaData[, "DG_Extn"] == "NA", 
                                  NA, 
                                  eqAreaData[, "DG_Extn"])

# CHAMBER VOLUME
# Calculate chamber volume based on relationship between water level
# and volume.  See chamberDesign.xlsx in East Fork folder.
eqAreaData <- mutate(eqAreaData, chmVol.L = (42.057 + (-0.2189 * chm_vol)))

# Deal with instances where chamber volume was not recorded in field.
# 1.  A site or two missed, whereas volume recorded at most other sites.
# Caeaser Cr.
toAdjChmVol <- with(eqAreaData, EvalStatus == "sampled" & is.na(chmVol.L))
adjChmVol <- with(eqAreaData, EvalStatus == "sampled" & !is.na(chmVol.L))
estChemVol <- mean(eqAreaData[adjChmVol, "chmVol.L"])
eqAreaData[toAdjChmVol, "chmVol.L"] =  estChemVol

