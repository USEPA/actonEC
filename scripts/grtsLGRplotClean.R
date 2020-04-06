# THIS SCRIPT WILL BE USED TO PLOT AND CLEAN LGR DATA IN PREPARATION
# FOR CALCULATION OF DIFFUSIVE EMISSION RATE

#1. INSPECT INSTANCES OF NA IN GGA
# Time/date stamp first
#filter(gga, is.na(gga$RDateTime))
# there are a bunch of NA for this field, likely related to corrupt LGR files.  Will just strip out for now.
gga <- filter(gga, !is.na(gga$RDateTime))  # strip out missing RDateTime.  They complicate functions below.
str(gga)

#2.  LOOP TO ASSIGN LAKE NAME, SITEID, AND DEPLY TIME TO LGR OBSERVATIONS.
# Many rows in eqAreaData have NA for chmDeplyDtTm.  For example, at all oversample
# sites where chambers were not deployed.  We want to remove these rows, or the missing
# values complicate the loop.

missingChmDeplyDtTm <- is.na(eqAreaData$chmDeplyDtTm) # logical for missing chamber deployment times

for (i in 1:length(eqAreaData[!missingChmDeplyDtTm, "chmDeplyDtTm"])) {  # exclude rows with no deployment time
  chmDeplyDtTm.i <- eqAreaData[!missingChmDeplyDtTm, "chmDeplyDtTm"][i] # exclude rows with no deployment time, select ith observation
  Lake_Name.i <- eqAreaData[!missingChmDeplyDtTm, "Lake_Name"][i] # exclude rows with no deployment time, select ith observation
  siteID.i <- eqAreaData[!missingChmDeplyDtTm, "siteID"][i]  # exclude rows with no deployment time, select ith observation
  
  # Create logical indicator to indicate time window corresponding to
  # the ith lake and siteID.  Add 1 minute to deployment and retrieval time to expand
  # x-axis range during plotting.
  logicalIndicator.i <- gga$RDateTime > (chmDeplyDtTm.i - 60) & # 1 min < field notes
    gga$RDateTime < (chmDeplyDtTm.i + (6*60))# 6 min > field notes.  Retr time not recorded, assume 5 min after deployment
  
  gga[logicalIndicator.i, "Lake_Name"] = Lake_Name.i # Set Lake_Name
  gga[logicalIndicator.i, "siteID"] = siteID.i # Set siteID 
  gga[logicalIndicator.i, "co2DeplyDtTm"] = chmDeplyDtTm.i # Set chamber deployment time 
  gga[logicalIndicator.i, "co2RetDtTm"] = chmDeplyDtTm.i + (60*5) # Set chamber deployment time 
  gga[logicalIndicator.i, "ch4DeplyDtTm"] = chmDeplyDtTm.i # Set chamber deployment time 
  gga[logicalIndicator.i, "ch4RetDtTm"] = chmDeplyDtTm.i + (60*5) # Set chamber deployment time   
}

# POSIXct class was stripped during the loop.  Put it back in here.
gga[ , c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")] <- 
  lapply(gga[ , c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")], 
         as.POSIXct, origin = "1970-01-01 00:00:00", tz= "UTC")  # set tz!

#3. RECORD ADJUSTMENTS TO TIME SERIES PLOTS
# Lake_Name,       siteID,     co2DeplyDtTm,            co2RetDtTm,          ch4DeplyDtTm,        ch4RetDtTm
# This order is critical!
adjDataGRTS <- c("Acton Lake 2017 July", "U-14", "2017-07-10 12:34:20", "2017-07-10 12:39:00", "2017-07-10 12:34:20", "2017-07-10 12:38:40",
             "Acton Lake 2017 July", "U-09", "2017-07-10 13:56:00", "2017-07-10 13:59:00", "2017-07-10 13:56:00", "2017-07-10 13:59:00",
             "Acton Lake 2017 July", "U-05", "2017-07-10 14:09:30", "2017-07-10 14:14:00", "2017-07-10 14:09:30", "2017-07-10 14:14:00",
             "Acton Lake 2017 July", "U-16", "2017-07-10 14:25:10", "2017-07-10 14:29:00", "2017-07-10 14:25:10", "2017-07-10 14:29:00",
             "Acton Lake 2017 July", "U-13", "2017-07-10 14:35:00", "2017-07-10 14:39:00", "2017-07-10 14:35:15", "2017-07-10 14:37:00",
             "Acton Lake 2017 July", "U-01", "2017-07-10 14:49:00", "2017-07-10 14:54:00", "2017-07-10 14:49:00", "2017-07-10 14:52:00",
             "Acton Lake 2017 July", "U-18", "2017-07-10 15:02:00", "2017-07-10 15:06:00", "2017-07-10 15:03:00", "2017-07-10 15:06:00",
             "Acton Lake 2017 July", "U-07", "2017-07-10 15:14:30", "2017-07-10 15:19:00", "2017-07-10 15:14:30", "2017-07-10 15:19:00",
             "Acton Lake 2017 July", "U-11", "2017-07-10 15:31:00", "2017-07-10 15:35:00", "2017-07-10 15:31:10", "2017-07-10 15:35:00",
             "Acton Lake 2017 July", "U-15", "2017-07-10 15:45:00", "2017-07-10 15:49:00", "2017-07-10 15:45:00", "2017-07-10 15:46:15",
             "Acton Lake 2017 July", "U-08", "2017-07-10 15:57:00", "2017-07-10 16:02:00", "2017-07-10 15:58:00", "2017-07-10 15:59:30",
             "Acton Lake 2017 July", "U-12", "2017-07-10 16:36:00", "2017-07-10 16:40:00", "2017-07-10 16:36:00", "2017-07-10 16:37:40",
             "Acton Lake 2017 July", "U-04", "2017-07-10 16:44:00", "2017-07-10 16:48:40", "2017-07-10 16:44:00", "2017-07-10 16:45:30",
             "Acton Lake 2017 Aug", "U-14", "2017-08-31 11:36:00", "2017-08-31 11:40:00", "2017-08-31 11:36:00", "2017-08-31 11:40:00",
             "Acton Lake 2017 Aug", "U-17", "2017-08-31 11:53:00", "2017-08-31 11:57:00", "2017-08-31 11:53:00", "2017-08-31 11:57:00",
             "Acton Lake 2017 Aug", "U-06", "2017-08-31 12:06:20", "2017-08-31 12:08:40", "2017-08-31 12:06:15", "2017-08-31 12:08:50",
             "Acton Lake 2017 Aug", "U-09", "2017-08-31 12:16:30", "2017-08-31 12:21:00", "2017-08-31 12:16:15", "2017-08-31 12:20:30",
             "Acton Lake 2017 Aug", "U-05", "2017-08-31 12:28:20", "2017-08-31 12:32:00", "2017-08-31 12:28:00", "2017-08-31 12:32:00",
             "Acton Lake 2017 Aug", "U-16", "2017-08-31 12:42:00", "2017-08-31 12:45:40", "2017-08-31 12:42:00", "2017-08-31 12:45:40",
             "Acton Lake 2017 Aug", "U-13", "2017-08-31 12:56:30", "2017-08-31 12:59:00", "2017-08-31 12:56:30", "2017-08-31 12:58:30",
             "Acton Lake 2017 Aug", "U-01", "2017-08-31 13:10:30", "2017-08-31 13:14:40", "2017-08-31 13:10:00", "2017-08-31 13:13:00",
             "Acton Lake 2017 Aug", "U-18", "2017-08-31 13:25:00", "2017-08-31 13:28:30", "2017-08-31 13:24:00", "2017-08-31 13:28:30",
             "Acton Lake 2017 Aug", "U-11", "2017-08-31 13:50:00", "2017-08-31 13:54:00", "2017-08-31 13:50:00", "2017-08-31 13:54:00",
             "Acton Lake 2017 Aug", "U-15", "2017-08-31 14:06:00", "2017-08-31 14:10:00", "2017-08-31 14:06:00", "2017-08-31 14:08:10",
             "Acton Lake 2017 Aug", "U-08", "2017-08-31 14:23:30", "2017-08-31 14:26:00", "2017-08-31 14:23:30", "2017-08-31 14:25:30",
             "Acton Lake 2017 Aug", "U-04", "2017-08-31 14:40:50", "2017-08-31 14:43:40", "2017-08-31 14:40:30", "2017-08-31 14:43:30",
             "Acton Lake 2017 Aug", "U-12", "2017-08-31 15:23:00", "2017-08-31 15:28:00", "2017-08-31 15:23:00", "2017-08-31 15:28:00",
             "Acton Lake 2017 Oct", "U-14", "2017-10-04 11:21:00", "2017-10-04 11:24:30", "2017-10-04 11:20:40", "2017-10-04 11:21:50",
             "Acton Lake 2017 Oct", "U-17", "2017-10-04 11:46:00", "2017-10-04 11:49:00", "2017-10-04 11:46:00", "2017-10-04 11:47:00",
             "Acton Lake 2017 Oct", "U-06", "2017-10-04 12:00:40", "2017-10-04 12:05:00", "2017-10-04 12:01:00", "2017-10-04 12:05:50",
             "Acton Lake 2017 Oct", "U-09", "2017-10-04 12:14:10", "2017-10-04 12:18:30", "2017-10-04 12:14:10", "2017-10-04 12:18:30",
             "Acton Lake 2017 Oct", "U-05", "2017-10-04 12:28:00", "2017-10-04 12:32:30", "2017-10-04 12:28:00", "2017-10-04 12:30:00",
             "Acton Lake 2017 Oct", "U-16", "2017-10-04 12:40:10", "2017-10-04 12:43:00", "2017-10-04 12:40:10", "2017-10-04 12:43:00",
             "Acton Lake 2017 Oct", "U-13", "2017-10-04 13:00:00", "2017-10-04 13:02:00", "2017-10-04 13:01:00", "2017-10-04 13:02:00",
             "Acton Lake 2017 Oct", "U-01", "2017-10-04 13:24:20", "2017-10-04 13:29:00", "2017-10-04 13:24:00", "2017-10-04 13:25:20",
             "Acton Lake 2017 Oct", "U-18", "2017-10-04 13:41:00", "2017-10-04 13:43:30", "2017-10-04 13:41:00", "2017-10-04 13:43:30",
             "Acton Lake 2017 Oct", "U-07", "2017-10-04 13:49:00", "2017-10-04 13:52:00", "2017-10-04 13:49:00", "2017-10-04 13:52:00",
             "Acton Lake 2017 Oct", "U-11", "2017-10-04 14:08:00", "2017-10-04 14:10:00", "2017-10-04 14:08:10", "2017-10-04 14:10:00",
             "Acton Lake 2017 Oct", "U-15", "2017-10-04 14:24:00", "2017-10-04 14:28:30", "2017-10-04 14:24:40", "2017-10-04 14:25:30",
             "Acton Lake 2017 Oct", "U-08", "2017-10-04 14:39:10", "2017-10-04 14:41:20", "2017-10-04 14:39:50", "2017-10-04 14:40:30",
             "Acton Lake 2017 Oct", "U-12", "2017-10-04 15:15:00", "2017-10-04 15:17:20", "2017-10-04 15:16:40", "2017-10-04 15:17:20",
             "Acton Lake 2017 Oct", "U-04", "2017-10-04 15:26:00", "2017-10-04 15:29:00", "2017-10-04 15:26:25", "2017-10-04 15:27:10",
             "Acton Lake 2018 July", "U-12", "2018-07-11 11:11:00", "2018-07-11 11:14:00", "2018-07-11 11:10:30", "2018-07-11 11:12:30",
             "Acton Lake 2018 July", "U-04", "2018-07-11 11:59:00", "2018-07-11 12:01:50", "2018-07-11 11:58:50", "2018-07-11 11:59:35",
             "Acton Lake 2018 July", "U-08", "2018-07-11 12:27:00", "2018-07-11 12:28:20", "2018-07-11 12:26:40", "2018-07-11 12:27:30",
             "Acton Lake 2018 July", "U-15", "2018-07-11 12:44:30", "2018-07-11 12:48:00", "2018-07-11 12:45:00", "2018-07-11 12:46:30",
             "Acton Lake 2018 July", "U-11", "2018-07-11 13:01:00", "2018-07-11 13:03:00", "2018-07-11 13:01:00", "2018-07-11 13:04:00",
             "Acton Lake 2018 July", "U-07", "2018-07-11 13:22:00", "2018-07-11 13:25:00", "2018-07-11 13:20:40", "2018-07-11 13:21:40",
             "Acton Lake 2018 July", "U-18", "2018-07-11 13:41:30", "2018-07-11 13:45:00", "2018-07-11 13:45:00", "2018-07-11 13:45:40",
             "Acton Lake 2018 July", "U-01", "2018-07-11 14:01:00", "2018-07-11 14:05:00", "2018-07-11 14:02:40", "2018-07-11 14:03:50",
             "Acton Lake 2018 July", "U-13", "2018-07-11 14:18:00", "2018-07-11 14:20:00", "2018-07-11 14:17:40", "2018-07-11 14:18:20",
             "Acton Lake 2018 July", "U-16", "2018-07-11 14:37:00", "2018-07-11 14:39:00", "2018-07-11 14:37:00", "2018-07-11 14:38:30",
             "Acton Lake 2018 July", "U-05", "2018-07-11 14:51:00", "2018-07-11 14:54:00", "2018-07-11 14:51:00", "2018-07-11 14:53:20",
             "Acton Lake 2018 July", "U-09", "2018-07-11 15:06:00", "2018-07-11 15:08:00", "2018-07-11 15:06:00", "2018-07-11 15:08:00",
             "Acton Lake 2018 July", "U-06", "2018-07-11 15:29:00", "2018-07-11 15:31:00", "2018-07-11 15:28:00", "2018-07-11 15:29:30",
             "Acton Lake 2018 July", "U-17", "2018-07-11 15:42:00", "2018-07-11 15:43:30", "2018-07-11 15:42:00", "2018-07-11 15:43:30",
             "Acton Lake 2018 July", "U-14", "2018-07-11 15:59:00", "2018-07-11 16:02:00", "2018-07-11 15:59:00", "2018-07-11 16:02:00",
             "Acton Lake 2018 Aug", "U-12", "2018-08-13 11:18:20", "2018-08-13 11:20:00", "2018-08-13 11:18:30", "2018-08-13 11:19:00",
             "Acton Lake 2018 Aug", "U-04", "2018-08-13 11:37:40", "2018-08-13 11:39:00", "2018-08-13 11:37:20", "2018-08-13 11:38:00",
             "Acton Lake 2018 Aug", "U-08", "2018-08-13 11:56:00", "2018-08-13 11:57:00", "2018-08-13 11:58:20", "2018-08-13 11:59:00",
             "Acton Lake 2018 Aug", "U-15", "2018-08-13 12:37:00", "2018-08-13 12:38:20", "2018-08-13 12:39:40", "2018-08-13 12:40:20",
             "Acton Lake 2018 Aug", "U-11", "2018-08-13 12:56:00", "2018-08-13 12:57:40", "2018-08-13 12:55:50", "2018-08-13 12:56:40",
             "Acton Lake 2018 Aug", "U-07", "2018-08-13 13:10:00", "2018-08-13 13:12:00", "2018-08-13 13:09:15", "2018-08-13 13:10:10",
             "Acton Lake 2018 Aug", "U-18", "2018-08-13 13:21:10", "2018-08-13 13:24:00", "2018-08-13 13:21:00", "2018-08-13 13:22:10",
             "Acton Lake 2018 Aug", "U-01", "2018-08-13 13:40:30", "2018-08-13 13:42:40", "2018-08-13 13:40:45", "2018-08-13 13:42:30",
             "Acton Lake 2018 Aug", "U-13", "2018-08-13 13:50:00", "2018-08-13 13:52:00", "2018-08-13 13:49:20", "2018-08-13 13:50:10",
             "Acton Lake 2018 Aug", "U-16", "2018-08-13 14:08:05", "2018-08-13 14:09:00", "2018-08-13 14:07:45", "2018-08-13 14:08:30",
             "Acton Lake 2018 Aug", "U-05", "2018-08-13 14:21:45", "2018-08-13 14:23:00", "2018-08-13 14:21:15", "2018-08-13 14:22:30",
             "Acton Lake 2018 Aug", "U-09", "2018-08-13 14:33:00", "2018-08-13 14:35:00", "2018-08-13 14:33:00", "2018-08-13 14:35:00",
             "Acton Lake 2018 Aug", "U-06", "2018-08-13 14:47:25", "2018-08-13 14:49:10", "2018-08-13 14:46:45", "2018-08-13 14:48:10",
             "Acton Lake 2018 Aug", "U-17", "2018-08-13 15:00:00", "2018-08-13 15:01:00", "2018-08-13 14:59:30", "2018-08-13 15:00:00",
             "Acton Lake 2018 Aug", "U-14", "2018-08-13 15:40:00", "2018-08-13 15:42:00", "2018-08-13 15:40:00", "2018-08-13 15:41:40",
             "Acton Lake 2018 Sept", "U-12", "2018-09-20 11:46:00", "2018-09-20 11:49:00", "2018-09-20 11:47:15", "2018-09-20 11:48:30",
             "Acton Lake 2018 Sept", "U-04", "2018-09-20 12:11:00", "2018-09-20 12:12:00", "2018-09-20 12:11:00", "2018-09-20 12:12:00",
             "Acton Lake 2018 Sept", "U-08", "2018-09-20 12:26:00", "2018-09-20 12:28:00", "2018-09-20 12:26:30", "2018-09-20 12:27:30",
             "Acton Lake 2018 Sept", "U-15", "2018-09-20 12:52:00", "2018-09-20 12:54:00", "2018-09-20 12:52:00", "2018-09-20 12:53:00",
             "Acton Lake 2018 Sept", "U-11", "2018-09-20 13:04:00", "2018-09-20 13:06:00", "2018-09-20 13:04:00", "2018-09-20 13:06:00",
             "Acton Lake 2018 Sept", "U-07", "2018-09-20 13:17:00", "2018-09-20 13:20:00", "2018-09-20 13:17:00", "2018-09-20 13:20:00",
             "Acton Lake 2018 Sept", "U-18", "2018-09-20 13:32:00", "2018-09-20 13:35:00", "2018-09-20 13:32:00", "2018-09-20 13:34:30",
             "Acton Lake 2018 Sept", "U-01", "2018-09-20 13:46:00", "2018-09-20 13:49:00", "2018-09-20 13:46:00", "2018-09-20 13:47:00",
             "Acton Lake 2018 Sept", "U-13", "2018-09-20 14:00:00", "2018-09-20 14:02:00", "2018-09-20 14:00:00", "2018-09-20 14:02:00",
             "Acton Lake 2018 Sept", "U-16", "2018-09-20 14:19:00", "2018-09-20 14:22:00", "2018-09-20 14:18:40", "2018-09-20 14:20:00",
             "Acton Lake 2018 Sept", "U-05", "2018-09-20 14:29:00", "2018-09-20 14:33:00", "2018-09-20 14:30:00", "2018-09-20 14:33:00",
             "Acton Lake 2018 Sept", "U-09", "2018-09-20 14:40:00", "2018-09-20 14:42:00", "2018-09-20 14:40:00", "2018-09-20 14:43:00",
             "Acton Lake 2018 Sept", "U-06", "2018-09-20 14:51:00", "2018-09-20 14:54:00", "2018-09-20 14:50:00", "2018-09-20 14:54:00",
             "Acton Lake 2018 Sept", "U-17", "2018-09-20 15:04:00", "2018-09-20 15:06:00", "2018-09-20 15:04:00", "2018-09-20 15:05:00",
             "Acton Lake 2018 Sept", "U-14", "2018-09-20 15:15:30", "2018-09-20 15:18:00", "2018-09-20 15:15:30", "2018-09-20 15:18:00"
             # Lake_Name,       siteID,     co2DeplyDtTm,            co2RetDtTm,          ch4DeplyDtTm,        ch4RetDtTm
)

# Coerce to data.frame  
adjDataDfGRTS <- data.frame(Lake_Name = adjDataGRTS[seq(1,length(adjDataGRTS), 6)],
                        siteID = adjDataGRTS[seq(2,length(adjDataGRTS), 6)],
                        co2DeplyDtTm = adjDataGRTS[seq(3,length(adjDataGRTS), 6)],
                        co2RetDtTm = adjDataGRTS[seq(4,length(adjDataGRTS), 6)],
                        ch4DeplyDtTm = adjDataGRTS[seq(5,length(adjDataGRTS), 6)],
                        ch4RetDtTm = adjDataGRTS[seq(6,length(adjDataGRTS), 6)],
                        stringsAsFactors = FALSE)

# Convert date/time data from character to POSIXct
adjDataDfGRTS[, c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")] <- 
  lapply(adjDataDfGRTS[ , c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")], 
         as.POSIXct, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")  # set tz!

#4. UPDATE DEPLOYMENT AND RETRIEVAL TIMES BASED ON FIXES ABOVE (SEE POINT 3)

for (i in 1:with(adjDataDfGRTS, length(unique(paste(siteID, Lake_Name))))) { # for each unique site x lake combination
  lake.i <- adjDataDfGRTS$Lake_Name[i]  # extract ith lake
  site.i <- adjDataDfGRTS$siteID[i]  # extract ith site
  data.i <- adjDataDfGRTS[i, ]  # extract data.i
  
  #Calculate earliest and latest observation we need for this lake x site.  Simply min/max deply time.
  #This will make sure x-axis range is good for time series plots.
  #Use do.call the concentate "c" the df (which is actually a list) to a vector that
  #can be fed to min, while preserving the POSIXct attribute.
  #Need to set na.rm=TRUE in min function to ignore NA deploy/retr times for
  #chambers with CH4 ebullition.
  #http://stackoverflow.com/questions/15659783/why-does-unlist-kill-dates-in-r
  #Unfortunately, this changes tz, which must be manually reset to UTC
  start.time.i <- min(do.call("c", data.i[, c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")]), na.rm = TRUE)
  end.time.i <- max(do.call("c", data.i[, c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")]), na.rm = TRUE)
  attr(start.time.i, "tzone") <- "UTC"  # reset time zone!
  attr(end.time.i, "tzone") <- "UTC"  # reset time zone!
  
  #Delete original CO2 and CH4 deployment / retrieval times from gga file.  These will be replaced with 
  #updated values.
  # gga.test<-filter(gga, Lake_Name==lake.i)
  # gga.test[gga.test$Lake_Name == lake.i &  !is.na(gga.test$Lake_Name) & gga.test$siteID == site.i & !is.na(gga.test$siteID), 
  #     c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")] = NA
  
  gga[gga$Lake_Name == lake.i &  !is.na(gga$Lake_Name) & gga$siteID == site.i & !is.na(gga$siteID), 
      c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")] = NA
  
  #Logical indicator indicator block of gga data that should be updated
  #The extra minute and begining and end extend x-axis range for plotting,
  #which is good for picking time range for modeling diffusion.
  logicalIndicator.i <- gga$RDateTime > (start.time.i - 60) & # 1 min < deployment
    gga$RDateTime < (end.time.i + 60) # 1 min > retrieval
  
  # Replace original time stamps with updated numbers
  # POSIXct and time zone preserved through this step.  Wow!
  gga[logicalIndicator.i, c("Lake_Name", "siteID", "co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")] =
    data.i[, c("Lake_Name", "siteID", "co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")]
}




#5.  PLOT CO2 AND CH4 PROFILES FOR INSPECTION

pdf("figures/ggaProfileGRTS.pdf", paper = "a4r") # landscape orientation

for (i in 1:with(gga[!is.na(gga$Lake_Name), ], # this eliminates observations without a Lake_Name (LGR data when chamber not deployed)
                 length(unique(paste(siteID, Lake_Name))))) {  # each combination of site and lake
  
  site.lake.i <- with(gga[!is.na(gga$Lake_Name), ],  # extract unique lake x site combination
                      unique(paste(siteID, Lake_Name)))[i]
  site.i <- gsub(" .*$", "", site.lake.i)  # extract site.  regex allows for siteIDs of different lengths (i.e. S-01, SU-01)
  lake.i <- substr(site.lake.i, start = nchar(site.i) + 2, stop = nchar(site.lake.i)) # extract lake name
  data.i <- filter(gga, Lake_Name == lake.i, siteID == site.i) %>%  # Pull out GGA data chunk
    select(-GasT_C) # No need to plot gas temperature
  RDate.i <- unique(data.i$RDate)  # for panel title
  
  plot.i <- ggplot(data.i,  aes(x = RDateTime, y = CH4._ppm)) + 
    geom_point() +
    geom_vline(data = data.i, aes(xintercept = as.numeric(ch4DeplyDtTm))) +
    geom_vline(data = data.i, aes(xintercept = as.numeric(ch4RetDtTm))) +
    scale_x_datetime(labels=date_format("%H:%M")) +
    ggtitle(paste(lake.i, site.i, RDate.i)) +
    theme(axis.text.x = element_text(size = 7),
          plot.title = element_text(size = 11))
  
  plot.ii <- ggplot(data.i,  aes(x = RDateTime, y = CO2._ppm)) + 
    geom_point() +
    geom_vline(data = data.i, aes(xintercept = as.numeric(co2DeplyDtTm))) +
    geom_vline(data = data.i, aes(xintercept = as.numeric(co2RetDtTm))) +
    scale_x_datetime(labels=date_format("%H:%M")) +
    ggtitle(paste(lake.i, site.i)) +
    theme(axis.text.x = element_text(size = 7))
  
  grid.arrange(plot.i, plot.ii, ncol = 2) # use to put two plots per page
}


dev.off() 
#should be 15 * number of GRTS survey pages
#only takes < 1 min










