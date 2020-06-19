


# EMISSION RATE CALCULATIONS--------------------
# STEP 1:  CALCULATE EMISSION RATE VIA LINEAR AND NONLINEAR REGRESSION
#          FOR SITES WHERE PERIODS OF LINEAR ACCUMULATION ARE INDICATED 
# STEP 2: USE AIC TO DETERMINE WHETHER LINEAR OF NON-LINEAR FIT IS BEST.
#         CONFIRM CHOICE BY INSPECTING RAW DATA
# STEP 3: MERGE WITH eqAreaData



# STEP 1: LINEAR AND NONLINEAR REGRESSION
n <- length(unique(paste(gga$Lake_Name, gga$siteID)))
temp <- rep(NA, n)

# Dataframe to hold results
OUT <- data.frame(site = temp, Lake_Name = temp,
                  ch4.diff.max=temp, #Sarah added on 6/14/17; making histogram of max ch4 levels measured by LGR
                  ch4.lm.slope = temp, ch4.lm.drate.mg.h = temp, ch4.lm.aic = temp, ch4.lm.r2 = temp, ch4.lm.pval = temp,
                  ch4.ex.aic = temp, ch4.ex.r2 = temp, ch4.ex.slope = temp, ch4.ex.drate.mg.h = temp, ch4.ex.k=temp, 
                  co2.lm.slope = temp, co2.lm.drate.mg.h = temp, co2.lm.aic = temp, co2.lm.r2 = temp, co2.lm.pval = temp,
                  co2.ex.aic = temp, co2.ex.r2 = temp, co2.ex.slope = temp, co2.ex.k = temp, co2.ex.drate.mg.h = temp)


# Remove data not recorded during deployment
gga.model <- filter(gga, !is.na(Lake_Name))
pdf("figures/curveFitsGRTS.pdf")
start.time <- Sys.time()
for (i in 1:length(unique(paste(gga.model$Lake_Name, gga.model$siteID)))) {  # For each unique site
  site.lake.i <- unique(paste(gga.model$siteID, gga.model$Lake_Name))[i]
  site.i <- gsub(" .*$", "", site.lake.i)  # extract site.  regex allows for siteIDs of different lengths (i.e. S-01, SU-01)
  lake.i <- substr(site.lake.i, start = nchar(site.i) + 2, stop = nchar(site.lake.i)) # extract lake name
  OUT[i,"site"] <- site.i
  OUT[i,"Lake_Name"] <- lake.i  
  # Need chamber volume from eqAreaData.
  chmVol.L.i <- filter(eqAreaData, siteID == site.i, Lake_Name == lake.i) %>% 
    select(chmVol.L)   
  
  
  
  data.i.ch4 <- filter(gga.model,  # extract data
                       RDateTime >= ch4DeplyDtTm, # based on diff start time
                       RDateTime <= ch4RetDtTm, # based on diff end time
                       siteID == site.i,  
                       Lake_Name == lake.i)  %>% 
    # Calculate elapsed time (seconds).  lm behaves strangely when used with POSIXct data.
    mutate(elapTime = RDateTime - RDateTime[1], # Calculate elapsed time (seconds). 
           chmVol.L = chmVol.L.i[1,1]) %>%  # subscripting needed to remove name
    select(Lake_Name, siteID, CH4._ppm, elapTime, GasT_C, chmVol.L)  # Pull out data of interest
  
  OUT[i, "ch4.diff.max"]<-max(data.i.ch4$CH4._ppm, na.rm=TRUE) #maximum CH4 mixing ratio measured during the chamber deployment time
  
  data.i.co2 <- filter(gga.model,  # extract data
                       RDateTime >= co2DeplyDtTm, # based on diff start time
                       RDateTime <= co2RetDtTm, # based on diff end time
                       siteID == site.i,  
                       Lake_Name == lake.i)  %>% 
    # Calculate elapsed time (seconds).  lm behaves strangely when used with POSIXct data.
    mutate(elapTime = RDateTime - RDateTime[1], # Calculate elapsed time (seconds). 
           chmVol.L = chmVol.L.i[1,1]) %>%  # subscripting needed to remove name
    select(Lake_Name, siteID, CO2._ppm, elapTime, GasT_C, chmVol.L)  # Pull out data of interest
  
  # Are there data available to run the model?
  co2.indicator <- length(data.i.co2$CO2._ppm) == 0
  ch4.indicator <- length(data.i.ch4$CH4._ppm) == 0
  
  # Data needed for emission rate calcs.  Same #'s for CO2 and CH4.  Arbitrarily pulled from CO2.  
  temp.i <- if (co2.indicator) mean(data.i.ch4$GasT_C, na.rm = TRUE) else (mean(data.i.co2$GasT_C, na.rm = TRUE))  # GGA measured temp
  volume.i <- if (co2.indicator) unique(data.i.ch4[!is.na(data.i.ch4$chmVol.L), "chmVol.L"]) else
    unique(data.i.co2[!is.na(data.i.co2$chmVol.L), "chmVol.L"])# Dome volume
  
  # lm
  lm.ch4.i <- try(lm(data.i.ch4$CH4._ppm ~ data.i.ch4$elapTime), silent = TRUE)  # suppress warning if fails 
  lm.co2.i <- try(lm(data.i.co2$CO2._ppm ~ data.i.co2$elapTime), silent = TRUE)  # linear regression
  
  # lm slopes
  slope.ch4.i <- if(ch4.indicator) NA else (as.numeric(coef(lm.ch4.i)[2]))  # lm slope: ppm s-1   
  slope.co2.i <- if(co2.indicator) NA else (as.numeric(coef(lm.co2.i)[2]))   # lm slope: ppm s-1
  OUT[i, c("ch4.lm.slope", "co2.lm.slope")] <- c(slope.ch4.i, slope.co2.i)
  
  # lm p-values
  fstat.ch4 <- if(ch4.indicator) rep(NA,3) else summary(lm.ch4.i)$fstatistic
  fstat.co2 <- if(co2.indicator) rep(NA,3) else summary(lm.co2.i)$fstatistic
  OUT[i, c("ch4.lm.pval")]  <- pf(fstat.ch4[1], fstat.ch4[2], fstat.ch4[3], lower.tail = FALSE)
  OUT[i, c("co2.lm.pval")]  <- pf(fstat.co2[1], fstat.co2[2], fstat.co2[3], lower.tail = FALSE)
  
  # lm r2 values
  OUT[i, c("ch4.lm.r2")]  <- if(ch4.indicator) NA else summary(lm.ch4.i)["r.squared"]
  OUT[i, c("co2.lm.r2")]  <- if(co2.indicator) NA else summary(lm.co2.i)["r.squared"]
  
  # lm AIC values
  OUT[i, c("ch4.lm.aic")] <- if(ch4.indicator) NA else AIC(lm.ch4.i)
  OUT[i, c("co2.lm.aic")] <- if(co2.indicator) NA else AIC(lm.co2.i)
  
  # Exponential Model
  cmax.ch4 <- data.i.ch4$CH4._ppm[max(which(!is.na(data.i.ch4$CH4._ppm)))]  # cmax = final CH4
  c.initial.ch4 <- data.i.ch4$CH4._ppm[min(which(!is.na(data.i.ch4$CH4._ppm)))]  # initial CH4 
  exp.ch4.i <-try(nlsLM(CH4._ppm~cmax-(cmax-b)*exp(-k*as.numeric(elapTime)),
                        data = data.i.ch4, start=list(cmax=cmax.ch4, b=cmax.ch4-c.initial.ch4, k=.03)),
                  silent = TRUE) 
  
  cmax.co2 <- data.i.co2$CO2._ppm[max(which(!is.na(data.i.co2$CO2._ppm)))]  # cmax = final CO2
  c.initial.co2 <- data.i.co2$CO2._ppm[min(which(!is.na(data.i.co2$CO2._ppm)))]  # initial CO2   
  exp.co2.i <-try(nlsLM(CO2._ppm~cmax-(cmax-b)*exp(-k*as.numeric(elapTime)),
                        data = data.i.co2, start=list(cmax=505, b= 400, k=0.004)),
                  silent=TRUE) 
  # Ex r2
  rss.ch4.i <- if(class(exp.ch4.i) == "try-error") NA else sum(residuals(exp.ch4.i)^2)
  tss.ch4.i <- if(class(exp.ch4.i) == "try-error") NA else 
    sum((data.i.ch4$CH4._ppm - mean(data.i.ch4$CH4._ppm, na.rm=TRUE))^2, na.rm=TRUE)
  OUT[i, "ch4.ex.r2"] = 1 - rss.ch4.i/tss.ch4.i
  
  rss.co2.i <- if(class(exp.co2.i) == "try-error") NA else sum(residuals(exp.co2.i)^2)
  tss.co2.i <- if(class(exp.co2.i) == "try-error") NA else 
    sum((data.i.co2$CO2._ppm - mean(data.i.co2$CO2._ppm, na.rm=TRUE))^2, na.rm=TRUE)
  OUT[i, "co2.ex.r2"] = 1 - rss.co2.i/tss.co2.i
  
  # Ex AIC
  OUT[i, "ch4.ex.aic"] = if(class(exp.ch4.i) == "try-error") NA else AIC(exp.ch4.i)
  OUT[i, "co2.ex.aic"] = if(class(exp.co2.i) == "try-error") NA else AIC(exp.co2.i)
  
  # Ex slope
  coef.exp.ch4.i <- if(class(exp.ch4.i) == "try-error") NA else coef(exp.ch4.i)  
  OUT[i, "ch4.ex.slope"] = if(class(exp.ch4.i) == "try-error") NA else 
    coef.exp.ch4.i["k"]*(coef.exp.ch4.i["cmax"]-coef.exp.ch4.i["b"])  # ppm s-1
  
  coef.exp.co2.i <- if(class(exp.co2.i) == "try-error") NA else coef(exp.co2.i)  
  OUT[i, "co2.ex.slope"] = if(class(exp.co2.i) == "try-error") NA else 
    coef.exp.co2.i["k"]*(coef.exp.co2.i["cmax"]-coef.exp.co2.i["b"])  # ppm s-1
  
  #Ex k  
  OUT[i, "ch4.ex.k"] = if(class(exp.ch4.i) == "try-error") NA else 
    coef.exp.ch4.i["k"]
  OUT[i, "co2.ex.k"] = if(class(exp.co2.i) == "try-error") NA else 
    coef.exp.co2.i["k"]
  
  # Emission rate.  Assumes atmospheric pressure of 1 atm.
  # Converting from parts per million to umole cross out.  No conversion factor necessary. Dome area = 0.2 m2
  ch4.lm.drate.i.umol.s <- ((volume.i * 1 * slope.ch4.i) / (0.082057 * (temp.i + 273.15))) / 0.2 #umol CH4 s-1
  OUT[i, "ch4.lm.drate.mg.h"] = if (length(ch4.lm.drate.i.umol.s) == 0)  # throws error if no data
    NA else
      ch4.lm.drate.i.umol.s * (16/1000) * (60*60)  # mg CH4 m-2 h-1
  
  co2.lm.drate.i.umol.s <- ((volume.i * 1 * slope.co2.i) / (0.082057 * (temp.i + 273.15))) / 0.2 #umol CO2 s-1
  OUT[i, "co2.lm.drate.mg.h"] =  if (length(co2.lm.drate.i.umol.s) == 0) # throws error if no data
    NA else
      co2.lm.drate.i.umol.s * (44/1000) * (60*60)  #mg CO2 m-2 h-1
  
  ch4.ex.drate.i.umol.s <- ((volume.i * 1 * OUT[i, "ch4.ex.slope"]) / (0.082057 * (temp.i + 273.15))) / 0.2 #umol CH4 s-1
  OUT[i, "ch4.ex.drate.mg.h"] = if (length(ch4.lm.drate.i.umol.s) == 0) # throws error if no data
    NA else
      ch4.ex.drate.i.umol.s * (16/1000) * (60*60)  # mg CH4 m-2 h-1
  
  co2.ex.drate.i.umol.s <- ((volume.i * 1 * OUT[i, "co2.ex.slope"]) / (0.082057 * (temp.i + 273.15))) / 0.2 #umol CO2 s-1
  OUT[i, "co2.ex.drate.mg.h"] =  if (length(co2.lm.drate.i.umol.s) == 0) # throws error if no data
    NA else
      co2.ex.drate.i.umol.s * (44/1000) * (60*60)  #mg CO2 m-2 h-1
  
  # Plots
  # CH4 first
  ch4.ex.pred <- try(  
    data.frame(ch4.pred = predict(exp.ch4.i, newdata = data.i.ch4), # pred values from exponential model
               elapTime = data.i.ch4$elapTime),
    silent = TRUE)
  
  ch4.title <- paste(OUT[i, "site"], # plot title
                     OUT[i, "Lake_Name"],
                     "ex.r2=",
                     round(OUT[i, "ch4.ex.r2"], 2),
                     "ex.AIC=",
                     round(OUT[i, "ch4.ex.aic"],2),
                     "ex.rate=",
                     round(OUT[i, "ch4.ex.drate.mg.h"], 2),                    
                     "\n lm.r2=",
                     round(OUT[i, "ch4.lm.r2"],2),
                     "lm.AIC=",
                     round(OUT[i, "ch4.lm.aic"],2),
                     "lm.rate=",
                     round(OUT[i, "ch4.lm.drate.mg.h"], 2),
                     sep=" ")
  p.ch4 <- ggplot(data.i.ch4, aes(as.numeric(elapTime), CH4._ppm)) + 
    geom_point() +
    xlab("Seconds") +
    ggtitle(ch4.title) +
    stat_smooth(method = "lm", se=FALSE)
  if (class(exp.ch4.i) == "try-error") p.ch4 else  # if exp model worked, add exp line
    p.ch4 <- p.ch4 + geom_line(data=ch4.ex.pred, aes(as.numeric(elapTime), ch4.pred), color = "red")
  print(p.ch4)
  
  
  # CO2 models
  co2.ex.pred <- try(
    data.frame(co2.pred = predict(exp.co2.i, newdata = data.i.co2),  # pred data from exp model
               elapTime = data.i.co2$elapTime),
    silent=TRUE)
  
  co2.title <- paste(OUT[i, "site"], # plot title
                     OUT[i, "Lake_Name"],
                     "ex.r2=",
                     round(OUT[i, "co2.ex.r2"], 2),
                     "ex.AIC=",
                     round(OUT[i, "co2.ex.aic"],2),
                     "ex.rate=",
                     round(OUT[i, "co2.ex.drate.mg.h"], 2),                    
                     "\n lm.r2=",
                     round(OUT[i, "co2.lm.r2"],2),
                     "lm.AIC=",
                     round(OUT[i, "co2.lm.aic"],2),
                     "lm.rate=",
                     round(OUT[i, "co2.lm.drate.mg.h"], 2),
                     sep=" ")
  p.co2 <- ggplot(data.i.co2, aes(as.numeric(elapTime), CO2._ppm)) + 
    geom_point() +
    xlab("Seconds") +
    ggtitle(co2.title) +
    stat_smooth(method = "lm", se=FALSE)
  if (class(exp.co2.i) == "try-error") p.co2 else  # if exp model worked, add exp line
    p.co2 <- p.co2 + geom_line(data=co2.ex.pred, aes(as.numeric(elapTime), co2.pred), color = "red")
  print(p.co2)
}  
dev.off()
start.time;Sys.time() 


# STEP 2: USE AIC TO DETERMINE WHETHER LINEAR OR NON-LINEAR FIT IS BEST.
#         CONFIRM CHOICE BY INSPECTING RAW DATA
# Choose best rate.  Just use AIC
# Cowan lake manual syringe sample data wouldn't support ex model.
# Include is.na(ex.aic) to accomodate this.
OUT <- mutate(OUT, 
              co2.best.model = ifelse(co2.lm.aic < co2.ex.aic | is.na(co2.ex.aic), 
                                      "linear", "exponential"),
              co2.drate.mg.h.best = ifelse(co2.best.model == "linear",
                                           co2.lm.drate.mg.h, co2.ex.drate.mg.h),
              ch4.best.model = ifelse(ch4.lm.aic < ch4.ex.aic | is.na(ch4.ex.aic), 
                                      "linear", "exponential"),
              ch4.drate.mg.h.best = ifelse(ch4.best.model == "linear",
                                           ch4.lm.drate.mg.h, ch4.ex.drate.mg.h)) 
# Inspect r2.
plot(with(OUT,ifelse(co2.best.model == "linear", co2.lm.r2, co2.ex.r2)))  # CO2: some low ones to investigate
plot(with(OUT,ifelse(ch4.best.model == "linear", ch4.lm.r2, ch4.ex.r2)))  # CH4:  some low ones to investigate

# If r2 of best model < 0.9 for CH4; 0.8 for CO2, then set to NA
OUT <- mutate(OUT, 
              co2.drate.mg.h.best = ifelse((co2.lm.aic < co2.ex.aic | is.na(co2.ex.aic)) & co2.lm.r2 < 0.8, # if ex is best, but r2<0.9
                                           NA, # then NA
                                           ifelse((co2.ex.aic < co2.lm.aic) & co2.ex.r2 < 0.8, # if lm is best, but r2<0.9
                                                  NA, # the NA
                                                  co2.drate.mg.h.best)), # otherwise assume value defined above
              
              ch4.drate.mg.h.best = ifelse((ch4.lm.aic < ch4.ex.aic | is.na(ch4.ex.aic)) & ch4.lm.r2 < 0.9, # if ex is best, but r2<0.9
                                           NA, # then NA
                                           ifelse((ch4.ex.aic < ch4.lm.aic) & ch4.ex.r2 < 0.9, # if lm is best, but r2<0.9
                                                  NA, # the NA
                                                  ch4.drate.mg.h.best))) # otherwise assume value defined above

# Inspect r2 after scrubbing r2<0.9
plot(with(OUT[!is.na(OUT$co2.drate.mg.h.best),], 
          ifelse(co2.best.model == "linear", co2.lm.r2, co2.ex.r2)))  # CO2: all > 0.9

plot(with(OUT[!is.na(OUT$ch4.drate.mg.h.best),], 
          ifelse(ch4.best.model == "linear", ch4.lm.r2, ch4.ex.r2)))  # CH4: all > 0.9

# STEP 3: MERGE DIFFUSION RATES WITH eqAreaData
# First, strip NA from OUT
OUT <- filter(OUT, !is.na(Lake_Name)) # Just one NA slipped in
eqAreaData <- merge(eqAreaData, OUT, by.x = c("Lake_Name", "siteID"), 
                    by.y = c("Lake_Name", "site"), all=TRUE)

str(eqAreaData) # 210 observations. 35 sites * 3 surveys * 2 years = 210. Only 90 msmts: 15*6

# Any sites not have a diffusive rate?
# Only a subset of Cowan Lake sites were sampled due to water in LGR.
# Other sites had strong ebullition in LGR profile.
filter(eqAreaData, EvalStatus == "sampled", !is.na(ch4.drate.mg.h.best)) %>%
  select(Lake_Name, siteID, 
         ch4.lm.drate.mg.h, ch4.ex.drate.mg.h, ch4.drate.mg.h.best,
         co2.lm.drate.mg.h, co2.ex.drate.mg.h, co2.drate.mg.h.best)



# CALCULATE EBULLITION RATE------------------

# First calculate volumetric ebullion rate.  Straightforward operation
# that can be vectorized across the entire df.
eqAreaData <- mutate(eqAreaData, 
                     ebMlHrM2 = TtTrpVl / 
                       (as.numeric(trapRtrvDtTm - trapDeplyDtTm) * 
                          ((3.14*.28^2)))) # funnel diameter = 22.25in=0.56m, r=.28m))

# PREPARE EXETAINER CODES----------------------
# READ DATA-----------------
# gc.all.2017<-read.table("L:/Lab/Lablan/GHG/GC/2017Data/gcMasterFile2017updated2018-03-15.txt",
gc.all.2017<-read.table("data/gcMasterFile2017updated2018-03-15.txt",
                        col.names=c("sample", "n2o.ppm", "co2.ppm", "ch4.ppm", "flag.n2o",
                                    "flag.co2", "flag.ch4", "o2.ar.percent", "n2.perc","o2.chk",
                                    "flag.n2", "flag.o2.ar"),
                        #colClasses=c("character", rep("num", 3), rep("int", 3), rep("num", 2),
                        #             rep("logi", 2)),
                        skip=1)
#gc.all.2018<-read.table("L:/Lab/Lablan/GHG/GC/2018Data/gcMasterFile2018updated2018-12-21.txt",
gc.all.2018<-read.table("data/gcMasterFile2018updated2018-12-21.txt",
                        col.names=c("sample", "ch4.ppm", "co2.ppm", "n2o.ppm", "flag.n2o",
                                    "flag.co2", "flag.ch4"),
                        #colClasses=c("character", rep("num", 3), rep("int", 3), rep("num", 2),
                        #             rep("logi", 2)),
                        skip=1)

gc.all<-rbind(select(gc.all.2017, sample, n2o.ppm, co2.ppm, ch4.ppm, flag.n2o, flag.co2, flag.ch4),
              select(gc.all.2018, sample, n2o.ppm, co2.ppm, ch4.ppm, flag.n2o, flag.co2, flag.ch4))
rm(gc.all.2017, gc.all.2018)
gc.all$sample<-as.character(gc.all$sample) 
gc.all$sampleNum<-gsub("([0-9]*).*","\\1",gc.all$sample) #extract numbers 
gc.all$sampleNum<-substring(gc.all$sample, 4)
# 
#filter the lab LAN master file, which includes all samples run in 2017 and 2018, for Acton samples
gc.all.Act<-dplyr::filter(gc.all, grepl("ACT",sample))
# # Check for duplicates.
filter(gc.all.Act, duplicated(sample,fromLast = TRUE) | duplicated(sample,fromLast = FALSE)) %>% 
  arrange(sample)

# Extract from eqAreaData
xtrCodes <- filter(eqAreaData, EvalStatus == "sampled") %>%
  select(Lake_Name, siteID, ArExtnrs, DG_Extn, TrapExtn)

# Remove white space
xtrCodes[, c("ArExtnrs", "DG_Extn", "TrapExtn")] <- apply(X = xtrCodes[, c("ArExtnrs", "DG_Extn", "TrapExtn")],
                                                          MARGIN = 2, 
                                                          function(x) gsub(x, pattern = " ", replacement = ""))

# Split codes into separate fields
xtrCodes <- separate(xtrCodes, ArExtnrs, into = c("ar.xtr.1", "ar.xtr.2", "ar.xtr.3"), sep = ",") %>%
  separate(DG_Extn, into = c("dg.xtr.1", "dg.xtr.2", "dg.xtr.3"), sep = ",") %>%
  separate(TrapExtn, into = c("tp.xtr.1", "tp.xtr.2", "tp.xtr.3"), sep = ",")

# Melt  
xtrCodes.m <- melt(xtrCodes, id.vars = c("Lake_Name", "siteID")) %>% # melt, converts exetainer code to factor
  mutate(value = as.integer(as.character(value))) %>%  # Must got from factor -->character-->integer
  mutate(variable = as.character(variable)) %>% # Must got from factor -->character
  filter(!is.na(value))  # remove NAs

# Simplify variable names
xtrCodes.m[grepl(pattern = ".1|.2|.3", x = xtrCodes.m$variable), "variable"] <- 
  gsub(pattern = ".1|.2|.3", replacement = "", x = xtrCodes.m[grepl(pattern = ".1|.2|.3", x = xtrCodes.m$variable), "variable"])


# Check for duplicates.  Should be none.
filter(xtrCodes.m, duplicated(value,fromLast = TRUE) | duplicated(value,fromLast = FALSE)) %>% arrange(value)
#on 10-12-2017, result is 0


# MERGE EXETAINER CODES WITH GC DATA-----
#acton 2018 exetainer codes have underscores in them, since they are one digit shorter
#need to remove the underscore so they match the eqAreaData --> xtrCode code
gc.all.Act$sampleNum<-sub("_", "", gc.all.Act$sampleNum)

xtrCodes.gas <- merge(xtrCodes.m, gc.all.Act, by.x = "value", by.y = "sampleNum", all = TRUE)

str(xtrCodes.m)  #141 obs; 12/27/2018: 291 obs
str(gc.all.Act) # 208 observations #2/6/2018: 65824 obs; #4/11/2018: 4775 obs; 12/27/2018: 864 obs (Acton only)
str(xtrCodes.gas) # 201 observations; 12/27/2018: 893 obs

# Specific fixes
# Still need to add codes for MIT trap redeployment

omitCodes <- c(170143, #outlier for CH4 and N2, but no field note
               170161, #outlier for CH4 and N2, but no field note
               170165, #this and the next have field note, but curiously the field note concerns 166 and 167
               170166) #any that need to be omitted due to physical QAQC



xtrCodes.gas <- filter(xtrCodes.gas, !(value %in% omitCodes))



# Sample run on GC, but not in data sheets
filter(xtrCodes.gas, is.na(Lake_Name)) %>% arrange(value)  # none

# Samples in data sheets, but GC data not yet read into R.  
filter(xtrCodes.gas, is.na(xtrCodes.gas$co2.ppm)) %>% arrange(variable, value)
# 12 air exetainers -- weren't run because of cross contaimination with trap gas

# Take a look at values
# ggplot(filter(xtrCodes.gas, variable == "tp.xtr"), aes(siteID, ch4.ppm/10000)) + 
#   geom_jitter(alpha=0.3) +
#   theme(axis.text.x = element_text(angle = 90))

# QA/QC GC REPS--------------

#pdf("C:/R_Projects/actonFluxProject/figures/scatterplot3dTrap.pdf",
pdf("figures/scatterplot3dTrap.pdf",
    paper = "a4r", width = 11, height = 8)  # initiate landscape pdf file)
par(mfrow = c(1,2))

uniqueCases <- filter(xtrCodes.gas, variable == "tp.xtr", # trap sample
                      !is.na(ch4.ppm), # has GC data
                      !is.na(Lake_Name)) %>% # is connected with Lake and station
  distinct(Lake_Name, siteID) # unique combinations of lake and site

for(i in 1:length(uniqueCases$Lake_Name)) {
  site.i <- uniqueCases$siteID[i]
  lake.i <- uniqueCases$Lake_Name[i]
  data.i <- filter(xtrCodes.gas,
                   siteID == site.i, Lake_Name == lake.i,
                   !is.na(ch4.ppm), variable == "tp.xtr")
  
  # CO2, CH4, N2 scatterplot
  try(
    with(data.i, {
      
      s3d <- scatterplot3d(co2.ppm/10000, ch4.ppm/10000, n2,
                           xlab = "CO2 (%)", ylab = "CH4 (%)", zlab = "N2 (%)",
                           pch=21, bg = "red", main = uniqueCases[i, ])
      
      s3d.coords <- s3d$xyz.convert(co2.ppm/10000, ch4.ppm/10000, n2)
      text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
           labels=value,               # text to plot
           cex=.5, pos=4)           # shrink text 50% and place to right of points)
    }),
    silent = TRUE)
  
  # n2o, o2, ar scatterplot
  try(
    with(data.i, {
      
      s3d <- scatterplot3d(n2o.ppm, o2, ar,
                           xlab = "N2O (ppm)", ylab = "O2 (%)", zlab = "ar (%)",
                           pch=21, bg = "red", main = uniqueCases[i, ])
      
      s3d.coords <- s3d$xyz.convert(n2o.ppm, o2, ar)
      text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
           labels=value,               # text to plot
           cex=.5, pos=4)           # shrink text 50% and place to right of points)
    }),
    silent = TRUE)
  
}
dev.off()

# Aggregate by Lake_Name and siteID, for now

xtrCodes.gas.g <- filter(xtrCodes.gas,
                         !is.na(ch4.ppm), # has GC data
                         !is.na(Lake_Name)) %>% # has lake and siteID
  group_by(Lake_Name, siteID, variable) # group for aggregation

xtrCodes.gas.agg <- summarise(xtrCodes.gas.g, 
                              n2o.sd=sd(n2o.ppm, na.rm=TRUE),
                              m.n2o.ppm=mean(n2o.ppm, na.rm=TRUE),
                              n2o.cv= (n2o.sd/m.n2o.ppm) * 100,
                              
                              co2.sd=sd(co2.ppm, na.rm=TRUE),
                              m.co2.ppm=mean(co2.ppm, na.rm=TRUE),
                              co2.cv=(co2.sd/m.co2.ppm) * 100,
                              
                              ch4.sd=sd(ch4.ppm, na.rm=TRUE),
                              m.ch4.ppm=mean(ch4.ppm, na.rm=TRUE),
                              ch4.cv=(ch4.sd/m.ch4.ppm) * 100) %>%
  rename(n2o.ppm = m.n2o.ppm, co2.ppm = m.co2.ppm, ch4.ppm = m.ch4.ppm
  ) 
#%>%
# mutate(total = (ch4.ppm/10000) + (co2.ppm/10000) + (n2o.ppm/10000) + n2 + o2 + ar)

xtrCodes.gas.agg <- ungroup(xtrCodes.gas.agg)  # This removes grouping, which complicates things down the line.

ggplot(xtrCodes.gas.agg, aes(siteID, ch4.ppm)) + # Everything appears to have agg correctly
  geom_point() +
  facet_grid(~variable, scales="free_y")   # lot of low CH4 trap values to look into

# MERGE RAW GC DATA WITH eqAreaData---------------
# Only merge air and trap data now.  Need to push dg through
# headspace equilibration calcs before using.
# 1) Need to melt, which requires a data.frame, not a dplyr tbl_df.
# 2) melt creates a 'variable' column, already have 'variable' column
# in xtrCodes.gas.agg. Must rename first.
xtrCodes.gas.agg <- rename(xtrCodes.gas.agg, type = variable) # rename 'variable'

xtrCodes.gas.agg.m <- melt(as.data.frame(xtrCodes.gas.agg), # convert tbl_df to df
                           id.vars = c("Lake_Name", "siteID", "type")) # specify id variable

xtrCodes.gas.agg.m <- mutate(xtrCodes.gas.agg.m, type =  # adopt more intuitive names
                               ifelse(type == "tp.xtr", "trap",
                                      ifelse(type == "ar.xtr", "air", type)))

xtrCodes.gas.agg.c <- dcast(filter(xtrCodes.gas.agg.m, type != "dg.xtr"), # cast
                            Lake_Name + siteID ~ type + variable) 
#%>%
# select(-air_o2.sd, -air_o2, -air_o2.cv, -air_ar.sd, -air_ar, -air_ar.cv, -air_n2.sd,
#       -air_n2, -air_n2.cv, -air_total)

# Merge



eqAreaData <- merge(xtrCodes.gas.agg.c, eqAreaData, all = TRUE)



# Mass flux rate must be calculated by Lake.  Tried to apply by group using
# by_group, ddply, and lapply.  I couldn't figure it out, resorted to for loop

myEbList <- list()
for (i in 1:length(unique(eqAreaData$Lake_Name))) {
  lake.i <- unique(eqAreaData$Lake_Name)[i]
  data.i <- filter(eqAreaData, Lake_Name == lake.i )
  out.ch4 <- mass.rate(data.i, choice1 = "ch4") 
  out.co2 <- mass.rate(data.i, choice1 = "co2")
  out.n2o <- mass.rate(data.i, choice1 = "n2o")
  
  myEbList[[i]] <- data.frame(ebCh4mgM2h = out.ch4,
                              ebCo2mgM2h = out.co2,
                              ebN2omgM2h = out.n2o,
                              Lake_Name = data.i$Lake_Name,
                              siteID = data.i$siteID)
}

ebResults <- do.call("rbind", myEbList) %>%  # This coerces the list into a dataframe. Cool.
  rename(ch4.erate.mg.h = ebCh4mgM2h,
         co2.erate.mg.h = ebCo2mgM2h,
         n2o.erate.mg.h = ebN2omgM2h)


str(eqAreaData) # 210
str(ebResults)  # 210 observations of 5 vars
ebResults<-ebResults%>%
  mutate(Lake_Name = as.character(Lake_Name),
         siteID = as.character(siteID))
eqAreaData <- merge(eqAreaData,ebResults, all = TRUE) 
str(eqAreaData) # 210 observations of 102 vars


# CALCULATE TOTAL EMISSION RATES------------------
# If CH4 ebul is measured, but CH4 diff couldn't be calculated,
# CH4 tot = CH4 ebb.  In all other cases TOT = diff + ebul,
# resulting tot = NA if is.na(ebul) or is.na(diff).
eqAreaData <- mutate(eqAreaData, 
                     co2.trate.mg.h = co2.drate.mg.h.best + co2.erate.mg.h,
                     ch4.trate.mg.h = ifelse(is.na(ch4.drate.mg.h.best) &
                                               !is.na(ch4.erate.mg.h),
                                             ch4.erate.mg.h,
                                             ch4.drate.mg.h.best +ch4.erate.mg.h))

eqAreaDataSub<-filter(eqAreaData, EvalStatus=="sampled")

eqAreaDataSub$siteID
eqAreaDataSub$siteDistFromShore<-rep(c(67,  #U01
                                       81,  #U04
                                       105, #U05 
                                       75,  #U06 
                                       24,  #U07
                                       163, #U08
                                       30,  #U09
                                       14,  #U11
                                       158, #U12
                                       98,  #U13
                                       218, #U14
                                       203, #U15
                                       268, #U16
                                       242, #U17
                                       170),#U18
                                     6)

rm(OUT)