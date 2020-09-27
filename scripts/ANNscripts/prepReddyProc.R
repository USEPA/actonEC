# Prep input for the rEddyProc code that uses the MDC 
# to gap fill LE, H, and ustar for input into the ANN gapfillng for CH4

# Run these scripts first:
source("scriptsAndRmd/masterLibraryActon.R")
source("scriptsAndRmd/loadEddyPro.R")
source("scriptsAndRmd/qcEddyPro.R") 



timeframe30<-seq.POSIXt(from = as.POSIXct("2017-01-01 00:00:00",
                                          format="%Y-%m-%d %H:%M:%S",
                                          tz = "UTC"),
                        to = as.POSIXct("2018-12-31 23:30:00",
                                        format="%Y-%m-%d %H:%M:%S",
                                        tz = "UTC"),
                        by = "30 min")

epTest<-as.data.frame(timeframe30)
epTest$RDateTime<-epTest$timeframe30
epTest <- subset(epTest, !duplicated(RDateTime, fromLast=TRUE))
epOut.R<-select(epOutSubFilt, -monthday, -year)
epREddy<-left_join(epTest, epOut.R, by="RDateTime")
#remove duplicate time rows
epREddy <- subset(epREddy, !duplicated(RDateTime, fromLast=TRUE))

epREddy<-epREddy%>%
  mutate(Year = year(RDateTime),
         Hour = as.numeric(times(strftime(RDateTime, format="%T", tz="UTC")))*24,
         DoY = as.numeric(strftime(RDateTime, format="%j", tz="UTC")),
         year = year(RDateTime),
         daytime = replace(daytime, is.na(daytime) & Hour>21, 0),
         daytime = replace(daytime, is.na(daytime) & Hour<6, 0),
         daytime = replace(daytime, is.na(daytime) & Hour>8 & Hour<18, 1),
         monthday = format(RDateTime, format="%m-%d %H:%M"))
epREddy$monthday<-as.POSIXct(epREddy$monthday, format="%m-%d %H:%M", tz="UTC")

epNightS1<-filter(epREddy, daytime==0, RDateTime<"2018-05-01")
sum(is.na(epNightS1$ch4_flux))
sum(is.na(epREddy$Hour))
nrow(epNightS1)

#add varnames and units as attributes for prep to use with REddyProc

# varnames.match<- c(RDateTime="RDateTime",	date = "date",	time = "time",DOY=	"DOY", daytime=	"daytime",	
#                    Tau ="Tau", qc_Tau =	"qc_Tau", H=	"H",	qc_H="qc_H",	rand_err_H="rand_err_H", 
#                    LE=	"LE",	qc_LE ="qc_LE",rand_err_LE=	"rand_err_LE",
#                    co2_flux="co2_flux",	qc_co2_flux="qc_co2_flux",	rand_err_co2_flux = "rand_err_co2_flux",
#                    ch4_flux = "ch4_flux",	qc_ch4_flux = "qc_ch4_flux",	rand_err_ch4_flux = "rand_err_ch4_flux",
#                    co2_mixing_ratio = "co2_mixing_ratio",	h2o_mixing_ratio = "h2o_mixing_ratio", ch4_mixing_ratio="ch4_mixing_ratio",
#                    air_temperature = "air_temperature", air_pressure = 	"air_pressure",
#                    air_density =	"air_density", air_heat_capacity = "air_heat_capacity",
#                    ET = "ET",	water_vapor_density ="water_vapor_density",	e= "e",	es = "es",	
#                    specific_humidity = "specific_humidity", RH=	"RH",	VPD="VPD",	Tdew="Tdew",
#                    u_rot = "u_rot",	v_rot = "v_rot",	w_rot = "w_rot",	wind_speed = "wind_speed",
#                    max_wind_speed = "max_wind_speed",	wind_dir = "wind_dir",	ustar = "ustar", TKE =	"TKE",L=	"L",	zL="zL",
#                    bowen_ratio = "bowen_ratio",	Tstar = "Tstar",	model = "model",	x_peak="x_peak",	
#                    x_offset = "x_offset",	x_10 = "x_10", x_30=	"x_30",	x_50 = "x_50",	x_70="x_70",
#                    x_90="x_90", w_unrot =	"w_unrot", Year = "Year", Hour = "Hour", DoY = "DoY")

units.match<-c(	RDateTime="-", date = "-", time =	"-", DOY =	"-", daytime = 	"1=daytime",
                Tau = "kgm-1s-2",qc_Tau =	"-",	H ="Wm-2", qc_H =	"-", rand_err_H = "Wm-2",	LE = "Wm-2", qc_LE =	"-", rand_err_LE=	"Wm-2",	
                co2_flux = "µmolm-2s-1", qc_co2_flux = "-",rand_err_co2_flux = "µmolm-2s-1",	
                ch4_flux = "µmolm-2s-1",qc_ch4_flux = "-",rand_err_ch4_flux = "µmolm-2s-1",	
                co2_mixing_ratio = "ppm",h2o_mixing_ratio = "mmol mol-1", ch4_mixing_ratio = "ppm", 
                air_temperature = "K",	air_pressure = "Pa", air_density = 	"kgm-3",	air_heat_capacity = "Jkg-1K-1",
                ET = "mm",	water_vapor_density = "kgm-3",	e= "Pa",	es="Pa",	specific_humidity = "kgkg-1", RH=	"%", VPD=	"Pa",	
                Tdew= "K",	u_rot = "ms-1",v_rot =	"ms-1",	w_rot = "ms-1",
                wind_speed = "ms-1",	max_wind_speed = "ms-1",	wind_dir = "deg_from_north",
                ustar = "ms-1", TKE =	"m+2s-2",	L = "m",	zL = "-",	bowen_ratio = "-",	Tstar = "K",	model = "0=KJ/1=KM/2=HS",	#footprint model
                x_peak = "m", x_offset = "m",x_10 =	"m", x_30 =	"m",	x_50 = "m",x_70 =	"m", x_90=	"m",	w_unrot = "ms-1", 
                Year = "-", Hour = "-", DoY = "-"
)

#epREddy<-Hmisc::upData(epREddy, labels = varnames.match)
epREddy<-Hmisc::upData(epREddy, units = units.match)

