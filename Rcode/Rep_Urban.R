rm(list = ls())
# -------------------------------------------------------------------------
# Urban Replication: Unwatched pollution
# ----------------------------------------------------------------
# Contact Information--------------------------------

# Modified    : Dec 07 2022
# Created     : Oct 25 2022
# Author      : Wonjong Kim
# Affiliation : University of Alabama
# E-mail      : wkim22@crimson.ua.edu
# ----------------------------------------------------------------------


# PM monitor data --------------------------------------
# Import first file, 2001
drct <- paste0(getwd(), "/Data/1_build")

epa.2001 <- read.csv(file = paste0(drct, "/epa/raw/annual_all_2001.csv"),
                  sep = ",",
                  quote = '\"', 
                  header = TRUE, 
                  colClasses = "character") 
setDT(epa.2001)

head(epa.2001, 10)
str(epa.2001)
summary(epa.2001)

## Just use PM10 and PM2.5
for (PM in c("PM10", "PM25")){
  Pname <- ifelse(PM == "PM10", "epa.2001.PM10", "epa.2001.PM25")
  Pcode <- ifelse(PM == "PM10", "81102", "88101")
  Pstan <- ifelse(PM == "PM10", "PM10 24-hour 2006", "PM25 24-hour 2006")
  
  
  assign(Pname, epa.2001[Parameter.Code == Pcode])
  
  
  ## Check 1 obs is state + county + site + poc + sample duration + eventtype + pollutantstandard 
  ### sort
  setorder(get(Pname), State.Code, County.Code, Site.Num, POC, 
                     Sample.Duration, Event.Type, Pollutant.Standard)
  
  ## Keep hourdata(2006 standard)
  assign(Pname, get(Pname)[Pollutant.Standard == Pstan])

  
  ## Keep relevant variables
  
  selectvar <- c("State.Code", "County.Code", "Site.Num", "POC", "Year",
                 "Completeness.Indicator", "Valid.Day.Count", "Required.Day.Count",
                 "Null.Data.Count", "Arithmetic.Mean",
                 "X99th.Percentile", "X98th.Percentile", "X95th.Percentile",
                 "X90th.Percentile", "X75th.Percentile", "X50th.Percentile",
                 "X10th.Percentile")
  assign(Pname, get(Pname)[,..selectvar])

}

epa.master.PM10 <- epa.2001.PM10
epa.master.PM25 <- epa.2001.PM25

# Iterate for 2002-2013 (and append it)
for (i in 2002:2013){
  epa.append <- read.csv(file = paste0(drct, "/epa/raw/annual_all_",i,".csv"),
                         sep = ",",
                         quote = '\"', 
                         header = TRUE,
                         colClasses = "character")
  setDT(epa.append)
  
  for (PM in c("PM10", "PM25")){
    Pname <- ifelse(PM == "PM10", "epa.append.PM10", "epa.append.PM25")
    Pcode <- ifelse(PM == "PM10", "81102", "88101")
    Pstan <- ifelse(PM == "PM10", "PM10 24-hour 2006", "PM25 24-hour 2006")
    
    
    
    assign(Pname, epa.append[Parameter.Code == Pcode])
    
    
    ## Check 1 obs is state + county + site + poc + sample duration + eventtype + pollutantstandard 
    ### sort
    setorder(get(Pname), State.Code, County.Code, Site.Num, POC, 
             Sample.Duration, Event.Type, Pollutant.Standard)
    
    ## Keep hourdata(2006 standard)
    assign(Pname, get(Pname)[Pollutant.Standard == Pstan])
    
    
    ## Keep relevant variables
    
    selectvar <- c("State.Code", "County.Code", "Site.Num", "POC", "Year",
                   "Completeness.Indicator", "Valid.Day.Count", "Required.Day.Count",
                   "Null.Data.Count", "Arithmetic.Mean",
                   "X99th.Percentile", "X98th.Percentile", "X95th.Percentile",
                   "X90th.Percentile", "X75th.Percentile", "X50th.Percentile",
                   "X10th.Percentile")
    assign(Pname, get(Pname)[,..selectvar])
    
  }
  epa.master.PM10 <- bind_rows(epa.master.PM10, epa.append.PM10)
  epa.master.PM25 <- bind_rows(epa.master.PM25, epa.append.PM25)
}

# Convert some variables as numeric
for (a in c("POC", "Year", "Valid.Day.Count", "Required.Day.Count",
            "Null.Data.Count", "Arithmetic.Mean", 
            "X99th.Percentile", "X98th.Percentile", "X95th.Percentile",
            "X90th.Percentile", "X75th.Percentile", "X50th.Percentile",
            "X10th.Percentile")){
  epa.master.PM10 <- epa.master.PM10[,a:= as.numeric(get(a)),with = FALSE]
  epa.master.PM25 <- epa.master.PM25[,a:= as.numeric(get(a)),with = FALSE]
  
}




# Monitor entry-exit info ------------------------------------------------
for (PM in c("PM10", "PM25")){
  Pname <- ifelse(PM == "PM10", "entext.PM10", "entext.PM25")
  Pcode <- ifelse(PM == "PM10", "81102", "88101")
  
  entext <- read.csv(file = paste0(drct, "/epa/raw/aqs_monitors.csv"),
                     sep = ",",
                     quote = '\"', 
                     header = TRUE,
                     colClasses = "character")
  
  str(entext)
  
  setDT(entext)
  
  # Drop unused obs (In paper)
  assign(Pname, entext[State.Code != "40" & 
                         County.Code != "71" &
                         Site.Number != "604" &
                         POC != " 3"&
                         Monitor.Type != "SPM"]
         )  
  setDT(get(Pname))
  ## only PM10 or PM2.5
  assign(Pname, get(Pname)[Parameter.Code == "81102"])
  
  ## Drop monitors in CC (Canada, Mexico)
  
  setorder(get(Pname), State.Code, County.Code, Site.Number, 
           Parameter.Code, POC)
  
  
  # Keep relevant variables
  selectvar <- c("State.Code", "County.Code", "Site.Number", "POC",
                 "First.Year.of.Data", "Last.Sample.Date"
                 )
  assign(Pname, get(Pname)[,..selectvar])
  # Rename variables
  setnames(get(Pname), c("Site.Number"), c("Site.Num"))

}

# Convert some variables as numeric
for (a in c("POC", "First.Year.of.Data")){
  entext.PM10 <- entext.PM10[,a:= as.numeric(get(a)),with = FALSE]
  entext.PM25 <- entext.PM25[,a:= as.numeric(get(a)),with = FALSE]
}

str(epa.master.PM10)
str(entext.PM10)

# Merge annual & entry exit data ----------------------------------------------------

setkey(epa.master.PM10, State.Code, County.Code, Site.Num, POC)
setkey(entext.PM10, State.Code, County.Code, Site.Num, POC)
# Merge m:1 (left join)
epa.PM10 <- entext.PM10[epa.master.PM10]

setkey(epa.master.PM25, State.Code, County.Code, Site.Num, POC)
setkey(entext.PM25, State.Code, County.Code, Site.Num, POC)
# Merge m:1 (left join)
epa.PM25 <- entext.PM25[epa.master.PM10]

# Remove 
rm(list = ls()[!ls() %in% c("drct", "epa.PM10", "epa.PM25")])

# Melt(Collapse monitor level profile to site level profile) ----------------------------
epa.full <- bind_rows(epa.PM10, epa.PM25)
setDT(epa.full)
# Keep complete monitors (As paper did)
epa.full <- epa.full[Completeness.Indicator != "N"]

str(epa.full)
# Monitor offdays indicators (by monitor)
## 1-in-k days(6, 3, 1 days.)
epa.full <- epa.full[Required.Day.Count %in% c(60, 61), schd1.1in6d := 1]
epa.full <- epa.full[!Required.Day.Count %in% c(60, 61), schd1.1in6d := 0]

epa.full <- epa.full[Required.Day.Count %in% c(121, 122), schd1.1in3d := 1]
epa.full <- epa.full[!Required.Day.Count %in% c(121, 122), schd1.1in3d := 0]

epa.full <- epa.full[Required.Day.Count %in% c(365, 366), schd1.1in1d := 1]
epa.full <- epa.full[!Required.Day.Count %in% c(365, 366), schd1.1in1d := 0]

# Melt site-year level
## (See how many monitors in the sites has offdays, portion.)
epa.col <- epa.full[, .(schd1.1in6d = mean(schd1.1in6d),
                        schd1.1in3d = mean(schd1.1in3d),
                        schd1.1in1d = mean(schd1.1in1d)),
                    by = .(State.Code, County.Code, Site.Num, Year)]

## Two indicators
## Offdays in any site
epa.col <- epa.col[schd1.1in6d != 0, any1in6d := 1]
epa.col <- epa.col[schd1.1in6d == 0, any1in6d := 0]

epa.col <- epa.col[schd1.1in3d != 0, any1in3d := 1]
epa.col <- epa.col[schd1.1in3d == 0, any1in3d := 0]

epa.col <- epa.col[schd1.1in1d != 0, any1in1d := 1]
epa.col <- epa.col[schd1.1in1d == 0, any1in1d := 0]
## Offdays in all sites
epa.col <- epa.col[schd1.1in6d == 1, all1in6d := 1]
epa.col <- epa.col[schd1.1in6d != 1, all1in6d := 0]

epa.col <- epa.col[schd1.1in3d == 1, all1in3d := 1]
epa.col <- epa.col[schd1.1in3d != 1, all1in3d := 0]

epa.col <- epa.col[schd1.1in1d == 1, all1in1d := 1]
epa.col <- epa.col[schd1.1in1d != 1, all1in1d := 0]


## Map sites data to grid (Use modified GIS data)

### Grid data
GRD.raw <- sf::st_read(paste0(drct, "/epa/proc/gisout_site_to_grid_cw.shp"))
setDT(GRD.raw)

GRD <- GRD.raw[, c("statecode", "countycode", "sitenum", "OBJECTID")]
setnames(GRD, c("statecode", "countycode", "sitenum", "OBJECTID"),
         c("State.Code", "County.Code", "Site.Num", "gridID"))

setnames(GRD,
         c("State.Code", "County.Code", "Site.Num"), 
         c("statecode", "countycode", "sitenum"))


# US National Grid data --------------------------------------------------
USNG.raw <- sf::st_read(paste0(drct, "/usng/proc/gisout_grid_to_county_cw.shp"))
setDT(USNG.raw)
USNG <- USNG.raw[, c("OBJECTID", "STATE", "COUNTY")]

## generate variables
USNG <- USNG[, countyfip := paste0(STATE,COUNTY)]
USNG <- USNG[countyfip == "NANA", countyfip := NA]

## rename
setnames(USNG, c("OBJECTID", "STATE"), c("gridID", "statefip"))
USNG <- USNG[,c("gridID", "statefip", "countyfip")]

# Drop the missing
USNG <- USNG[!countyfip %in% NA]


# Satellite Aerosol data -----------------------------------------------------
## Hard to deal with GIS: use interim dta file instead...
## Already organized one.
modis.raw <- haven::read_dta(paste0(drct, "/modis/modis_grid_day.dta"))
setDT(modis.raw)

# AirNow Air quality Action Day data (Weather Forecast data) --------------------

Actday.raw <- read.csv(file = paste0(drct, "/actday/raw/ActionDayForecasts.csv"),
                       sep = ",",
                       quote = '\"', 
                       header = TRUE, 
                       colClasses = "character")
setDT(Actday.raw)
Actday.raw <- Actday.raw[,Latitude := as.numeric(Latitude)]
Actday.raw <- Actday.raw[,Longitude := as.numeric(Longitude)]


## Generate group ID
Actday <- Actday.raw[, AreaID := .GRP, by = .(ReportingArea, State)]
## Collapse down to area level and keep lat long data
Actday <- Actday[,.(ReportingArea, AreaID, Latitude, Longitude)]
Actday <- Actday[,.SD[1],  by = .(AreaID)]
Actday <- Actday[,Latitude := as.numeric(Latitude)]
Actday <- Actday[,Longitude := as.numeric(Longitude)]

# CBSA boundary file
CBSA <- sf::st_read(paste0(drct, "/actday/proc/gisout_areaID_to_cbsa_cw.shp"))
setnames(CBSA, c("latitude", "longitude"), c("Latitude", 'Longitude'))
setDT(CBSA)

setkey(CBSA, AreaID, Latitude, Longitude)
setkey(Actday, AreaID, Latitude, Longitude)

ACT <- Actday[CBSA]

### Combine code to Actday data
setkey(Actday.raw, AreaID, Latitude, Longitude)
setkey(ACT, AreaID, Latitude, Longitude)
Act.pol <- ACT[Actday.raw]


# Temperature: Global Historical Climatology Network(GHCN) ---------------------
## Use station unique data
GHCN.2001 <- read.csv(file = paste0(drct, "/ghcn/raw/2001-2013/2001.csv"),
                       sep = ",",
                       quote = '\"', 
                       header = FALSE, 
                       colClasses = "character")
## keep if there is no quality issue
setDT(GHCN.2001)
GHCN.2001 <- GHCN.2001[V6 %in% ""]

### Data is too large, use processed data (sample data above)
GHCN <- haven::read_dta(paste0(drct, "/ghcn/ghcn_county_day.dta"))
setDT(GHCN)


# Wind Speed and direction data: North American Regional Reanalysis(NARR) ---------


# Census administrative boundary data ------------------------------------------

## Core based stat area shape file
CBSA.US <- sf::st_read(paste0(drct, "/geo/cbsa_2013/cb_2013_us_cbsa_5m.shp"))

## Census county shapefile

Cnty.Cen <- sf::st_read(paste0(drct, "/geo/county_2010/cnty_cen2010.shp"))


# Create Regression file: site main regression ------------------------------------
head(epa.col)

epa.col.id <-epa.col[, siteyrID := 1:.N] 

## expand to daily panel
epa.daily <- epa.col
for (i in 1:365){
  epa.daily <- bind_rows(epa.daily, epa.col)
}
setDT(epa.daily)
### Sort
setorder(epa.daily, State.Code, County.Code, Site.Num, Year)
### daily variable, day of year
epa.daily <- epa.daily[, doy := 1:.N, by = .(State.Code, County.Code, Site.Num, Year)]
### generate date
epa.daily <- epa.daily[, dt := paste0(1,"/",1,"/",Year)]
epa.daily <- epa.daily[, dt := as.Date(dt, "%m/%d/%Y") + doy - 1]

### Date info
epa.daily <- epa.daily[, month := month(dt)]
epa.daily <- epa.daily[, dow := wday(dt)]

## Generate countyfip code
epa.daily <- epa.daily[,countyfip := paste0(State.Code, County.Code)]
epa.daily <- epa.daily[,countyfip := as.numeric(countyfip)]

# Merge relevant data ---------------------------------------------------------

## merge with GHCN data (weather)
setnames(epa.daily, 
         c("State.Code", "County.Code", "Site.Num", "Year"), 
         c("statecode", "countycode", "sitenum", "year"))

setkey(epa.daily, countyfip, year, doy)
setkey(GHCN, countyfip, year, doy)

master.dat <- epa.daily[GHCN]
master.dat <- master.dat[!statecode %in% NA]  # keep matched data

## merge with grid data
master.dat <- master.dat[, statecode := as.numeric(statecode)]
master.dat <- master.dat[, countycode := as.numeric(countycode)]
master.dat <- master.dat[, sitenum := as.numeric(sitenum)]
setkey(master.dat, statecode, countycode, sitenum)
setkey(GRD, statecode, countycode, sitenum)

master.dat <- master.dat[GRD]
master.dat <- master.dat[!year %in% NA]  # keep matched data

## merge airosol data
setkey(master.dat, gridID, year, doy)
setkey(modis.raw, gridID, year, doy)

master.dat <- modis.raw[master.dat]
master.dat <- master.dat[!aod %in% NA] # keep matched data

## merge observed PM2.5 data, external data
pm25 <- haven::read_dta(paste0(drct, "/misc/pm25_site_day.dta"))
setDT(pm25)

head(pm25)

setkey(master.dat, statecode, countycode, sitenum, year, doy)
setkey(pm25, statecode, countycode, sitenum, year, doy)

master.dat <- pm25[master.dat]
master.dat <- master.dat[!pm25_inc %in% NA] # keep matched data

master.dat.temp <- master.dat

# Generate cyclical event day --------------------------------------------------
Cyclic <- master.dat[, c("siteyrID", "statecode", "countycode", "sitenum", "year", "doy")]
Cyclic <- Cyclic[, cycle_ID := 1:.N]

Cyclic <- Cyclic[, eday1 := doy + 1 - 1]
Cyclic <- Cyclic[, eday2 := doy + 2 - 1]
Cyclic <- Cyclic[, eday3 := doy + 3 - 1]
Cyclic <- Cyclic[, eday4 := doy + 4 - 1]
Cyclic <- Cyclic[, eday5 := doy + 5 - 1]
Cyclic <- Cyclic[, eday6 := doy + 6 - 1]

Cyc.long <- melt(Cyclic, id.vars = c("cycle_ID", "siteyrID", "year"),
                 measure.vars = patterns("^eday"),
                 variable.name = "eday",
                 value.name = "doy")
Cyc.long <- Cyc.long[, eday := str_remove(eday, "eday")]
Cyc.long <- Cyc.long[, eday := as.numeric(eday)]

## merge with master data
setkey(master.dat.temp, siteyrID, year, doy)
setkey(Cyc.long, siteyrID, year, doy)

master.dat <- Cyc.long[master.dat.temp]

## 1 in 3 day cycle
master.dat <- master.dat[, eday_1in3 := eday]
master.dat <- master.dat[eday_1in3 == 4, eday_1in3 := eday - 3]
master.dat <- master.dat[eday_1in3 == 5, eday_1in3 := eday - 3]
master.dat <- master.dat[eday_1in3 == 6, eday_1in3 := eday - 3]

## variables for regression
master.dat <- master.dat[, tmean := (tmax+tmin)/2]
master.dat <- master.dat[!tmean %in% NA]
master.dat <- master.dat[, g10_tmean := min(max(10 * floor(tmean/10), 10), 90)]

## Use wind speed data
narr <- haven::read_dta(paste0(drct, "/narr/narr_county_day.dta"))
setDT(narr)

setkey(master.dat, countyfip, year, doy)
setkey(narr, countyfip, year, doy)

master.dat <- master.dat[narr]
master.dat <- master.dat[!cycle_ID %in% NA]# keep matched data

master.dat <- master.dat[, g_wdsp := cut(wdsp, 5, labels = FALSE)]
setorder(master.dat, cycle_ID, eday)

master.dat <- master.dat[, g_wdsp := g_wdsp[1], by = cycle_ID]

## Log aod
master.dat <- master.dat[, lnaod := log(aod) ]
master.dat <- master.dat[!lnaod %in% NA]
master.dat <- master.dat[!lnaod %in% -Inf]

## group site
master.dat <- master.dat[, g_site := .GRP, by = .(statecode, countycode, sitenum) ]


## 1in6 treat
master.dat <- master.dat[, offday := 1]
master.dat <- master.dat[eday != 1, offday := 0]

## 1in3 treat
master.dat <- master.dat[, offday_1in3 := 1]
master.dat <- master.dat[eday_1in3 != 1, offday_1in3 := 0]

## Scale AOD
master.dat <- master.dat[, aod := aod * 100]

## regression
reg <- lm(lnaod ~ factor(eday), na.action = na.omit, data = master.dat )
print(reg)
summary(reg)

stargazer::stargazer(reg, type = "text")

cis <- coefci(reg)

Result <- as.data.frame(cis)
setDT(Result)

Result <- bind_cols(Result, as.data.frame(reg$coefficients))
setDT(Result)

Result <- Result[, eday := 1:.N]
setnames(Result, c(1,2,3,4), c("ci_lower", "ci_upper", "coef", "eday"))

Result <- Result[eday == 1, ci_lower := NA]
Result <- Result[eday == 1, ci_upper := NA]
Result <- Result[eday == 1, coef := 0]

Result <- Result[, eday := eday - 7]
Result <- Result[eday < -3, eday := eday + 6]

setorder(Result, eday)

ggplot(Result, aes(x = eday)) +
  geom_line(aes(y = coef, color = "coef"))+
  geom_line(aes(y = ci_lower, color = "ci"), linetype = "dashed")+
  geom_line(aes(y = ci_upper, color = "ci"), linetype = "dashed")+
  geom_point(aes(y = coef, color = "coef"))+
  geom_point(aes(y = ci_lower, color = "ci"))+
  geom_point(aes(y = ci_upper, color = "ci"))+
  labs(x = "Days since scheduled monitoring", 
       y = "log Aerosol (Pollution level)", 
       colour = " ")
