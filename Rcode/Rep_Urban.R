rm(list = ls())
# -------------------------------------------------------------------------
# Urban Replication: Unwatched pollution
# ----------------------------------------------------------------
# Contact Information--------------------------------

# Modified    : Nov 30 2022
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

# AirNow Air quality Action Day data (Weather Forecast data) --------------------

Actday.raw <- read.csv(file = paste0(drct, "/actday/raw/ActionDayForecasts.csv"),
                       sep = ",",
                       quote = '\"', 
                       header = TRUE, 
                       colClasses = "character")

# Temperature: Global Historical Climatology Network(GHCN) ---------------------
GHCN.2001 <- read.csv(file = paste0(drct, "/ghcn/raw/2001-2013/2001.csv"),
                       sep = ",",
                       quote = '\"', 
                       header = FALSE, 
                       colClasses = "character")
## keep if there is no quality issue
setDT(GHCN.2001)
GHCN.2001 <- GHCN.2001[V6 %in% ""]

# Wind Speed and direction data: North American Regional Reanalysis(NARR) ---------


# Census administrative boundary data ------------------------------------------

## Core based stat area shape file

## Census county shapefile


