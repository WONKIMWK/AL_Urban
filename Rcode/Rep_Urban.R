# -------------------------------------------------------------------------
# Urban Replication: Unwatched pollution
# ----------------------------------------------------------------
# Contact Information--------------------------------

# Modified    : Nov 15 2022
# Created     : Oct 25 2022
# Author      : Wonjong Kim
# Affiliation : University of Alabama
# E-mail      : wkim22@crimson.ua.edu
# ----------------------------------------------------------------------


# Import first file, 2001
drct <- paste0(getwd(), "/Data/1_build")

epa.2001 <- read.csv(file = paste0(drct, "/epa/raw/annual_all_2001.csv"),
                  header = TRUE) 
setDT(epa.2001)

head(epa.2001, 10)
str(epa.2001)
summary(epa.2001)

## Just use PM10 and PM2.5
for (PM in c("PM10", "PM25")){
  Pname <- ifelse(PM == "PM10", "epa.2001.PM10", "epa.2001.PM25")
  Pcode <- ifelse(PM == "PM10", 81102, 88101)
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
