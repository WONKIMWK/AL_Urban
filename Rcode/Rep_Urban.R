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


# PM monitor data
# Import first file, 2001
drct <- paste0(getwd(), "/Data/1_build")

epa.2001 <- read.table(file = paste0(drct, "/epa/raw/annual_all_2001.csv"),
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




# Monitor entry-exit info
entext <- read.csv(file = paste0(drct, "/epa/raw/aqs_monitors.csv"),
                   sep = ",",
                   quote = '\"', 
                   header = TRUE,
                   colClasses = "character")

str(entext)

setDT(entext)

entext.PM10 <- entext[State.Code != "40" & 
                      County.Code != "71" &
                      Site.Number != "604" &
                      POC != " 3"&
                      Monitor.Type != "SPM"]

setorder(entext.PM10, State.Code, County.Code, Site.Number, 
         Parameter.Code, POC)
