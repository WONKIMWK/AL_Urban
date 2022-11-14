# -------------------------------------------------------------------------
# Urban Replication: Unwatched pollution
# ----------------------------------------------------------------
# Contact Information--------------------------------

# Modified    : Nov 10 2022
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
epa.2001 <- epa.2001[Parameter.Code == 88101|
                     Parameter.Code == 81102]

## Check 1 obs is state + county + site + poc + sample duration + eventtype + pollutantstandard 
### sort
setorder(epa.2001, State.Code, County.Code, Site.Num, POC, 
                   Sample.Duration, Event.Type, Pollutant.Standard)

## Keep relevant variables

