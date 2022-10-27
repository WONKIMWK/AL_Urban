# -------------------------------------------------------------------------
# Urban Replication: Unwatched pollution
# ----------------------------------------------------------------
# Contact Information--------------------------------

# Modified    : Oct 27 2022
# Created     : Oct 25 2022
# Author      : Wonjong Kim
# Affiliation : University of Alabama
# E-mail      : wkim22@crimson.ua.edu
# ----------------------------------------------------------------------

getwd()
drct <- paste0(getwd(), "/Data/124621-V1/replicate_monitor/1_build")

epa.2001 <- read.csv(file = paste0(drct, "/epa/raw/annual_all_2001.csv"),
                  header = TRUE) 
head(dta.1, 10)
str(dta.1)
summary(dta.1)
