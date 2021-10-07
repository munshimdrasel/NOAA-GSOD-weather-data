#script 2
#combined individual years data into one file


rm(list = ls())

library(tidyverse)
library(geosphere) #Needed for Haversine distance
library(readxl)
library(rgeos)
library(zoo)
library(sp)
library(fst)
library(data.table)
library(parallel)
library(sf)
library(viridis)
library(ggplot2)
library(stringi)
library(lubridate)
library(dslabs)
library(stringi)
library(gridExtra)
library(ggmap)
library(Rmisc)
library(usmap)

# setwd("/projects/HAQ_LAB/mrasel/R/NOAA-GSOD-weather-data")
setwd("/Volumes/GoogleDrive/My Drive/R/NOAA-GSOD-weather-data")

met.1997 <- read.fst("data/county_weather_1997.fst")
met.1998 <- read.fst("data/county_weather_1998.fst")
met.1999 <- read.fst("data/county_weather_1999.fst")
met.2000 <- read.fst("data/county_weather_2000.fst")
met.2001 <- read.fst("data/county_weather_2001.fst")
met.2002 <- read.fst("data/county_weather_2002.fst")
met.2003 <- read.fst("data/county_weather_2003.fst")
met.2004 <- read.fst("data/county_weather_2004.fst")
met.2005 <- read.fst("data/county_weather_2005.fst")
met.2006 <- read.fst("data/county_weather_2006.fst")
met.2007 <- read.fst("data/county_weather_2007.fst")
met.2008 <- read.fst("data/county_weather_2008.fst")
met.2009 <- read.fst("data/county_weather_2009.fst")
met.2010 <- read.fst("data/county_weather_2010.fst")
met.2011 <- read.fst("data/county_weather_2011.fst")
met.2012 <- read.fst("data/county_weather_2012.fst")
met.2013 <- read.fst("data/county_weather_2013.fst")
met.2014 <- read.fst("data/county_weather_2014.fst")
met.2015 <- read.fst("data/county_weather_2015.fst")
met.2016 <- read.fst("data/county_weather_2016.fst")
met.2017 <- read.fst("data/county_weather_2017.fst")
met.2018 <- read.fst("data/county_weather_2018.fst")
met.2019 <- read.fst("data/county_weather_2019.fst")
met.2020 <- read.fst("data/county_weather_2020.fst")

#putting all together

met.combined <- rbind(met.1997, met.1998, met.1999, met.2000, met.2001, met.2002, met.2003,
                      met.2004, met.2005, met.2006, met.2007, met.2008, met.2009, met.2010, met.2011,
                      met.2012, met.2013, met.2014, met.2015, met.2016, met.2017, met.2018, met.2019, 
                      met.2020)

#details of dataset can be found here
# https://www.ncei.noaa.gov/data/global-summary-of-the-day/doc/readme.txt



write.fst (met.combined,  "data/us_county_meteorology_1997_2020.fst")


