#script 1

#thid code's purpose is to download meteorological dataset from NOAA-GSOD website
# website that I followed to build this codes
# https://www.kaggle.com/johnjdavisiv/us-counties-weather-health-hospitals-covid19-data/report

# prepared by: Munshi Md Rasel

rm(list = ls())

library(tidyverse)
library(geosphere) #Needed for Haversine distance
library(readxl)
library(rgeos)
library(zoo)
library(fst)
library(data.table)
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

#getting data set ready

#I need this dataset to get STATE ID information
fips_kevin_study <- read.fst("data/fips_kevin_study.fst")

#Read directly from NOAA
gsod_url <- "https://www.ncei.noaa.gov/data/global-summary-of-the-day/access/2000"

#I went to the above address and then downloaded site information using "Download table as CSV" extension on  google chrome
#manually deleted first couple of lines and got GSOD_stations_20XX.txt file

#this file containts stations information for that specific year
gsod_directory_file <- "data/GSOD_stations_2000.txt"

# Restrict weather data so we only look at stations with recent data
min_weather_begin_date <- as.Date("2000-01-01")
min_weather_end_date <- as.Date("2001-01-01")


#murders data from dslabs package has state name which I'll use to merge FIPS code from meteorology data

data(murders)
state.abb <- murders %>% dplyr::select (state, abb)
state.abb <- dplyr::rename(state.abb, "STATE"= "abb")
rm(murders)

# ampd_raw <- as.data.table(read.fst ("/projects/HAQ_LAB/mrasel/R/ampd-raw-data-processing/data/ampd_monthly_all.fst"))

#getting monthly AMPD dataset to get each facilities counties fips code
ampd_raw <- as.data.table(read.fst ("/Volumes/GoogleDrive/My Drive/R/ampd-raw-data-processing/data/ampd_monthly_all.fst"))

# ampd_raw <- ampd_raw [, ID := paste(ORISPL_CODE, UNITID, sep = "-")]

ampd_raw <- merge(ampd_raw, state.abb,  by=c("STATE"), all.x=T )

#getting state name and state code
fips_kevin_study2 <- fips_kevin_study %>% dplyr::rename(state= State.Name, FIPS.Code= County.Code) %>%
  dplyr::select(State.Code,  state)

fips_kevin_study2 <- unique(fips_kevin_study2, by = "state")

#merging Full state name
ampd_raw <-  merge(ampd_raw, fips_kevin_study2, by=c("state" ), all.x=T)


#creating county FIPS code with 5 digits (first 2 digit comes from state code and last 3 digit from County FIPS code)
ampd_raw$FIPS.Code <- ampd_raw[ , stri_pad_left(ampd_raw$FIPS.Code, pad="0", width=3)]
ampd_raw$State.Code <- ampd_raw[ , stri_pad_left(ampd_raw$State.Code, pad="0", width=2)]
ampd_raw <-ampd_raw [, fips := paste(State.Code, FIPS.Code, sep = "")]

# Puerterico and DC state code added into the fips column
ampd_raw$fips <- with(ampd_raw, ifelse(fips=="NA057", "72057",ifelse(fips=="NA001", "11001", fips)))

#removing facilities with no Facility locations
ampd_raw <- subset(ampd_raw, !is.na(Facility.Latitude))

ampd_unique_id <- unique(ampd_raw, by= "ORISPL_CODE")



gsod_filenames <- read.table(gsod_directory_file, header = FALSE,
                             stringsAsFactors = FALSE,
                             col.names = c("file","last_modified","time","size"))


gsod_filenames$last_modified <- as.POSIXct(gsod_filenames$last_modified, format = '%m/%d/%y')
# gsod_filenames$last_modified <- as.Date(gsub('^0{2}', '20', gsod_filenames$last_modified))

#These stations have lots of missing data or other issues, so ignore them.
#Kaggle (will look into it)
bad_stations <- c("72211800482", #Sarasota FL
                  "72334703809", #Dyer, TN
                  "99818099999", #Sanilac, MI
                  "99726099999", #Sanilac MI
                  "72522904858",
                  "72340703953",
                  "72028803711",
                  "74003024103", #Tooele, UT
                  "72575399999", #Tooele, UT
                  "91197721508", #Also in the mountains on Hawaii
                  "99999921514") #On top of a volcano at 11,000' in Hawaii

#Set up filenames for all stations
gsod_filenames <- gsod_filenames %>%
  mutate(station_id = sub(".csv", "", file)) %>%
  dplyr::select(file, last_modified, station_id)

#Reading fixed-width file 
noaa_col_names <- c("USAF",
                    "WBAN",
                    "STATION_NAME",
                    "CTRY",
                    "ST",
                    "CALL",
                    "LAT",
                    "LON",
                    "ELEV_M",
                    "BEGIN",
                    "END")

#NOAA station historical informations all over the world
noaa_station_file <- "data/NOAA_GSOD_stations_updated.txt"


#Get station locations
noaa_stations <- read_fwf(noaa_station_file, 
                          fwf_positions(c(1, 8, 14, 44, 49, 52, 58, 66, 75, 83, 92), 
                                        c(7,13, 43, 46, 51, 56, 65, 74, 82, 91, 99), 
                                        noaa_col_names),
                          skip = 1, col_types = "ccccccccccc")

# USAF = Air Force station ID. May contain a letter in the first position.
# WBAN = NCDC WBAN number
# CTRY = FIPS country ID
# ST = State for US stations
# ICAO = ICAO ID
# LAT = Latitude in thousandths of decimal degrees
# LON = Longitude in thousandths of decimal degrees
# ELEV = Elevation in meters
# BEGIN = Beginning Period Of Record (YYYYMMDD). There may be reporting gaps within the P.O.R.
# END = Ending Period Of Record (YYYYMMDD). There may be reporting gaps within the P.O.R.



#Must filter by END > a few days ago
#Also filter by BEGIN < Jan 1
#Finally remove the bad stations
# https://www.ncei.noaa.gov/pub/data/noaa/isd-history.txt




#Join location to file names 
noaa_stations <- noaa_stations %>%
  unite(usaf_wban, USAF, WBAN, sep="") %>%
  mutate(LAT = as.numeric(sub("\\+","", LAT)),
         LON = as.numeric(sub("\\+","", LON)),
         ELEV_M = as.numeric(sub("\\+","", ELEV_M)),
         BEGIN = as.Date(BEGIN, format = "%Y%m%d"),
         END = as.Date(END, format = "%Y%m%d")) %>%
  inner_join(gsod_filenames,
             by = c("usaf_wban" = "station_id")) %>%
  filter(END >= min_weather_end_date) %>%
  filter(BEGIN <= min_weather_begin_date) %>%
  filter(!usaf_wban %in% bad_stations)

#Plot station locations
# noaa_stations %>%
#   ggplot(aes(x=LON, y=LAT)) + 
#   geom_point(alpha=0.1) + 
#   coord_equal() + 
#   ggtitle("NOAA GSOD Weather Station Locations")

# ampd_county <- ampd_daily_units_ec %>% dplyr::select ()


# ampd_raw <- ampd_raw %>% filter (!STATE %in% c("PR", "AK") )



#View facilities
# ampd_unique_id %>%
#   ggplot(aes(x=Facility.Longitude, y=Facility.Latitude)) + 
#   geom_point(alpha=0.5, size=0.5, color = "black") + 
#   geom_point(data = noaa_stations, 
#              aes(x=LON,y=LAT), 
#              color = "red", alpha = 0.1) + 
#   coord_fixed(ratio = 1, xlim = c(-130,-60), ylim = c(25,55)) + #Sorry, Hawaii
#   ggtitle("US electric facility locations (black) and GSOD weather stations (red)") + theme_bw()

#Weather station distance matrix
noaa_longlat <- cbind(noaa_stations$LON, noaa_stations$LAT)


#For each county...make distance matrix, find nearest

for (i in 1:nrow(ampd_unique_id)) {
  #print(i) #for monitoring progress
  
  #For each county...
  this_id_loc <- cbind(ampd_unique_id[i,"Facility.Longitude"], 
                       ampd_unique_id[i,"Facility.Latitude"]) 
  
  #Get distances to all stations - in km
  distance_to_stations <- distHaversine(this_id_loc, noaa_longlat)
  
  #Note closest station
  closest_ind <- which.min(distance_to_stations)
  closest_station_id <- noaa_stations[closest_ind,"usaf_wban"]
  ampd_unique_id$closest_station_usaf_wban[i] <- as.character(closest_station_id)
  ampd_unique_id$km_to_closest_station[i] <- distance_to_stations[closest_ind]/1000
  
} 

ampd_unique_id <- ampd_unique_id %>%  dplyr::select(ORISPL_CODE, fips, closest_station_usaf_wban, km_to_closest_station, STATE)

#Distribution of distances to closest station
# ampd_unique_id %>%
#   ggplot(aes(x=km_to_closest_station)) + 
#   geom_histogram(binwidth=5, color="black") +
#   labs(x= "Facility distance to closest NOAA GSOD location (km)", 
#        y = "Count",
#        title = "") 

# Join county data with closest GSOD station
ampd_id_noaa <- ampd_unique_id %>% 
  left_join(noaa_stations, 
            by = c("closest_station_usaf_wban" = "usaf_wban")) %>%
  select(-CTRY, -ST, -LAT, -LON, -BEGIN, -END)

ampd_noaa_unique_fips <- unique(ampd_id_noaa, by ="fips")

# Download daily weather data

# Pull CSVs directly from NOAA's server

all_county_weather <- list()

#Probably a cleverer coder could vectorize or lapply this
for (i in 1:nrow(ampd_noaa_unique_fips)){
  #print(i) #Tracks progress
  #For each county, get the daily weather data for that year
  this_county_fips <- ampd_noaa_unique_fips$fips[i]
  this_county_weather_file <- ampd_noaa_unique_fips$file[i]
  this_county_weather_url <- paste(gsod_url, this_county_weather_file, sep="/")
  this_county_weather <- read_csv(this_county_weather_url,
                                  col_types = cols(
                                    STATION = col_character(),
                                    DATE = col_date(format = ""),
                                    LATITUDE = col_double(),
                                    LONGITUDE = col_double(),
                                    ELEVATION = col_double(),
                                    NAME = col_character(),
                                    TEMP = col_double(),
                                    TEMP_ATTRIBUTES = col_double(),
                                    DEWP = col_double(),
                                    DEWP_ATTRIBUTES = col_double(),
                                    SLP = col_double(),
                                    SLP_ATTRIBUTES = col_double(),
                                    STP = col_double(),
                                    STP_ATTRIBUTES = col_double(),
                                    VISIB = col_double(),
                                    VISIB_ATTRIBUTES = col_double(),
                                    WDSP = col_double(),
                                    WDSP_ATTRIBUTES = col_double(),
                                    MXSPD = col_double(),
                                    GUST = col_double(),
                                    MAX = col_double(),
                                    MAX_ATTRIBUTES = col_character(),
                                    MIN = col_double(),
                                    MIN_ATTRIBUTES = col_character(),
                                    PRCP = col_double(),
                                    PRCP_ATTRIBUTES = col_character(),
                                    SNDP = col_double(),
                                    FRSHTT = col_character()
                                  ))
  
  #Only keeping relevant/useful data
  #Note: These are still in Freedom Units, not metric
  clean_weather_data <- this_county_weather %>%
    transmute(station_id = STATION,
              station_name = NAME,
              station_lat = LATITUDE,
              station_lon = LONGITUDE,
              date = DATE, 
              mean_temp = TEMP,
              min_temp = MIN,
              max_temp = MAX,
              dewpoint = DEWP,
              sea_level_pressure = SLP,
              station_pressure = STP,
              visibility = VISIB,
              wind_speed = WDSP,
              max_wind_speed = MXSPD,
              wind_gust = GUST,
              precipitation = PRCP,
              precip_flag = PRCP_ATTRIBUTES,
              FRSHTT = FRSHTT) %>%
    separate(FRSHTT, into = c("fog", "rain", "snow", 
                              "hail", "thunder", "tornado"),
             sep=c(1,2,3,4,5)) %>%
    mutate(county_fips = this_county_fips)
  
  #999.9 is the missing data indicator, also 9999.9 and 99.9
  clean_weather_data[clean_weather_data == 99.9] <- NA
  clean_weather_data[clean_weather_data == 99.99] <- NA
  clean_weather_data[clean_weather_data == 999.9] <- NA
  clean_weather_data[clean_weather_data == 9999.9] <- NA
  
    #Store in list
  all_county_weather[[i]] <- clean_weather_data
}

#Put it all together
all_county_weather_df <- bind_rows(all_county_weather)

#details of dataset can be found here
# https://www.ncei.noaa.gov/data/global-summary-of-the-day/doc/readme.txt

#Plot
# all_county_weather_df %>%
#   ggplot(aes(x=date, y=mean_temp_3d_avg, 
#              group = factor(station_id))) + 
#   geom_line(alpha = 0.04, color = "blue") + 
#   ylab("Mean daily temperature (F)") + 
#   theme(legend.position = "none")


write.fst (all_county_weather_df,  "data/county_weather_2000.fst")

# read.fst("data/county_weather_1997.fst")
