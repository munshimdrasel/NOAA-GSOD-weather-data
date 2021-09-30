library(tidyverse)
library(geosphere) #Needed for Haversine distance
library(readxl)
library(rgeos)
library(zoo)
library(sp)
library(gsynth)
library(fst)
library(data.table)
library(tidyverse)
library(parallel)
library(sf)
library(viridis)
library(ggplot2)
library(stringi)
library(panelView)
library(lubridate)
library(dslabs)
library(stringi)
library(openair)
library(gridExtra)
library(ggmap)
library(Rmisc)
library(usmap)
library(ggpubr)

setwd("/Volumes/GoogleDrive/My Drive/R/NOAA-GSOD-weather-data")

fips_kevin_study <- read.fst("data/fips_kevin_study.fst")

#murders data from dslabs package has state name which I'll use to merge FIPS code from meteorology data

data(murders)
state.abb <- murders %>% dplyr::select (state, abb)
state.abb <- dplyr::rename(state.abb, "STATE"= "abb")
rm(murders)

ampd_raw <- as.data.table(read.fst ("/Volumes/GoogleDrive/My Drive/R/ampd-raw-data-processing/data/ampd_monthly_all.fst"))

ampd_raw <- ampd_raw [, ID := paste(ORISPL_CODE, UNITID, sep = "-")]


ampd_raw <- merge(ampd_raw, state.abb,  by=c("STATE"), all.x=T )

fips_kevin_study2 <- fips_kevin_study %>% dplyr::rename(state= State.Name, FIPS.Code= County.Code) %>%
  dplyr::select(State.Code,  state)

fips_kevin_study2 <- unique(fips_kevin_study2, by = "state")

#merging Full state name
ampd_raw <-  merge(ampd_raw, fips_kevin_study2, by=c("state" ), all.x=T)


ampd_raw$FIPS.Code <- ampd_raw[ , stri_pad_left(ampd_raw$FIPS.Code, pad="0", width=3)]
ampd_raw$State.Code <- ampd_raw[ , stri_pad_left(ampd_raw$State.Code, pad="0", width=2)]
ampd_raw <-ampd_raw [, fips := paste(State.Code, FIPS.Code, sep = "")]


# Restrict weather data so we only look at stations with recent data
min_weather_end_date <- as.Date("2020-04-01")

#For NYC munging
nyc_boroughs_fips <- c("36081",
                       "36085",
                       "36061",
                       "36047",
                       "36005")
#For KSC munging
#Want Jackson, Clay, Platte, and Cass counties
ksc_county_fips <- c("29037", "29047", 
                     "29095", "29165")

#Read directly from NOAA
gsod_url <- "https://www.ncei.noaa.gov/data/global-summary-of-the-day/access/2020"

gsod_directory_file <- "data/GSOD_directory.txt"
gsod_filenames <- read.table(gsod_directory_file, header = FALSE,
                             stringsAsFactors = FALSE,
                             col.names = c("file","last_modified","time","size"))

#These stations have lots of missing data or other issues, so ignore them.
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
  select(file, last_modified, station_id)

#Reading this fixed-width file is a mess
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

noaa_station_file <- "data/NOAA_GSOD_stations_clean.txt"


#Get station locations
noaa_stations <- read_fwf(noaa_station_file, 
                          fwf_positions(c(1, 8, 14, 44, 49, 52, 58, 66, 75, 83, 92), 
                                        c(7,13, 43, 46, 51, 56, 65, 74, 82, 91, 99), 
                                        noaa_col_names),
                          skip = 1, col_types = "ccccccccccc")


#Must filter by END > a few days ago
#Also filter by BEGIN < Jan 1
#Finally remove the bad stations

min_weather_end_date <- as.Date("2020-04-01")

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
  filter(BEGIN <= as.Date("2020-01-01")) %>%
  filter(!usaf_wban %in% bad_stations)

#Plot station locations
noaa_stations %>%
  ggplot(aes(x=LON, y=LAT)) + 
  geom_point(alpha=0.1) + 
  coord_equal() + 
  ggtitle("NOAA GSOD Weather Station Locations")

# ampd_county <- ampd_daily_units_ec %>% dplyr::select ()


ampd_raw <- ampd_raw %>% filter (!STATE %in% c("PR", "AK") )

#removing facilities with no Facility locations
ampd_raw <- subset(ampd_raw, !is.na(Facility.Latitude))


ampd_unique_id <- (unique(ampd_raw, by= "ORISPL_CODE"))

#View counties
ampd_unique_id %>%
  ggplot(aes(x=Facility.Longitude, y=Facility.Latitude)) + 
  geom_point(alpha=0.5, size=0.5, color = "black") + 
  geom_point(data = noaa_stations, 
             aes(x=LON,y=LAT), 
             color = "red", alpha = 0.1) + 
  coord_fixed(ratio = 1, xlim = c(-170,-60), ylim = c(25,70)) + #Sorry, Hawaii
  ggtitle("US electric facility locations (black) and GSOD weather stations (red)") + theme_bw()

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
ampd_unique_id %>%
  ggplot(aes(x=km_to_closest_station)) + 
  geom_histogram(binwidth=5, color="black") +
  labs(x= "Facility distance to closest NOAA GSOD location (km)", 
       y = "Count",
       title = "") 

# Join county data with closest GSOD station
ampd_id_noaa <- ampd_unique_id %>% 
  left_join(noaa_stations, 
            by = c("closest_station_usaf_wban" = "usaf_wban")) %>%
  select(-CTRY, -ST, -LAT, -LON, -BEGIN, -END)





# Download 2020 daily weather data

# Pull CSVs directly from NOAA's server
#  This takes between 12 and 60 minutes
options(timeout = 400000)

#The ol' loop and bind_rows strategy
all_county_weather <- list()

#Probably a cleverer coder could vectorize or lapply this
for (i in 1:nrow(ampd_id_noaa)){
  #print(i) #Tracks progress
  #For each county, get the daily weather data for 2020
  this_county_fips <- ampd_id_noaa$fips[i]
  
  this_county_weather_file <- ampd_id_noaa$file[i]
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
  
  #Moving averages
  clean_weather_data_with_avgs <- clean_weather_data %>%
    mutate(mean_temp_3d_avg = rollmean(mean_temp, 3, na.pad = TRUE, align = "center")) %>%
    mutate(mean_temp_5d_avg = rollmean(mean_temp, 5, na.pad = TRUE, align = "center")) %>%
    mutate(mean_temp_10d_avg = rollmean(mean_temp, 10, na.pad = TRUE, align = "center")) %>%
    mutate(mean_temp_15d_avg = rollmean(mean_temp, 15, na.pad = TRUE, align = "center")) %>%
    mutate(max_temp_3d_avg = rollmean(max_temp, 3, na.pad = TRUE, align = "center")) %>%
    mutate(max_temp_5d_avg = rollmean(max_temp, 5, na.pad = TRUE, align = "center")) %>%
    mutate(max_temp_10d_avg = rollmean(max_temp, 10, na.pad = TRUE, align = "center")) %>%
    mutate(max_temp_15d_avg = rollmean(max_temp, 15, na.pad = TRUE, align = "center")) %>% 
    mutate(min_temp_3d_avg = rollmean(min_temp, 3, na.pad = TRUE, align = "center")) %>%
    mutate(min_temp_5d_avg = rollmean(min_temp, 5, na.pad = TRUE, align = "center")) %>%
    mutate(min_temp_10d_avg = rollmean(min_temp, 10, na.pad = TRUE, align = "center")) %>%
    mutate(min_temp_15d_avg = rollmean(min_temp, 15, na.pad = TRUE, align = "center")) %>%
    mutate(dewpoint_3d_avg = rollmean(dewpoint, 3, na.pad = TRUE, align = "center")) %>%
    mutate(dewpoint_5d_avg = rollmean(dewpoint, 5, na.pad = TRUE, align = "center")) %>%
    mutate(dewpoint_10d_avg = rollmean(dewpoint, 10, na.pad = TRUE, align = "center")) %>%
    mutate(dewpoint_15d_avg = rollmean(dewpoint, 15, na.pad = TRUE, align = "center"))
  
  #Store in list
  all_county_weather[[i]] <- clean_weather_data_with_avgs
}

#Put it all together
all_county_weather_df <- bind_rows(all_county_weather)

#Plot
all_county_weather_df %>%
  ggplot(aes(x=date, y=mean_temp_3d_avg, 
             group = factor(station_id))) + 
  geom_line(alpha = 0.04, color = "blue") + 
  ylab("Mean daily temperature (F)") + 
  theme(legend.position = "none")
