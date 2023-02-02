library(here)
library(tidyr)
library(dplyr)
library(lubridate)

#import data
station_df <- readRDS(here("data", "station", "zambia_station.RDS"))
chrips_df <- readRDS(here("data", "satellite", "zambia_chirps.RDS"))
chrip_df <- readRDS(here("data", "satellite", "zambia_chirp.RDS"))
era5_rain_df <- readRDS(here("data", "satellite", "zambia_era5.RDS"))
tamsat_rain_df <- readRDS(here("data", "satellite", "zambia_tamsat.RDS")) %>%
  unique()
if(anyDuplicated(tamsat_rain_df %>% dplyr::select(station, date))) stop("Duplicates found!")
eumetsat_cloud <- readRDS(here("data", "satellite", "zambia_eumetsat_cfc.RDS"))

station_metadata <- readRDS(here("data", "station", "zambia_metadata.RDS"))

merged_df <- station_df %>% group_by(station, date) %>%
  left_join(chrips_df, by = c("station", "date"), suffix = c("", "")) %>%
  left_join(chrip_df, by = c("station", "date"), suffix = c("", "")) %>%
  left_join(era5_rain_df, by = c("station", "date"), suffix = c("", "")) %>%
  left_join(tamsat_rain_df, by = c("station", "date"), suffix = c("", ""))
merged_df$station <- recode(merged_df$station, CHIPAT01 = "Chipata", PETAUK01 = "Petauke")

#Check and fill date gaps
dates_list <- list()
merged_df <- merged_df %>% 
  mutate(year = year(date))
for(s in unique(merged_df$station)) {
  min_year = min((merged_df %>% filter(station == s))$year)
  max_year = max((merged_df %>% filter(station == s))$year)
  #Ensure complete years, starting from January first.
  dates <- seq(as.Date(paste0(min_year, "-01-01")), 
               as.Date(paste0(max_year, "-12-31")),
               by = 1)
  dd <- data.frame(station = s, date = dates)
  dates_list[[length(dates_list) + 1]] <- dd
}
date_df <- bind_rows(dates_list)


nr <- nrow(date_df)
merged_df <- merged_df %>%
  dplyr::select(-year) 
merged_df$date <- as.Date(merged_df$date)
if(nrow(merged_df) < nr) {
  print(paste("Filled data with", nr - nrow(merged_df), "rows"))
  merged_df <- full_join(date_df, merged_df, by = c("station", "date"))
}

if(anyDuplicated(merged_df %>% dplyr::select(station, date))) stop("Duplicates found!")


station_metadata$station <- recode(station_metadata$station, CHIPAT01 = "Chipata", PETAUK01 = "Petauke") 

eumetsat_cloud_filtered_merged <- merged_df %>% 
  filter((date >= as.Date("2001-11-01") ) & (date <= as.Date("2015-12-31"))) %>% left_join(eumetsat_cloud, by = c("station", "date"), suffix = c("", ""))
#save dataframes 
saveRDS(merged_df, here("data", "station", "cleaned", "zambia_station_cleaned.RDS"))
saveRDS(station_metadata, here("data", "station", "cleaned", "zambia_station_metadata_cleaned.RDS"))
saveRDS(eumetsat_cloud_filtered_merged, here("data", "station", "cleaned", "eumetsat_cloud_filtered_merged.RDS"))
