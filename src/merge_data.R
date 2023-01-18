library(here)
library(tidyr)
library(dplyr)

#import data
station_df <- readRDS(here("data", "station", "zambia_station.RDS"))
chrips_df <- readRDS(here("data", "satellite", "zambia_chirps.RDS"))
chrip_df <- readRDS(here("data", "satellite", "zambia_chirp.RDS"))
chrits_df <- readRDS(here("data", "satellite", "zambia_chirts.RDS"))
era5_rain_df <- readRDS(here("data", "satellite", "zambia_era5.RDS"))
tamsat_rain_df <- readRDS(here("data", "satellite", "zambia_tamsat.RDS"))

stattion_metadata <- readRDS(here("data", "station", "zambia_metadata.RDS"))

merged_df <- station_df %>% 
  left_join(chrips_df, by = c("station", "date"), suffix = c("", "")) %>%
  left_join(chrip_df, by = c("station", "date"), suffix = c("", "")) %>%
  left_join(chrits_df, by = c("station", "date"), suffix = c("", "")) %>%
  left_join(era5_rain_df, by = c("station", "date"), suffix = c("", "")) %>%
  left_join(tamsat_rain_df, by = c("station", "date"), suffix = c("", ""))
merged_df$station <- recode(merged_df$station, CHIPAT01 = "Chipata", PETAUK01 = "Petauke")

stattion_metadata$station <- recode(stattion_metadata$station, CHIPAT01 = "Chipata", PETAUK01 = "Petauke") 


#save dataframes 
saveRDS(merged_df, here("data", "station", "cleaned", "zambia_station_cleaned.RDS"))
saveRDS(stattion_metadata, here("data", "station", "cleaned", "zambia_station_metadata_cleaned.RDS"))

