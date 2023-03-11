library(here)
library(tidyr)
library(dplyr)

chrips_df <- readRDS(here("data", "satellite", "malawi", "cleaned", "malawi_chirps.RDS"))
chrip_df <- readRDS(here("data", "satellite", "malawi", "cleaned", "malawi_chirp.RDS"))
tamsat_rain_df <- readRDS(here("data", "satellite", "malawi", "cleaned", "malawi_tamsat.RDS"))
agera5_rain_df <- readRDS(here("data", "satellite", "malawi", "cleaned", "malawi_agera5_79_22_final.RDS")) %>% 
  unique()

merged_df <- agera5_rain_df %>% group_by(station, date) %>%
  full_join(chrip_df, by = c("station", "date"), suffix = c("", "")) %>%
  full_join(tamsat_rain_df, by = c("station", "date"), suffix = c("", "")) %>%
  full_join(chrips_df, by = c("station", "date"), suffix = c("", "")) %>% 
  group_by(station, date)

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


saveRDS(merged_df, here("data", "satellite", "malawi", "cleaned", "malawi_satellite_merged_final.RDS"))
