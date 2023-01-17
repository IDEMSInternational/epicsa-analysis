library(here)
library(readxl)
library(dplyr)
library(lubridate)

daily_rain <- readRDS("/media/johnbagiliko/TOSHIBA EXT/E-PICSA/data/station/zambia/zambia.RDS")

Chipata_Petauke <- readRDS("/media/johnbagiliko/TOSHIBA EXT/E-PICSA/data/station/zambia/Chipata_Petauke_summary.RDS")$get_data_frame(data_name = "Chipata_Petauke_Stations") %>% 
  rename(tmax = tmax2)
Chipata_Petauke$region <- "Africa"
Chipata_Petauke$country <- "Zambia"

daily_rain <- daily_rain %>%
  rbind(Chipata_Petauke %>% select(region, country, station, date, rain, tmax, tmin))

saveRDS(daily_rain, here("data", "station", "zambia_station.RDS"))

station_metadata <- read_excel("/media/johnbagiliko/TOSHIBA EXT/E-PICSA/data/station/zambia/ZMD COORDINATES_manual.xls")
zambia_metadata <- station_metadata %>%
  rename(station = STATION_NA,
         longitude = LONGITUDE,
         latitude = LATITUDE,
         elevation = ELEVATION) %>%
  select(-elevation) %>%
  rbind(data.frame(station=c("CHIPAT01", "PETAUK01"),
                   longitude=c(32.64, 31.33),
                   latitude=c(-13.64, -14.25))) %>%
  filter(station %in% unique(daily_rain$station))

saveRDS(zambia_metadata, here("data", "station", "zambia_metadata.RDS"))
