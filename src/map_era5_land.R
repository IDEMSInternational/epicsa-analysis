library(here)
library(ncdf4)
library(dplyr)
library(lubridate)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(tidync)

source(here("src", "helper_funs.R"))
files <- list.files("/media/johnbagiliko/Seagate Backup Plus Drive/ERA5_land", pattern = "africa-total_precipitation", full.names = TRUE)
#leave out 2022 (last 11 months)
nc <- nc_open(files[[1]])
lon <- nc$dim$longitude$vals
lon <- lon[lon > 21 & lon < 34]
lat <- nc$dim$latitude$vals
lat <- lat[lat > -19 & lat < -8]
#time <- seq(as.Date("1960/01/01"), as.Date("2021/12/31"), by = 1)
years <- 1950:1962
nc_close(nc)

total_arr <- array(NA, dim = c(length(lon), length(lat), length(years)), dimnames = list(lon, lat, years))
mean_arr <- array(NA, dim = c(length(lon), length(lat), length(years)), dimnames = list(lon, lat, years))
n_arr <- array(NA, dim = c(length(lon), length(lat), length(years)), dimnames = list(lon, lat, years))

files_path <- "/media/johnbagiliko/Seagate Backup Plus Drive/ERA5_land"

for(y in seq_along(years)) {
  months <- 1:12
  yy <- years[y]
  days <- seq(as.Date(paste(yy, "01", "01", sep = "/")), as.Date(paste(yy, "12", "31", sep = "/")), by = 1)
  y_arr <- array(NA, c(length(lon), length(lat), length(days)), dimnames = list(lon, lat, as.character(days)))
  for(m in months) {
    #mm <- sprintf("%02d", month(days[i]))
    #dd <- sprintf("%02d", day(days[i]))
    mm <- sprintf("%02d", m)
    #"zambia-total_precipitation-201003.nc"
    f <- paste0(files_path, "/", "africa-total_precipitation-", yy , mm, ".nc")
    print(print(f))
    x <- tidync(f) %>%
      hyper_filter(longitude = longitude > 21 & longitude < 34, latitude = latitude > -19 & latitude < -8) %>%
      hyper_array(select_var = "tp")
    print(dim(x[[1]]))
    num_days <- dim(x[[1]])[3]/24
    hourly_rain <- x[[1]]
    #print(hourly_rain)
    initial_day <- 1
    for (d in 1:num_days){
      day_rain <- hourly_rain[,,initial_day:((d*24))]
      day <- as.Date(as.character(paste0(yy, "/", mm, "/", sprintf("%02d", d))))
      doy <- yday(day)
      k <- apply(day_rain, 1:2, sum)
      k_rec <- apply(k, 1:2, reLu)
      k_rec_milimeters <- apply(k_rec, 1:2, function(x) x * 1000)
      y_arr[,,doy] <- k_rec_milimeters
      initial_day <- initial_day + 24
    }
  }
  total_arr[,,y] <- apply(y_arr, 1:2, sum)
  n_arr[,,y] <- apply(y_arr, 1:2, function(x) sum(x > 0.85))
  mean_arr[,,y] <- total_arr[,,y]/n_arr[,,y]
}

total_arr_f <- total_arr[,,2:13]
n_arr_f <- n_arr[,,2:13]
mean_arr_f <- mean_arr[,,2:13]

total_means <- apply(total_arr_f, 1:2, mean)
total_means <- reshape2::melt(total_means)
names(total_means) <- c("lon", "lat", "total_rain")

n_means <- apply(n_arr_f, 1:2, mean)
n_means <- reshape2::melt(n_means)
names(n_means) <- c("lon", "lat", "n_rain")

mean_means <- apply(mean_arr_f, 1:2, mean)
mean_means <- reshape2::melt(mean_means)
names(mean_means) <- c("lon", "lat", "mean_rain")

saveRDS(total_means, here("results", "maps", "era5_land_zambia_total.RDS"))
saveRDS(n_means, here("results", "maps", "era5_land_zambia_n.RDS"))
saveRDS(mean_means, here("results", "maps", "era5_land_zambia_mean.RDS"))

total_means <- readRDS(here("results", "maps", "era5_land_zambia_total.RDS"))
n_means <- readRDS(here("results", "maps", "era5_land_zambia_n.RDS"))
mean_means <- readRDS(here("results", "maps", "era5_land_zambia_mean.RDS"))

#zambia <- readRDS(here("data", "station", "cleaned", "zambia_1979_qc.RDS"))
zambia_metadata <- readRDS(here("data", "station", "cleaned", "zambia_station_metadata_cleaned.RDS"))
#zambia_metadata <- station_metadata %>%
#  filter(country == "Zambia")

zambia_sf <- ne_countries(country = "Zambia", returnclass = "sf")
era5_land_total_rain <- ggplot(zambia_sf) + 
  geom_raster(data = total_means, aes(x = lon, y = lat, fill = total_rain)) +
  geom_sf(fill = NA, colour = "black") +
  geom_point(data = zambia_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = zambia_metadata, aes(x = longitude, y = latitude, label = station)) +
  labs(fill = "Total Rainfall (mm/year)", title = "ERA5_land: Mean Annual Total Rainfall",
       x = "Longitude", y = "Latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(direction = -1)
ggsave(here("results", "maps", "era5_land_zambia_total_rain.png"), 
       era5_land_total_rain, 
       width = 16, height = 8)

era5_land_n_rain <- ggplot(zambia_sf) + 
  geom_raster(data = n_means, aes(x = lon, y = lat, fill = n_rain)) +
  geom_sf(fill = NA, colour = "black") +
  geom_point(data = zambia_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = zambia_metadata, aes(x = longitude, y = latitude, label = station)) +
  labs(fill = "Number of Rain Days\n(rainday/year)", title = "ERA5_land: Mean Annual Number of Rain Days",
       x = "Longitude", y = "Latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(direction = -1)
ggsave(here("results", "maps", "era5_land_zambia_n_rain.png"), 
       era5_land_n_rain,
       width = 16, height = 8)

era5_land_mean_rain <- ggplot(zambia_sf) + 
  geom_raster(data = mean_means, aes(x = lon, y = lat, fill = mean_rain)) +
  geom_sf(fill = NA, colour = "black") +
  geom_point(data = zambia_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = zambia_metadata, aes(x = longitude, y = latitude, label = station)) +
  labs(fill = "Mean Rain per Rainday", title = "ERA5_land: Mean Rain per Rainday (mm/rainday)",
       x = "Longitude", y = "Latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(direction = -1)
ggsave(here("results", "maps", "era5_land_zambia_mean_rain.png"), width = 16, height = 8)
