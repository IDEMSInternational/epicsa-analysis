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

files <- c()
for(y in 1983:2022) {
  for(m in 1:12) {
    mm <- sprintf("%02d", m)
    files <- c(files, list.files(paste(paste0("/media/johnbagiliko/TOSHIBA EXT/E-PICSA/data/satellite/TAMSAT/TAMSATv3.1_rfe_daily_", y), y, mm, sep = "/"), 
                                 full.names = TRUE))
  }
}

#files <- list.files(files_path, pattern = "rfe", full.names = TRUE, recursive = TRUE)

files_path <- "/media/johnbagiliko/TOSHIBA EXT/E-PICSA/data/satellite/TAMSAT"

nc <- nc_open(files[[1]])
lon <- nc$dim$lon$vals
lon <- lon[lon > 21 & lon < 34]
lat <- nc$dim$lat$vals
lat <- lat[lat > -19 & lat < -8]
time <- seq(as.Date("1983/01/01"), as.Date("2022/12/15"), by = 1)
years <- min(year(time)):max(year(time))
nc_close(nc)

total_arr <- array(NA, dim = c(length(lon), length(lat), length(years)), dimnames = list(lon, lat, years))
mean_arr <- array(NA, dim = c(length(lon), length(lat), length(years)), dimnames = list(lon, lat, years))
n_arr <- array(NA, dim = c(length(lon), length(lat), length(years)), dimnames = list(lon, lat, years))

for(y in seq_along(years)) {
  yy <- years[y]
  days <- seq(as.Date(paste(yy, "01", "01", sep = "/")), as.Date(paste(yy, "12", "31", sep = "/")), by = 1)
  y_arr <- array(NA, c(length(lon), length(lat), length(days)), dimnames = list(lon, lat, as.character(days)))
  for(i in seq_along(days)) {
    mm <- sprintf("%02d", month(days[i]))
    dd <- sprintf("%02d", day(days[i]))
    f <- paste0(files_path, "/", "TAMSATv3.1_rfe_daily_",yy, "/", yy, "/", mm, "/", "rfe", yy, "_", mm, "_", dd, ".v3.1", ".nc")
    print(f)
    x <- tidync(f) %>%
      hyper_filter(lon = lon > 21 & lon < 34, lat = lat > -19 & lat < -8) %>%
      hyper_array(select_var = "rfe_filled")
    k <- x[[1]]
    k_rec <- apply(k, 1:2, reLu)
    y_arr[,,i] <- k_rec
    print(k_rec)
  }
  total_arr[,,y] <- apply(y_arr, 1:2, sum)
  n_arr[,,y] <- apply(y_arr, 1:2, function(x) sum(x > 0.85))
  mean_arr[,,y] <- total_arr[,,y]/n_arr[,,y]
}

total_arr_39 <- total_arr[,,1:39]
n_arr_39 <- n_arr[,,1:39]
mean_arr_39 <- mean_arr[,,1:39]

total_means <- apply(total_arr_39, 1:2, mean)
total_means <- reshape2::melt(total_means)
names(total_means) <- c("lon", "lat", "total_rain")

n_means <- apply(n_arr_39, 1:2, mean)
n_means <- reshape2::melt(n_means)
names(n_means) <- c("lon", "lat", "n_rain")

mean_means <- apply(mean_arr_39, 1:2, mean)
mean_means <- reshape2::melt(mean_means)
names(mean_means) <- c("lon", "lat", "mean_rain")

saveRDS(total_means, here("results", "maps", "tamsat_zambia_total.RDS"))
saveRDS(n_means, here("results", "maps", "tamsat_zambia_n.RDS"))
saveRDS(mean_means, here("results", "maps", "tamsat_zambia_mean.RDS"))

total_means <- readRDS(here("results", "maps", "tamsat_zambia_total.RDS"))
n_means <- readRDS(here("results", "maps", "tamsat_zambia_n.RDS"))
mean_means <- readRDS(here("results", "maps", "tamsat_zambia_mean.RDS"))

#zambia <- readRDS(here("data", "station", "cleaned", "zambia_1979_qc.RDS"))
zambia_metadata <- readRDS(here("data", "station", "cleaned", "zambia_station_metadata_cleaned.RDS"))
#zambia_metadata <- station_metadata %>%
#  filter(country == "Zambia")

zambia_sf <- ne_countries(country = "Zambia", returnclass = "sf")
tamsat_total_rain <- ggplot(zambia_sf) + 
  geom_raster(data = total_means, aes(x = lon, y = lat, fill = total_rain)) +
  geom_sf(fill = NA, colour = "black") +
  geom_point(data = zambia_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = zambia_metadata, aes(x = longitude, y = latitude, label = station)) +
  labs(fill = "Total Rainfall (mm/year)", title = "TAMSAT: Mean Annual Total Rainfall 1983 - 2021",
       x = "Longitude", y = "Latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(limits = c(400, 2000), direction = -1)
ggsave(here("results", "maps", "tamsat_zambia_total_rain.png"), 
       tamsat_total_rain, 
       width = 16, height = 8)

tamsat_n_rain <- ggplot(zambia_sf) + 
  geom_raster(data = n_means, aes(x = lon, y = lat, fill = n_rain)) +
  geom_sf(fill = NA, colour = "black") +
  geom_point(data = zambia_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = zambia_metadata, aes(x = longitude, y = latitude, label = station)) +
  labs(fill = "Number of Rain Days\n(rainday/year)", title = "TAMSAT: Mean Annual Number of Rain Days 1983 - 2021",
       x = "Longitude", y = "Latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(breaks = seq(50, 150, 25), limits = c(50, 150), direction = -1)
ggsave(here("results", "maps", "tamsat_zambia_n_rain.png"), 
       tamsat_n_rain,
       width = 16, height = 8)

tamsat_mean_rain <- ggplot(zambia_sf) + 
  geom_raster(data = mean_means, aes(x = lon, y = lat, fill = mean_rain)) +
  geom_sf(fill = NA, colour = "black") +
  geom_point(data = zambia_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = zambia_metadata, aes(x = longitude, y = latitude, label = station)) +
  labs(fill = "Mean Rain per Rainday", title = "TAMSAT: Mean Rain per Rainday (mm/rainday) 1983 - 2021",
       x = "Longitude", y = "Latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(direction = -1)
ggsave(here("results", "maps", "tamsat_zambia_mean_rain.png"), width = 16, height = 8)
