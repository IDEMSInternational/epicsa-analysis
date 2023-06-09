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

files <- list.files("/media/johnbagiliko/TOSHIBA EXT/E-PICSA/data/satellite/CHIRP/", pattern = "chirp.", full.names = TRUE)
files <- head(files, -1)
#files <- list.files(files_path, pattern = "rfe", full.names = TRUE, recursive = TRUE)

nc <- nc_open(files[[1]])
lon <- nc$dim$lon$vals
lon <- lon[lon > 21 & lon < 34]
lat <- nc$dim$lat$vals
lat <- lat[lat > -19 & lat < -8]
years <- 1981:2021
nc_close(nc)

total_arr <- array(NA, dim = c(length(lon), length(lat), length(years)), dimnames = list(lon, lat, years))
mean_arr <- array(NA, dim = c(length(lon), length(lat), length(years)), dimnames = list(lon, lat, years))
n_arr <- array(NA, dim = c(length(lon), length(lat), length(years)), dimnames = list(lon, lat, years))

files_path <- "/media/johnbagiliko/TOSHIBA EXT/E-PICSA/data/satellite/CHIRP"
for(y in seq_along(years)) {
   #days <- seq(as.Date(paste(years[y], "01", "01", sep = "/")), as.Date(paste(years[y], "12", "31", sep = "/")), by = 1)
   #y_arr <- array(NA, c(length(lon), length(lat), length(days)), dimnames = list(lon, lat, 1))
    f <- paste0(files_path, "/", "chirp.",years[y], ".days_p05.nc")
    print(f)
    x <- tidync(f) %>%
      hyper_filter(longitude = longitude > 21 & longitude < 34, latitude = latitude > -19 & latitude < -8) %>%
      hyper_array(select_var = "precip")
      y_arr <-x[[1]] 
      print(y_arr)
  #}
    
  total_arr[,,y] <- apply(y_arr, 1:2, sum)
  n_arr[,,y] <- apply(y_arr, 1:2, function(x) sum(x > 0.85))
  mean_arr[,,y] <- total_arr[,,y]/n_arr[,,y]
}


total_means <- apply(total_arr, 1:2, mean)
total_means <- reshape2::melt(total_means)
names(total_means) <- c("lon", "lat", "total_rain")

n_means <- apply(n_arr, 1:2, mean)
n_means <- reshape2::melt(n_means)
names(n_means) <- c("lon", "lat", "n_rain")

mean_means <- apply(mean_arr, 1:2, mean)
mean_means <- reshape2::melt(mean_means)
names(mean_means) <- c("lon", "lat", "mean_rain")

saveRDS(total_means, here("results", "maps", "chirp_zambia_total.RDS"))
saveRDS(n_means, here("results", "maps", "chirp_zambia_n.RDS"))
saveRDS(mean_means, here("results", "maps", "chirp_zambia_mean.RDS"))

total_means <- readRDS(here("results", "maps", "chirp_zambia_total.RDS"))
n_means <- readRDS(here("results", "maps", "chirp_zambia_n.RDS"))
mean_means <- readRDS(here("results", "maps", "chirp_zambia_mean.RDS"))

#zambia <- readRDS(here("data", "station", "cleaned", "zambia_1979_qc.RDS"))
zambia_metadata <- readRDS(here("data", "station", "cleaned", "zambia_station_metadata_cleaned.RDS"))
#zambia_metadata <- station_metadata %>%
#  filter(country == "Zambia")

zambia_sf <- ne_countries(country = "Zambia", returnclass = "sf")
chirp_total_rain <- ggplot(zambia_sf) + 
  geom_raster(data = total_means, aes(x = lon, y = lat, fill = total_rain)) +
  geom_sf(fill = NA, colour = "black") +
  geom_point(data = zambia_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = zambia_metadata, aes(x = longitude, y = latitude, label = station)) +
  labs(fill = "Total rainfall (mm)", title = "CHIRP: Mean Yearly Rainfall (mm/year) 1981 - 2021") +
  scale_fill_viridis_c(limits = c(350, 2050), direction = -1)
ggsave(here("results", "maps", "chirp_zambia_total_rain.png"), 
       chirp_total_rain,
       width = 16, height = 8)

chirp_n_rain <- ggplot(zambia_sf) + 
  geom_raster(data = n_means, aes(x = lon, y = lat, fill = n_rain)) +
  geom_sf(fill = NA, colour = "black") +
  geom_point(data = zambia_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = zambia_metadata, aes(x = longitude, y = latitude, label = station)) +
  labs(fill = "Number of rain days", title = "CHIRP: Mean Yearly Number of rain days (rainday/year) 1981 - 2021") +
  scale_fill_viridis_c( direction = -1)
ggsave(here("results", "maps", "chirp_zambia_n_rain.png"), 
       chirp_n_rain,
       width = 16, height = 8)

chirp_mean_rain <- ggplot(zambia_sf) + 
  geom_raster(data = mean_means, aes(x = lon, y = lat, fill = mean_rain)) +
  geom_sf(fill = NA, colour = "black") +
  geom_point(data = zambia_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = zambia_metadata, aes(x = longitude, y = latitude, label = station)) +
  labs(fill = "Average rainfall (mm)", title = "CHIRP: Mean Rain per rainday (mm/rainday) 1981 - 2021") +
  scale_fill_viridis_c(limits = c(4, 12), direction = -1)
ggsave(here("results", "maps", "chirp_zambia_mean_rain.png"), 
       chirp_mean_rain,
       width = 16, height = 8)
