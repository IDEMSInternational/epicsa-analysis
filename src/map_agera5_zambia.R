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
dir <- "~/Documents/John/PHD 2021/IDEMS work/epicsa-analysis/data/satellite/Zambia_AgERA5"
dir_list <- list.files(dir, recursive = FALSE)

file_name <- list.files(paste0(dir, "/", dir_list[1]), recursive = TRUE)[1]
nc <- nc_open(paste0(dir, "/", dir_list[1], "/", file_name))
lon <- nc$dim$lon$vals
lon <- lon[lon > 21 & lon < 34]
lat <- nc$dim$lat$vals
lat <- lat[lat > -19 & lat < -8]
time <- seq(as.Date("1979/01/01"), as.Date("2022/12/31"), by = 1)
years <- min(year(time)):max(year(time))
nc_close(nc)

total_arr <- array(NA, dim = c(length(lon), length(lat), length(years)), dimnames = list(lon, lat, years))
mean_arr <- array(NA, dim = c(length(lon), length(lat), length(years)), dimnames = list(lon, lat, years))
n_arr <- array(NA, dim = c(length(lon), length(lat), length(years)), dimnames = list(lon, lat, years))

folder_indices <- seq(1, length(dir_list), 2)
for(y in seq_along(years)) { 
  months <- 1:12
  yy <- years[y]
  print(yy)
  days <- seq(as.Date(paste(yy, "01", "01", sep = "/")), as.Date(paste(yy, "12", "31", sep = "/")), by = 1)
  y_arr <- array(NA, c(length(lon), length(lat), length(days)), dimnames = list(lon, lat, as.character(days)))
  
  first_folder_in_year_index <- folder_indices[y]
  year_folder_list <- c(dir_list[first_folder_in_year_index], dir_list[first_folder_in_year_index+1])
  
  year_files <- c()
  for (folder in year_folder_list){
    year_files[length(year_files)+1 : length(list.files(paste0(dir, "/", folder), recursive = TRUE))] <- list.files(paste0(dir, "/", folder), recursive = TRUE, full.names = TRUE)
  }
  
  doy <- 1
  for (f in year_files){
    
    x <- tidync(f) %>%
      hyper_filter(longitude = longitude > 21 & longitude < 34, latitude = latitude > -19 & latitude < -8) %>%
      hyper_array(select_var = "Precipitation_Flux")
      print(x[[1]])
      k <- x[[1]]
      k_rec <- apply(k, 1:2, reLu)
      y_arr[,,doy] <- k_rec
      doy <- doy + 1
  }
  
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

#saveRDS(total_means, here("results", "maps", "agera5_zambia_total.RDS"))
#saveRDS(n_means, here("results", "maps", "agera5_zambia_n.RDS"))
#saveRDS(mean_means, here("results", "maps", "agera5_zambia_mean.RDS"))

#total_means <- readRDS(here("results", "maps", "agera5_zambia_total.RDS"))
#n_means <- readRDS(here("results", "maps", "agera5_zambia_n.RDS"))
#mean_means <- readRDS(here("results", "maps", "agera5_zambia_mean.RDS"))

#zambia <- readRDS(here("data", "station", "cleaned", "zambia_1979_qc.RDS"))
zambia_metadata <- readRDS(here("data", "station", "cleaned", "zambia_station_metadata_cleaned.RDS"))

zambia_sf <- ne_countries(country = "Zambia", returnclass = "sf")
agera5_total_rain <- ggplot(zambia_sf) + 
  geom_raster(data = total_means, aes(x = lon, y = lat, fill = total_rain)) +
  geom_sf(fill = NA, colour = "black") +
  geom_point(data = zambia_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = zambia_metadata, aes(x = longitude, y = latitude, label = station)) +
  labs(fill = "Total Rainfall (mm/year)", title = "AgERA5: Mean Annual Total Rainfall 1979 - 2022",
       x = "Longitude", y = "Latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(breaks = seq(500, 5000, 500), limits = c(500, 5000), direction = -1)
ggsave(here("results", "maps", "agera5_zambia_total_rain.png"), 
       agera5_total_rain, 
       width = 16, height = 8)

agera5_n_rain <- ggplot(zambia_sf) + 
  geom_raster(data = n_means, aes(x = lon, y = lat, fill = n_rain)) +
  geom_sf(fill = NA, colour = "black") +
  geom_point(data = zambia_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = zambia_metadata, aes(x = longitude, y = latitude, label = station)) +
  labs(fill = "Number of Rain Days\n(rainday/year)", title = "AgERA5: Mean Annual Number of Rain Days 1979 - 2022",
       x = "Longitude", y = "Latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(breaks = seq(50, 250, 25), limits = c(50, 250), direction = -1)
ggsave(here("results", "maps", "agera5_zambia_n_rain.png"), 
       agera5_n_rain,
       width = 18, height = 10)

agera5_mean_rain <- ggplot(zambia_sf) + 
  geom_raster(data = mean_means, aes(x = lon, y = lat, fill = mean_rain)) +
  geom_sf(fill = NA, colour = "black") +
  geom_point(data = zambia_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = zambia_metadata, aes(x = longitude, y = latitude, label = station)) +
  labs(fill = "Mean Rain per Rainday", title = "AgERA5: Mean Rain per Rainday (mm/rainday) 1979 - 2022",
       x = "Longitude", y = "Latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(direction = -1, limits = c(5, 25))
ggsave(here("results", "maps", "agera5_zambia_mean_rain.png"), width = 16, height = 8)
