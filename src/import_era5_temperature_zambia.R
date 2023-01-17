library(here)
library(dplyr)
library(ggplot2)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(tidync)
library(ncdf4)
library(sp)

source(here("src", "helper_funs.R"))

zambia <- readRDS(here("data", "zambia_station.RDS"))
zambia_metadata <- readRDS(here("data", "zambia_metadata.RDS"))

# station location check --------------------------------------------------
zambia_sf <- ne_countries(country = "Zambia", returnclass = "sf")
ggplot(zambia_sf) + 
  geom_sf() +
  geom_point(data = zambia_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = zambia_metadata, aes(x = longitude, y = latitude, label = station))



# ERA5 Import -------------------------------------------------------------

files <- list.files("E:/datasets/satellite/era5_temp", pattern = "zambia-2m_temperature", full.names = TRUE)

nc <- nc_open(files[1])
stopifnot(min(nc$dim$longitude$vals) < min(zambia_metadata$longitude))
stopifnot(max(nc$dim$longitude$vals) > max(zambia_metadata$longitude))
stopifnot(min(nc$dim$latitude$vals) < min(zambia_metadata$latitude))
stopifnot(max(nc$dim$latitude$vals) > max(zambia_metadata$latitude))
xs <- nc$dim$longitude$vals
ys <- nc$dim$latitude$vals
resx <- xs[2] - xs[1]
resy <- ys[2] - ys[1]
max_dist <- sqrt((resx/2)^2 + (resy/2)^2)
xy_points <- expand.grid(xs, ys)
xy_extract <- closest_point(points = xy_points, 
                            target = zambia_metadata %>% select(longitude, latitude))
xy_extract$station <- zambia_metadata$station
xy_extract$req_longitude <- zambia_metadata$longitude
xy_extract$req_latitude <- zambia_metadata$latitude
xy_extract$dist <- apply(xy_extract, 1, function(r) {
  sp::spDistsN1(matrix(as.numeric(c(r[["Var1"]], r[["Var2"]])), ncol = 2),
                as.numeric(c(r[["req_longitude"]], r[["req_latitude"]])))
}
)
stopifnot(all(xy_extract$dist <= max_dist))
print(nc$dim$time$units)
nc_close(nc)

era5_dfs <- list()
pb <- txtProgressBar(min = 0, max = length(files) * nrow(xy_extract), style = 3)
count <- 0
for(f in files) {
  for(i in seq_len(nrow(xy_extract))) {
    df <- tidync(f) %>%
      hyper_filter(longitude = longitude == xy_extract[i, 1], 
                   latitude = latitude == xy_extract[i, 2]) %>%
      hyper_tibble()
    df$station <- xy_extract$station[i]
    df$req_longitude <- xy_extract$req_longitude[i]
    df$req_latitude <- xy_extract$req_latitude[i]
    df$dist <- xy_extract$dist[i]
    era5_dfs[[length(era5_dfs) + 1]] <- df
    count <- count + 1
    setTxtProgressBar(pb, count)
  }
}
zambia_era5 <- bind_rows(era5_dfs) %>%
  mutate(date_time = as.POSIXct(time * 60 * 60,
                                origin = as.POSIXct("1900/01/01 00:00:00"), tz = "GMT"),
         date = as.Date(date_time))

zambia_era5 <- zambia_era5 %>% select(station, date, era5_temp = t2m)
zambia_era5$era5_temp <- zambia_era5$era5_temp - 273.15

saveRDS(zambia_era5, here("data", "satellite", "zambia_era5_temp.RDS"))
