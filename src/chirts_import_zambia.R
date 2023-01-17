library(here)
library(readxl)
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

daily_rain <- readRDS(here("data", "station", "zambia_station.RDS"))
zambia_metadata <- readRDS(here("data", "station", "zambia_metadata.RDS"))

# station location check --------------------------------------------------
zambia_sf <- ne_countries(country = "Zambia", returnclass = "sf")
ggplot(zambia_sf) + 
  geom_sf() +
  geom_point(data = zambia_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = zambia_metadata, aes(x = longitude, y = latitude, label = station))

rm(zambia_sf)



# CHIRTS tmin Import Zambia -----------------------------------------------
tmin_files <- list.files("/media/johnbagiliko/TOSHIBA EXT/E-PICSA/data/satellite/CHIRTS", pattern = "Tmin", full.names = TRUE)

nc <- nc_open(tmin_files[1])
stopifnot(min(nc$dim$X$vals) < min(zambia_metadata$longitude))
stopifnot(max(nc$dim$X$vals) > max(zambia_metadata$longitude))
stopifnot(min(nc$dim$Y$vals) < min(zambia_metadata$latitude))
stopifnot(max(nc$dim$Y$vals) > max(zambia_metadata$latitude))
xs <- nc$dim$X$vals
ys <- nc$dim$Y$vals
resx <- xs[2] - xs[1]
resy <- ys[2] - ys[1]
max_dist <- sqrt((resx/2)^2 + (resy/2)^2)
xy_points <- expand.grid(xs, ys)
xy_extract <- closest_point(points = xy_points, 
                            target = zambia_metadata %>% dplyr::select(longitude, latitude))
xy_extract$station <- zambia_metadata$station
xy_extract$req_longitude <- zambia_metadata$longitude
xy_extract$req_latitude <- zambia_metadata$latitude
xy_extract$dist <- apply(xy_extract, 1, function(r) {
  sp::spDistsN1(matrix(as.numeric(c(r[["Var1"]], r[["Var2"]])), ncol = 2),
                as.numeric(c(r[["req_longitude"]], r[["req_latitude"]])))
}
)
stopifnot(all(xy_extract$dist <= max_dist))
print(nc$dim$`T`$units)
nc_close(nc)

locs_chirts <- xy_extract

tmin_dfs <- list()
pb <- txtProgressBar(min = 0, max = length(tmin_files) * nrow(xy_extract), style = 3)
count <- 0
for(f in tmin_files) {
  for(i in seq_len(nrow(xy_extract))) {
    df <- tidync(f) %>%
      hyper_filter(X = X == xy_extract[i, 1], 
                   Y = Y == xy_extract[i, 2]) %>%
      hyper_tibble()
    df$station <- xy_extract$station[i]
    df$req_longitude <- xy_extract$req_longitude[i]
    df$req_latitude <- xy_extract$req_latitude[i]
    df$dist <- xy_extract$dist[i]
    tmin_dfs[[length(tmin_dfs) + 1]] <- df
    count <- count + 1
    setTxtProgressBar(pb, count)
  }
}
chirts_tmin <- bind_rows(tmin_dfs) %>%
  group_by(station) %>%
  mutate(date = as.Date(T, origin = structure(-2440588, class = "Date"))) %>%
  select(station, date, tmin_chirts = tmin)

# CHIRTS tmax Import Zambia -----------------------------------------------

tmax_files <- list.files("/media/johnbagiliko/TOSHIBA EXT/E-PICSA/data/satellite/CHIRTS", pattern = "tmax", full.names = TRUE)

nc <- nc_open(tmax_files[1])
stopifnot(min(nc$dim$X$vals) < min(temp_metadata$longitude))
stopifnot(max(nc$dim$X$vals) > max(temp_metadata$longitude))
stopifnot(min(nc$dim$Y$vals) < min(temp_metadata$latitude))
stopifnot(max(nc$dim$Y$vals) > max(temp_metadata$latitude))
xs <- nc$dim$X$vals
ys <- nc$dim$Y$vals
resx <- xs[2] - xs[1]
resy <- ys[2] - ys[1]
max_dist <- sqrt((resx/2)^2 + (resy/2)^2)
xy_points <- expand.grid(xs, ys)
xy_extract <- closest_point(points = xy_points, 
                            target = temp_metadata %>% dplyr::select(longitude, latitude))
xy_extract$station <- temp_metadata$station
xy_extract$req_longitude <- temp_metadata$longitude
xy_extract$req_latitude <- temp_metadata$latitude
xy_extract$dist <- apply(xy_extract, 1, function(r) {
  sp::spDistsN1(matrix(as.numeric(c(r[["Var1"]], r[["Var2"]])), ncol = 2),
                as.numeric(c(r[["req_longitude"]], r[["req_latitude"]])))
}
)
stopifnot(all(xy_extract$dist <= max_dist))
print(nc$dim$`T`$units)
nc_close(nc)

tmax_dfs <- list()
pb <- txtProgressBar(min = 0, max = length(tmax_files) * nrow(xy_extract), style = 3)
count <- 0
for(f in tmax_files) {
  for(i in seq_len(nrow(xy_extract))) {
    df <- tidync(f) %>%
      hyper_filter(X = X == xy_extract[i, 1], 
                   Y = Y == xy_extract[i, 2]) %>%
      hyper_tibble()
    df$station <- xy_extract$station[i]
    df$req_longitude <- xy_extract$req_longitude[i]
    df$req_latitude <- xy_extract$req_latitude[i]
    df$dist <- xy_extract$dist[i]
    tmax_dfs[[length(tmax_dfs) + 1]] <- df
    count <- count + 1
    setTxtProgressBar(pb, count)
  }
}
chirts_tmax <- bind_rows(tmax_dfs) %>%
  group_by(station) %>%
  mutate(date = as.Date(T, origin = structure(-2440588, class = "Date"))) %>%
  select(station, date, tmax_chirts = tmax)

chirts_zambia <- full_join(chirts_tmax, chirts_tmin, by = c("station", "date"))

saveRDS(chirts_zambia, here("data", "satellite", "zambia_chirts.RDS"))
