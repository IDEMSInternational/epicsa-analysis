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
zambia_metadata <- readRDS(here("data", "station", "cleaned", "zambia_station_metadata_cleaned.RDS"))
# station location check
#--------------------------------------------------
zambia_sf <- ne_countries(country = "zambia", returnclass = "sf")
ggplot(zambia_sf) +
  geom_sf() +
  geom_point(data = zambia_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = zambia_metadata, aes(x = longitude, y =
                                                latitude, label = station))
rm(zambia_sf)
# set the path to your parent folder
path <- here("data", "satellite", "Zambia_AgERA5")
# use the recursive argument to list all files in subfolders
files <- list.files(path, recursive = TRUE)
nc <- nc_open(paste0(path, "/", files[1]))
stopifnot(min(nc$dim$lon$vals) < min(zambia_metadata$longitude))
stopifnot(max(nc$dim$lon$vals) > max(zambia_metadata$longitude))
stopifnot(min(nc$dim$lat$vals) < min(zambia_metadata$latitude))
stopifnot(max(nc$dim$lat$vals) > max(zambia_metadata$latitude))
xs <- nc$dim$lon$vals
ys <- nc$dim$lat$vals
resx <- xs[2] - xs[1]
resy <- ys[2] - ys[1]
max_dist <- sqrt((resx/2)^2 + (resy/2)^2)
xy_points <- expand.grid(xs, ys)
xy_extract <- closest_point(points = xy_points,
                            target = zambia_metadata %>%
                              select(longitude, latitude))
xy_extract$station <- zambia_metadata$station
xy_extract$req_longitude <- zambia_metadata$longitude
xy_extract$req_latitude <- zambia_metadata$latitude
xy_extract$dist <- apply(xy_extract, 1, function(r) {
  sp::spDistsN1(matrix(as.numeric(c(r[["Var1"]], r[["Var2"]])), ncol =2),
                as.numeric(c(r[["req_longitude"]],
                             r[["req_latitude"]])))
}
)
stopifnot(all(xy_extract$dist <= max_dist))
print(nc$dim$time$units)
nc_close(nc)
agera5_dfs <- list()
pb <- txtProgressBar(min = 0, max = length(files) * nrow(xy_extract),
                     style = 3)
count <- 0
for(f in files) {
  print(f)
  nc <- nc_open(paste0(path, "/", f))
  df <- tidync(paste0(path, "/", f))
  for(i in seq_len(nrow(xy_extract))) {
    df_tmp <- df %>%
      hyper_filter(lon = lon == xy_extract[i, 1],
                   lat = lat == xy_extract[i, 2]) %>%
      hyper_tibble() %>%
      mutate(date = as.Date(time, origin =
                              as.Date(substr(nc$dim$time$units, 12, 21))))
    df_tmp$station <- xy_extract$station[i]
    df_tmp$req_longitude <- xy_extract$req_longitude[i]
    df_tmp$req_latitude <- xy_extract$req_latitude[i]
    df_tmp$dist <- xy_extract$dist[i]
    agera5_dfs[[length(agera5_dfs) + 1]] <- df_tmp
    count <- count + 1
    setTxtProgressBar(pb, count)
  }
  nc_close(nc)
}
zambia_agera5 <- bind_rows(agera5_dfs)

zambia_agera5 <- zambia_agera5 %>% select(station, date, agera5_rain =
                                            Precipitation_Flux)
saveRDS(zambia_agera5, here("data", "satellite", "zambia_agera5.RDS"))
