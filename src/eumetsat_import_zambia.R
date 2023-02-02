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

zambia <- readRDS(here("data", "cleaned", "zambia_station_cleaned.RDS"))
zambia_metadata <- readRDS(here("data", "cleaned", "zambia_station_metadata_cleaned.RDS")) %>% 
  filter(station == "Moorings")

# station location check --------------------------------------------------
zambia_sf <- ne_countries(country = "Zambia", returnclass = "sf")
ggplot(zambia_sf) + 
  geom_sf() +
  geom_point(data = zambia_metadata, aes(x = longitude, y = latitude)) +
  geom_text_repel(data = zambia_metadata, aes(x = longitude, y = latitude, label = station))
rm(zambia_sf)

files <- list.files("E:/datasets/reanalysis/", pattern = "CFC", full.names = TRUE)

nc <- nc_open(files[1])
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

eumetsat_dfs <- list()
pb <- txtProgressBar(min = 0, max = length(files) * nrow(xy_extract), style = 3)
count <- 0
for(f in files) {
  for(i in seq_len(nrow(xy_extract))) {
    df <- tidync(f) %>%
      hyper_filter(lon = lon == xy_extract[i, 1], 
                   lat = lat == xy_extract[i, 2]) %>%
      hyper_tibble()
    df$station <- xy_extract$station[i]
    df$req_latitude <- xy_extract$req_latitude[i]
    df$req_longitude <- xy_extract$req_longitude[i]
    df$dist <- xy_extract$dist[i]
    eumetsat_dfs[[length(eumetsat_dfs) + 1]] <- df
    #print(eumetsat_dfs)
    count <- count + 1
    setTxtProgressBar(pb, count)
  }
}
#zambia_eumetsat <- bind_rows(eumetsat_dfs) %>%
  #mutate(date = as.Date(time, origin = as.Date("1970/01/01 0:0:0:0")))
zambia_eumetsat <- bind_rows(eumetsat_dfs) %>%
  mutate(date_time = as.POSIXct(time,
                              origin = as.POSIXct("1970/01/01 0:0:0"), tz = "GMT"),
       date = as.Date(date_time))

zambia_eumetsat1 <- bind_rows(eumetsat_dfs) %>%
  mutate(date_time = as.POSIXct(time,
                                origin = as.POSIXct("1970/01/01 0:0:0"), tz = "GMT"),
         date = as.Date(date_time), 
         cfc_lead_7 = dplyr::lead(CFC, 7)) %>%
  group_by(station, date) %>%
  summarise(cfc = max(cfc_lead_7))

#zambia_eumetsat <- zambia_eumetsat %>% select(station, date, arc2_rain = est_prcp)

saveRDS(zambia_eumetsat, here("data", "satellite", "zambia_eumetsat_raw.RDS"))
saveRDS(zambia_eumetsat1, here("data", "satellite", "zambia_eumetsat_cfc.RDS"))

