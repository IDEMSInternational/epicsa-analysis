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

malawi_metadata <- read.csv(here("data", "station", "malawi_metadata.csv"))

malawi_metadata$YearOpened <- as.Date(malawi_metadata$YearOpened)
summary(malawi_metadata)

# station location check --------------------------------------------------
malawi_sf <- ne_countries(country = "Malawi", returnclass = "sf")
gg <- ggplot(malawi_sf) + 
  geom_sf() +
  geom_point(data = malawi_metadata, aes(x = longitude, y = latitude, col = District)) +
  geom_text_repel(data = malawi_metadata, aes(x = longitude, y = latitude, label = station))
ggsave(plot = gg, width = 12, height = 12, dpi = 600, filename = "malawi.jpeg")
rm(malawi_sf)

# CHIRPS Import -----------------------------------------------------------

files <- list.files("/media/johnbagiliko/TOSHIBA EXT/E-PICSA/data/satellite/CHIRPS/", pattern = "chirps-v2.0", full.names = TRUE)

nc <- nc_open(files[1])
stopifnot(min(nc$dim$longitude$vals) < min(malawi_metadata$longitude))
stopifnot(max(nc$dim$longitude$vals) > max(malawi_metadata$longitude))
stopifnot(min(nc$dim$latitude$vals) < min(malawi_metadata$latitude))
stopifnot(max(nc$dim$latitude$vals) > max(malawi_metadata$latitude))
xs <- nc$dim$longitude$vals
ys <- nc$dim$latitude$vals
resx <- xs[2] - xs[1]
resy <- ys[2] - ys[1]
max_dist <- sqrt((resx/2)^2 + (resy/2)^2)
xy_points <- expand.grid(xs, ys)
xy_extract <- closest_point(points = xy_points, 
                            target = malawi_metadata %>% select(longitude, latitude))
xy_extract$station <- malawi_metadata$station
xy_extract$req_longitude <- malawi_metadata$longitude
xy_extract$req_latitude <- malawi_metadata$latitude
xy_extract$dist <- apply(xy_extract, 1, function(r) {
  sp::spDistsN1(matrix(as.numeric(c(r[["Var1"]], r[["Var2"]])), ncol = 2),
                as.numeric(c(r[["req_longitude"]], r[["req_latitude"]])))
}
)
stopifnot(all(xy_extract$dist <= max_dist))
print(nc$dim$time$units)
nc_close(nc)

chirps_dfs <- list()
pb <- txtProgressBar(min = 0, max = length(files) * nrow(xy_extract), style = 3)
count <- 0
for(f in files) {
  for(i in seq_len(nrow(xy_extract))) {
    df <- tidync(f) %>%
      hyper_filter(longitude = longitude == xy_extract[i, 1], 
                   latitude = latitude == xy_extract[i, 2]) %>%
      hyper_tibble()
    df$station <- xy_extract$station[i]
    df$req_latitude <- xy_extract$req_latitude[i]
    df$req_longitude <- xy_extract$req_longitude[i]
    df$dist <- xy_extract$dist[i]
    chirps_dfs[[length(chirps_dfs) + 1]] <- df
    count <- count + 1
    setTxtProgressBar(pb, count)
  }
}
malawi_chirps <- bind_rows(chirps_dfs) %>%
  mutate(date = as.Date(time, origin = as.Date("1980/1/1")))

malawi_chirps <- malawi_chirps %>% select(station, date, chirps_rain = precip)

saveRDS(malawi_chirps, here("data", "satellite", "malawi", "malawi_chirps.RDS"))
