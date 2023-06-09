library(here)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)

source(here("src", "helper_funs.R"))
# Rainfall ----------------------------------------------------------------

zm <- readRDS(here("data", "station", "cleaned", "zambia_station_cleaned.RDS"))
if(anyDuplicated(zm %>% dplyr::select(station, date))) stop("Duplicates found!")
zm <- zm %>% 
  dplyr::select(station, date, rain, chirps_rain, chirp_rain, era5_rain, tamsat_rain, agera5_rain)
# 1 Aug = 214
s_doy_start <- 214
zm <- zm %>% mutate(doy = yday_366(date),
                    year = year(date),
                    month = month(date),
                    s_doy = (doy - s_doy_start + 1) %% 366,
                    s_doy = ifelse(s_doy == 0, 366, s_doy),
                    syear = year,
                    syear = ifelse(s_doy > (366 - s_doy_start + 1), syear - 1, syear),
                    month = factor(month, levels = c(8:12, 1:7)),
                    month_abb = factor(month, labels = month.abb[c(8:12, 1:7)]),
                    rain = ifelse(rain < 0, 0, rain),
                    era5_rain = ifelse(era5_rain < 0, 0, era5_rain),
                    chirps_rain = ifelse(chirps_rain < 0, 0, chirps_rain), 
                    chirp_rain = ifelse(chirp_rain < 0, 0, chirp_rain), 
                    tamsat_rain = ifelse(tamsat_rain < 0, 0, tamsat_rain),
                    agera5_rain = ifelse(agera5_rain <0, 0, agera5_rain)
)
if(anyDuplicated(zm %>% dplyr::select(station, date))) stop("Duplicates found!")

neg_rain <- zm %>% filter(rain < 0)
if(nrow(neg_rain) > 0) View(neg_rain)

# Replace negative with NA
if(nrow(neg_rain) > 0) {
  zm <- zm %>% 
    mutate(rain = ifelse(rain < 0, NA, rain))
}

by_month <- zm %>%
  group_by(station, syear, month_abb) %>%
  summarise(n_rain = sum(rain > 0.85),
            t_rain = sum(rain))

# Rain amounts visual check
ggplot(zm %>% filter(rain > 0.85), aes(x = month_abb, y = rain)) +
  geom_boxplot(varwidth = TRUE) +
  facet_wrap(~station)

# Number rain days visual check
ggplot(by_month, aes(x = month_abb, y = n_rain)) +
  geom_boxplot() +
  facet_wrap(~station)

# Inventory plot
ggplot(zm, aes(x = date, y = station, fill = !is.na(rain))) +
  geom_tile() +
  geom_hline(yintercept = seq(0.5, by = 1, length.out = length(unique(zm$station)) + 1)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

ggplot(zm, aes(x = date, y = station, fill = !is.na(tamsat_rain))) +
  geom_tile() +
  geom_hline(yintercept = seq(0.5, by = 1, length.out = length(unique(zm$station)) + 1)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y")

# Not needed as done in R-Instat
# # Fill Date gaps
# dates_list <- list()
# for(s in unique(zm$station)) {
#   
#   dates <- seq(min((zm %>% filter(station == s))$date), 
#                max((zm %>% filter(station == s))$date),
#                by = 1)
#   dd <- data.frame(station = s, date = dates)
#   dates_list[[length(dates_list) + 1]] <- dd
# }
# date_df <- bind_rows(dates_list)
# 
# nr <- nrow(date_df)
# if(nrow(zm) < nr) {
#   zm <- full_join(date_df, zm, by = c("station", "date"))
#   print(paste("Filled", nrow(zm) - nr, "rows"))
#   zm <- zm %>%
#     mutate(year = year(date), month = factor(month(date)))
#   # Inventory plot again
#   ggplot(zm, aes(x = date, y = station, fill = !is.na(rain))) +
#     geom_tile() +
#     geom_hline(yintercept = seq(0.5, by = 1, length.out = length(unique(zm$station)) + 1))
# }

# Large or negative values check
large_check <- zm %>% 
  filter(rain > 150)
if(nrow(large_check) > 0) View(large_check)
# Some large values but no rainfall over 300mm.
# Do not remove any values.

# Consecutive non-zero values check
consec_check <- zm %>% 
  group_by(station) %>%
  mutate(same = rep(rle(as.numeric(rain))$lengths, rle(as.numeric(rain))$lengths)) %>%
  filter(rain > 1.5 & same >= 2)
if(nrow(consec_check) > 0) View(consec_check)
# Some repeated values but no more than 2 consecutive and generally small values
# Do not remove any values.

# Consecutive rain days check
raindays_check <- zm %>%
  group_by(station) %>%
  mutate(raindays = cumsum(rain > 0) - cummax(((rain > 0) == 0) * cumsum(rain > 0))) %>%
  filter(raindays > 10)
if(nrow(raindays_check) > 0) View(raindays_check)
# Only oddity is August 1978 - Beira - 11 consecutive wet days but all small values < 1mm
# Do not remove any values.

# Dry months check - strict
drymonths_check <- zm %>%
  filter(month_abb %in% c("May", "Jun", "Jul", "Aug", "Sep")) %>%
  group_by(station, syear, month_abb) %>%
  summarise(t_rain = sum(rain)) %>%
  filter(t_rain == 0)
if(nrow(drymonths_check) > 0) View(drymonths_check) 

# Other periods of dry November months do not seem impossible. Do not remove values.