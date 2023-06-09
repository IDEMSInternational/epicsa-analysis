library(here)
library(ggplot2)
library(lubridate)
library(reshape2)
library(viridis)
library(RColorBrewer)
library(tidyr)
library(hydroGOF)
library(stringr)
library(ggrepel)
library(sp)
library(tibble)
library(purrr)
library(dplyr)
library(naflex)

source(here("src", "helper_funs.R"))

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

zm_long <- zm %>% 
  melt(id.vars = c("station", "date", "year", "syear", "month", "month_abb", "doy", "s_doy"),
       measure.vars = names(zm)[endsWith(names(zm), "rain")],
       variable.name = "product", value.name = "rain")

zm_long$product <- recode(zm_long$product, rain = "station")

stations <- c("Moorings", "Choma", "Kasama", "Livingstone", "Magoye", "Mansa", "Mpika", "Chipata", "Petauke")
products <- levels(zm_long$product)
products <- products[-1]
names(products) <- substr(products, 1, nchar(products) - 5)
zm_long$station <- factor(zm_long$station, levels = stations)


start_of_rains_dry <- zm_long %>% 
  group_by(station, product) %>%
  mutate(rain_day = rain >= 0.85,
         dry_spell = .spells(x = rain_day == 0),
         roll_max_dry_spell = dplyr::lead(x = RcppRoll::roll_maxl(x = dry_spell, n = 21, fill = NA)),
         roll_sum_rain = RcppRoll::roll_sumr(x = rain, n = 3, fill = NA, na.rm = FALSE)) %>%
  filter((rain >= 0.85 & roll_sum_rain > 20 & roll_max_dry_spell <= 9) | 
           is.na(rain) | is.na(roll_sum_rain) | is.na(roll_max_dry_spell), 
         .preserve = TRUE) %>%
  group_by(syear, .add = TRUE) %>%
  filter(s_doy >= 93 & s_doy <= 184, 
         .preserve = TRUE) %>%
  summarise(start_rain_dry = ifelse(test = is.na(dplyr::first(x=rain)) | 
                                      is.na(dplyr::first(x=roll_sum_rain)) | 
                                      is.na(dplyr::first(x=roll_max_dry_spell)), 
                                    yes = NA, 
                                    no = dplyr::first(x = s_doy, default = NA)))


saveRDS(start_of_rains_dry, here("results", "picsa", "start_of_rains_dry.RDS"))


start_of_rains_dry_with_mean_y <- start_of_rains_dry %>% 
  group_by(station, product) %>%
  dplyr::mutate(.mean_y = as.Date(round(mean(start_rain_dry, na.rm = TRUE)), origin = "2015-07-31"))


theme_annual <- theme(
  panel.grid.major = element_line(colour = "lightblue", linetype = "longdash", linewidth = 1),
  panel.grid.minor = element_line(colour = "lightblue", linetype = "longdash", linewidth = 1),
  panel.border = element_rect(colour = "black", fill = "NA", linewidth = 0),
  axis.text.x = element_text(angle = 90, size = 12, vjust = 0.4),
  axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14),
  title = element_text(size = 20),
  plot.subtitle = element_text(size = 15),
  plot.caption = element_text(size = 8),
  axis.text.y = element_text(size = 12),
  panel.background = element_rect(colour = "white", fill = "white", linewidth = 0.5, linetype = "solid")
)


for (s in unique(start_of_rains_dry_with_mean_y$station)) {
  g <- start_of_rains_dry_with_mean_y %>% filter(station == s & syear >= 1978 ) %>%
    ggplot(mapping = aes(x = syear, y = as.Date(start_rain_dry, origin = "2015-07-31"))) + 
    geom_line(colour = "blue", linewidth = 0.8) + 
    geom_point(size = 3, colour = "red") + 
    geom_hline(mapping = aes(yintercept=.mean_y), linewidth=1.5) +
    geom_label(mapping = aes(x = -Inf, y = .mean_y,
                             label = paste("Mean:", format(x = .mean_y, format = "%d %b"))), hjust = 0, vjust = -0.3) + 
    theme_annual + 
    xlab(label = "") + 
    ylab(label = "Start date") + 
    labs(caption = "First occurance from 1 Nov with more than 20mm in 3 days and no 10 day dry spell in the next 21 days.") +
    scale_y_date(date_labels = "%d %b", date_breaks ="1 months") +
    #scale_color_manual(values = c("black", "brown", "#8DA0CB", "red", "#A6D854", "#00A9FF")) 
    #scale_x_continuous(breaks = seq(1900, 2022, 4)) 
    facet_wrap(~product, ncol = 1)
  ggsave(here("results", "picsa", paste0("start_rains_", s, ".png")), 
        g, width = 12, height = 12)
 #print(g)
}


#end of rains
end_of_rains <- zm_long %>%
  group_by(station, product) %>%
  mutate(roll_sum_rain = RcppRoll::roll_sumr(x = rain, n = 1, fill = NA, na.rm = FALSE)) %>%
  filter((roll_sum_rain > 10) | is.na(roll_sum_rain)) %>% 
  group_by(syear, .add = TRUE) %>%
  filter(s_doy >= 185 & s_doy <= 289, .preserve = TRUE) %>% 
  summarise(end_rains = ifelse(test = is.na(dplyr::last(roll_sum_rain)), 
                               yes = NA, 
                               no = dplyr::last(s_doy)))

#end of season
end_of_season <- zm_long %>% 
  group_by(station, product) %>%
  mutate(rain_min = ifelse(test = is.na(rain), yes = 0, no = rain),
         wb_min = Reduce(f = function(x, y) pmin(pmax(x + y, 0), 120), x = tail(x = rain_min - 5, n = -1), 
                         init = 0, accumulate = TRUE),
         rain_max = ifelse(test = is.na(rain), yes = 120, no = rain),
         wb_max = Reduce(f = function(x, y) pmin(pmax(x + y, 0), 120), x = tail(x = rain_max - 5, n = -1), 
                         init = 0, accumulate = TRUE),
         wb = ifelse(test = (wb_min != wb_max) | is.na(rain), yes = NA, no = wb_min)) %>%
  filter((wb <= 0) | is.na(rain)) %>% 
  group_by(syear, .add = TRUE) %>%
  left_join(end_of_rains, by = c("station", "product", "syear"), suffix = c("", "")) %>% 
  filter(s_doy >= end_rains & s_doy <= 366, .preserve = TRUE) %>%
  summarise(end_season = ifelse(test = is.na(dplyr::first(x=wb)), yes = NA, no = dplyr::first(s_doy)),
            end_season_date = dplyr::if_else(condition = is.na(dplyr::first(wb)), 
                                             true = as.Date(NA), false = dplyr::first(date)))


saveRDS(end_of_season, here("results", "picsa", "end_of_season.RDS"))

end_season_with_mean_y <- end_of_season %>% 
  group_by(station, product) %>%
  dplyr::mutate(.mean_y = as.Date(round(mean(end_season, na.rm = TRUE)), origin = "2015-07-31"))

for (s in unique(end_season_with_mean_y$station)) {
  g <- end_season_with_mean_y %>% filter(station == s & syear >= 1978) %>%
    ggplot(mapping = aes(x = syear, y = as.Date(end_season, origin = "2015-07-31"))) + 
    geom_line(colour = "blue", linewidth = 0.8) + 
    geom_point(size = 3, colour = "red") + 
    geom_hline(mapping = aes(yintercept=.mean_y), linewidth=1.5) +  
    geom_label(mapping = aes(x = -Inf, y = .mean_y,
                             label = paste("Mean:", format(x = .mean_y, format = "%d %b"))),
               hjust = 0, vjust = -0.3) + 
    theme_annual + 
    xlab(label = "") + 
    ylab(label = "End date") + 
    facet_wrap(~product, ncol = 1) +
    labs(caption = "First occasion from the last rainfall of more than 10mm with empty water balance.\n 
         Capacity is 120mm and evaporation is taken as 5mm per day.") +
    scale_y_date(date_labels = "%d %b", date_breaks = "1 months") 
    #scale_x_continuous(breaks = seq(1950, 2022, 4))
  ggsave(here("results", "picsa", paste0("d.figure_4_end_season_", s, ".png")), 
         g, width = 12, height = 12)
}


# Figure 4: Length of season

combined_rain_summaries <- start_of_rains_dry %>% 
  left_join(end_of_rains, by = c("station", "product", "syear"), suffix = c("", "")) %>%
  left_join(end_of_season, by = c("station", "product", "syear"), suffix = c("", "")) %>% 
  mutate(length_season = end_season - start_rain_dry)

length_season_with_mean_y <- combined_rain_summaries %>% 
  group_by(station, product) %>%
  dplyr::mutate(.mean_y = mean(length_season, na.rm = TRUE))

saveRDS(combined_rain_summaries, here("results", "picsa", "length_of_season.RDS"))

for (s in unique(length_season_with_mean_y$station)) {
  g <- length_season_with_mean_y %>% filter(station == s & syear >= 1978) %>%
    ggplot(mapping=aes(x = syear, y = as.numeric(length_season))) + 
    geom_line(colour = "blue", linewidth = 0.8) + geom_point(size = 3, colour = "red") + 
    geom_hline(mapping = aes(yintercept = .mean_y), linewidth = 1.5) + 
    geom_label(mapping = aes(x = -Inf, y = .mean_y, label = paste("Mean:", round(.mean_y))), hjust = 0, vjust = -0.3) + 
    theme_annual + 
    facet_wrap(~product, ncol = 1) + 
    xlab(label = "") + 
    ylab(label = "Length of Season") + 
    labs(caption = "Season length: Number of days from start of rains date to end of season date.") +
    #scale_x_continuous(breaks = seq(1970, 2018, 4)) + 
    scale_y_continuous(limits = c(0, 300), expand = expansion(mult = c(0,0)), 
                       breaks = seq(0, 200, 50))  
  ggsave(here("results", "picsa", paste0("e.figure_4_length_season_", s, ".png")), 
         g, width = 12, height = 12)
}

# Figure 5: Total seasonal rainfall

combined_rain_summaries_2 <- combined_rain_summaries %>% 
  left_join((zm_long %>%
               left_join(combined_rain_summaries, by = c("station", "product", "syear"), suffix = c("", "")) %>% 
               group_by(station, product) %>%
               filter(s_doy >= start_rain_dry & s_doy <= end_season, .preserve = TRUE) %>%
               group_by(product, syear, .add = TRUE, .drop = FALSE) %>%
               summarise(total_rain = sum(rain, na.rm = FALSE))), by = c("station", "product", "syear"), suffix = c("", ""))


saveRDS(combined_rain_summaries_2, here("results", "picsa", "season_total_rainfall.RDS"))


total_rainfall_with_mean_y <- combined_rain_summaries_2 %>% 
  group_by(station, product) %>% 
  dplyr::mutate(.mean_y = mean(total_rain, na.rm = TRUE))

for (s in unique(total_rainfall_with_mean_y$station)) {
  g <- total_rainfall_with_mean_y %>% filter(station == s & syear >= 1978) %>%
    ggplot(mapping = aes(x = syear, y = as.numeric(total_rain))) + 
    geom_line(colour = "blue", linewidth = 0.8) + 
    geom_point(size = 3, colour = "red") + 
    geom_hline(mapping = aes(yintercept = .mean_y), linewidth = 1.5) + 
    geom_label(mapping = aes(x = -Inf, y = .mean_y, label = paste("Mean:", round(.mean_y))), hjust = 0, vjust = -0.3) + 
    theme_annual + 
    facet_wrap(~product, ncol = 1) +
    xlab(label = "") + 
    ylab(label = "Seasonal total rainfall (mm)") + 
    labs(caption = "Seasonal rainfall: Total rainfall between the start of rains and the end of the season.") 
    #scale_x_continuous(breaks = seq(1900, 2022, 4)) + 
    #scale_y_continuous(breaks = seq(0, 00, 100))
  ggsave(here("results", "picsa", paste0("f.figure_5_total_seasonal_rain_", s, ".png")),
         g, width = 12, height = 8)
}






 

