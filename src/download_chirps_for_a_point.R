#install.packages("remotes") #uncomment to install
#remotes::install_github("ropensci/chirps") #uncomment to install
library(chirps)

#This is for half the month of December 2017. Modify the dates as desired
dates <- c("2017-12-15","2017-12-31")
#we are specifying two points in Malawi
lonlat <- data.frame(lon = c(33.48333,33.5), lat = c(-13.06666, -13.28333))

#downloads that data as a dataframe
df <- get_chirps(lonlat, dates, server = "CHC")

