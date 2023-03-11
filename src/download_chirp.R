#' Download CHIRP Daily Rainfall Estimates
#'
#' 
#' @param years Years as interger(s) for which data will be downloaded
#' @param outlocation Folder (string) to save downloaded .zip files into
#' 
chirp_daily_download <- function(years, outlocation) {
  
  for (i in years) {
    filePath <- paste0("https://data.chc.ucsb.edu/products/CHIRP/daily/netcdf/chirp.", i, ".days_p05.nc")
    destPath <- paste0(outlocation, basename(filePath))
    print(paste0("downloading CHIRP data for year ", i))
    curl::curl_download(url = filePath, destfile = destPath, mode = "wb")
  }
}

chirp_daily_download(c(1992, 1993), "data/test2/")
