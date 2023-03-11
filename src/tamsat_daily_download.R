#' Download Tamsat Daily Rainfall Estimate
#'
#' Automate downloading of daily RFE's from the TAMSAT server
#' Note that the unZip utilities within R are particually slow
#' 
#' 
#' @param years Years as interger(s) for which data will be downloaded
#' @param outlocation Folder (string) to save downloaded .zip files into
#' @param unZip Should the downloaded zips be unzipped
# 

tamsat_daily_download <- function(years, outlocation, unZip) {
  
  for (i in years) {
    filePath <- paste0("https://gws-access.jasmin.ac.uk/public/tamsat/rfe/data_zipped/v3.1/daily/TAMSATv3.1_rfe_daily_", i, ".zip")
    destPath <- paste0(outlocation, basename(filePath))
    print(paste0("downloading year ", i))
    curl::curl_download(url = filePath, destfile = destPath, mode = "wb")
  }
  
  if (unZip == TRUE) {
    for (i in list.files(pattern = ".zip", path = outlocation)) {
      
      unzip_folder <- paste0(outlocation, "/", file_path_sans_ext(i))
      print(paste0("Un-ziping to folder-", unzip_folder))
      
      unzip(zipfile = i, exdir = unzip_folder, overwrite = TRUE)
      file.remove(i)
    }
  }
}

tamsat_daily_download(c(1992, 1993), "data/test/", unZip=FALSE)

