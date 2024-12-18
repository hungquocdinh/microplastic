# lib for download data from Pocloud
debug = T
if (debug){
base.url = "https://cmr.earthdata.nasa.gov/virtual-directory/collections/C2893924134-POCLOUD/temporal"
base.download.url = "https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/CYGNSS_L3_MICROPLASTIC_V3.2/"
date = "2022-01-21"
}


library(httr)
library(xml2)
library(rvest)
library(httr2)
library(XML)
library(lubridate)


pocloud.download <- function(
    base.url = "https://cmr.earthdata.nasa.gov/virtual-directory/collections/C2893924134-POCLOUD/temporal",
    base.download.url = "https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/CYGNSS_L3_MICROPLASTIC_V3.2/",
    date ,
    token = Sys.getenv("EARTH_DATA_BEARER_TOKEN"),
    destination = "./data/"
) {
  if (!dir.exists(destination)) {
    dir.create(destination)
  }
  stopifnot(inherits(date, "Date") |  is.character(date))
  if (is.character(date)) {
    date = as.Date(date)
    stopifnot(inherits(date, "Date"))
  }
  # extract time from date parameter
  d = day(date)
  m = month(date)
  y = year(date)
  
  # create url for checking file name
  url = sprintf("%s/%.4d/%.2d/%.2d/", base.url, y, m, d)
  response = GET(url)
  
  if (status_code(response) == 200) {
    catalog_xml <- content(response, as = "text")
  } else {
    stop("Failed to fetch catalog: ", status_code(response))
  }
  # get file name from html page of cmr pocloud
  x = readHTMLTable(catalog_xml)
  
  nc4.filename = paste0(x[[1]][, 1], ".nc")
  file.url = paste0(
    "https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/CYGNSS_L3_MICROPLASTIC_V3.2/",
    nc4.filename
  )
  if (file.exists(paste0(destination, nc4.filename))) {
    return(paste0(destination, nc4.filename))
  }
  #download
  req <- request(file.url) |>
    req_auth_bearer_token(Sys.getenv("EARTH_DATA_BEARER_TOKEN")) |>    # Place token into .Renviron file
    req_progress() |>
    req_perform(paste0(destination, nc4.filename))
  

  return(paste0(destination, nc4.filename))
}


