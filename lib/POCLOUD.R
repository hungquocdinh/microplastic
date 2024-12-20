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
    date ,
    base.url = "https://cmr.earthdata.nasa.gov/virtual-directory/collections/C2893924134-POCLOUD/temporal",
    base.download.url = "https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/CYGNSS_L3_MICROPLASTIC_V3.2/",
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
  
  #check exist
  lf = list.files(destination, pattern = ".nc", full.names = T)
  pattern = sprintf("s%.4d%.2d%.2d", y, m, d)
  matching_elements <- lf[grepl(pattern, lf)]
  if (length(matching_elements)!=0) {
    return(matching_elements)
  }
  
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
  if (is.null(x[[1]][, 1])) {
    return(NA)
  }
  nc4.filename = paste0(x[[1]][, 1], ".nc")
  file.url = paste0(
    "https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/CYGNSS_L3_MICROPLASTIC_V3.2/",
    nc4.filename
  )

  #download
  in4= sprintf("downloading  %s to %s\n", nc4.filename, destination)
  print(in4)
  try(showNotification(in4, type = "message"), silent = T)
  req <- request(file.url) |>
    req_auth_bearer_token(Sys.getenv("EARTH_DATA_BEARER_TOKEN")) |>    # Place token into .Renviron file
    req_progress() |>
    req_perform(paste0(destination, nc4.filename))
  

  return(paste0(destination, nc4.filename))
}


