igra2_url <- "https://www.ncei.noaa.gov/data/integrated-global-radiosonde-archive/"

#' read IGRA2 stations
#'
#' read IGRA2 stations from NOAA NCEI
#' @return list
#' @examples
#' stations <- igra2_stations()
#' @export
igra2_stations <- function() {
  url <- paste(igra2_url, "doc/igra2-station-list.txt")
  station_colnames <- c("id", "latitude", "longitude", "elevation",
                        "state", "name", "fstyear", "lstyear", "nobs")
  read.fwf(url, c(11, 9, 10, 7, 3, 31, 5, 5, 7),
                           col.names = station_colnames, header = FALSE)
}

#' download IGRA2 station data
#'
#' download IGRA2 station data for the full period
#' @param id station id
#' @param method download method
#' @export
igra2_full <- function(id, method = "libcurl") {
  destfile <- paste0(id, "-data.txt.zip")
  url <- paste0(igra2_url, "access/data-por/", destfile)
  download.file(url, destfile, method)
}

#' download IGRA2 yearly station data
#'
#' download IGRA2 station data for the last year
#' @param id station id
#' @param year year
#' @param method download method
#' @export
igra2_y2d <- function(id, year = NULL, method = "libcurl") {
  if (is.null(year)) year <- format(Sys.Date(), "%Y")
  destfile <- paste0(id, "-data-beg", year, ".txt.zip")
  url <- paste0(igra2_url, "access/data-y2d/", destfile)
  download.file(url, destfile, method)
}

#' download IGRA2 derived station data
#'
#' download IGRA2 station derived data for the full period
#' @param id station id
#' @param method download method
#' @export
igra2_derived <- function(id, method = "libcurl") {
  destfile <- paste0(id, "-drvd.txt.zip")
  url <- paste0(igra2_url, "access/derived-por/", destfile)
  download.file(url, destfile, method)
}

#' download IGRA2 monthly mean
#'
#' download IGRA2 monthly mean for specified hour and variable
#' @param var one of "ghgt", "temp", "uwnd", "vapr", "vwnd"
#' @param hh 00 or 12
#' @param method download method
#' @export
igra2_monthly <- function(var, hh, method = "libcurl") {
  if (var %in% c("ghgt", "temp", "uwnd", "vapr", "vwnd")) {
    destfile <- paste0(var, "_", hh, "z-mly.txt.zip")
    url <- paste0(igra2_url, "access/monthly-por/", destfile)
    download.file(url, destfile, method)
  } else {
    stop("var not found")
  }
}

#' download IGRA2 monthly mean
#'
#' download IGRA2 monthly mean for specified hour and variable
#' @param var one of "ghgt", "temp", "uwnd", "vapr", "vwnd"
#' @param hh 00 or 12
#' @param yyyymm year and month
#' @param method download method
#' @export
igra2_monthly_upd <- function(var, hh, yyyymm = NULL, method = "libcurl") {
  if (var %in% c("ghgt", "temp", "uwnd", "vapr", "vwnd")) {
    if (is.null(yyyymm)) yyyymm <- format(Sys.Date() - 38, "%Y%m")
    destfile <- paste0(var, "_", hh, "z-mly-", yyyymm, ".txt.zip")
    url <- paste0(igra2_url, "access/monthly-upd/", destfile)
    download.file(url, destfile, method)
  } else {
    stop("var not found")
  }
}
