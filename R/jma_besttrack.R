#' Decode RSMC Tokyo Best Track Header Line
#'
#' Parses a single header line from the JMA Best Track data into a structured list of metadata.
#'
#' @param header A character string containing a header line (starting with "66666").
#' @return A list containing the international ID, number of data lines, tropical cyclone ID, flags, name, and date.
#' @keywords internal
decode_header <- function(header) {
  bbbb <- substr(header, 7, 10)
  ccc <- substr(header, 13, 15)
  dddd <- substr(header, 17, 20)
  f <- substr(header, 27, 27)
  g <- substr(header, 29, 29)
  h_h <- substr(header, 31, 50)
  i_i <- substr(header, 65, 72)
  list(
    intl_id = bbbb,
    n_data_lines = as.integer(ccc),
    tc_id= as.integer(dddd),
    flag = as.integer(f),
    d_tlast_tfinal = as.integer(g),
    name = trimws(h_h),
    i_i = as.Date(i_i, format = "%y%m%d"))
}

#' Decode RSMC Tokyo Best Track Data Lines
#'
#' Parses a character vector of best track data lines into a data frame containing storm track details.
#'
#' @param data A character vector where each element is a data line for a specific tropical cyclone.
#' @return A data frame containing parsed track data, including time, location, intensity, and wind radii.
#' @keywords internal
decode_data <- function(data) {
  cat2str <- c(NA, "TD", "TS", "STS", "TY", "L", "R", "TTC", NA, ">TS")
  int2dir <- c("NO", "NE", "E", "SE", "S", "SW", "W", "NW", "N", "SYM")
  dir2deg8 <- c(NA, seq(45, 315, length.out=7), 0, NA)
  nm2km <- 1.852

  yy <- as.integer(substr(data, 1, 2))
  yyyy <- ifelse (yy >= 51, 1900 + yy, 2000 + yy)
  a_a <- paste0(yyyy, substr(data, 3, 8))
  c1 <- substr(data, 14, 14)
  ddd <- substr(data, 16, 18)
  eeee <- substr(data, 20, 23)
  ffff <- substr(data, 25, 28)
  ggg <- substr(data, 34, 36)
  h <- substr(data, 42, 42)
  iiii <- substr(data, 43, 46)
  jjjj <- substr(data, 48, 51)
  k <- substr(data, 53, 53)
  llll <- substr(data, 54, 57)
  mmmm <- substr(data, 59, 62)
  p <- substr(data, 72, 72)
  df <- data.frame(
    time = as.POSIXct(a_a, format = "%Y%m%d%H", tz = "UTC"),
    grade = as.integer(c1),
    grade_str = cat2str[c1],
    lat = as.integer(ddd) * 0.1,
    lon = as.integer(eeee) * 0.1,
    pc = as.numeric(ffff),
    us_knot = as.numeric(ggg),
    us_ms = as.numeric(ggg) * 0.514444,
    dir50 = int2dir[as.integer(h) + 1],
    dir50_deg = dir2deg8[as.integer(h) + 1],
    r50l = as.integer(iiii),
    r50l_km = as.integer(iiii) * nm2km,
    r50s = as.integer(jjjj),
    r50s_km = as.integer(jjjj) * nm2km,
    dir30 = int2dir[as.integer(k) + 1],
    dir30_deg = dir2deg8[as.integer(k) + 1],
    r30l = as.integer(llll),
    r30l_km = as.integer(llll) * nm2km,
    r30s = as.integer(mmmm),
    r30s_km = as.integer(mmmm) * nm2km,
    landfall = ifelse(p == "#", TRUE, FALSE))
}

#' Decode JMA Best Track Connection
#'
#' Reads and parses the text connection extracted from the Best Track zip file.
#'
#' @param con A connection to the extracted text file.
#' @return A named list of data frames, where each element contains track data for a tropical cyclone, indexed by its international ID.
#' @keywords internal
decode_bst <- function(con) {
  lines <- readLines(con)
  header_lineno <- which(substr(lines, 1, 5) == "66666")
  headers <- lines[header_lineno]
  tc_list <- list()
  for (i in 1:length(header_lineno)) {
    header <- decode_header(headers[i])
    intl_id <- as.character(header$intl_id)
    ls <- header_lineno[i] + 1
    le <- header_lineno[i] + header$n_data_lines
    if (le > ls) {
      data <- lines[ls:le]
      df <- decode_data(data)
      df$intl_id <- intl_id
    }
    tc_list[[intl_id]] <- df
  }
  tc_list
}

#' Fetch and Parse RSMC Tokyo Best Track Data
#'
#' Downloads the complete Best Track dataset (`bst_all.zip`) from the Japan Meteorological Agency (JMA),
#' extracts the text file, and parses it into a structured list of data frames for all available storms.
#'
#' @param destfile An optional character string specifying where to save the downloaded zip file. If `NA` (default), the file is not saved permanently.
#' @return A named list of data frames containing track data for all available tropical cyclones.
#' @export
#' @examples
#' \dontrun{
#'   # Download and parse all JMA best track data
#'   tc_data <- rsmc_tokyo_bst()
#'   
#'   # Download, parse, and save a copy of the zip file locally
#'   tc_data <- rsmc_tokyo_bst(destfile = "data/jma_bst.zip")
#' }
rsmc_tokyo_bst <- function(destfile = NA) {
  txtfile <- "bst_all.txt"
  url <- "https://www.jma.go.jp/jma/jma-eng/jma-center/rsmc-hp-pub-eg/Besttracks/bst_all.zip"
  zipfile <- tempfile()
  utils::download.file(url, zipfile, mode = "wb")
  if (! is.na(destfile)) file.copy(zipfile, destfile)
  con <- unz(zipfile, txtfile)
  on.exit(close(con))
  decode_bst(con)
}

