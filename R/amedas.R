#' read AMeDAS csv
#'
#' read AMeDAS csv from JMA
#' @param fname file name
#' @param quality rows with all quality flags greater than equal to
#' this value will be retained.
#' @param drop.consistency drop consistency flag fields (TRUE)
#' @return data frame
#' @examples
#' fpath <- system.file("extdata", "kyoto.csv", package="mettools")
#' df <- read.amedas(fpath)
#' @export
read.amedas <- function(fname, quality=8, drop.consistency=TRUE) {
  header <- read.csv(fname, header=FALSE, skip=3, nrows=3, fileEncoding="cp932")
  qrow <- 2
  qcol <- header[qrow,] == "品質情報"
  if (!any(qcol)) {
    qrow <- 3
    qcol <- header[qrow,] == "品質情報"
  }
  skip <- qrow + 3
  data <- read.csv(fname, header=FALSE, skip=skip, fileEncoding="cp932")
  colnames(data) <- header[1, ]
  ecol <- header[qrow,] == "現象なし情報"
  colnames(data)[ecol] = paste0(colnames(data)[ecol], header[qrow, ecol])
  filtered <- data[apply(data[, qcol] >= quality, 1, all), !qcol]
  if (drop.consistency) {
    filtered <- filtered[, !header[qrow, !qcol] == "均質番号"]
  }
  filtered
}

#' convert wind directions to degrees
#'
#' convert wind directions to degrees
#' @param x wind directions
#' @return degreee
#' @examples
#' df <- dir2deg(c("北"))
#' @export
dir2deg <- function(x) {
  dir <- seq(0, 360, length.out=17)[1:16]
  names(dir) <- c("北", "北北東", "北東", "東北東",
                  "東", "東南東", "南東", "南南東",
                  "南", "南南西", "南西", "西南西",
                  "西", "西北西", "北西", "北北西")
  dir[x]
}

deg2decimal <- function(d) {
  dmat <- matrix(d, ncol = 2)
  dmat[, 1] + dmat[, 2] / 60
}

dec.lonlat <- function(lst) {
  lst$lon <- deg2decimal(lst$lon)
  lst$lat <- deg2decimal(lst$lat)
  lst
}

library(jsonlite)

#' generate AMeDAS station list in a data.frame
#'
#' generate AMeDAS station list in a data.frame
#' @return df
#' @examples
#' df <- amedas.stations()
#' @export
amedas.stations <- function() {
  url <- "https://www.jma.go.jp/bosai/amedas/const/amedastable.json"
  json <- fromJSON(url)
  station.id <- names(json)
  df <- data.frame(matrix(ncol = 6, nrow = 0))
  cn <- c("lat", "lon", "alt", "kjName", "knName", "enName")
  colnames(df) <- c(cn)
  for (id in station.id) {
    stn <- dec.lonlat(json[[id]])
    df <- rbind(df, stn[cn])
  }
  rownames(df) <- station.id
  df
}

