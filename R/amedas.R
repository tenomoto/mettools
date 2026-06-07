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
  header <- utils::read.csv(fname, header=FALSE, skip=3, nrows=3, fileEncoding="cp932")
  qrow <- 2
  qcol <- header[qrow,] == amedas_dict$quality
  if (!any(qcol)) {
    qrow <- 3
    qcol <- header[qrow,] == amedas_dict$quality
  }
  skip <- qrow + 3
  data <- utils::read.csv(fname, header=FALSE, skip=skip, fileEncoding="cp932")
  colnames(data) <- header[1, ]
  ecol <- header[qrow,] == amedas_dict$no_phenom
  colnames(data)[ecol] = paste0(colnames(data)[ecol], header[qrow, ecol])
  filtered <- data[apply(data[, qcol] >= quality, 1, all), !qcol]
  if (drop.consistency) {
    filtered <- filtered[, !header[qrow, !qcol] == amedas_dict$homogeneity]
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
  names(dir) <- amedas_dict$wind_dir
  dir[x]
}

#' @noRd
deg2decimal <- function(d) {
  dmat <- matrix(d, ncol = 2)
  dmat[, 1] + dmat[, 2] / 60
}

#' @noRd
dec.lonlat <- function(lst) {
  lst$lon <- deg2decimal(lst$lon)
  lst$lat <- deg2decimal(lst$lat)
  lst
}

#' generate AMeDAS station list in a data.frame
#'
#' generate AMeDAS station list in a data.frame
#' @return df
#' @examples
#' df <- amedas.stations()
#' @export
amedas.stations <- function() {
  url <- "https://www.jma.go.jp/bosai/amedas/const/amedastable.json"
  cn <- c("type", "elems", "lat", "lon", "alt", "kjName", "knName", "enName")
  json <- jsonlite::fromJSON(url)
  station.id <- names(json)
  df <- data.frame(matrix(ncol = length(cn), nrow = 0))
  colnames(df) <- c(cn)
  for (id in station.id) {
    stn <- dec.lonlat(json[[id]])
    df <- rbind(df, stn[cn])
  }
  rownames(df) <- station.id
  df
}

