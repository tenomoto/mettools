read.amedas <- function(fname, quality=8, drop.consistency=TRUE) {
  header <- read.csv(fname, header=FALSE, fileEncoding="cp932",
                     skip=3, nrows=3)
  isblank <- apply(header == "", 1, all)
  header <- header[!isblank,]
  qcol <- header[2,] == "品質情報"
  skip <- 5 + sum(isblank)
  data <- read.csv(fname, header=FALSE, skip=skip, fileEncoding="cp932")
  colnames(data) <- header[1, ]
  ecol <- header[2,] == "現象なし情報"
  colnames(data)[ecol] = paste0(colnames(data)[ecol], header[2, ecol])
  filtered <- data[apply(data[, qcol] >= quality, 1, all), !qcol]
  if (drop.consistency) {
    filtered <- filtered[, !header[2, !qcol] == "均質番号"]
  }
  filtered
}
kyoto <- read.amedas("data.csv")
