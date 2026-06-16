amedas_dict <- list(
  quality     = "品質情報",
  no_phenom   = "現象なし情報",
  homogeneity = "均質番号",
  wind_dir    = c("北", "北北東", "北東", "東北東",
                  "東", "東南東", "南東", "南南東",
                  "南", "南南西", "南西", "西南西",
                  "西", "西北西", "北西", "北北西")
)

usethis::use_data(amedas_dict, internal = TRUE, overwrite = TRUE)
