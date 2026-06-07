physcon <- list(
  air.rd = 287.0,
  air.cp = 1004,
  air.cv = 717.0,
  air.pr = 1000e2,
  water.t0 = 273.15,
  water.rv = 461.0,
  earth.grav = 9.81,
  earth.dayinsec = 86400
)
physcon$air.kappa <- physcon$air.rd / physcon$air.cp
physcon$air.gamma  <-  physcon$air.cp  / physcon$air.cv
physcon$water.eps <-  physcon$air.rd / physcon$water.rv
physcon$earth.omega <- 2 * pi / physcon$earth.dayinsec

usethis::use_data(physcon)
