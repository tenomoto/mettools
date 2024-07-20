eps <- 0.622

#' vapour pressure to specific humidity
#'
#' Convert vapour pressure to specific humidity
#' @param e vapour pressure Pa| hPa
#' @param p air pressure Pa | hPa (must be consistent with e)
#' @return specific humidity kg/kg
#' @examples
#' q <- e2q(20, 1000)
#' q <- e2q(20e2, 1000e2)
#' @export
e2q <- function (e, p) {
  eps*e/(p-(1.0-eps)*e)
}

#' specific humidity to vapour pressure
#'
#' Convert specific humidity vapour pressure
#' @param q specific humidity kg/kg
#' @param p air pressure Pa | hPa
#' @return vapour pressure in the same unit as p
#' @examples
#' e.hPa <- q2e(0.01, 1000)
#' e.Pa <- q2e(0.01, 1000e2)
#' @export
q2e <- function(q, p) {
	p*q/(eps+(1.-eps)*q)
}

#' vapour pressure to mixing ratio
#'
#' Convert vapour pressure to mixing ratio
#' @param e vapour pressure Pa| hPa
#' @param p air pressure Pa | hPa (must be consistent with e)
#' @return mixing ratio kg/kg
#' @examples
#' q <- e2w(20, 1000)
#' q <- e2w(20e2, 1000e2)
#' @export
e2w <- function(e, p) {
	eps*e/(p-e)
}

#' saturation vapour pressure
#'
#' Calculate saturation vapour pressure
#'     based on the formula used by WMO and JMA
#' @param T temperature
#' @return saturation vapour pressure Pa
#' @examples
#' es <- calc.es(300)
#' @export
calc.es <- function(T) {
	exp(19.482-4303.4/(T-29.65))*100
}

#' dew point temperature
#'
#' Calculate dew point temperature
#'     Bolton (1980)
#' @param T temperature
#' @param e vapour pressure Pa
#' @return condensation temperature K
#' @examples
#' t.cond <- calc.condtemp(300, 2000)
#' @export
calc.condtemp <- function (T, e){
	2840.0/(3.5*log(T)-log(e*0.01)-4.805)+55.0
}

#' dew point depression to specific humidity
#'
#' Calculate specific humidity from dew point depression
#' @param ttd dew point depression (T - Td)
#' @param T temperature
#' @param p pressure Pa
#' @return specific humidity kg/kg
#' @examples
#' q <- ttd2q(12, 300, 1000e2)
#' @export
ttd2q <- function(ttd, T, p) {
	e2q(calc.es(T-ttd), p)
}

#' relative humidity to specific humidity
#'
#' Calculate specific humidity from relative humidity
#' @param rh relative humidity %
#' @param T temperature
#' @param p pressure Pa
#' @return specific humidity kg/kg
#' @examples
#' q <- rh2q(50, 300, 1000e2)
#' @export
rh2q <- function (rh, T, p) {
	e2q(calc.es(T)*0.01*rh, p)
}

#' specific humidity to dew point depression
#'
#' Calculate dew point depression from specific humidity
#' @param q specific humidity kg/kg
#' @param T temperature
#' @param p pressure Pa
#' @return dew point depression K
#' @examples
#' ttd <- q2ttd(0.01, 300, 1000e2)
#' @export
q2ttd <- function(q, T, p) {
# WMO, JMA
  T-29.65-4303.4/(19.482-log(q2e(q,p)*0.01))
}

#' potential temperature
#'
#' Calculate potential temperature
#'     Bolton (1980)
#' @param T temperature
#' @param w mixing ratio kg/kg
#' @param p pressure Pa
#' @return potential temperature K
#' @examples
#' theta <- calc.theta(300, 0.01, 850e2)
#' @export
calc.theta <- function (T, w, p) {

	T*(100000.0/p)^(0.2854*(1.0-0.28*w))
}

#' equivalent potential temperature
#'
#' Calculate equivalent potential temperature
#'     Bolton (1980)
#' @param T temperature K
#' @param e vapour pressure Pa
#' @param p pressure Pa
#' @return equivalent potential temperature K
#' @examples
#' thetae <- calc.thetae(300, 1000, 850e2)
#' @export
calc.thetae <- function (T, e, p) {
# Bolton (1980)
# T(K), e(Pa), p(Pa)
	w <- e2w(e,p)
	TL <- calc.condtemp(T, e)
	calc.theta(T, w, p) * exp((3376.0/TL-2.54)*w*(1.0+0.81*w))
}

#' Calculate saturated equivalent potential temperature
#'     Bolton (1980)
#' @param T temperature K
#' @param p pressure Pa
#' @return saturated equivalent potential temperature K
#' @examples
#' thetaes <- calc.thetaes(300, 1000e2)
#' @export
calc.thetaes <- function(T, p) {
# Bolton (1980)
# T(K), e=es(T)(Pa), p(Pa)
	es <- calc.es(T)
	w <- e2w(es,p)
	calc.theta(T, w, p) * exp((3376.0/T-2.54)*w*(1.0+0.81*w))
}
