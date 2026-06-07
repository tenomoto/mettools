#' Compute Hoskins Q-Vectors and Their Divergence
#'
#' This function computes the components of the Q-vector (\eqn{Q_x}, \eqn{Q_y}) 
#' and its spatial divergence (\eqn{\nabla \cdot \mathbf{Q}}) from 2D geopotential 
#' height and temperature fields on a standard longitude-latitude grid using 
#' second-order \eqn{O(h^2)} accurate finite differences.
#'
#' @param z A numeric matrix of geopotential height (\eqn{m}) where rows index 
#'   longitudes (\eqn{i}) and columns index latitudes (\eqn{j}).
#' @param T A numeric matrix of temperature (\eqn{K}) with the same dimensions 
#'   as \code{z}.
#' @param dx A numeric scalar representing the grid spacing along the X-axis 
#'   (longitudinal distance in meters).
#' @param dy A numeric scalar representing the grid spacing along the Y-axis 
#'   (latitudinal distance in meters). Defaults to \code{dx}.
#' @param lat A numeric vector containing the latitudes (\code{degrees_north}) 
#'   corresponding to the columns (\eqn{j}) of \code{z}.
#' @param plev A numeric scalar representing the pressure level operating 
#'   strictly in Pascals (\eqn{Pa}).
#'
#' @details 
#' The Q-vector components are calculated under the geostrophic approximation 
#' following Hoskins et al. (1978):
#' \deqn{Q_x = -\frac{R_d}{p} \left( \frac{\partial u_g}{\partial x}\frac{\partial T}{\partial x} + \frac{\partial v_g}{\partial x}\frac{\partial T}{\partial y} \right)}
#' \deqn{Q_y = -\frac{R_d}{p} \left( \frac{\partial u_g}{\partial y}\frac{\partial T}{\partial x} + \frac{\partial v_g}{\partial y}\frac{\partial T}{\partial y} \right)}
#' Where \eqn{u_g} and \eqn{v_g} are the geostrophic wind components computed 
#' internally using \code{grad()}, \eqn{R_d} is the dry gas constant, and \eqn{p} 
#' is the pressure level (\code{plev}). 
#' 
#' The latitude vector is automatically broadcasted as columns across the grid using 
#' \code{byrow = TRUE} to ensure precise mapping with the longitudinal row profiles.
#'
#' @return A named list containing three 2D matrices matching the dimensions of \code{z}:
#' \item{x}{Matrix of the \eqn{Q_x} component (\eqn{K m^{-1} s^{-1}}).}
#' \item{y}{Matrix of the \eqn{Q_y} component (\eqn{K m^{-1} s^{-1}}).}
#' \item{div}{Matrix of the divergence of the Q-vector (\eqn{K m^{-2} s^{-1}}).}
#'
#' @references 
#' Hoskins, B. J., I. Draghici, and H. C. Davies, 1978: A new look at the \eqn{\omega}-equation. 
#' \emph{Quart. J. Roy. Meteor. Soc.}, \strong{104}, 31-38.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming z and T are 100x72 matrices, lat is a vector of length 72
#' q_diag <- qvector(z = hgt_matrix, T = tmp_matrix, dx = 1e5, lat = lats, plev = 50000)
#' }
qvector <- function(z, T, dx, dy = dx, lat, plev) {
  z <- as.matrix(z)
  T <- as.matrix(T)
  nx <- nrow(z)
  ny <- ncol(z)
  if (length(lat) != nx) {
    stop("Length of lat must match the row dimension of z")
  }
  lat_mat <- matrix(lat, nx, ny, byrow = TRUE)
  gfr <- physcon$earth.grav / (2 * physcon$earth.omega * sin(lat_mat * pi / 180))
  dz <- grad(z, dx, dy)
  ug <- -dz$dy * gfr
  vg <-  dz$dx * gfr
  dug <- grad(ug, dx, dy)
  dvg <- grad(vg, dx, dy)
  dT <- grad(T, dx, dy)
  Qx <- -physcon$air.rd / plev * (dug$dx * dT$dx + dvg$dx * dT$dy)
  Qy <- -physcon$air.rd / plev * (dug$dy * dT$dx + dvg$dy * dT$dy)
  dQ <- div(Qx, Qy, dx, dy)
  list(x = Qx, y = Qy, div = dQ)
}
