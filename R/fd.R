#' @title Cyclic Index Shifter
#' 
#' @description A utility function to compute shifted matrix indices for periodic/cyclic 
#' boundary conditions.
#'
#' @param n An integer scalar representing the maximum size of the grid dimension.
#' @param k An integer scalar representing the size and direction of the index shift.
#'
#' @return A vector of length \code{n} containing the shifted indices wrapped within range \code{1:n}.
#' @keywords internal
cshift <- function(n, k) {
  ((1:n + k - 1) %% n + 1)
}

#' @title Second-Order Accurate Partial Derivative
#'
#' @description Computes the first derivative of a 2D matrix field along a chosen axis 
#' using centered \eqn{O(h^2)} differences in the interior and one-sided 3-point \eqn{O(h^2)} 
#' schemes at the non-cyclic boundaries.
#'
#' @param f A numeric matrix to differentiate.
#' @param h A numeric scalar representing the grid spacing along the chosen axis.
#' @param axis A character string specifying the dimension to differentiate. 
#'   Must be either \code{"x"} (rows) or \code{"y"} (columns). Defaults to \code{"x"}.
#' @param cyclic A logical scalar. If \code{TRUE}, periodic boundary conditions are applied via 
#'   \code{\link{cshift}}. If \code{FALSE}, asymmetric one-sided boundary stencils are applied.
#'
#' @return A numeric matrix of the same dimensions as \code{f} containing the computed partial derivative.
#' @export
deriv_O2 <- function(f, h, axis = "x", cyclic = FALSE) {
  f <- as.matrix(f)
  nx <- nrow(f)
  ny <- ncol(f)
  df <- matrix(NA, nx, ny)
  
  if (axis == "x") {
    if (cyclic) {
      df <- f[cshift(nx, 1), , drop = FALSE] - f[cshift(nx, -1), , drop = FALSE]
    } else {
      df[2:(nx - 1), ] <- f[3:nx, ] - f[1:(nx - 2), ]
      df[1, ]          <- -3 * f[1, ] + 4 * f[2, ] - f[3, ]
      df[nx, ]         <-  3 * f[nx, ] - 4 * f[nx - 1, ] + f[nx - 2, ]
    }
  } else if (axis == "y") {
    if (cyclic) {
      df <- f[, cshift(ny, 1), drop = FALSE] - f[, cshift(ny, -1), drop = FALSE]
    } else {
      df[, 2:(ny - 1)] <- f[, 3:ny] - f[, 1:(ny - 2)]
      df[, 1]          <- -3 * f[, 1] + 4 * f[, 2] - f[, 3]
      df[, ny]         <-  3 * f[, ny] - 4 * f[, ny - 1] + f[, ny - 2]
    }
  }
  df / (2 * h)
}

#' @title Spatial Gradient of a 2D Field
#'
#' @description Evaluates both components of a 2D spatial gradient vector 
#' \eqn{\nabla f = \left(\frac{\partial f}{\partial x}, \frac{\partial f}{\partial y}\right)} 
#' using uniform second-order accurate schemes.
#'
#' @param f A numeric matrix representing the scalar field.
#' @param hx A numeric scalar representing the grid spacing in the x-direction.
#' @param hy A numeric scalar representing the grid spacing in the y-direction. 
#'   Defaults to \code{hx}.
#' @param cyclic A logical scalar. Set to \code{TRUE} if the field domain wraps periodically. 
#'   Defaults to \code{FALSE}.
#'
#' @return A named list containing two matrices of the same size as \code{f}:
#' \item{dx}{The partial derivative with respect to x (\code{df/dx}).}
#' \item{dy}{The partial derivative with respect to y (\code{df/dy}).}
#' @export
grad <- function(f, hx, hy = hx, cyclic = FALSE) {
  u <- as.matrix(f)
  if (!cyclic && (nrow(u) < 3 || ncol(u) < 3)) {
    stop("Grid dimensions must be at least 3x3 for non-cyclic O(h^2) gradients")
  }
  fx <- deriv_O2(u, hx, axis = "x", cyclic = cyclic)
  fy <- deriv_O2(u, hy, axis = "y", cyclic = cyclic)
  list(dx = fx, dy = fy)
}

#' @title 2D Spatial Laplacian Operator
#'
#' @description Computes the 2D Laplacian field \eqn{\nabla^2 f = \frac{\partial^2 f}{\partial x^2} + \frac{\partial^2 f}{\partial y^2}} 
#' using second-order accurate \eqn{O(h^2)} central finite differences for the interior points. 
#' For non-cyclic configurations, edge boundaries and corners are closed using custom 
#' 4-point one-sided second derivative approximations.
#'
#' @param f A numeric matrix representing the scalar field.
#' @param hx A numeric scalar representing the spatial grid resolution along rows.
#' @param hy A numeric scalar representing the spatial grid resolution along columns. 
#'   Defaults to \code{NULL}, which mirrors the resolution of \code{hx}.
#'
#' @inheritParams grad
#'
#' @return A numeric matrix of the same dimensions as \code{f} containing the computed 
#'   Laplacian field.
#' @export
lap <- function(f, hx, hy = NULL, cyclic = FALSE) {
  if (is.null(hy)) hy <- hx
  nx <- nrow(f)
  ny <- ncol(f)

  if (!cyclic && (nrow(f) < 4 || ncol(f) <4)) {
    stop("Grid dimensions must be at least 4x4 for non-cyclic O(h^2) boundaries")
  }
  lf <- matrix(NA, nx, ny)
  if (cyclic) {
    ir <- cshift(nx, 1)
    il <- cshift(nx, -1)
    jt <- cshift(ny, 1)
    jb <- cshift(ny, -1)
    lf <- (f[ir, ] - 2 * f + f[il, ]) / hx^2 +
                  (f[, jt] - 2 * f + f[, jb]) / hy^2
  } else {
    ii <- 2:(nx - 1)
    ji <- 2:(ny - 1)
    ir <- ii + 1
    il <- ii - 1
    jt <- ji + 1
    jb <- ji - 1
    lf[ii, ji] <- (f[ir, ji] - 2 * f[ii, ji] + f[il, ji]) / hx^2 +
                  (f[ii, jt] - 2 * f[ii, ji] + f[ii, jb]) / hy^2
    lf[1, ji] <- (2*f[1, ji] - 5*f[2, ji] + 4*f[3, ji] - f[4, ji]) / hx^2 + 
                 (f[1, ji+1] - 2*f[1, ji] + f[1, ji-1]) / hy^2
    lf[nx, ji] <- (2*f[nx, ji] - 5*f[nx-1, ji] + 4*f[nx-2, ji] - f[nx-3, ji]) / hx^2 + 
                  (f[nx, ji+1] - 2*f[nx, ji] + f[nx, ji-1]) / hy^2
    lf[ii, 1] <- (f[ii+1, 1] - 2*f[ii, 1] + f[ii-1, 1]) / hx^2 + 
                 (2*f[ii, 1] - 5*f[ii, 2] + 4*f[ii, 3] - f[ii, 4]) / hy^2
    lf[ii, ny] <- (f[ii+1, ny] - 2*f[ii, ny] + f[ii-1, ny]) / hx^2 + 
                  (2*f[ii, ny] - 5*f[ii, ny-1] + 4*f[ii, ny-2] - f[ii, ny-3]) / hy^2
    lf[1, 1] <- (2*f[1,1] - 5*f[2,1] + 4*f[3,1] - f[4,1]) / hx^2 + 
                (2*f[1,1] - 5*f[1,2] + 4*f[1,3] - f[1,4]) / hy^2
    lf[1, ny] <- (2*f[1,ny] - 5*f[2,ny] + 4*f[3,ny] - f[4,ny]) / hx^2 + 
                 (2*f[1,ny] - 5*f[1,ny-1] + 4*f[1,ny-2] - f[1,ny-3]) / hy^2
    lf[nx, 1] <- (2*f[nx,1] - 5*f[nx-1,1] + 4*f[nx-2,1] - f[nx-3,1]) / hx^2 + 
                 (2*f[nx,1] - 5*f[nx,2] + 4*f[nx,3] - f[nx,4]) / hy^2
    lf[nx, ny] <- (2*f[nx,ny] - 5*f[nx-1,ny] + 4*f[nx-2,ny] - f[nx-3,ny]) / hx^2 + 
                  (2*f[nx,ny] - 5*f[nx,ny-1] + 4*f[nx,ny-2] - f[nx,ny-3]) / hy^2
  }
  lf
}

#' @title Spatial Divergence of a 2D Vector Field
#'
#' @description Calculates the scalar divergence field \code{div(u,v)} 
#' \eqn{\nabla \cdot \mathbf{v} = \frac{\partial u}{\partial x} + \frac{\partial v}{\partial y}} 
#' from its coordinate vector components.
#'
#' @param u A numeric matrix representing the horizontal/longitudinal components of the vector field.
#' @param v A numeric matrix representing the vertical/latitudinal components of the vector field.
#'
#' @inheritParams grad
#'
#' @return A numeric matrix matching the input vector field grid dimensions containing 
#'   the computed spatial divergence.
#' @export
div <- function(u, v, hx, hy = hx, cyclic = FALSE) {
  u <- as.matrix(u)
  v <- as.matrix(v)
  if (!cyclic && (nrow(u) < 3 || ncol(u) < 3)) {
    stop("Grid dimensions must be at least 3x3 for non-cyclic O(h^2) gradients")
  }
  dudx <- deriv_O2(u, hx, axis = "x", cyclic = cyclic)
  dvdy <- deriv_O2(v, hy, axis = "y", cyclic = cyclic)
  dudx + dvdy
}

#' @title Spatial Rotation (Scalar Curl) of a 2D Vector Field
#'
#' @description Evaluates the scalar 2D curl / vertical vorticity field 
#' \eqn{\zeta = \frac{\partial v}{\partial x} - \frac{\partial u}{\partial y}} 
#' from its coordinate components.
#'
#' @inheritParams div
#'
#' @return A numeric matrix matching the input vector grid dimensions containing the 
#'   computed scalar rotation.
#' @export
rot <- function(u, v, hx, hy = hx, cyclic = FALSE) {
  u <- as.matrix(u)
  v <- as.matrix(v)
  if (!cyclic && (nrow(u) < 3 || ncol(u) < 3)) {
    stop("Grid dimensions must be at least 3x3 for non-cyclic O(h^2) gradients")
  }
  dvdx <- deriv_O2(v, hx, axis = "x", cyclic = cyclic)
  dudy <- deriv_O2(u, hy, axis = "y", cyclic = cyclic)
  dvdx - dudy
}
