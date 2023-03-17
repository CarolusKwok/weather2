#' ggplot2: Radar Coordiante System
#'
#' Adds a radar coordinate system for radar charts. Inspired by see
#'
#' @param theta Variable to map angle to x or y
#' @param start Offset of starting point from 12 o'clock in radians. Offset is applied clockwise or anticlockwise depending on value of direction.
#' @param direction 1, clockwise; -1, anticlockwise
#' @param ... Other arguments to be passed to ggproto
#'
#' @return
#' @export
#'
#' @examples
coord_radar <- function(theta = "x", start = 0, direction = 1, ...) {
  theta <- match.arg(theta, c("x", "y"))
  r <- ifelse(theta == "x", "y", "x")
  ggplot2::ggproto(
    "CoordRadar",
    ggplot2::CoordPolar,
    theta = theta,
    r = r,
    start = start,
    direction = sign(direction),
    is_linear = function(coord){TRUE},
    ...
  )
}
