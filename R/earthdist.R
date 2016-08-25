#' earth.dist
#' 
#' Function to calculate the metric distance between two coordinates (lat, lon)
#' @param long1 longitude of the first coordinate
#' @param lat1 latitude of the first coordinate
#' @param long2 longitude of the second coordinate
#' @param lat2 latitude of the second coordinate
#' @export
#' @examples
#' earth.dist(-70.34, 43.23, -71.29, 43.21)

earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}
