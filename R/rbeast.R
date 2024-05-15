#' Change Point Detection using rbeast algorithm
#' @param data Numeric vector containing the data to segment.
#' @return A numeric vector containing the segmentation points.
#' @export
#' @examples
#' data <- c(rnorm(40,0,2),rnorm(60,5,2),rnorm(60,1,2),rnorm(40,5,2))
#' rbeast(data)
rbeast <- function(data) {
  seg = beast(data,season = "none")
  ncp_mode <- seg$trend$ncp_mode
  cp <- seg$trend$cp
  cpt_points <- cp[1:ncp_mode]
  cpt_points <- sort(cpt_points)

  cpt <- c(1, cpt_points, length(data)+1)
  plot(data, type = "l", xlab = "Distance", ylab = "Valeur")
  lines(model_signal(data,cpt), col = "red",lwd  = 2)
  return(cpt_points)
}
