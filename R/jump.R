#' Change Point Detection using jump algorithm
#' @param data Numeric vector containing the data to segment.
#' @return Un vecteur contenant les points de changement identifiés.
#' @export
#' @examples
#' data <- c(rnorm(40,0,2),rnorm(60,5,2),rnorm(60,1,2),rnorm(40,5,2))
#' jump(data)
jump <- function(data) {
  seg = jumpoints(data, output = "2")
  cpt_points <- seg$psi

  cpt <- c(1, cpt_points, length(data)+1)
  plot(data, type = "l",xlab = "Distance", ylab = "Valeur")
  lines(model_signal(data,cpt), col = "red",lwd  = 2)
  return(cpt_points)
}
