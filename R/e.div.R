#' Change Point Detection using the e.div Algorithm
#' @param data Numeric vector containing the data to segment.
#' @param sig.lvl The significance level for detecting change points.
#' @param R The maximum number of change points to detect.
#' @param k The number of clusters for the data. If not specified, it will be automatically determined.
#' @param min.size The minimum size of a segment to be considered as a change point.
#' @param alpha The significance level for the test of normality.
#' @return A numeric vector containing the detected change points.
#' @export
#' @examples
#' data <- c(rnorm(40,0,2),rnorm(60,5,2),rnorm(60,1,2),rnorm(40,5,2))
#' sig.lvl <- 0.05
#' R <- 199
#' K <- NULL
#' min.size <- 4
#' alpha <- 1
#' e.div(data, sig.lvl, R, K, min.size, alpha)
e.div <- function(data, sig.lvl = 0.05, R = 199, k = NULL, min.size = 4, alpha = 1) {
  seg <- e.divisive(matrix(data), sig.lvl, R, k, min.size, alpha)
  cpt_points <- seg$estimates
  cpt_points <- cpt_points[-length(cpt_points)]
  cpt_points <- cpt_points[-1]

  cpt <- c(1, cpt_points, length(data)+1)
  plot(data, type = "l", xlab = "Distance", ylab = "Valeur")
  lines(model_signal(data,cpt), col = "red",lwd  = 2)

  return(cpt_points)
}
