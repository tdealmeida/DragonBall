#' Change Point Detection using hubert algorithm
#' @param data Numeric vector containing the data to segment.
#' @param alpha Numeric value representing the alpha parameter for Hubert segmentation.
#' @return A numeric vector containing the segmentation points.
#' @export
#' @examples
#' data <- c(rnorm(40,0,2),rnorm(60,5,2),rnorm(60,1,2),rnorm(40,5,2))
#' alpha <- 0.05
#' hubert(data, alpha)
hubert <- function(data, alpha = 0.05) {
  seg <- hubr::Hubert_segmentation(data, alpha)
  cpt_points <- seg$locations
  cpt_points <- cpt_points[-length(cpt_points)]
  cpt_points <- cpt_points[-1]

  cpt <- c(1, cpt_points, length(data)+1)
  plot(data, type = "l", xlab = "Distance", ylab = "Valeur")
  lines(model_signal(data,cpt), col = "red",lwd  = 2)

  return(cpt_points)
}

