#' Change Point Detection using pelt algorithm
#' @param data Numeric vector containing the data to segment.
#' @param type_changement Type of change to detect. Maybe "mean","var" or "meanvar".
#' @param penalty Method of penalty. Maybe "Manual" or "BIC" for example.
#' @param pen.value if penalty is "Manual", this is the value of the penalty.
#' @return A numeric vector containing the segmentation points.
#' @export
#' @examples
#' data <- c(rnorm(40,0,2),rnorm(60,5,2),rnorm(60,1,2),rnorm(40,5,2))
#' type_changement = "mean"
#' penalty = "Manual"
#' pen.value = var(data) * log(length(data))
#' pelt(data, type_changement, penalty, pen.value )
pelt <- function(data,type_changement = "mean",penalty = "Manual",pen.value = var(data) * log(length(data))){
  if (type_changement == "mean") {
  seg <- cpt.mean(data, method = "PELT",penalty, pen.value,minseglen = 4)
  } else if (type_changement == "var") {
  seg <- cpt.var(data, method = "PELT",penalty , pen.value,minseglen = 4)
  } else if (type_changement == "meanvar") {
  seg <- cpt.meanvar(data, method = "PELT",penalty, pen.value,minseglen = 4)
  }
  cpt_points <- seg@cpts
  cpt_points <- cpt_points[-length(cpt_points)]

  cpt <- c(1, cpt_points, length(data)+1)
  plot(data, type = "l", xlab = "Distance", ylab = "Valeur")
  lines(model_signal(data,cpt), col = "red",lwd  = 2)
  return(cpt_points)
}
