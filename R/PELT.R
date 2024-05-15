#' Change Point Detection using pelt algorithm
#' @param data Numeric vector containing the data to segment.
#' @param type_changement Le type de changement à détecter. Peut être "mean" pour le changement de moyenne, "var" pour le changement de variance, ou "meanvar" pour les deux.
#' @param penalty Le paramètre de pénalité pour l'algorithme PELT.
#' @param pen.value La valeur de la pénalité.
#' @return Un vecteur contenant les points de changement identifiés.
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
