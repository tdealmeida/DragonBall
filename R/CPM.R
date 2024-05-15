#' Change Point Detection using cpm algorithm
#' @param data Numeric vector containing the data to segment.
#' @param cpmType Type of CPM algorithm to use.
#' @param ARL0 ARL0 parameter for the CPM algorithm.
#' @param startup Startup parameter for the CPM algorithm.
#' @return A numeric vector containing the segmentation points.
#' @export
#' @examples
#' data <- c(rnorm(40,0,2),rnorm(60,5,2),rnorm(60,1,2),rnorm(40,5,2))
#' cpmType = "Student"
#' ARL0 = ceiling(0.5 *length(data)/1000)*1000
#' startup = 20
#' cpm(data,cpmType,ARL0, startup )
cpm <- function(data,cpmType = "Student",ARL0 = ceiling(0.5 *length(data)/1000)*1000,startup = 20){
  seg = processStream(data,cpmType,ARL0, startup)
  cpt_points <- seg$changePoints

  cpt <- c(1, cpt_points, length(data)+1)
  plot(data, type = "l", xlab = "Distance", ylab = "Valeur")
  lines(model_signal(data,cpt), col = "red",lwd  = 2)
  return(cpt_points)
}
