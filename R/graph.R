graph <- function(data, cpt_points) {
  cpt <- c(1, cpt_points, length(data)+1)
  p <- ggplot(data = data.frame(x = 1:length(data), y = data), aes(x = x, y = y)) +
    geom_line() +
    geom_line(aes(y = model_signal(data, cpt)), color = "red", size = 1) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(size = 0.5, color = "black")
    ) +
    xlab("Distance") +
    ylab("valeur")
  return(p)
}
