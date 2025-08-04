library(ggplot2)

court_points <- data.frame(
  x = c(-25, -25, 25, 25, -25),
  y = c(0, 47, 47, 0, 0),
  group = 1
)

plot_court <- function(court_theme = NULL) {
  ggplot() +
    geom_path(data = court_points, aes(x = x, y = y, group = group), color = "black") +
    coord_fixed() +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "#fdf6e3"),
      plot.background = element_rect(fill = "#fdf6e3")
    )
}
