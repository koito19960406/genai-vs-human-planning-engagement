library(ggplot2)

theme_beautiful <- function() {
  theme_minimal(base_size = 16, base_family = "Helvetica") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),
    strip.text = element_text(size = 18, face = "bold"),
    plot.caption = element_text(size = 14, hjust = 1, face = "italic")
  )
} 