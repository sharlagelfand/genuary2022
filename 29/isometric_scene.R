library(dplyr)
library(ggplot2)
library(purrr)

source(here::here("29", "isometric_base_functions.R"))
source(here::here("29", "blocks.R"))
source(here::here("colours.R"))

set.seed(1234)

blocks <- generate_blocks()

plot_limits <- blocks %>%
  summarise(across(c(x, y), list(min = min, max = max)))

plot_crop <- list(
  xlim = c(
    runif(1, plot_limits[["x_min"]] * 0.6, plot_limits[["x_min"]] * 0.2),
    runif(1, plot_limits[["x_max"]] * 0.2, plot_limits[["x_max"]] * 0.6)
  ),
  ylim = c(
    runif(1, plot_limits[["y_min"]] * 0.6, plot_limits[["y_min"]] * 0.2),
    runif(1, plot_limits[["y_max"]] * 0.6, plot_limits[["y_max"]] * 0.8)
  )
)

ggplot() +
  geom_rect(aes(xmin = plot_limits[["x_min"]], xmax = plot_limits[["x_max"]], ymin = plot_limits[["y_min"]], ymax = plot_limits[["y_max"]]), colour = dark, fill = dark) +
  geom_polygon(data = blocks, aes(x = x, y = y, group = id, fill = colour), color = "black", size = 0.1) +
  # geom_polygon(data = buildings, aes(x = x, y = y, group = id, fill = colour), color = "black", size = 0.1) +
  coord_fixed(xlim = plot_crop[["xlim"]], ylim = plot_crop[["ylim"]]) +
  scale_fill_identity() +
  theme_void()
