library(dplyr)
library(ggplot2)
library(purrr)
library(sf)
library(prismatic)
library(magick)

source(here::here("noise.R"))
source(here::here("29", "isometric_base_functions.R"))
source(here::here("29", "blocks.R"))
source(here::here("29", "sidewalks.R"))
source(here::here("29", "buildings.R"))
source(here::here("colours.R"))

set.seed(12345)

block_colour <- dark

blocks <- generate_blocks(colour = block_colour)

sidewalk_colour <- clr_lighten(block_colour, 0.1)

sidewalks <- blocks %>%
  generate_sidewalks()

sidewalk_transpose <- sidewalks %>%
  transpose() %>%
  map(bind_rows)

buildings <- sidewalks %>%
  generate_buildings(palette = colorRampPalette(c(red, darkpink, orange, yellow))(160))

plot_limits <- blocks %>%
  bind_rows() %>%
  summarise(across(c(x, y), list(min = min, max = max)))

plot_crop <- list(
  xlim = c(
    runif(1, plot_limits[["x_min"]] * 0.6, plot_limits[["x_min"]] * 0.4),
    runif(1, plot_limits[["x_max"]] * 0.4, plot_limits[["x_max"]] * 0.6)
  ),
  ylim = c(
    runif(1, plot_limits[["y_min"]] * 0.4, plot_limits[["y_min"]] * 0.2),
    runif(1, plot_limits[["y_max"]] * 0.4, plot_limits[["y_max"]] * 0.6)
  )
)

p <- ggplot() +
  geom_rect(aes(xmin = plot_limits[["x_min"]] * 1.1, xmax = plot_limits[["x_max"]] * 1.1, ymin = plot_limits[["y_min"]] * 1.1, ymax = plot_limits[["y_max"]] * 1.15), colour = clr_lighten(red, 0.8), fill = clr_lighten(red, 0.8)) +
  geom_polygon(
    data = bind_rows(blocks), aes(x = x, y = y, group = id, fill = colour),
    color = NA,
    size = 0.1
  ) +
  geom_path(
    data = sidewalk_transpose[["sidewalk_main"]], aes(x = x, y = y, group = id),
    color = sidewalk_colour,
    # alpha = 0.3,
    size = 0.4
  ) +
  geom_segment(data = sidewalk_transpose[["sidewalk_lines"]], aes(x = x, y = y, xend = xend, yend = yend), size = 0.4, colour = sidewalk_colour) +
  geom_polygon(
    data = buildings, aes(x = x, y = y, group = id, fill = colour),
    # color = "black",
    color = NA,
  ) +
  # coord_fixed(xlim = plot_crop[["xlim"]], ylim = plot_crop[["ylim"]]) +
  # coord_fixed(xlim = c(-35, 35), ylim = c(0, 45)) +
  # geom_point(data = buildings %>% filter(point == "bottom", section == "left_side", x > 15, y > 40), aes(x = x, y = y)) +
  coord_fixed(expand = FALSE) +
  scale_fill_identity() +
  theme_void()

p

ggsave(here::here("29", "day_29.png"), width = 17.3, height = 10.35, dpi = 300)

img <- image_read(here::here("29", "day_29.png"))

img %>%
  add_partial_noise(noisetype = "Laplacian") %>%
  image_write(here::here("29", "day_29.jpeg"))
