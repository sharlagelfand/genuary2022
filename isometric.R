library(ggplot2)
library(dplyr)
library(stringr)
library(prismatic)
library(purrr)

source(here::here("colours.R"))

# Start to build pieces! -----

# Plaza

plaza_height <- 0.15

plazas <- map2_dfr(
  c(0, -19, -19, 19, 19, 0),
  c(0, -11, 11, -11, 11, 22),
  ~ generate_building(.x, .y, 15, 15, 0.25, lightpink)
)

# Buildings

buildings <- pmap_dfr(
  list(
    bottom_center_x = c(-6, -2, -10),
    bottom_center_y = c(6, 3, 4),
    size_left = c(2, 3.5, 2),
    size_right = c(2, 3, 2),
    height = c(6, 3, 4)
  ),
  generate_building
)

ggplot() +
  geom_rect(aes(xmin = -20, xmax = 25, ymin = -5, ymax = 30), colour = red, fill = red) +
  geom_polygon(data = plazas, aes(x = x, y = y, group = id, fill = colour), color = "black", size = 0.1) +
  geom_polygon(data = buildings, aes(x = x, y = y, group = id, fill = colour), color = "black", size = 0.1) +
  coord_fixed(xlim = c(-15, 20), ylim = c(0, 25)) +
  scale_fill_identity() +
  theme_void()

ggsave("test.png", width = 6, height = 4, dpi = 300)
