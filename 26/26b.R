library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(ggforce)

source(here::here("colours.R"))

backgrounds <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~color,
  0, 2, 0, 2, darkpink,
  0, 2, 2, 4, lightpink,
  2, 4, 0, 2, lightpink,
  2, 4, 2, 4, darkpink
)

squares <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~color,
  0, 1, 3, 4, darkpink,
  3, 4, 3, 4, lightpink,
  0, 1, 0, 1, lightpink,
  3, 4, 0, 1, darkpink
)

circles <- tribble(
  ~x0, ~y0, ~r, ~color,
  2, 2, 1, red
)

triangles <- tribble(
  ~x, ~y, ~group,
  0, 1, 1,
  0, 2, 1,
  0.5, 2, 1,
  0, 2, 2,
  0, 3, 2,
  0.5, 2, 2,
  1, 0, 3,
  1, 1, 3,
  2, 0, 3,
  2, 0, 4,
  3, 0, 4,
  3, 1, 4,
  1, 3, 5,
  1, 4, 5,
  2, 4, 5,
  2, 4, 6,
  3, 4, 6,
  3, 3, 6,
  3.5, 2, 7,
  4, 2, 7,
  4, 3, 7,
  3.5, 2, 8,
  4, 2, 8,
  4, 1, 8
)

triangles_color <- tribble(
  ~group, ~color,
  1, orange,
  2, yellow,
  3, orange,
  4, yellow,
  5, yellow,
  6, orange,
  7, orange,
  8, yellow
)

triangles <- triangles %>%
  left_join(triangles_color, by = "group")

grid <- crossing(x = 1:5, y = 1:4)

full_pattern <- map2(
  grid[["x"]], grid[["y"]],
  function(grid_x, grid_y) {
    list(
      backgrounds = backgrounds %>%
        mutate(
          across(c(xmin, xmax), ~ .x + 4 * grid_x),
          across(c(ymin, ymax), ~ .x + 4 * grid_y)
        ),
      squares = squares %>%
        mutate(
          across(c(xmin, xmax), ~ .x + 4 * grid_x),
          across(c(ymin, ymax), ~ .x + 4 * grid_y)
        ),
      circles = circles %>%
        mutate(
          x0 = x0 + 4 * grid_x,
          y0 = y0 + 4 * grid_y
        ),
      triangles = triangles %>%
        mutate(
          x = x + 4 * grid_x,
          y = y + 4 * grid_y,
          group = glue::glue("{group}_{grid_x}_{grid_y}")
        )
    )
  }
)

full_pattern <- full_pattern %>%
  transpose() %>%
  map(bind_rows)

p <- ggplot() +
  geom_rect(data = full_pattern[["backgrounds"]], aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    fill = color
  )) +
  geom_circle(data = full_pattern[["circles"]], aes(x0 = x0, y0 = y0, r = r, fill = color), color = NA) +
  geom_rect(data = full_pattern[["squares"]], aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    fill = color
  )) +
  geom_polygon(data = full_pattern[["triangles"]], aes(x = x, y = y, group = group, fill = color)) +
  # geom_hline(yintercept = seq(4, 12, 2), color = dark, alpha = 0.8, size = 1) +
  # geom_vline(xintercept = seq(4, 16, 2), color = dark, alpha = 0.8, size = 1) +
  # geom_rect(aes(xmin = 4, xmax = 16, ymin = 4, ymax = 12), fill = light, alpha = 0.2) +
  scale_fill_identity() +
  coord_fixed(expand = FALSE, xlim = c(6, 22), ylim = c(6, 18)) +
  theme_void()

ggsave(here::here("26", "26c.png"), width = 12, height = 9, dpi = 300)

