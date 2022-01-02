# Perlin circle packing

library(ggplot2)
library(ggforce)
library(dplyr)
library(purrr)

source(here::here("colours.R"))
source(here::here("perlin_circle.R"))

set.seed(1234)

circles_control <- tribble(
  ~size, ~n,
  20, 10,
  15, 50,
  10, 40,
  7, 200,
  5, 100,
  3, 500,
  2, 500,
  1, 500,
  0.5, 500
)

x_min <- y_min <- 0
x_max <- 500
y_max <- 300

circles <- tribble(
  ~x0, ~y0, ~x, ~y, ~r, ~color
)

walk2(
  circles_control[["size"]],
  circles_control[["n"]],
  function(size, n) {
    size_n <- 0
    errors <- 0

    while (size_n < n & errors < 500) {
      current_x0 <- runif(1, x_min + size, x_max - size)
      current_y0 <- runif(1, y_min + size, y_max - size)

      circle <- perlin_circle(current_x0, current_y0, r_min = 0.8, r_max = 0.9, x_mult = size, y_mult = size)

      circle_size <- pmax(max(circle[["x"]]) - min(circle[["x"]]), max(circle[["y"]]) - min(circle[["y"]])) / 2

      circles_overlap <- circles %>%
        mutate(overlap = sqrt((current_x0 - x0)^2 + (current_y0 - y0)^2) < (r + circle_size) + 2) %>%
        filter(overlap) %>%
        nrow() > 0

      if (!circles_overlap) {
        temp_circles <- bind_rows(
          circles,
          circle %>%
            select(x, y) %>%
            mutate(
              x0 = current_x0,
              y0 = current_y0,
              r = circle_size,
              color = sample(genuary_colours(), 1),
              id = glue::glue("{size}_{circle_size}")
            )
        )

        circles <<- temp_circles

        size_n <- size_n + 1
      } else {
        errors <- errors + 1
      }
    }
  }
)

ggplot() +
  geom_polygon(data = circles, aes(x = x, y = y, fill = color, group = id), color = NA) +
  scale_fill_identity() +
  coord_fixed() +
  theme_void()
