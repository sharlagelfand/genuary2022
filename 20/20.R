library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(ggstar)

source(here::here("colours.R"))
source(here::here("perlin_circle.R"))

sea_of_shapes <- function(seed) {

  set.seed(seed)

  main_colors <- c(lightgreen, darkblue, orange, lightblue)
  # colors <- c(dark, main_colors)
  colors <- main_colors

  grid <- crossing(
    x0 = seq(1, 10, 0.3),
    y0 = seq(1, 10, 0.3)) %>%
    rowwise() %>%
    mutate(across(c(x0, y0), list(jitter = ~ .x * runif(1, 0.99, 1.01)))) %>%
    ungroup()

  # Choose a % to be star shapes, and the rest perlin blobs -----

  star_points <- grid %>%
    distinct(x0, y0) %>%
    sample_frac(size = runif(1, 0.4, 0.9))

  perlin_points <- grid %>%
    anti_join(star_points, by = c("x0", "y0")) %>%
    rowwise() %>%
    mutate(
      mult = rnorm(1, 0.1, 0.05),
      mult = max(mult, 0.1),
      mult = min(0.15, mult),
      colour = sample(colors, 1),
      r_max = runif(1, 1, 4),
      noise_max = runif(1, 1, 5),
      perlin_circle = map2(x0_jitter, y0_jitter, perlin_circle, noise_max = noise_max, r_max = r_max, x_mult = mult / r_max, y_mult = mult / r_max)
    ) %>%
    ungroup() %>%
    mutate(id = row_number()) %>%
    unnest(perlin_circle)

  dark_perlin_points <- perlin_points %>%
    mutate(across(c(x, y), ~ .x + 0.01 * sample(c(0, 1, -1), 1)))

  star_points <- star_points %>%
    rowwise() %>%
    mutate(
      shape = sample(c(
        27, 28, 30, 21, 23, 24, 25, 17, 18,
        19, 20, 11, 12, 13, 14, 15, 6, 7,
        8, 9, 10, 1, 3, 4, 5
      ), 1),
      size = sample(seq(1, 3.5, 0.25), 1),
      colour = sample(colors, 1),
      angle = sample(0:360, 1)
    ) %>%
    ungroup()

  dark_star_points <- star_points %>%
    mutate(across(c(x0, y0), ~ .x + runif(1, 0.005, 0.015) * sample(c(0, 1, -1), 1)))

  # Plot ----

  p <- ggplot() +
    geom_polygon(
      data = dark_perlin_points,
      aes(
        x = x, y = y,
        group = id
      ),
      fill = dark
    ) +
    geom_polygon(
      data = perlin_points,
      aes(
        x = x, y = y,
        group = id, fill = colour
      )
    ) +
    geom_star(
      data = dark_star_points,
      aes(
        x = x0, y = y0, size = size,
        starshape = shape, angle = angle
      ),
      fill = dark, colour = dark
    ) +
    geom_star(
      data = star_points,
      aes(
        x = x0, y = y0, size = size,
        starshape = shape, fill = colour,
        color = colour, angle = angle
      )
    ) +
    coord_fixed() +
    scale_fill_identity() +
    scale_colour_identity() +
    scale_size_identity() +
    scale_starshape_identity() +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", colour = "white"))

  p
}

sea_of_shapes(1)

walk(1:10, ~ print(sea_of_shapes(.x)))
