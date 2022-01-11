# Perlin circle packing

library(ggplot2)
library(dplyr)
library(purrr)
library(magick)

source(here::here("colours.R"))
source(here::here("perlin_circle.R"))
source(here::here("pack_circles.R"))
source(here::here("noise.R"))

set.seed(20220117)

x_min <- y_min <- 0
x_max <- y_max <- 200

# Strawberries

strawberry_size <- 15

strawberries <- tribble(
  ~size, ~n,
  strawberry_size, 30
) %>%
  pack_circles(x_min, x_max, y_min, y_max, n_errors = 200) %>%
  mutate(id = row_number()) %>%
  split(.$id) %>%
  map(function(data) {
    perlin_circle(data[["x0"]], data[["y0"]],
      r_min = 0.8, r_max = 0.9,
      x_mult = data[["r"]], y_mult = data[["r"]] * 1.2
    ) %>%
      bind_cols(data %>%
        select(x0, y0, original_r = r))
  }) %>%
  bind_rows(.id = "id") %>%
  mutate(colour = red)

# Add stems to each

generate_stem_data <- function(cx = 1, cy = 1, r = 1, petal_color = "pink", size = 1) {
  dplyr::tribble(
    ~cx, ~cy, ~n, ~noise_max, ~octaves, ~r_min, ~r_max, ~color, ~section,
    cx, cy, 2000, 3, 1, r, r * 2.5, petal_color, "petal",
  ) %>%
    dplyr::mutate(circle = purrr::pmap(list(cx, cy, n, noise_max, octaves, r_min, r_max), perlin_circle,
      x_mult = size,
      y_mult = size
    )) %>%
    dplyr::select(section, colour = color, circle) %>%
    tidyr::unnest(circle)
}

stems <- strawberries %>%
  distinct(x0, y0, id, r = original_r) %>%
  split(.$id) %>%
  map(function(data) {
    generate_stem_data(data[["x0"]], data[["y0"]] + data[["r"]] * 0.8, petal_color = darkgreen, size = strawberry_size * 0.3)
  }) %>%
  bind_rows(.id = "id")

# Add specks

specks <- strawberries %>%
  distinct(x0, y0, id, r = original_r) %>%
  split(.$id) %>%
  map(function(data) {
    n_specks <- sample(10:15, 1)
    sizes <- runif(n_specks, 0.5, 1.25)

    x0 <- data[["x0"]]
    y0 <- data[["y0"]]
    r <- data[["r"]]

    tibble(size = sizes * 2) %>%
      mutate(n = 1) %>%
      arrange(-size) %>%
      pack_circles(x0 - r / 2, x0 + r / 2, y0 - r / 2, y0 + r / 2, n_errors = 10) %>%
      mutate(id = row_number()) %>%
      split(.$id) %>%
      map(function(data) {
        perlin_circle(data[["x0"]], data[["y0"]],
          n = 50,
          r_min = 0.8, r_max = 0.9,
          x_mult = data[["r"]] / 2, y_mult = data[["r"]]
        ) %>%
          bind_cols(data %>%
            select(x0, y0, original_r = r)) %>%
          mutate(
            colour = sample(c("white", lightpink), 1),
            alpha = runif(1, 0.3, 0.8)
          )
      }) %>%
      bind_rows(.id = "id") %>%
      mutate(id = glue::glue("{data[['id']]}_{id}"))
  }) %>%
  bind_rows()

border_inset <- 0.025

p <- ggplot() +
  geom_rect(aes(
    xmin = x_min + x_max * border_inset, xmax = x_max * (1 - border_inset),
    ymin = y_min + y_max * border_inset, ymax = y_max * (1 - border_inset)
  ),
  fill = NA, color = lightpink, size = 0.75
  ) +
  geom_polygon(data = stems, aes(x = x + 0.5, y = y - 0.5, fill = colour, group = id), color = NA, alpha = 0.9) +
  geom_polygon(data = strawberries, aes(x = x + 1, y = y, fill = colour, group = id), fill = lightpink) +
  geom_polygon(data = strawberries, aes(x = x, y = y, fill = colour, group = id), color = NA, alpha = 0.8) +
  geom_polygon(data = specks, aes(x = x, y = y, group = id, fill = colour, alpha = alpha)) +
  geom_polygon(data = stems, aes(x = x - 0.5, y = y, fill = colour, group = id), color = NA, alpha = 0.7) +
  geom_polygon(data = stems, aes(x = x, y = y, fill = colour, group = id), color = NA, alpha = 0.7) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_alpha_identity() +
  coord_fixed(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
  theme_void()

ggsave(here::here("17", "day_17.png"), p, width = 7, height = 7, dpi = 500, bg = "white")

image_read(here::here("17", "day_17.png")) %>%
  add_partial_noise() %>%
  image_write(here::here("17", "day_17_noisy.png"))

