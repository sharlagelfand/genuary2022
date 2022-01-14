library(ggplot2)
library(dplyr)
library(purrr)
library(magick)
library(tidyr)
library(ggfx)

source(here::here("colours.R"))
source(here::here("perlin_circle.R"))
source(here::here("pack_circles.R"))
source(here::here("noise.R"))

set.seed(20220117)

x_min <- y_min <- 0
paper_width <- 8.5
paper_height <- 11
x_max <- paper_width * 25
y_max <- paper_height * 25

# Strawberries

strawberry_size <- 17

strawberries <- tribble(
  ~size, ~n,
  strawberry_size, 40
) %>%
  pack_circles(x_min, x_max, y_min, y_max, n_errors = 500) %>%
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
    cx, cy, 500, 3, 1, r, r * 2.5, petal_color, "petal",
  ) %>%
    dplyr::mutate(circle = purrr::pmap(list(cx, cy, n, noise_max, octaves, r_min, r_max), perlin_circle,
      x_mult = size,
      y_mult = size * 0.75
    )) %>%
    dplyr::select(section, colour = color, circle) %>%
    tidyr::unnest(circle)
}

stems <- strawberries %>%
  distinct(x0, y0, id, r = original_r) %>%
  split(.$id) %>%
  map(function(data) {
    generate_stem_data(data[["x0"]], data[["y0"]] + data[["r"]], petal_color = darkgreen, size = strawberry_size * 0.3)
  }) %>%
  bind_rows(.id = "id")

# Add specks

data_specks <- strawberries %>%
  distinct(x0, y0, id, r = original_r) %>%
  split(.$id) %>%
  map(function(data) {
    n_specks <- sample(5:10, 1)
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
            select(x0, y0, original_r = r))
      }) %>%
      bind_rows(.id = "id") %>%
      mutate(id = glue::glue("{data[['id']]}_{id}"))
  }) %>%
  bind_rows()

border_inset <- 0.025

# Split into layers

p <- ggplot() +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_alpha_identity() +
  coord_fixed(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
  theme_void()

# Border

p_border <- p +
  geom_rect(aes(
    xmin = x_min + x_max * border_inset, xmax = x_max * (1 - border_inset),
    ymin = y_min + y_max * border_inset, ymax = y_max * (1 - border_inset)
  ),
  fill = lightpink, color = lightpink, size = 0.75
  )

ggsave(here::here("17", "layers", "border.png"), p_border, width = paper_width, height = paper_height, dpi = 500, bg = "transparent")

# Back stems

p_back_stems <- p +
  geom_polygon(data = stems %>%
    rowwise() %>%
    mutate(across(c(x), ~ .x * rnorm(1, 1, 0.0005))), aes(x = x + 0.5, y = y - 0.5, fill = colour, group = id), color = NA, alpha = 0.9)

ggsave(here::here("17", "layers", "back_stems.png"), p_back_stems, width = paper_width, height = paper_height, dpi = 500, bg = "transparent")

# First strawberries

p_strawberries_1 <- p +
  geom_polygon(data = strawberries %>%
    rowwise() %>%
    mutate(across(c(x), ~ .x * rnorm(1, 1, 0.0005))), aes(x = x + 1, y = y, fill = colour, group = id), fill = lightpink)

ggsave(here::here("17", "layers", "first_strawberries.png"), p_strawberries_1, width = paper_width, height = paper_height, dpi = 500, bg = "transparent")

# Second strawberries

p_strawberries_2 <- p +
  geom_polygon(data = strawberries %>%
    rowwise() %>%
    mutate(across(c(x), ~ .x * rnorm(1, 1, 0.0005))), aes(x = x, y = y, group = id), color = NA, alpha = 0.8, fill = lightpink)

ggsave(here::here("17", "layers", "second_strawberries.png"), p_strawberries_2, width = paper_width, height = paper_height, dpi = 500, bg = "transparent")

# Specks

p_specks <- p +
  geom_polygon(data = data_specks %>%
    rowwise() %>%
    mutate(across(c(x), ~ .x * rnorm(1, 1, 0.0005))), aes(x = x, y = y, group = id), alpha = 1, fill = lightpink)

ggsave(here::here("17", "layers", "specks.png"), p_specks, width = paper_width, height = paper_height, dpi = 500, bg = "transparent")

# Everything stamped out in white

p_stamp <- p +
  geom_polygon(data = stems %>%
    rowwise() %>%
    mutate(x = x * rnorm(1, 1, 0.0005) - 0.5), aes(x = x, y = y, group = id), color = NA, alpha = 1, fill = light) +
  geom_polygon(data = strawberries %>%
    rowwise() %>%
    mutate(x = x * rnorm(1, 1, 0.0005) - 0.5), aes(x = x - 1, y = y, group = id), color = NA, alpha = 1, fill = light)

ggsave(here::here("17", "layers", "stamp.png"), p_stamp, width = paper_width, height = paper_height, dpi = 500, bg = "transparent")

# Halftone stamp

p_stamp_halftone <- p +
  with_halftone_dither(
    geom_polygon(data = stems %>%
      rowwise() %>%
      mutate(x = x * rnorm(1, 1, 0.0005) - 0.5), aes(x = x, y = y, group = id), color = NA, alpha = 1, fill = lightpink)
  ) +
  with_halftone_dither(
    geom_polygon(data = strawberries %>%
      rowwise() %>%
      mutate(x = x * rnorm(1, 1, 0.0005) - 0.5), aes(x = x - 1, y = y, group = id), color = NA, alpha = 1, fill = lightpink),
    size = 16
  )

ggsave(here::here("17", "layers", "stamp_halftone.png"), p_stamp_halftone, width = paper_width, height = paper_height, dpi = 500, bg = "transparent")

# Save smaller because the dots are a fixed size no matter how it's saved, and then by resizing to larger, the dots will be resized.... I know okay

ggsave(here::here("17", "layers", "stamp_halftone_small.png"), p_stamp_halftone,
  width = paper_width / 3,
  height = paper_height / 3, dpi = 500, bg = "transparent"
)

# Fake a risograph look overall, with points
riso_points <- crossing(
  x = seq(x_min, x_max, 1),
  y = seq(y_min, y_max, 1)
)

p_riso <- p +
  geom_point(data = riso_points, aes(x = x, y = y), colour = light, size = 0.1)

ggsave(here::here("17", "layers", "riso.png"), p_riso, width = paper_width, height = paper_height, dpi = 500, bg = "transparent")

# Read em back in and combine!

walk(
  c("border", "back_stems", "first_strawberries", "second_strawberries", "specks", "stamp", "stamp_halftone", "stamp_halftone_small", "riso"),
  function(x) {
    assign(x, image_read(here::here("17", "layers", glue::glue("{x}.png"))), envir = .GlobalEnv)
  }
)

# Fix size of halftone stamp, makes circles bigger..... I know

stamp_halftone <- image_resize(stamp_halftone_small, geometry_size_pixels(4250, 5500))

stamp_halftone_data <- image_data(stamp_halftone, "rgba")
stamp_halftone_data[4, , ] <- as.raw(round(as.integer(stamp_halftone_data[4, , ]) * 0.3))
stamp_halftone <- image_read(stamp_halftone_data)

# Lower opacity of riso
riso_data <- image_data(riso, "rgba")
riso_data[4, , ] <- as.raw(round(as.integer(riso_data[4, , ]) * 0.03))
riso <- image_read(riso_data)

border %>%
  image_composite(riso) %>%
  image_composite(stamp) %>%
  image_composite(stamp_halftone) %>%
  image_composite(stamp_halftone, "Overlay") %>%
  image_composite(first_strawberries, operator = "Multiply") %>%
  image_composite(second_strawberries, operator = "Multiply") %>%
  image_composite(back_stems, operator = "Multiply") %>%
  image_composite(specks, operator = "Multiply") %>%
  image_background(light) %>%
  image_write(here::here("17", "day_17.png"))
