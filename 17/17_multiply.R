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

ggsave(here::here("17", "multiply", "border.png"), p_border, width = 7, height = 7, dpi = 500, bg = "transparent")

p_border_halftone <-  p +
  with_halftone_dither(
    geom_rect(aes(
      xmin = x_min + x_max * border_inset, xmax = x_max * (1 - border_inset),
      ymin = y_min + y_max * border_inset, ymax = y_max * (1 - border_inset)
    ),
    fill = lightpink, color = lightpink, size = 0.75
    )
  )

ggsave(here::here("17", "multiply", "border_halftone.png"), p_border_halftone, width = 7, height = 7, dpi = 500, bg = "transparent")

# Back stems

p_back_stems <- p +
  geom_polygon(data = stems %>%
    rowwise() %>%
    mutate(across(c(x), ~ .x * rnorm(1, 1, 0.0005))), aes(x = x + 0.5, y = y - 0.5, fill = colour, group = id), color = NA, alpha = 0.9)

ggsave(here::here("17", "multiply", "back_stems.png"), p_back_stems, width = 7, height = 7, dpi = 500, bg = "transparent")

# First strawberries

p_strawberries_1 <- p +
  geom_polygon(data = strawberries %>%
    rowwise() %>%
    mutate(across(c(x), ~ .x * rnorm(1, 1, 0.0005))), aes(x = x + 1, y = y, fill = colour, group = id), fill = lightpink)

ggsave(here::here("17", "multiply", "first_strawberries.png"), p_strawberries_1, width = 7, height = 7, dpi = 500, bg = "transparent")

# Second strawberries

p_strawberries_2 <- p +
  geom_polygon(data = strawberries %>%
    rowwise() %>%
    mutate(across(c(x), ~ .x * rnorm(1, 1, 0.0005))), aes(x = x, y = y, group = id), color = NA, alpha = 0.8, fill = lightpink)

ggsave(here::here("17", "multiply", "second_strawberries.png"), p_strawberries_2, width = 7, height = 7, dpi = 500, bg = "transparent")

# Specks

p_specks <- p +
  geom_polygon(data = data_specks %>%
    rowwise() %>%
    mutate(across(c(x), ~ .x * rnorm(1, 1, 0.0005))), aes(x = x, y = y, group = id), alpha = 1, fill = lightpink)

ggsave(here::here("17", "multiply", "specks.png"), p_specks, width = 7, height = 7, dpi = 500, bg = "transparent")

# Everything stamped out in white


p_stamp <- p +
    geom_polygon(data = stems %>%
      rowwise() %>%
      mutate(x = x * rnorm(1, 1, 0.0005) - 0.5), aes(x = x, y = y, group = id), color = NA, alpha = 1, fill = "white") +
    geom_polygon(data = strawberries %>%
      rowwise() %>%
      mutate(x = x * rnorm(1, 1, 0.0005) - 0.5), aes(x = x - 1, y = y, group = id), color = NA, alpha = 1, fill = "white")

ggsave(here::here("17", "multiply", "stamp.png"), p_stamp, width = 7, height = 7, dpi = 500, bg = "transparent")

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
                   mutate(x = x * rnorm(1, 1, 0.0005) - 0.5), aes(x = x - 1, y = y, group = id), color = NA, alpha = 1, fill = lightpink), size = 16
  )

ggsave(here::here("17", "multiply", "stamp_halftone.png"), p_stamp_halftone, width = 7, height = 7, dpi = 500, bg = "transparent")


# Read em back in and combine!

walk(
  c("border", "border_halftone", "back_stems", "first_strawberries", "second_strawberries", "specks", "stamp", "stamp_halftone"),
  function(x) {
    assign(x, image_read(here::here("17", "multiply", glue::glue("{x}.png"))), envir = .GlobalEnv)
  }
)

stamp_halftone_data <- image_data(stamp_halftone, "rgba")
stamp_halftone_data[4, , ] <- as.raw(round(as.integer(stamp_halftone_data[4, , ]) * 0.3))
stamp_halftone <- image_read(stamp_halftone_data)

image_blank(image_info(border)[["width"]], image_info(border)[["height"]]) %>%
  image_composite(border, operator = "Multiply") %>%
  image_composite(stamp) %>%
  image_composite(stamp_halftone) %>%
  image_composite(first_strawberries, operator = "Multiply") %>%
  image_composite(second_strawberries, operator = "Multiply") %>%
  image_composite(back_stems, operator = "Multiply") %>%
  image_composite(specks, operator = "Multiply") %>%
  image_background("white") %>%
  image_write(here::here("17", "multiply", "day_17_multiply.png"))
