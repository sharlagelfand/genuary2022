library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(prismatic)
library(ggstar)
library(ggsvg)

width <- 16
height <- 8

n_x <- 5
n_y <- 9

source(here::here("colours.R"))

starfish <- paste(readLines(here::here("15", "starfish.svg")), collapse = "\n")

colours <- c(red, lightpink, darkblue, purple, lightblue) %>%
  map_chr(clr_desaturate, 0.5) %>%
  map_chr(clr_lighten, 0.75)

colours <- c(
  colours,
  c(yellow) %>%
    map_chr(clr_desaturate, 0.7) %>%
    map_chr(clr_lighten, 0.8),
  light
)

waves <- crossing(x = 1:n_x, y = 1:n_y) %>%
  mutate(data = map2(
    x, y,
    function(col, row) {
      n_waves <- 5
      colours <- sample(colours, n_waves + 1)

      wave_bottom <- tibble(
        x = c(width * (col + 1), width * col, width * col),
        y = c(-height / 2 + height * row, -height / 2 + height * row, height * row),
        section = "bottom"
      )

      wave <- tibble(
        x = seq(width * col, width * (col + 1), 0.1),
        y = sin(x) * 0.5 + row * height + 2 * height / n_waves
      )

      waves <- map_dfr(
        (1:n_waves) - 1,
        function(i) {
          wave %>%
            mutate(y = y - height / n_waves * i) %>%
            bind_rows(wave_bottom) %>%
            mutate(
              colour = colours[[i + 1]],
              id = glue::glue("{col}_{row}_{i}")
            )
        }
      ) %>%
        mutate(
          background_colour = colours[[n_waves + 1]],
          row,
          col
        )

      if (row %% 2 != 1) {
        waves <- waves %>%
          mutate(x = x - width / 2)
      }

      waves %>%
        mutate(
          xmin = min(x),
          xmax = max(x),
          ymin = height * row,
          ymax = height * (row + 1)
        )
    }
  )) %>%
  select(data) %>%
  unnest(data)

p <- ggplot() +
  # Waves backgrounds
  geom_rect(
    data = waves %>% filter(stringr::str_ends(id, "_1")) %>%
      distinct(id, .keep_all = TRUE),
    aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax,
      fill = background_colour
    )
  ) +
  # Waves
  geom_polygon(data = waves, aes(x = x, y = y, fill = colour, group = id)) +
  # Clear tiles
  geom_tile(
    data = waves %>% filter(stringr::str_ends(id, "_1")) %>%
      distinct(id, .keep_all = TRUE),
    aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2),
    height = height / 2, width = width / 2,
    fill = light, alpha = 0.5
  ) +
  # Waves outlines
  geom_path(data = waves %>% filter(is.na(section)), aes(x = x, y = y, group = id), size = 0.3, alpha = 0.5, colour = darkblue) +
  # Waves shadows
  geom_path(data = waves %>% filter(is.na(section)), aes(x = x, y = y + 0.1, group = id), colour = darkblue, size = 1, alpha = 0.1) +
  # Stars
  # geom_star(data =
  #   waves %>% filter(stringr::str_ends(id, "_1")) %>%
  #     distinct(id, .keep_all = TRUE),
  #   aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2),
  #   fill = lightpink, color = lightpink,
  #   size = 5
  # ) +
  # Starfish
  geom_point_svg(
    data =
      waves %>% filter(stringr::str_ends(id, "_1")) %>%
        distinct(id, .keep_all = TRUE),
    aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2),
    svg = starfish,
    size = 10
  ) +
  scale_fill_identity() +
  coord_fixed(xlim = c(width, n_x * width + width / 2), ylim = c(height / 2 + height, (n_y - 1) * height + height / 2), expand = FALSE) +
  theme_void()

p

ggsave(here::here("15", "day_15.png"), p, width = 12, height = 9.3333, dpi = 300)
