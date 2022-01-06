# Day 7 - Sol LeWitt Wall Drawing
# 413: https://massmoca.org/event/walldrawing413/

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)
library(magick)

source(here::here("colours.R"))

# 24 possible combinations of 4 colours
# Each then arranged into a grid
# Top left: original
# Top right: mirrored on y
# Bottom right: mirrored on x
# Bottom left: mirrored on x, y

positions <- c("top_left", "top_right", "bottom_left", "bottom_right")

colours <- tibble(c1 = lightgreen, c2 = darkblue, c3 = lightpink, c4 = red) %>%
  pivot_longer(everything(), names_to = "name", values_to = "colour")

position_placement <- tribble(
  ~position, ~xmin, ~xmax, ~ymin, ~ymax,
  "top_left", -1, 0, 0, 1,
  "top_right", 0, 1, 0, 1,
  "bottom_right", 0, 1, -1, 0,
  "bottom_left", -1, 0, -1, 0
)

originals <- crossing(c1 = positions, c2 = positions, c3 = positions, c4 = positions) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id) %>%
  group_by(id) %>%
  filter(n_distinct(value) == 4) %>%
  ungroup() %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, values_to = "position") %>%
  left_join(colours, by = "name") %>%
  select(-name)

make_full_set_of_squares <- function(x) {
  diff <- 2.2

  # Top right: flip y
  top_right <- x %>%
    mutate(across(c(ymin, ymax), ~ .x * -1)) %>%
    mutate(across(c(xmin, xmax), ~ .x + diff))

  # Bottom right: flip x, y,
  bottom_right <- x %>%
    mutate(across(c(xmin, xmax), ~ .x * -1 + diff)) %>%
    mutate(across(c(ymin, ymax), ~ .x - diff))

  # Bottom left: flip y, x
  bottom_left <- x %>%
    mutate(across(c(xmin, xmax), ~ .x * -1)) %>%
    mutate(across(c(ymin, ymax), ~ .x * -1 - diff))

  bind_rows(
    x,
    top_right,
    bottom_right,
    bottom_left
  )
}

originals <- originals %>%
  left_join(position_placement, by = "position") %>%
  split(.$id)

combination_order <- sample(1:length(originals), length(originals))

p <- originals[combination_order] %>%
  map(function(x) {
    x %>%
      make_full_set_of_squares() %>%
      ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = colour)) +
      geom_rect() +
      scale_fill_identity() +
      coord_fixed() +
      theme_void()
  }) %>%
  wrap_plots(ncol = 8) &
  plot_annotation(theme = theme(plot.background = element_rect(fill = light, colour = light)))

ggsave(here::here("07", "day_7_clean.png"), width = 16, height = 6, dpi = 300)

image_read(here::here("07", "day_7_clean.png")) %>%
  image_noise() %>%
  image_write(here::here("07", "day_7_noisy.png"))
