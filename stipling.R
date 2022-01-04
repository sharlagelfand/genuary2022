library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

source(here::here("colours.R"))

generate_points_from_grid(0, 10, 0, 4, dark, "white",
  granularity = 10, horizontal = FALSE
) %>%
  filter(color == dark) %>%
  sample_frac(0.6) %>%
  rowwise() %>%
  mutate(
    across(c(x, y), ~ .x + rnorm(1, 0.1, 0.2)),
    angle = 5 * pi / 3,
    angle = angle * rnorm(1, 1, 0.005),
    length = runif(1, 0.05, 0.15),
    xend = x + length * sin(angle),
    yend = y + length * cos(angle),
    size = rnorm(1, 3, 0.5)
  ) %>%
  ungroup() %>%
  ggplot() +
  geom_segment(aes(
    x = x, xend = xend, y = y, yend = yend,
    colour = color, size = size
  ),
  lineend = "round", alpha = 0.8
  ) +
  coord_fixed() +
  scale_color_identity() +
  scale_size_identity()
