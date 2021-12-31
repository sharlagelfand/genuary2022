# Day 6 - Trade styles with a friend
# Obviously ripping off Ijeamaka Anyene: https://twitter.com/ijeamaka_a

library(dplyr)
library(ggplot2)
library(purrr)
library(ggforce)
library(magick)

source(here::here("colours.R"))

squares <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax,
  0, 10, 0, 3.5,
  0, 4.75, 3.75, 10,
  0, 4.75, 3.75, 10,
  5, 10, 3.75, 7,
  5, 10, 7.25, 10
)

inset <- 0.125

inner_squares <- squares %>%
  mutate(
    across(c(xmin, ymin), ~ .x + inset),
    across(c(xmax, ymax), ~ .x - inset)
  )

circles <- squares %>%
  mutate(id = row_number()) %>%
  group_split(id) %>%
  map(function(data) {
    n <- 200

    tibble(
      x = runif(n, data[["xmin"]], data[["xmax"]]),
      y = runif(n, data[["ymin"]], data[["ymax"]]),
      r = runif(n, 2, 5)
    )
  }) %>%
  bind_rows()

p <- ggplot() +
  geom_rect(
    data = squares, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = lightgreen, color = darkblue
  ) +
  geom_rect(
    data = inner_squares, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = darkblue, color = darkblue
  ) +
  geom_point(
    data = circles, aes(x = x, y = y, size = r),
    shape = 21, fill = lightgreen, color = darkblue, stroke = 0.75
  ) +
  scale_size_identity() +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(color = light, fill = light))

ggsave(here::here("6", "day_6_clean.png"), p, width = 10, height = 10, dpi = 300)

clean_image <- image_read(here::here("6", "day_6_clean.png"))

image_noise <- clean_image %>%
  image_noise() %>%
  image_noise()

image_write(image_noise, here::here("6", "day_6_noisy.png"))
