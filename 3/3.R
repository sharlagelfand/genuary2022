# Day 3 - Space

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(magick)

source(here::here("gradients.R"))
source(here::here("perlin_circle.R"))
source(here::here("colours.R"))

set.seed(20220103)

# Background ----

background <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~colour_2, ~colour_1,
  0, 10, 0, 10, dark, "black"
) %>%
  mutate(points = pmap(list(xmin, xmax, ymin, ymax, colour_1, colour_2), generate_points_from_grid, granularity = 200)) %>%
  select(points) %>%
  unnest(cols = c(points))

# Stars ----

n <- 5000

stars <- tibble(
  x = runif(n, 0, 10),
  y = runif(n, 0, 10),
  r_min = rnorm(n, 0.001, 0.005),
  r_max = rnorm(n, 0.001, 0.005)
) %>%
  mutate(perlin = pmap(list(x, y, r_min, r_max), function(x, y, r_min, r_max) perlin_circle(x, y, n = 50, r_min = r_min, r_max = r_max))) %>%
  mutate(id = row_number()) %>%
  select(id, perlin) %>%
  unnest(cols = perlin)

# Planets ----

planet_1 <- perlin_circle(2, 5) %>%
  mutate(colour = darkblue)
planet_2 <- perlin_circle(7.75, 3.5, r_min = 1.2, r_max = 1.5) %>%
  mutate(colour = red)
planet_3 <- perlin_circle(6, 8, r_min = 0.5, r_max = 0.75) %>%
  mutate(colour = orange)

planets <- bind_rows(
  planet_1 %>%
    mutate(id = "1a"),
  planet_1 %>%
    mutate(across(c(x, y), ~ .x * rnorm(1, mean = 1, sd = 0.02))) %>%
    mutate(id = "1b"),
  planet_1 %>%
    mutate(across(c(x, y), ~ .x * rnorm(1, mean = 1, sd = 0.02))) %>%
    mutate(id = "1c"),
  planet_1 %>%
    mutate(across(c(x, y), ~ .x * rnorm(1, mean = 1, sd = 0.02))) %>%
    mutate(id = "1d"),
  planet_2 %>%
    mutate(id = "2a"),
  planet_2 %>%
    mutate(across(c(x, y), ~ .x * rnorm(1, mean = 1, sd = 0.02))) %>%
    mutate(id = "2b"),
  planet_2 %>%
    mutate(across(c(x, y), ~ .x * rnorm(1, mean = 1, sd = 0.02))) %>%
    mutate(id = "2c"),
  planet_2 %>%
    mutate(across(c(x, y), ~ .x * rnorm(1, mean = 1, sd = 0.02))) %>%
    mutate(id = "2d"),
  planet_3 %>%
    mutate(id = "3a"),
  planet_3 %>%
    mutate(across(c(x, y), ~ .x * rnorm(1, mean = 1, sd = 0.02))) %>%
    mutate(id = "3b"),
  planet_3 %>%
    mutate(across(c(x, y), ~ .x * rnorm(1, mean = 1, sd = 0.02))) %>%
    mutate(id = "3c"),
  planet_3 %>%
    mutate(across(c(x, y), ~ .x * rnorm(1, mean = 1, sd = 0.02))) %>%
    mutate(id = "3d")
)

p <- ggplot() +
  geom_point(data = background, aes(x = x, y = y, color = color), size = 0.1, shape = 15) +
  geom_polygon(data = stars, aes(x = x, y = y, group = id), fill = lightgreen, alpha = 1) +
  geom_polygon(data = planets, aes(x = x, y = y, group = id, fill = colour), alpha = 0.8) +
  scale_color_identity() +
  scale_fill_identity() +
  theme_void() +
  coord_fixed(ylim = c(3, 10), expand = FALSE)

ggsave(here::here("3", "day_3_clean.png"), p, width = 10, height = 7, dpi = 300)

image_read(here::here("3", "day_3_clean.png")) %>%
  image_noise() %>%
  image_write(here::here("3", "day_3_noisy.png"))
