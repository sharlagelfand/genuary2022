# Day 16 - Gradients gone wrong

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(magick)

source(here::here("colours.R"))
source(here::here("gradients.R"))

set.seed(12345)

width <- 8 * 2
height <- 80 * 2

colour_palette <- genuary_colours()[c("yellow", "orange", "lightpink", "lightblue", "lightgreen", "purple")]

make_flowy_strips <- function(n, width, height, horizontal = width > height) {
  colours <- c()
  points <- vector("list", length = n)

  colours[1] <- sample(colour_palette, 1)

  for (i in 1:n) {
    colour <- sample(colour_palette, 1)

    if (i > 1) {
      previous_colour <- colours[i]
      while (colour == previous_colour | colour == colours[i - 1]) {
        colour <- sample(colour_palette, 1)
      }
    } else {
      previous_colour <- sample(colour_palette, 1)
    }

    colours[i + 1] <- colour

    if (i %% 2 == 0) {
      colour_1 <- previous_colour
      colour_2 <- colour
    } else {
      colour_2 <- previous_colour
      colour_1 <- colour
    }

    if (height > width) {
      xmin <- width * ( 10 / n) * i
      xmax <- width * ( 10 / n) * (i + 1)
      ymin <- 0
      ymax <- height
    } else {
      ymin <- height * ( 10 / n) * i
      ymax <- height * ( 10 / n) * (i + 1)
      xmin <- 0
      xmax <- width
    }

    points[[i]] <- generate_points_from_grid(xmin, xmax, ymin, ymax, colour_1 = colour_1, colour_2 = colour_2, granularity = 4, horizontal = horizontal)
  }

  points %>%
    bind_rows(.id = "strip")
}

set_1 <- bind_rows(
  # Top left
  make_flowy_strips(20, width, height, TRUE) %>%
    mutate(x = x + width / 2),
  # Top right
  make_flowy_strips(10, height, width, FALSE) %>%
    mutate(
      x = x + width + height,
      y = y - width
    ),
  # Bottom right
  make_flowy_strips(10, width, height, TRUE) %>%
    mutate(
      x = x + height,
      y = y - height
    ),
  # Bottom left
  make_flowy_strips(10, height, width, FALSE) %>%
    mutate(
      y = y - width - height,
      x = x + width
    )
)

set_2 <- bind_rows(
  # Top left
  make_flowy_strips(20, width, height, TRUE) %>%
    mutate(x = x + width / 2),
  # Top right
  make_flowy_strips(10, height, width, FALSE) %>%
    mutate(
      x = x + width + height,
      y = y - width
    ),
  # Bottom right
  make_flowy_strips(10, width, height, TRUE) %>%
    mutate(
      x = x + height,
      y = y - height
    ),
  # Bottom left
  make_flowy_strips(10, height, width, FALSE) %>%
    mutate(
      y = y - width - height,
      x = x + width
    )
) %>%
  mutate(x = x + height * 2)

p <- set_1 %>%
  bind_rows(set_2) %>%
  ggplot() +
  geom_point(aes(x = x, y = y, colour = color), size = 0.1, shape = 15) +
  scale_colour_identity() +
  coord_polar() +
  theme_void()

ggsave(here::here("16", "day_16_gradients.png"), p, height = 8, width = 8, dpi = 300, bg = "transparent")

# Create background

background <- generate_points_from_grid(xmin = 0, xmax = 100, ymin = 0, ymax = 100, colour_1 = lightblue, colour_2 = lightgreen, granularity = 20, horizontal = FALSE) %>%
  ggplot(aes(x = x, y = y, colour = color)) +
  geom_point(size = 0.1, shape = 15) +
  scale_colour_identity() +
  coord_fixed(expand = FALSE) +
  theme_void()

ggsave(here::here("16", "day_16_background.png"), background, height = 16, width = 16, dpi = 300)

img <- image_read(here::here("16", "day_16_gradients.png"))
background <- image_read(here::here("16", "day_16_background.png"))
background <- image_resize(background, "50%")
background <- image_rotate(background, 180)

background %>%
  image_composite(img) %>%
  image_write(here::here("16", "day_16.png"))
