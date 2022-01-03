# Day 13 - 80x800 (x80)

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

source(here::here("colours.R"))
source(here::here("gradients.R"))

set.seed(20220113)

# colour_palette <- genuary_colours()[c("red", "orange", "lightpink", "darkblue", "lightblue", "lightgreen")]
colour_palette <- genuary_colours()

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
      xmin <- width * i
      xmax <- width * (i + 1)
      ymin <- 0
      ymax <- height
    } else {
      ymin <- height * i
      ymax <- height * (i + 1)
      xmin <- 0
      xmax <- width
    }

    points[[i]] <- generate_points_from_grid(xmin, xmax, ymin, ymax, colour_1 = colour_1, colour_2 = colour_2, granularity = 1, horizontal = horizontal)
  }

  points %>%
    bind_rows(.id = "strip")
}

set_1 <- bind_rows(
  make_flowy_strips(10, 80, 800, FALSE),
  make_flowy_strips(10, 800, 80, FALSE) %>%
    mutate(
      x = x + 890,
      y = y - 80
    ),
  make_flowy_strips(10, 80, 800, TRUE) %>%
    mutate(
      x = x + 810,
      y = y - 810
    ),
  make_flowy_strips(10, 800, 80, TRUE) %>%
    mutate(
      y = y - 890,
      x = x + 80
    )
)

set_2 <- bind_rows(
  make_flowy_strips(10, 80, 800, TRUE),
  make_flowy_strips(10, 800, 80, TRUE) %>%
    mutate(
      x = x + 890,
      y = y - 80
    ),
  make_flowy_strips(10, 80, 800, FALSE) %>%
    mutate(
      x = x + 810,
      y = y - 810
    ),
  make_flowy_strips(10, 800, 80, FALSE) %>%
    mutate(
      y = y - 890,
      x = x + 80
    )
) %>%
  mutate(x = x + 810 * 2)

p <- set_1 %>%
  bind_rows(set_2) %>%
  ggplot() +
  geom_point(aes(x = x, y = y, colour = color), size = 0.1, shape = 15) +
  scale_colour_identity() +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = light, colour = light))

ggsave(here::here("13", "day_13.png"), p, height = 6, width = 11, dpi = 300)
