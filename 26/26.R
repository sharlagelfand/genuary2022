# Day 26: Airport Carpet

library(dplyr)
library(ggplot2)
library(ggforce)
library(tidyr)
library(purrr)
library(magick)

source(here::here("colours.R"))
source(here::here("noise.R"))

r <- 4

pink_circle_tile_1 <- tribble(
  ~x0, ~y0, ~start, ~end, ~color, ~background,
  0, 0, 0, pi / 2, darkpink, lightblue,
  2 * r, 2 * r, pi, 3 * pi / 2, lightpink, lightblue
)

pink_circle_tile_1_flipped <- pink_circle_tile_1 %>%
  mutate(color = ifelse(x0 == 0, lightpink, darkpink))

pink_circle_tile_2 <- tribble(
  ~x0, ~y0, ~start, ~end, ~color, ~background,
  0, 2 * r, pi / 2, pi, lightpink, lightblue,
  2 * r, 0, 3 * pi / 2, 2 * pi, darkpink, lightblue
)

pink_circle_tile_2_flipped <- pink_circle_tile_2 %>%
  mutate(color = ifelse(x0 == 0, darkpink, lightpink))

green_circle_tile_1 <- pink_circle_tile_1 %>%
  mutate(
    background = lightpink,
    color = ifelse(x0 == 0, lightblue, darkblue)
  )

green_circle_tile_1_flipped <- green_circle_tile_1 %>%
  mutate(
    color = ifelse(x0 != 0, lightblue, darkblue)
  )

green_circle_tile_2 <- pink_circle_tile_2 %>%
  mutate(
    background = lightpink,
    color = ifelse(x0 == 0, darkblue, lightblue),
  )

green_circle_tile_2_flipped <- green_circle_tile_2 %>%
  mutate(
    color = ifelse(x0 == 0, lightblue, darkblue)
  )

mega_tile_1 <- bind_rows(
  pink_circle_tile_1 %>%
    mutate(row = 1),
  pink_circle_tile_2 %>%
    mutate(x0 = x0 + 2 * r, row = 1),
  green_circle_tile_1 %>%
    mutate(
      y0 = y0 + 2 * r, row = 2
    ),
  green_circle_tile_2 %>%
    mutate(
      y0 = y0 + 2 * r,
      x0 = x0 + 2 * r,
      row = 2
    )
)

mega_tile_2 <- bind_rows(
  green_circle_tile_2_flipped %>%
    mutate(row = 1),
  green_circle_tile_1_flipped %>%
    mutate(x0 = x0 + 2 * r, row = 1),
  pink_circle_tile_2_flipped %>%
    mutate(
      y0 = y0 + 2 * r,
      row = 2
    ),
  pink_circle_tile_1_flipped %>%
    mutate(
      y0 = y0 + 2 * r,
      x0 = x0 + 2 * r,
      row = 2
    )
)

mega_row_1 <- bind_rows(
  mega_tile_1,
  mega_tile_2 %>%
    mutate(x0 = x0 + 4 * r),
  mega_tile_2 %>%
    mutate(x0 = x0 + 8 * r),
  mega_tile_1 %>%
    mutate(x0 = x0 + 12 * r)
)

mega_row_2 <- bind_rows(
  mega_tile_2,
  mega_tile_1 %>%
    mutate(x0 = x0 + 4 * r),
  mega_tile_1 %>%
    mutate(x0 = x0 + 8 * r),
  mega_tile_2 %>%
    mutate(x0 = x0 + 12 * r)
) %>%
  mutate(y0 = y0 + 16)

mega_tile <- bind_rows(
  mega_row_1,
  mega_row_2
)

mega_tile_background <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~color,
  0, 4 * r, 0, 2 * r, lightblue,
  4 * r, 12 * r, 0, 2 * r, lightpink,
  12 * r, 16 * r, 0, 2 * r, lightblue,
  0, 4 * r, 2 * r, 6 * r, lightpink,
  4 * r, 12 * r, 2 * r, 6 * r, lightblue,
  12 * r, 16 * r, 2 * r, 6 * r, lightpink,
  0, 4 * r, 6 * r, 8 * r, lightblue,
  4 * r, 12 * r, 6 * r, 8 * r, lightpink,
  12 * r, 16 * r, 6 * r, 8 * r, lightblue
)

mega <- crossing(x = 1:3, y = 1:4)
mega <- map2(
  mega[["x"]], mega[["y"]],
  function(mega_x, mega_y) {
    list(
      background = mega_tile_background %>%
      mutate(
        across(c(xmin, xmax), ~ .x + 16 * r * mega_x),
        across(c(ymin, ymax), ~ .x + 8 * r * mega_y)
      ),
      tile =
        mega_tile %>%
        mutate(
          across(c(x0), ~ .x + 16 * r * mega_x),
          across(c(y0), ~ .x + 8 * r * mega_y)
        )
    )
  }
)

mega <- mega %>%
  transpose() %>%
  map(bind_rows)

p <- ggplot() +
  geom_rect(
    data = mega[["background"]],
    aes(
      xmin = xmin,
      ymin = ymin,
      xmax = xmax,
      ymax = ymax,
      fill = color
    )
  ) +
  geom_arc_bar(
    data = mega[["tile"]], aes(x0 = x0, y0 = y0, start = start, end = end, r = r, fill = color, r0 = 0),
    color = NA
  ) +
  scale_fill_identity() +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = light, color = light))

ggsave(here::here("26", "day_26_clean.png"), p, width = 12, height = 8, dpi = 300)

image_read(here::here("26", "day_26_clean.png")) %>%
  add_partial_noise(0.75) %>%
  image_write(here::here("26", "day_26_noisy.png"))
