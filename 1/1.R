library(dplyr)
library(ggplot2)
library(purrr)
library(magick)

source(here::here("colours.R"))
source(here::here("noise.R"))

set.seed(20220101)

height <- 1000
width <- 1600

# Background

background_colours <- sample(c(darkblue, lightblue, purple), 3)
background_split <- runif(1, 0.2, 0.4)

background <- tibble(x = 1:width)
background <- bind_rows(
  background %>%
    mutate(
      y = 0, y_end = round(height * background_split),
      colour = ifelse(x %% 2, background_colours[1], background_colours[2])
    ),
  background %>%
    mutate(
      y = round(height * background_split), y_end = height,
      colour = ifelse(x %% 2, background_colours[1], background_colours[3])
    )
)

# Squares of stripes

squares <- vector("list", length = 250)

for (i in seq_along(squares)) {
  squares[[i]]$x <- sample(1:width, 1)
  squares[[i]]$x_end <- squares[[i]]$x + sample(10:300, 1)
  squares[[i]]$y <- sample(1:height, 1)
  squares[[i]]$y_end <- squares[[i]]$y + sample(10:300, 1)

  square_colours <- sample(genuary_colours(), 2)
  square_vertical <- sample(c(TRUE, FALSE), 1)

  size <- runif(1, 1, 1.5)
  remove_n <- sample(1:5, 1)

  if (square_vertical) {
    squares[[i]]$df <- tibble(x = squares[[i]]$x:squares[[i]]$x_end) %>%
      mutate(
        vertical = TRUE,
        size = size,
        x_end = x,
        y = squares[[i]]$y,
        y_end = squares[[i]]$y_end,
        colour = ifelse(x %% 2, square_colours[[1]], square_colours[[2]])
      ) %>%
      filter(x %% remove_n == 0)
  } else {
    squares[[i]]$df <- tibble(y = squares[[i]]$y:squares[[i]]$y_end) %>%
      mutate(
        vertical = FALSE,
        size = size,
        y_end = y,
        x = squares[[i]]$x,
        x_end = squares[[i]]$x_end,
        colour = ifelse(y %% 2, square_colours[[1]], square_colours[[2]])
      )%>%
      filter(y %% remove_n == 0)
  }
}

squares_df <- squares %>%
  transpose() %>%
  pluck("df") %>%
  bind_rows(.id = "id")

squares_df <- squares_df %>%
  filter((vertical & x < width - 10) |
    (!vertical & y < height - 10)) %>%
  mutate(
    y_end = ifelse(vertical & y_end > height, height, y_end),
    x_end = ifelse(!vertical & x_end > width, width, x_end)
  ) %>%
  head(10000 - nrow(background))

p <- ggplot() +
  geom_rect(
    aes(xmin = -10, xmax = width + 10, ymin = -10, ymax = height + 10),
    fill = NA, color = darkblue
  ) +
  geom_segment(
    data = background, aes(
      x = x, xend = x, y = y, yend = y_end,
      color = colour
    ),
    size = 1
  ) +
  geom_segment(
    data = squares_df, aes(
      x = x, xend = x_end, y = y, yend = y_end,
      color = colour,
      size = size
    ),
    alpha = 0.75
  ) +
  scale_color_identity() +
  scale_size_identity() +
  coord_fixed() +
  theme_void() +
  theme(panel.background = element_rect(color = light, fill = light))

p

ggsave(here::here("1", "day_1_clean.png"), p, width = 12.6, height = 8, dpi = 300)

clean_image <- image_read(here::here("1", "day_1_clean.png"))

image_noise <- clean_image %>%
  add_partial_noise()

image_write(image_noise, here::here("1", "day_1_noisy.png"))
