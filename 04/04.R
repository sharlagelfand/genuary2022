library(dplyr)
library(ggplot2)
library(tidyr)

set.seed(1234)

source(here::here("colours.R"))

width <- 600
height <- 1000

spacing <- 5

n_row <- height / spacing
n_col <- width / spacing

x <- seq(width * -0.5, width * 1.5, spacing)

x_df <- tibble(x = x) %>%
  mutate(col = row_number())

y <- seq(height * -0.5, height * 1.5, spacing)

y_df <- tibble(y = y) %>%
  mutate(row = row_number())

grid <- crossing(x = x, y = y) %>%
  left_join(x_df, by = "x") %>%
  left_join(y_df, by = "y")

grid <- grid %>%
  mutate(angle = row / n_row * pi)

grid <- grid %>%
  mutate(
    xend = x + 20 * cos(angle),
    yend = y + 20 * sin(angle),
  )

# grid %>%
#   ggplot() +
#   geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
#   geom_point(aes(x = x, y = y), size = 0.2) +
#   coord_fixed()

find_closest_angle <- function(point, grid) {
  grid %>%
    mutate(distance = sqrt((x - point[["x"]])^2 + (y - point[["y"]])^2)) %>%
    arrange(distance) %>%
    slice(1) %>%
    pull(angle)
}

# Need to give a start / end point for geom_segment

n_curves <- 1000

curves <- vector("list", length = n_curves)

for (j in 1:n_curves) {
  length <- runif(1, 0, 100)
  n_steps <- sample(5:20, 1)

  random_point <- list(
    x = runif(1, width * -0.5, width * 1.5),
    y = runif(1, width * -0.5, height * 1.5)
  )

  random_point_df <- tribble(
    ~x, ~y, ~xend, ~yend
  )

  for (i in 1:n_steps) {
    closest_angle <- random_point %>%
      find_closest_angle(grid)

    random_point_df_new <- random_point %>%
      as_tibble() %>%
      mutate(
        xend = x + length * cos(closest_angle),
        yend = y + length * sin(closest_angle)
      )

    random_point <- list(
      x = random_point_df_new[["xend"]],
      y = random_point_df_new[["yend"]]
    )

    random_point_df <- bind_rows(
      random_point_df,
      random_point_df_new
    )
  }
  curves[[j]] <- random_point_df
}

curves <- curves %>%
  bind_rows(.id = "curve")

chosen_curves_id <- tibble(curve = sample(
  1:n_curves,
  sample(1:n_curves), 1
) %>%
  as.character())

colours <- tibble(colour = sample(genuary_colours(), nrow(chosen_curves_id), replace = TRUE))
chosen_curves_id <- bind_cols(chosen_curves_id, colours)

chosen_curves <- curves %>%
  inner_join(chosen_curves_id, by = "curve")

ggplot() +
  # geom_rect(aes(xmin = 0, xmax = width, ymin = 0, ymax = height)) +
  # geom_point(data = grid, aes(x = x, y = y), size = 0.2) +
  geom_segment(
    data = chosen_curves, aes(
      x = x, xend = xend,
      y = y, yend = yend, group = curve,
      colour = colour
    ),
    size = 2
  ) +
  scale_colour_identity() +
  coord_fixed(xlim = c(0, width), ylim = c(0, height)) +
  theme_void()
