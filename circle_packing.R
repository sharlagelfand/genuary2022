# Circle packing

# Method via https://tylerxhobbs.com/essays/2016/a-randomized-approach-to-cicle-packing

# Pick size and how many circles of each size
# Begin with largest and end with smallest circles
# Attempt to insert new circles until count is hit or until 2000 failures in a row
# Then move on to next size
# To see if random location fits, check collision with other circles:
# ###
# See if the distance between center points is larger than sum of radii
# ###

library(ggplot2)
library(ggforce)
library(dplyr)
library(purrr)

source(here::here("colours.R"))

circles_control <- tribble(
  ~size, ~n,
  10, 5,
  7, 10,
  5, 20,
  3, 200,
  2, 300,
  1, 500
)

x_min <- y_min <- 0
x_max <- y_max <- 300

circles <- tribble(
  ~x0, ~y0, ~r, ~color
)

walk2(
  circles_control[["size"]],
  circles_control[["n"]],
  function(size, n) {

    size_n <- 0
    errors <- 0

    while(size_n < n | errors < 100) {
      current_x0 <- runif(1, x_min + size, x_max - size)
      current_y0 <- runif(1, y_min + size, y_max - size)

      circles_overlap <- circles %>%
        mutate(overlap = sqrt((current_x0 - x0)^2 + (current_y0 - y0)^2) < (r + size) + 2) %>%
        filter(overlap) %>%
        nrow() > 0

      if (!circles_overlap) {
        temp_circles <- bind_rows(
          circles,
          tibble(
            x0 = current_x0,
            y0 = current_y0,
            r = size,
            color = sample(genuary_colours(), 1)
          )
        )

        circles <<- temp_circles

        size_n <- size_n + 1
      } else {
        errors <- errors + 1
      }
    }
  }
)

ggplot() +
  geom_circle(data = circles, aes(x0 = x0, y0 = y0, r = r, fill = color), color = NA) +
  scale_fill_identity() +
  coord_fixed() +
  theme_void()
