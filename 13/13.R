# Day 13 - Packing

library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)

source(here::here("colours.R"))
source(here::here("perlin_circle.R"))

set.seed(1234)

colours <- genuary_colours()[-which(names(genuary_colours()) == "darkgreen")]

flowers_control <- tribble(
  ~size, ~n,
  20, 3,
  10, 5,
  5, 10,
  3, 30,
  2, 20,
  1, 20
)

x_min <- y_min <- 0
x_max <- 500
y_max <- 300

flowers <- tribble(
  ~x0, ~y0, ~x, ~y, ~r, ~color, ~edge, ~join, ~section, ~id, ~distance_to_current
)

for (flower_row in 1:nrow(flowers_control)) {
  size <- flowers_control[flower_row, ][["size"]]
  n <- flowers_control[flower_row, ][["n"]]

  size_n <- 0
  errors <- 0

  while (size_n < n & errors < 100) {
    current_x0 <- runif(1, x_min + size, x_max - size)
    current_y0 <- runif(1, y_min + size, y_max - size)

    colors <- sample(colours, 2)

    flower <- generate_spiky_flower_data(cx = current_x0, cy = current_y0, size = size, petal_color = colors[1], pistil_color = colors[2]) %>%
      mutate(
        join = TRUE,
        distance_from_center = sqrt((current_x0 - x)^2 + (current_y0 - y)^2),
        edge = distance_from_center > 2 * size
      )

    if (any(flower[["y"]] > y_max)) {
      diff <- max(flower[["y"]]) - y_max

      flower <- flower %>%
        mutate(y = y - ceiling(diff))
    }

    if (any(flower[["y"]] < y_min)) {
      diff <- y_min - min(flower[["y"]])

      flower <- flower %>%
        mutate(y = y + ceiling(diff))
    }

    if (any(flower[["x"]] > x_max)) {
      diff <- max(flower[["x"]]) - x_max

      flower <- flower %>%
        mutate(x = x - ceiling(diff))
    }

    if (any(flower[["x"]] < x_min)) {
      diff <- x_min - min(flower[["x"]])

      flower <- flower %>%
        mutate(x = x + ceiling(diff))
    }

    flower_size <- pmax(max(flower[["x"]]) - min(flower[["x"]]), max(flower[["y"]]) - min(flower[["y"]])) / 2

    # Check if too close based on radii

    flowers <- flowers %>%
      mutate(distance_to_current = sqrt((x0 - current_x0)^2 + (y0 - current_y0)^2))

    too_close <- flowers %>%
      filter(distance_to_current < (r + flower_size) * 0.5) %>%
      nrow() > 0

    # cat("too close:", too_close, "\n")

    if (!too_close) {

      # If not, check if overlap
      # Checking overlap is inefficient so don't want to do this unless we need to!

      flowers_to_check <- flowers %>%
        filter(edge, section == "petal")

      where_to_check <- flowers_to_check %>%
        distinct(id, x0, y0) %>%
        mutate(
          new_flower_right = current_x0 > x0,
          new_flower_top = current_y0 > y0
        ) %>%
        select(id, new_flower_right, new_flower_top)

      flowers_to_check <- flowers_to_check %>%
        left_join(where_to_check, by = "id") %>%
        group_nest(id) %>%
        mutate(new_data = map(data, function(x) {
          if (all(x[["new_flower_right"]])) {
            x <- x %>%
              filter(x > x0)
          }

          if (!all(x[["new_flower_right"]])) {
            x <- x %>%
              filter(x < x0)
          }

          if (all(x[["new_flower_top"]])) {
            x <- x %>%
              filter(y > y0)
          }

          if (!all(x[["new_flower_top"]])) {
            x <- x %>%
              filter(y < y0)
          }

          x
        })) %>%
        select(id, new_data) %>%
        unnest(new_data)

      # Check in order of closeness, to not have to join all if not needed

      if (nrow(flowers) == 0) {
        flowers_overlap <- FALSE
      } else {
        flowers_to_check <- flowers_to_check %>%
          arrange(distance_to_current) %>%
          split(.$id)

        for (i in 1:length(flowers_to_check)) {
          flowers_overlap <- flowers_to_check[[i]] %>%
            left_join(flower %>%
              filter(edge, section == "petal"), by = "join", suffix = c("_existing", "_new")) %>%
            mutate(overlap = sqrt((x_existing - x_new)^2 + (y_existing - y_new)^2) < 10) %>%
            filter(overlap) %>%
            nrow() > 0

          if (flowers_overlap) {
            break
          }
        }
      }

      # cat("overlap:", flowers_overlap, "\n")

      if (!flowers_overlap) {
        temp_flowers <- bind_rows(
          flowers,
          flower %>%
            select(x, y, section, color, join, edge) %>%
            mutate(
              x0 = current_x0,
              y0 = current_y0,
              r = flower_size,
              id = glue::glue("{size}_{flower_size}")
            )
        )

        flowers <- temp_flowers

        size_n <- size_n + 1

        cat("flowers:", nrow(flowers) / 4000, "\n")
      }
    } else {
      errors <- errors + 1

      cat("errors:", errors, "on size", size, "\n")
    }
  }
}

p <- ggplot() +
  geom_polygon(data = flowers %>%
    filter(section == "petal"), aes(x = x, y = y, fill = color, group = id), color = NA) +
  geom_polygon(data = flowers %>%
    filter(section == "pistil"), aes(x = x, y = y, fill = color, group = id), color = NA) +
  scale_fill_identity() +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = light, color = light))

ggsave(here::here("13", "day_13_clean.png"), p, width = 15, height = 9, dpi = 300)
