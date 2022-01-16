library(ggplot2)
library(dplyr)
library(stringr)
library(prismatic)

source(here::here("colours.R"))

size_right <- 6
size_left <- 5
height <- 4

isometric_points_from_bottom <- function(x = 0, y = 0, size_right = 2, size_left = 4) {
  slope_right <- 1 / sqrt(3)
  slope_left <- -slope_right

  bottom_point <- tibble(x = x, y = y) %>%
    mutate(point = "bottom")

  right_x <- bottom_point[["x"]] + size_right
  right_y <- slope_right * right_x + bottom_point[["y"]]
  right_point <- tibble(x = right_x, y = right_y) %>%
    mutate(point = "right")

  right_line_for_intersecting <- tibble(m = slope_left, b = right_y * 2)

  left_x <- bottom_point[["x"]] - size_left
  left_y <- slope_left * left_x + bottom_point[["y"]]
  left_point <- tibble(x = left_x, y = left_y) %>%
    mutate(point = "left")

  left_line_for_intersecting <- tibble(m = slope_right, b = left_y * 2)

  int_a <- right_line_for_intersecting[["m"]]
  int_b <- left_line_for_intersecting[["m"]]
  int_c <- right_line_for_intersecting[["b"]]
  int_d <- left_line_for_intersecting[["b"]]

  top_point_x <- (int_d - int_c) / (int_a - int_b)
  top_point_y <- int_a * (int_d - int_c) / (int_a - int_b) + int_c

  top_point <- tibble(x = top_point_x, y = top_point_y) %>%
    mutate(point = "top")

  bind_rows(
    bottom_point,
    right_point,
    top_point,
    left_point
  )
}

generate_building <- function(bottom_center, size_right, size_left, height, colour = NULL) {

  building_id <- ids::random_id()

  if (is.null(colour)) {
    colour <- sample(genuary_colours(), 1)
  }

  left_colour <- as.character(clr_darken(colour, 0.15))
  right_colour <- as.character(clr_darken(colour, 0.2))

  # Roof ----

  roof <- isometric_points_from_bottom(
    bottom_center[["x"]],
    bottom_center[["y"]],
    size_right, size_left
  ) %>%
    mutate(
      section = "roof",
      id = glue::glue("{building_id}_roof"),
      colour = colour
    )

  # Building ----

  building_bottom_point <- roof %>%
    filter(point == "bottom")

  center_point <- tibble(
    x = building_bottom_point[["x"]],
    y = building_bottom_point[["y"]] - height
  )

  building_points <- isometric_points_from_bottom(
    center_point[["x"]],
    center_point[["y"]],
    size_right,
    size_left
  )

  # Left side ----

  left_side <- building_points %>%
    filter(point %in% c("bottom", "left")) %>%
    mutate(order = ifelse(point == "left", 1, 4)) %>%
    bind_rows(
      roof %>%
        filter(point %in% c("bottom", "left")) %>%
        mutate(order = ifelse(point == "left", 2, 3))
    ) %>%
    arrange(order) %>%
    mutate(
      section = "left_side",
      id = glue::glue("{building_id}_left"),
      colour = left_colour
    )

  # Right side ----

  right_side <- building_points %>%
    filter(point %in% c("bottom", "right")) %>%
    mutate(order = ifelse(point == "right", 1, 4)) %>%
    bind_rows(
      roof %>%
        filter(point %in% c("bottom", "right")) %>%
        mutate(order = ifelse(point == "right", 2, 3))
    ) %>%
    arrange(order) %>%
    mutate(
      section = "right_side",
      id = glue::glue("{building_id}_right"),
      colour = right_colour
    )

  bind_rows(
    roof,
    left_side,
    right_side
  )
}

# Start to build pieces! -----

# Plaza

plaza_1 <- generate_building(tibble(x = 0, y = 0), 15, 15, 0.5, colour = light)

plaza_2 <- generate_building(tibble(x = 0, y = 0), 15, 15, 0.5, colour = light) %>%
  mutate(x = x - 19, y = y - 11)

plaza_3 <- generate_building(tibble(x = 0, y = 0), 15, 15, 0.5, colour = light) %>%
  mutate(x = x + 19, y = y - 11)

plaza_4 <- generate_building(tibble(x = 0, y = 0), 15, 15, 0.5, colour = light) %>%
  mutate(x = x - 19, y = y + 11)

plaza_5 <- generate_building(tibble(x = 0, y = 0), 15, 15, 0.5, colour = light) %>%
  mutate(x = x + 19, y = y + 11)

plaza_6 <- generate_building(tibble(x = 0, y = 0), 15, 15, 0.5, colour = light) %>%
  mutate(y = y + 22)

plazas <- bind_rows(
  plaza_1,
  plaza_2,
  plaza_3,
  plaza_4,
  plaza_5,
  plaza_6
)

# Buildings

building_1 <- generate_building(tibble(x = 0, y = 0), 2, 2, 6) %>%
  mutate(x = x - 6, y = y + 6 + 4)

building_2 <- generate_building(tibble(x = 0, y = 0), 3.5, 3, 3) %>%
  mutate(x = x - 2, y = y + 4.75)

building_3 <- generate_building(tibble(x = 0, y = 0), 2, 2, 8) %>%
  mutate(x = x, y = y + 8 + 13)

buildings <- bind_rows(
  building_1,
  building_2,
  building_3
)

ggplot() +
  geom_rect(aes(xmin = -20, xmax = 20, ymin = -5, ymax = 30), colour = dark, fill = dark) +
  geom_polygon(data = plazas, aes(x = x, y = y, group = id, fill = colour), color = "black") +
  geom_polygon(data = buildings, aes(x = x, y = y, group = id, fill = colour), color = "black") +
  coord_fixed(xlim = c(-15, 15), ylim = c(-3, 25)) +
  scale_fill_identity()

