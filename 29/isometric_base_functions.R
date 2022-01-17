slope_right <- 1 / sqrt(3)
slope_left <- -slope_right

isometric_points_from_bottom <- function(x = 0, y = 0, size_right = 2, size_left = 4) {
  slope_right <- 1 / sqrt(3)
  slope_left <- -slope_right

  bottom_point <- tibble(x = x, y = y) %>%
    mutate(point = "bottom")

  right_x <- bottom_point[["x"]] + size_right
  right_y <- slope_right * right_x + (y - slope_right * x) # b
  right_point <- tibble(x = right_x, y = right_y) %>%
    mutate(point = "right")

  bottom_right_line <- tibble(m = slope_right, b = bottom_point[["y"]] - bottom_point[["x"]] * slope_right) %>%
    mutate(line = "bottom_right")
  top_right_line <- tibble(m = slope_left, b = right_point[["y"]] - right_point[["x"]] * slope_left) %>%
    mutate(line = "top_right")

  left_x <- bottom_point[["x"]] - size_left
  left_y <- slope_left * left_x + (y - slope_left * x) # b
  left_point <- tibble(x = left_x, y = left_y) %>%
    mutate(point = "left")

  bottom_left_line <- tibble(m = slope_left, b = bottom_point[["y"]] - bottom_point[["x"]] * slope_left) %>%
    mutate(line = "bottom_left")
  top_left_line <- tibble(m = slope_right, b = left_point[["y"]] - left_point[["x"]] * slope_right) %>%
    mutate(line = "top_left")

  int_a <- top_right_line[["m"]]
  int_b <- top_left_line[["m"]]
  int_c <- top_right_line[["b"]]
  int_d <- top_left_line[["b"]]

  top_point_x <- (int_d - int_c) / (int_a - int_b)
  top_point_y <- int_a * (int_d - int_c) / (int_a - int_b) + int_c

  top_point <- tibble(x = top_point_x, y = top_point_y) %>%
    mutate(point = "top")

  points <- bind_rows(
    bottom_point,
    right_point,
    top_point,
    left_point
  )

  attr(points, "line_formulas") <- bind_rows(
    bottom_right_line,
    bottom_left_line,
    top_left_line,
    top_right_line
  )

  points
}

generate_building <- function(bottom_center_x, bottom_center_y, size_right, size_left, height, colour = NULL) {
  building_id <- ids::random_id()

  if (is.null(colour)) {
    colour <- sample(genuary_colours(), 1)
  }

  left_colour <- as.character(prismatic::clr_darken(colour, 0.15))
  right_colour <- as.character(prismatic::clr_darken(colour, 0.2))

  # Roof ----

  roof <- isometric_points_from_bottom(
    bottom_center_x,
    bottom_center_y + height,
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

  building <- bind_rows(
    roof,
    left_side,
    right_side
  )

  attr(building, "roof_line_formulas") <- attr(roof, "line_formulas")
  attr(building, "building_line_formulas") <- attr(left_side, "line_formulas") %>%
    filter(line %in% c("bottom_right", "bottom_left"))

  building
}
