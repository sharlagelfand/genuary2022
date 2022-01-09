# Day 9 - Architecture
# Inspiration: https://metropolismag.com/viewpoints/women-feminism-american-architecture/

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(prismatic)

source(here::here("colours.R"))

generate_day_9 <- function(seed) {

  set.seed(seed)

  x_min <- y_min <- 0
  x_max <- 500
  y_max <- 300

  # Noise ----

  noise_n <- 100000

  noise <- tibble(
    x = runif(noise_n, 0, x_max),
    y = runif(noise_n, 0, y_max),
    size = rnorm(noise_n, mean = 0.2, sd = 0.001),
    alpha = rnorm(noise_n, mean = 0.8, sd = 0.5)
  )

  # Rectangles ----

  ## Place rectangles -----

  rectangles_placement <- tribble(
    ~xmin, ~xmid, ~xmax, ~ymin, ~ymid, ~ymax, ~width, ~height
  )

  total_n <- 100
  n <- 0
  errors <- 0

  rectangle_sizes <- tibble(
    width = abs(rnorm(total_n * 100, 0.1, 0.3) * 100),
    height = abs(rnorm(total_n * 100, 0.1, 0.3) * 100),
  ) %>%
    filter(abs(width / height - 1) > 0.1) %>%
    mutate(size = width * height) %>%
    filter(size > 10) %>%
    arrange(desc(size))

  for (i in 1:nrow(rectangle_sizes)) {
    current_width <- rectangle_sizes[i, ][["width"]]
    current_height <- rectangle_sizes[i, ][["height"]]
    current_xmin <- runif(1, x_min, x_max - current_width)
    current_xmid <- current_xmin + current_width / 2
    current_xmax <- current_xmin + current_width
    current_ymin <- runif(1, y_min, y_max - current_height)
    current_ymid <- current_ymin + current_height / 2
    current_ymax <- current_ymin + current_height


    rectangles_overlap <- rectangles_placement %>%
      mutate(overlap = sqrt((xmid - current_xmid)^2 + (ymid - current_ymid)^2) < 0.5 * (pmax(width, height) + pmax(current_width, current_height)) + 10) %>%
      filter(overlap) %>%
      nrow() > 0

    if (!rectangles_overlap) {
      rectangles_placement <- bind_rows(
        rectangles_placement,
        tibble(
          xmin = current_xmin,
          xmid = current_xmid,
          xmax = current_xmax,
          ymin = current_ymin,
          ymid = current_ymid,
          ymax = current_ymax,
          width = current_width,
          height = current_height
        )
      )

      n <- n + 1
    } else {
      errors <- errors + 1
    }

    if (n >= total_n | errors > 10000) {
      return(rectangles_placement)
    }
  }

  ## Generate their points ----

  generate_points_for_rectangle <- function(xmin, xmax, ymin, ymax) {
    # Do left edge, top edge, right edge, bottom edge

    line_sd <- 0.001
    length_seq_by <- 1

    # Left edge
    left_edge <- tibble(
      y = seq(ymin, ymax, by = length_seq_by),
      section = "left"
    ) %>%
      mutate(x = xmin) %>%
      rowwise() %>%
      mutate(x = x * rnorm(1, mean = 1, sd = line_sd)) %>%
      ungroup()

    # Top edge
    top_edge <- tibble(
      x = seq(xmin, xmax, by = length_seq_by),
      section = "top"
    ) %>%
      mutate(y = ymax) %>%
      rowwise() %>%
      mutate(y = y * rnorm(1, mean = 1, sd = line_sd)) %>%
      ungroup()

    # Right edge
    right_edge <- tibble(
      y = seq(ymax, ymin, by = -length_seq_by),
      section = "right"
    ) %>%
      mutate(x = xmax) %>%
      rowwise() %>%
      mutate(x = x * rnorm(1, mean = 1, sd = line_sd)) %>%
      ungroup()

    bottom_edge <- tibble(
      x = seq(xmax, xmin, by = -length_seq_by),
      section = "bottom"
    ) %>%
      mutate(y = ymin) %>%
      rowwise() %>%
      mutate(y = y * rnorm(1, mean = 1, sd = line_sd)) %>%
      ungroup()

    bind_rows(
      left_edge,
      top_edge,
      right_edge,
      bottom_edge,
      left_edge[1, ]
    )
  }

  rectangles_placement <- rectangles_placement %>%
    mutate(
      size = width * height,
      id = row_number()
    )

  rectangles <- rectangles_placement %>%
    mutate(points = pmap(
      list(xmin, xmax, ymin, ymax),
      generate_points_for_rectangle
    )) %>%
    select(id, points) %>%
    unnest(points)

  # Checks ----

  checks_placement <- rectangles_placement %>%
    filter(size > 300) %>%
    sample_n(sample(6:10, 1))

  red_checks <- sample(c(1:nrow(checks_placement)), sample(c(1:nrow(checks_placement), 1)))

  checks_placement <- checks_placement %>%
    mutate(colour = ifelse(row_number() %in% red_checks, red, dark))

  generate_checkerboard_in_rectangle <- function(xmin, xmax, ymin, ymax, width, height) {
    check_size <- 999999

    while (check_size / 2 > width | check_size / 2 > height) {
      check_size <- sample(5:20, 1)
    }

    x <- tibble(xmin = seq(floor(xmin), ceiling(xmax), by = check_size)) %>%
      mutate(id_x = row_number())
    y <- tibble(ymin = seq(floor(ymin), ceiling(ymax), by = check_size)) %>%
      mutate(id_y = row_number())

    df <- expand.grid(xmin = x[["xmin"]], ymin = y[["ymin"]]) %>%
      as_tibble() %>%
      left_join(x, by = "xmin") %>%
      left_join(y, by = "ymin") %>%
      mutate(
        xmax = xmin + check_size, ymax = ymin + check_size
      )

    black_first <- sample(c(TRUE, FALSE), 1)

    if (black_first) {
      df <- df %>%
        mutate(
          colour = ifelse((id_x %% 2) == (id_y %% 2), dark, "white")
        )
    } else {
      df <- df %>%
        mutate(
          colour = ifelse((id_x %% 2) != (id_y %% 2), dark, "white")
        )
    }

    df <- df %>%
      filter(colour == dark) %>%
      mutate(data = pmap(list(xmin, xmax, ymin, ymax), function(xmin, xmax, ymin, ymax) {
        expand.grid(x = seq(xmin + 0.1, xmax - 0.1, by = 0.5), y = seq(ymin + 0.1, ymax - 0.1, by = 0.5)) %>%
          as_tibble()
      })) %>%
      select(data) %>%
      unnest(cols = data)

    df
  }

  checks <- checks_placement %>%
    mutate(checkerboard = pmap(list(xmin, xmax, ymin, ymax, width, height), generate_checkerboard_in_rectangle)) %>%
    unnest(checkerboard) %>%
    filter(x < xmax, x > xmin, y < ymax, y > ymin)

  checks <- checks %>%
    mutate(colour = map_chr(
      colour,
      function(x) {
        change <- sample(c("lightness", "saturation"), 1)
        higher <- sample(c(TRUE, FALSE), 1)

        if (change == "lightness") {
          if (higher) {
            x %>%
              clr_lighten(shift = rnorm(1, 0.05, sd = 0.05), space = "HSL") %>%
              as.character()
          } else {
            x %>%
              clr_darken(shift = rnorm(1, 0.05, sd = 0.05), space = "HSL") %>%
              as.character()
          }
        } else if (change == "saturation") {
          if (higher) {
            x %>%
              clr_saturate(shift = rnorm(1, 0.05, sd = 0.05)) %>%
              as.character()
          } else {
            x %>%
              clr_desaturate(shift = rnorm(1, 0.05, sd = 0.05)) %>%
              as.character()
          }
        }
      }
    ))

  # Grids ----

  grid_placement <- rectangles_placement %>%
    anti_join(checks_placement, by = "id") %>%
    filter(size > 500) %>%
    sample_n(sample(6:10, 1))

  generate_grid_in_rectangle <- function(xmin, xmax, ymin, ymax, width, height) {
    grid_size <- 999999

    while (grid_size / 2 > width | grid_size / 2 > height) {
      grid_size <- sample(2:20, 1)
    }

    horizontal_lines <- expand.grid(x = xmin, y = seq(ymin, ymax, by = grid_size)) %>%
      mutate(
        xend = xmax,
        yend = y
      ) %>%
      # filter(!y %in% c(ymin, ymax))
      filter(!(y - ymin < 1) & !(ymax - y) < 1)

    vertical_lines <- expand.grid(y = ymin, x = seq(xmin, xmax, by = grid_size)) %>%
      mutate(
        yend = ymax,
        xend = x
      ) %>%
      # filter(!x %in% c(xmin, xmax))
      filter(!(x - xmin < 1) & !(xmax - x) < 1)

    if (grid_size > 10) {
      size <- runif(1, 1, 2)
    } else {
      size <- runif(1, 0.1, 0.5)
    }

    colour <- sample(c(dark, red), 1)

    bind_rows(
      horizontal_lines,
      vertical_lines
    ) %>%
      mutate(
        size = size,
        colour = colour
      )
  }

  grids <- grid_placement %>%
    arrange(xmin) %>%
    mutate(grid = pmap(list(xmin, xmax, ymin, ymax, width, height), generate_grid_in_rectangle)) %>%
    select(id, grid) %>%
    unnest(grid)

  # Plot ----

  p <- ggplot() +
    geom_point(
      data = noise, aes(x = x, y = y, size = size, alpha = alpha),
      shape = 15, color = dark
    ) +
    geom_polygon(data = rectangles, aes(x = x, y = y, group = id), fill = "white") +
    geom_point(data = checks, aes(x = x, y = y, colour = colour), size = 0.2, shape = 15) +
    geom_segment(data = grids, aes(x = x, y = y, xend = xend, yend = yend, group = id, size = size, colour = colour)) +
    geom_path(data = rectangles, aes(x = x, y = y, group = id), color = dark) +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_fill_identity() +
    scale_colour_identity() +
    theme_void() +
    coord_fixed() +
    theme(plot.background = element_rect(fill = "white", color = "white"))

  ggsave(here::here("09", glue::glue("day_9_clean_{seed}.png")), p, width = 10, height = 6, dpi = 300)

}

generate_day_9(20220109)
generate_day_9(19910624)
generate_day_9(19910624)
