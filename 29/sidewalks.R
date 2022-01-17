generate_sidewalks <- function(blocks) {

  # Side lengths are block sizes - sidewalk_size * 2

  # Double sidewalk size so that it can be accounted for at top corners too

  sidewalk_size <- runif(1, 2, 3)

  size_left <- attr(blocks, "size_left") - sidewalk_size * 2
  size_right <- attr(blocks, "size_right") - sidewalk_size * 2
  sidewalk_line_granularity <- min(size_left, size_right) * runif(1, 0.1, 0.2)

  # Get bottom point of each plaza
  # Start sidewalk at bottom_x, bottom_y + sidewalk_size
  # Just keep the "roof" section, use as points

  sidewalks <- blocks %>%
    map(function(block) {
      block <- block %>%
        filter(
          stringr::str_ends(id, "roof"),
          point == "bottom"
        ) %>%
        mutate(y = y + sidewalk_size * 1.25)

      sidewalk <- generate_building(block[["x"]], block[["y"]], size_right, size_left, 0) %>%
        filter(section == "roof")

      sidewalk <- bind_rows(
        sidewalk %>%
          mutate(order = 0),
        sidewalk %>%
          filter(point == "left") %>%
          mutate(order = 1)
      ) %>%
        arrange(-order)

      # Sidewalk lines
      sidewalk_lines <- sidewalk %>%
        summarize(
          id = id,
          x = x,
          y = y,
          point = point,
          point_next = lead(point),
          x_next = lead(x),
          y_next = lead(y),
          .groups = "drop"
        ) %>%
        filter(!is.na(x_next)) %>%
        split(.$point) %>%
        map_dfr(function(x) {
          x_min <- min(x$x, x$x_next)
          x_max <- max(x$x, x$x_next)

          x_seq <- seq(x_min, x_max, sidewalk_line_granularity)

          line_eqs <- attr(sidewalk, "roof_line_formulas") %>%
            split(.$line)

          placement_line_equation <- switch(x$point,
            "bottom" = line_eqs[["bottom_right"]], # bottom to right
            "left" = line_eqs[["bottom_left"]], # left to bottom
            "top" = line_eqs[["top_left"]], # top to left
            "right" = line_eqs[["top_right"]] # right to top
          )

          y_seq <- placement_line_equation[["m"]] * x_seq + placement_line_equation[["b"]]

          # The slope of the sidewalk line is perpendicular to the sidewalk it's on
          m <- -placement_line_equation[["m"]]
          b <- y_seq - m * x_seq

          sidewalk_lines <- tibble(x = x_seq, y = y_seq, m = m, b = b)

          # The end of the line is where it intersects the corresponding line of the actual block
          line_eqs <- attr(block, "roof_line_formulas") %>%
            split(.$line)

          intersection_line_equation <- switch(x$point,
            "bottom" = line_eqs[["bottom_right"]], # bottom to right
            "left" = line_eqs[["bottom_left"]], # left to bottom
            "top" = line_eqs[["top_left"]], # top to left
            "right" = line_eqs[["top_right"]] # right to top
          )

          sidewalk_line_ends <- sidewalk_lines %>%
            mutate(row = row_number()) %>%
            split(.$row) %>%
            map_dfr(function(sidewalk_line) {
              int_a <- sidewalk_line[["m"]]
              int_b <- intersection_line_equation[["m"]]
              int_c <- sidewalk_line[["b"]]
              int_d <- intersection_line_equation[["b"]]

              intersection_x <- (int_d - int_c) / (int_a - int_b)
              intersection_y <- int_a * (int_d - int_c) / (int_a - int_b) + int_c

              tibble(xend = intersection_x, yend = intersection_y)
            })

          df <- sidewalk_lines %>%
            bind_cols(sidewalk_line_ends)

          df
        })

      return(
        list(
          sidewalk_main = sidewalk,
          sidewalk_lines = sidewalk_lines
        )
      )
    })
}
