
generate_blocks <- function() {

  # Generate 5 "blocks" - a main one, one to the left, right, front, and back
  # All the same size for now

  block_height <- 0.3

  size_right <- runif(1, 20, 40)
  size_left <- runif(1, 20, 40)

  # First block

  starting_block <- generate_building(1, 1, size_right, size_left, block_height, colour = lightpink)

  # Road size

  road_size <- runif(1, 5, 6)

  # Bottom center of each block is then road size + corresponding "side" away, from each corner
  # Just do distance from the "roof", since the height is negligable

  starting_block_corner <- starting_block %>%
    filter(section == "roof") %>%
    filter(point == "bottom")

  # Parallel blocks ----

  parallel_block_intercept <- starting_block_corner[["y"]] - slope_left * starting_block_corner[["x"]] - block_height

  # Right block

  # The bottom corner point is further away, the whole size_left away
  right_block_x <- starting_block_corner[["x"]] + road_size + size_left
  right_block_y <- slope_left * right_block_x + parallel_block_intercept

  right_block <- generate_building(right_block_x, right_block_y, size_right, size_left, block_height, colour = lightpink)

  # Left block

  # The bottom corner point is the whole size_left away, in the other direction
  left_block_x <- starting_block_corner[["x"]] - road_size - size_left
  left_block_y <- slope_left * left_block_x + parallel_block_intercept

  left_block <- generate_building(left_block_x, left_block_y, size_right, size_left, block_height, colour = lightpink)

  # Perpendicular blocks -----

  perpendicular_block_intercept <- starting_block_corner[["y"]] - slope_right * starting_block_corner[["x"]] - block_height

  # Back block

  # The bottom corner point is the whole size_right away
  back_block_x <- starting_block_corner[["x"]] + road_size + size_right
  back_block_y <- slope_right * back_block_x + perpendicular_block_intercept

  back_block <- generate_building(back_block_x, back_block_y, size_right, size_left, block_height, colour = lightpink)

  # Front block

  # The bottom corner point is the whole size_right away, in the other direction
  front_block_x <- starting_block_corner[["x"]] - road_size - size_right
  front_block_y <- slope_right * front_block_x + perpendicular_block_intercept

  front_block <- generate_building(front_block_x, front_block_y, size_right, size_left, block_height, colour = lightpink)

  blocks <- bind_rows(
    starting_block,
    right_block,
    left_block,
    back_block,
    front_block
  )

  blocks
}
