pack_circles <- function(circles_control, x_min, x_max, y_min, y_max, n_errors = 100) {

  circles <- tribble(
    ~x0, ~y0, ~r, ~color
  )

  walk2(
    circles_control[["size"]],
    circles_control[["n"]],
    function(size, n) {

      size_n <- 0
      errors <- 0

      while(size_n < n & errors < n_errors) {
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

  circles
}
