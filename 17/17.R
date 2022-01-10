library(ggplot2)
library(dplyr)
library(scrawl)
library(tidyr)
library(purrr)

source(here::here("colours.R"))
source(here::here("perlin_circle.R"))

swirl_with_border <- function(seed) {
  main_colors <- c(red, darkblue, yellow)
  colors <- c(dark, main_colors)

  # Swirls ----

  swirl <- scrawl_build(seed = seed, n_paths = 2, n_steps = 100)

  swirl_colours <- swirl %>%
    distinct(path_id) %>%
    rowwise() %>%
    mutate(colour = sample(colors, 1)) %>%
    ungroup()

  swirl <- swirl %>%
    left_join(swirl_colours, by = "path_id")

  # Keep only one swirl with 80% probability

  one_swirl <- sample(c(TRUE, FALSE), 1, prob = c(0.8, 0.2))

  if (one_swirl) {
    swirl <- swirl %>%
      filter(path_id == 1)
  }

  # Border, based on swirls ----

  swirl_bounds <- list(x = range(swirl[["x"]]), y = range(swirl[["y"]]))

  border_bounds <- list(
    x = c(
      runif(1, min = swirl_bounds$x[1] - 1, swirl_bounds$x[1]),
      runif(1, min = swirl_bounds$x[2], swirl_bounds$x[2] + 1)
    ),
    y = c(
      runif(1, min = swirl_bounds$y[1] - 1, swirl_bounds$y[1]),
      runif(1, min = swirl_bounds$y[2], swirl_bounds$y[2] + 1)
    )
  ) %>%
    map(plyr::round_any, 0.1)

  perlin_border <- crossing(
    x0 = seq(border_bounds$x[1], border_bounds$x[2], plyr::round_any(runif(1, 0.075, 0.125), 0.05)),
    y0 = seq(border_bounds$y[1], border_bounds$y[2], plyr::round_any(runif(1, 0.075, 0.125), 0.05))
  ) %>%
    filter(x0 %in% c(min(x0), max(x0)) | y0 %in% c(min(y0), max(y0))) %>%
    rowwise() %>%
    mutate(across(c(x0, y0), list(jitter = ~ .x * rnorm(1, 1, 0.005)))) %>%
    mutate(
      mult = rnorm(1, 0.01, 0.005),
      mult = max(mult, 0.005),
      colour = sample(colors, 1),
      perlin_circle = map2(x0_jitter, y0_jitter, perlin_circle, x_mult = mult, y_mult = mult)
    ) %>%
    ungroup() %>%
    mutate(id = row_number()) %>%
    unnest(perlin_circle) %>%
    mutate(border_section = case_when(
      x0 == min(x0) ~ "left",
      x0 == max(x0) ~ "right",
      y0 == min(y0) ~ "bottom",
      y0 == max(y0) ~ "top"
    ))

  border_remove_options <- c("top", "right", "bottom", "left", "none")

  # Optionally add a "secondary" border ----

  double_border <- sample(c(TRUE, FALSE), 1)

  if (double_border) {
    secondary_perlin_border <- crossing(
      x0 = seq(
        border_bounds$x[1] * rnorm(1, 1, 0.01),
        border_bounds$x[2] * rnorm(1, 1, 0.01),
        plyr::round_any(runif(1, 0.075, 0.125), 0.05)
      ),
      y0 = seq(
        border_bounds$y[1] * rnorm(1, 1, 0.01),
        border_bounds$y[2] * rnorm(1, 1, 0.01),
        plyr::round_any(runif(1, 0.075, 0.125), 0.05)
      )
    ) %>%
      filter(!(x0 %in% c(min(x0), max(x0))) & !(y0 %in% c(min(y0), max(y0)))) %>%
      filter(x0 %in% c(min(x0), max(x0)) | y0 %in% c(min(y0), max(y0))) %>%
      rowwise() %>%
      mutate(across(c(x0, y0), list(jitter = ~ .x * rnorm(1, 1, 0.005)))) %>%
      mutate(
        mult = rnorm(1, 0.01, 0.005),
        mult = max(mult, 0.005),
        colour = sample(colors, 1),
        perlin_circle = map2(x0_jitter, y0_jitter, perlin_circle, x_mult = mult, y_mult = mult)
      ) %>%
      ungroup() %>%
      mutate(id = row_number()) %>%
      unnest(perlin_circle) %>%
      mutate(border_section = case_when(
        x0 == min(x0) ~ "secondary_left",
        x0 == max(x0) ~ "secondary_right",
        y0 == min(x0) ~ "secondary_bottom",
        y0 == max(y0) ~ "secondary_top"
      ))

    perlin_border <- bind_rows(
      perlin_border,
      secondary_perlin_border
    )

    border_remove_options <- c(border_remove_options, "secondary_left", "secondary_right", "secondary_bottom", "secondary_top")
  }

  # Blobs ----

  n_blobs <- sample(c(1:3), 1)

  blobs <- map(
    1:n_blobs,
    function(x) {
      blob_size <- runif(1, 0.05, 0.15)

      x0 <- runif(
        1,
        (perlin_border %>%
          filter(stringr::str_detect(border_section, "left")) %>%
          pull(x) %>%
          max()) + blob_size,
        (perlin_border %>%
          filter(stringr::str_detect(border_section, "right")) %>%
          pull(x) %>%
          min()) - blob_size
      )
      y0 <- runif(
        1,
        (perlin_border %>%
          filter(stringr::str_detect(border_section, "bottom")) %>%
          pull(y) %>%
          max()) + blob_size,
        (perlin_border %>%
          filter(stringr::str_detect(border_section, "top")) %>%
          pull(y) %>%
          min() - blob_size)
      )

      circle <- perlin_circle(x0, y0, x_mult = blob_size, y_mult = blob_size) %>%
        mutate(
          colour = sample(main_colors, 1),
          id = !!x
        )

      list(
        main = circle,
        dark = circle %>%
          mutate(across(c(x, y), ~ .x + 0.01 * sample(c(0, 1, -1), 1)))
      )
    }
  )

  blobs <- blobs %>%
    transpose() %>%
    map(bind_rows)

  blobs_main <- blobs[["main"]]
  blobs_dark <- blobs[["dark"]]

  # Remove 0 - 3 borders -----

  remove_border <- sample(
    border_remove_options,
    sample(0:3, 1)
  )

  perlin_border <- perlin_border %>%
    filter(!border_section %in% remove_border)

  # Plot ----

  p <- ggplot() +
    geom_path(
      data = swirl, aes(x = x, y = y, group = path_id, colour = colour),
      size = sample(2:10, 1),
      lineend = "round"
    ) +
    geom_polygon(
      data = blobs_dark,
      aes(
        x = x, y = y,
        group = id
      ),
      fill = dark
    ) +
    geom_polygon(
      data = blobs_main,
      aes(
        x = x, y = y,
        group = id, fill = colour
      )
    ) +
    geom_polygon(
      data = perlin_border,
      aes(
        x = x, y = y,
        group = id, fill = colour
      )
    ) +
    coord_fixed() +
    scale_fill_identity() +
    scale_colour_identity() +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", colour = "white"))

  suppressMessages(ggsave(here::here("17", glue::glue("{seed}.png")), dpi = 500))
}

walk(1:10, ~ print(swirl_with_border(.x)))
