library(ggplot2)
library(geomtextpath)
library(dplyr)
library(purrr)
library(magick)
library(tidyr)
library(ambient)

source(here::here("colours.R"))

set.seed(1234)

checkerboard_grid <- crossing(
  x = seq(1, 50, length.out = 400),
  y = seq(1, 30, length.out = 200)
)

checkerboard_grid$colour <- gen_checkerboard(checkerboard_grid$x, checkerboard_grid$y, frequency = 0.075)

grids <- map(
  1:4,
  function(id) {
    granularity <- runif(1, 0.1, 1)

    checkerboard_grid %>%
      rowwise() %>%
      mutate(across(c(x, y), ~ .x * rnorm(1, 1, 0.003)),
        alpha = runif(1, 0.7, 1)
      ) %>%
      ungroup() %>%
      mutate(colour = case_when(
        colour == 1 ~ lightblue,
        colour == 0 ~ lightgreen))
  }
)

walk(
  1:length(grids),
  function(id) {
    size <- runif(1, 1, 3)
    p <- ggplot() +
      geom_text(
        data = grids[[id]] %>%
          sample_frac(0.85), aes(x, y, alpha = alpha, colour = factor(colour)),
        label = ">", fontface = "bold", size = size
      ) +
      geom_text(
        data = grids[[id]] %>%
          sample_frac(0.01), aes(x, y, alpha = alpha),
        label = ">", fontface = "bold", size = size, colour = light
      ) +
      coord_fixed(
        xlim = c(1, 50),
        ylim = c(1, 30)
      ) +
      # scale_colour_manual(values = c(lightblue, lightgreen)) +
      scale_colour_identity() +
      # scale_size_identity() +
      scale_alpha_identity() +
      theme_void() +
      theme(legend.position = "none")

    ggsave(here::here("19", "temp", glue::glue("{id}.png")), p, width = 10, height = 6, dpi = 300)
  }
)

starter <- image_read(here::here("19", "temp", "1.png"))
starter <- image_blank(image_info(starter)[["width"]], image_info(starter)[["height"]])

walk(
  1:length(grids),
  function(id) {
    img <- image_read(here::here("19", "temp", glue::glue("{id}.png")))

    starter <<- starter %>%
      image_composite(img, operator = "Multiply")
  }
)

starter %>%
  image_background("white") %>%
  image_write(here::here("19", "day_19.png"))
