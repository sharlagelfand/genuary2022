library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(ggforce)
library(magick)
library(purrr)
library(prismatic)

set.seed(12345)

source(here::here("colours.R"))

full_grid <- crossing(x = 1:3, y = 1:3)

save_width <- 1.5 * max(full_grid[["x"]])
save_height <- 1.5 * max(full_grid[["y"]])
dpi <- 500

p <- ggplot() +
  theme_void() +
  # theme_minimal() +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_size_identity() +
  scale_alpha_identity()

fray_swatch <- function(data, keep = FALSE) {
  x_range <- range(data[["x"]])
  x_fray_amount <- (x_range[[2]] - x_range[[1]]) * 0.03
  x_fray_range <- c(x_range[[1]] + x_fray_amount, x_range[[2]] - x_fray_amount)

  y_range <- range(data[["y"]])
  y_fray_amount <- (y_range[[2]] - y_range[[1]]) * 0.03
  y_fray_range <- c(y_range[[1]] + y_fray_amount, y_range[[2]] - y_fray_amount)

  data <- data %>%
    mutate(
      fray_x = x < x_fray_range[[1]] | x > x_fray_range[[2]],
      fray_y = y < y_fray_range[[1]] | y > y_fray_range[[2]]
    )

  no_fray <- data %>%
    filter(!(fray_x | fray_y))

  if (!keep) {
    return(no_fray)
  }

  fray_x <- data %>%
    filter(fray_x &
      !fray_y) %>%
    mutate(y_rounded = round(y, 2))

  fray_x_remove_y <- fray_x %>%
    distinct(y_rounded) %>%
    sample_frac(0.5)

  fray_x <- fray_x %>%
    anti_join(fray_x_remove_y, by = "y_rounded")

  fray_y <- data %>%
    filter(fray_y & !fray_x) %>%
    mutate(x_rounded = round(x, 2))

  fray_y_remove_x <- fray_y %>%
    distinct(x_rounded) %>%
    sample_frac(0.5)

  fray_y <- fray_y %>%
    anti_join(fray_y_remove_x, by = "x_rounded")

  bind_rows(
    no_fray,
    fray_x,
    fray_y
  )
}

# Background and setups ----

backgrounds <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~color,
  0, 2, 0, 2, darkpink,
  0, 2, 2, 4, lightpink,
  2, 4, 0, 2, lightpink,
  2, 4, 2, 4, darkpink
) %>%
  mutate(background_id = row_number())

limits <- backgrounds %>%
  summarize(
    xmin = min(xmin),
    xmax = max(xmax),
    ymin = min(ymin),
    ymax = max(ymax)
  )

plot_limits <- list(
  x = c(
    limits[["xmin"]] + 6,
    limits[["xmax"]] * max(full_grid[["x"]]) + 2
  ),
  y = c(
    limits[["ymin"]] + 6,
    limits[["ymax"]] * max(full_grid[["y"]]) + 2
  )
)

p <- p +
  coord_fixed(
    # expand = FALSE,
    xlim = plot_limits[["x"]],
    ylim = plot_limits[["y"]]
  )

background_files <- map_chr(
  1:3,
  # 1:1,
  function(id) {
    granularity <- runif(1, 0.01, 0.03)
    alpha <- runif(1, 0.2, 0.4)
    size <- runif(1, 0.6, 1.1)

    background_points <- map2_dfr(
      full_grid[["x"]], full_grid[["y"]],
      function(grid_x, grid_y) {
        backgrounds %>%
          split(.$background_id) %>%
          map_dfr(
            function(backgrounds) {
              df <- crossing(
                x = seq(backgrounds[["xmin"]], backgrounds[["xmax"]], by = granularity),
                y = seq(backgrounds[["ymin"]], backgrounds[["ymax"]], by = granularity),
                alpha = alpha,
                size = size
              )

              n <- nrow(df)

              df_options <- tibble(
                s = sample(c("saturate", "desaturate", "none"), n, replace = TRUE),
                l = sample(c("lighten", "darken", "none"), n, replace = TRUE),
                saturate_amount = runif(n, 0, 0.125),
                brighten_amount = runif(n, 0, 0.125),
                colour = backgrounds[["color"]],
                x_shift = rnorm(n, 1, 0.001),
                y_shift = rnorm(n, 1, 0.001),
                alpha_shift = rnorm(n, 1, 0.05)
              ) %>%
                mutate(
                  colour = case_when(
                    s == "saturate" ~ clr_saturate(colour, shift = saturate_amount),
                    s == "desaturate" ~ clr_desaturate(colour, shift = saturate_amount),
                    TRUE ~ colour
                  ),
                  colour = case_when(
                    l == "lighten" ~ clr_lighten(colour, shift = brighten_amount),
                    l == "darken" ~ clr_darken(colour, shift = brighten_amount),
                    TRUE ~ colour
                  )
                ) %>%
                mutate(id = row_number())

              df %>%
                mutate(id = row_number()) %>%
                left_join(df_options, by = "id") %>%
                mutate(
                  x = x * x_shift,
                  y = y * y_shift,
                  alpha = alpha * alpha_shift,
                  x = x + 4 * grid_x,
                  y = y + 4 * grid_y
                )
            }
          )
      }
    ) %>%
      filter(
        x >= plot_limits[["x"]][[1]],
        x <= plot_limits[["x"]][[2]],
        y >= plot_limits[["y"]][[1]],
        y <= plot_limits[["y"]][[2]]
      )

    # Fray the swatch a bit
    background_points <- background_points %>%
      fray_swatch(keep = TRUE)

    p <- p +
      geom_text(
        data = background_points %>%
          sample_frac(0.97),
        aes(
          x = x,
          y = y,
          color = colour,
          alpha = alpha
        ),
        label = ">",
        fontface = "bold",
        size = size
      )

    # p <- p %>%
    #   add_dust()

    file <- tempfile(fileext = ".png")

    ggsave(file, p, width = save_width, height = save_height, dpi = dpi, bg = "transparent")

    file
  }
)

background_img <- image_read(background_files[[1]])

walk(background_files[-1], function(x) {
  img <- image_read(x)

  background_img <<- background_img %>%
    image_composite(img, operator = "Multiply")
})

# Circles ----

distance <- function(x2, x1, y2, y1) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

circle <- tribble(
  ~x0, ~y0, ~r, ~color,
  2, 2, 0.75, red
)

circle_files <- map(
  1:5,
  # 1:1,
  function(id) {
    granularity <- runif(1, 0.012, 0.042)
    alpha <- runif(1, 0.27, 0.57)
    size <- runif(1, 0.5, 1)

    circle_points <- map2_dfr(
      full_grid[["x"]], full_grid[["y"]],
      function(grid_x, grid_y) {
        df <- crossing(
          x = seq(circle[["x0"]] - circle[["r"]], circle[["x0"]] + circle[["r"]], by = granularity),
          y = seq(circle[["y0"]] - circle[["r"]], circle[["y0"]] + circle[["r"]], by = granularity)
        ) %>%
          mutate(d = distance(
            x2 = circle[["x0"]],
            x = x,
            y2 = circle[["y0"]],
            y = y
          )) %>%
          filter(d <= 0.99 * circle[["r"]])

        n <- nrow(df)

        df_options <- tibble(
          s = sample(c("saturate", "desaturate", "none"), n, replace = TRUE),
          l = sample(c("lighten", "darken", "none"), n, replace = TRUE),
          saturate_amount = runif(n, 0, 0.125),
          brighten_amount = runif(n, 0, 0.125),
          colour = red,
          x_shift = rnorm(n, 1, 0.001),
          y_shift = rnorm(n, 1, 0.001),
          alpha_shift = rnorm(n, 1, 0.05)
        ) %>%
          mutate(
            colour = case_when(
              s == "saturate" ~ clr_saturate(colour, shift = saturate_amount),
              s == "desaturate" ~ clr_desaturate(colour, shift = saturate_amount),
              TRUE ~ colour
            ),
            colour = case_when(
              l == "lighten" ~ clr_lighten(colour, shift = brighten_amount),
              l == "darken" ~ clr_darken(colour, shift = brighten_amount),
              TRUE ~ colour
            )
          ) %>%
          mutate(id = row_number())

        df %>%
          mutate(id = row_number()) %>%
          left_join(df_options, by = "id") %>%
          mutate(
            x = x * x_shift,
            y = y * y_shift,
            alpha = alpha * alpha_shift,
            x = x + 4 * grid_x,
            y = y + 4 * grid_y
          )
      }
    ) %>%
      filter(
        x >= plot_limits[["x"]][[1]],
        x <= plot_limits[["x"]][[2]],
        y >= plot_limits[["y"]][[1]],
        y <= plot_limits[["y"]][[2]]
      )

    # Fray the swatch a bit
    circle_points <- circle_points %>%
      fray_swatch()

    p_circle <- p +
      geom_text(
        data = circle_points %>%
          sample_frac(0.91),
        aes(
          x = x,
          y = y,
          alpha = alpha,
          color = colour
        ),
        label = ">",
        fontface = "bold",
        size = size
      )
    #
    #     p_circle <- p_circle %>%
    #       add_dust(frac = 0.0001, alpha_min = 0.01, alpha_max = 0.05)

    file <- tempfile(fileext = ".png")

    ggsave(file, p_circle, width = save_width, height = save_height, dpi = dpi)

    p_blank <- p +
      geom_text(
        data = circle_points,
        aes(
          x = x,
          y = y,
          alpha = alpha
        ),
        label = ">",
        fontface = "bold",
        size = size,
        alpha = alpha,
        color = "white"
      )

    blank_file <- tempfile(fileext = ".png")

    if (id == 1) {
      ggsave(blank_file, p_blank, width = save_width, height = save_height, dpi = dpi, bg = "transparent")
    }

    list(
      file = file,
      blank_file = blank_file
    )
  }
)

circle_files <- circle_files %>%
  transpose()

circle_img <- image_read(circle_files[["file"]][[1]])

walk(circle_files[["file"]][-1], function(x) {
  img <- image_read(x)

  circle_img <<- circle_img %>%
    image_composite(img, operator = "Multiply")
})

blank_circle_img <- image_read(circle_files[["blank_file"]][[1]])

# Squares -----

squares <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~color,
  0, 1, 3, 4, darkpink,
  3, 4, 3, 4, lightpink,
  0, 1, 0, 1, lightpink,
  3, 4, 0, 1, darkpink
) %>%
  mutate(square_id = row_number())

square_files <- map(
  1:5,
  # 1:1,
  function(id) {
    granularity <- runif(1, 0.012, 0.042)
    alpha <- runif(1, 0.27, 0.57)
    size <- runif(1, 0.6, 1.1)

    square_points <- map2_dfr(
      full_grid[["x"]], full_grid[["y"]],
      function(grid_x, grid_y) {
        squares %>%
          split(.$square_id) %>%
          map_dfr(
            function(squares) {
              x <- seq(squares[["xmin"]], squares[["xmax"]], by = granularity)
              # x <- c(min(x) - granularity, x, max(x) + granularity)
              y <- seq(squares[["ymin"]], squares[["ymax"]], by = granularity)
              # y <- c(min(y) - granularity, y, max(y) + granularity)

              df <- crossing(
                x = x,
                y = y,
                original_colour = squares[["color"]]
              )

              n <- nrow(df)

              df_options <- tibble(
                s = sample(c("saturate", "desaturate", "none"), n, replace = TRUE),
                l = sample(c("lighten", "darken", "none"), n, replace = TRUE),
                saturate_amount = runif(n, 0, 0.125),
                brighten_amount = runif(n, 0, 0.125),
                colour = squares[["color"]],
                x_shift = rnorm(n, 1, 0.001),
                y_shift = rnorm(n, 1, 0.001),
                alpha_shift = rnorm(n, 1, 0.05)
              ) %>%
                mutate(
                  colour = case_when(
                    s == "saturate" ~ clr_saturate(colour, shift = saturate_amount),
                    s == "desaturate" ~ clr_desaturate(colour, shift = saturate_amount),
                    TRUE ~ colour
                  ),
                  colour = case_when(
                    l == "lighten" ~ clr_lighten(colour, shift = brighten_amount),
                    l == "darken" ~ clr_darken(colour, shift = brighten_amount),
                    TRUE ~ colour
                  )
                ) %>%
                mutate(id = row_number())

              df %>%
                mutate(id = row_number()) %>%
                left_join(df_options, by = "id") %>%
                mutate(
                  x = x * x_shift,
                  y = y * y_shift,
                  alpha = alpha * alpha_shift,
                  x = x + 4 * grid_x,
                  y = y + 4 * grid_y
                )
            }
          )
      }
    ) %>%
      filter(
        x >= plot_limits[["x"]][[1]],
        x <= plot_limits[["x"]][[2]],
        y >= plot_limits[["y"]][[1]],
        y <= plot_limits[["y"]][[2]]
      )

    p_square <- p +
      geom_text(
        data = square_points %>%
          filter(original_colour == darkpink) %>%
          sample_frac(0.945),
        aes(
          x = x,
          y = y,
          color = colour,
          alpha = alpha
        ),
        label = ">",
        fontface = "bold",
        size = size
      ) +
      geom_text(
        data = square_points %>%
          filter(original_colour == lightpink) %>%
          sample_frac(0.945),
        aes(
          x = x,
          y = y,
          color = colour,
          alpha = alpha
        ),
        label = ">",
        fontface = "bold",
        size = size
      )

    # p_square <- p_square %>%
    #   add_dust()

    file <- tempfile(fileext = ".png")

    ggsave(file, p_square, width = save_width, height = save_height, dpi = dpi, bg = "transparent")

    p_blank <- p +
      geom_point(
        data = square_points,
        aes(
          x = x,
          y = y
        ),
        colour = "white",
        size = 0.1,
        shape = 15,
        alpha = 0.2
      )

    blank_file <- tempfile(fileext = ".png")

    if (id == 1) {
      ggsave(blank_file, p_blank, width = save_width, height = save_height, dpi = dpi, bg = "transparent")
    }

    list(
      file = file,
      blank_file = blank_file
    )
  }
)

square_files <- square_files %>%
  transpose()

square_img <- image_read(square_files[["file"]][[1]])

walk(square_files[["file"]][-1], function(x) {
  img <- image_read(x)

  square_img <<- square_img %>%
    image_composite(img, operator = "Multiply")
})

blank_square_img <- image_read(square_files[["blank_file"]][[1]])

# Triangles -----

triangles <- tribble(
  ~x, ~y, ~group,
  0, 1, 1,
  0, 2, 1,
  0.5, 2, 1,
  0, 2, 2,
  0, 3, 2,
  0.5, 2, 2,
  1, 0, 3,
  1, 1, 3,
  2, 0, 3,
  2, 0, 4,
  3, 0, 4,
  3, 1, 4,
  1, 3, 5,
  1, 4, 5,
  2, 4, 5,
  2, 4, 6,
  3, 4, 6,
  3, 3, 6,
  3.5, 2, 7,
  4, 2, 7,
  4, 3, 7,
  3.5, 2, 8,
  4, 2, 8,
  4, 1, 8
)

triangles_color <- tribble(
  ~group, ~colour, ~m, ~b, ~d,
  1, orange, 1, 2, "g",
  2, yellow, 3, -2, "l",
  3, orange, 2, -1, "l",
  4, yellow, -2, 1, "l",
  5, yellow, 2, 1, "g",
  6, orange, 6, -1, "g",
  7, orange, -5, 2, "l",
  8, yellow, 9, -2, "g"
)

triangles <- triangles %>%
  left_join(triangles_color, by = "group")

triangle_files <- map(
  1:6,
  # 1:1,
  function(id) {
    granularity <- runif(1, 0.01, 0.04)
    alpha <- runif(1, 0.4, 0.6)
    size <- runif(1, 0.6, 0.8)

    triangle_points <- map2_dfr(
      full_grid[["x"]], full_grid[["y"]],
      function(grid_x, grid_y) {
        df <- triangles %>%
          split(.$group) %>%
          map_dfr(function(df) {
            grid <- crossing(
              x = seq(min(df[["x"]]), max(df[["x"]]), granularity),
              y = seq(min(df[["y"]]), max(df[["y"]]), granularity)
            ) %>%
              mutate(colour = unique(df[["colour"]]))

            if (any(df[["d"]] == "g")) {
              grid %>%
                filter(y >= unique(df[["m"]]) + unique(df[["b"]]) * x)
            } else {
              grid %>%
                filter(y <= unique(df[["m"]]) + unique(df[["b"]]) * x)
            }
          }, .id = "group")

        n <- nrow(df)


        df_options <- tibble(
          s = sample(c("saturate", "desaturate", "none"), n, replace = TRUE),
          l = sample(c("lighten", "darken", "none"), n, replace = TRUE),
          saturate_amount = runif(n, 0, 0.125),
          brighten_amount = runif(n, 0, 0.125),
          x_shift = rnorm(n, 1, 0.001),
          y_shift = rnorm(n, 1, 0.001),
          alpha_shift = rnorm(n, 1, 0.05)
        ) %>%
          mutate(id = row_number())
        df %>%
          mutate(id = row_number()) %>%
          left_join(df_options, by = "id") %>%
          mutate(
            colour = case_when(
              s == "saturate" ~ clr_saturate(colour, shift = saturate_amount),
              s == "desaturate" ~ clr_desaturate(colour, shift = saturate_amount),
              TRUE ~ colour
            ),
            colour = case_when(
              l == "lighten" ~ clr_lighten(colour, shift = brighten_amount),
              l == "darken" ~ clr_darken(colour, shift = brighten_amount),
              TRUE ~ colour
            ),
            x = x * x_shift,
            y = y * y_shift,
            alpha = alpha * alpha_shift,
            x = x + 4 * grid_x,
            y = y + 4 * grid_y
          )
      }
    ) %>%
      filter(
        x >= plot_limits[["x"]][[1]],
        x <= plot_limits[["x"]][[2]],
        y >= plot_limits[["y"]][[1]],
        y <= plot_limits[["y"]][[2]]
      )

    # Fray the swatch a bit
    triangle_points <- triangle_points %>%
      fray_swatch()

    p_triangle <- p +
      geom_text(
        data = triangle_points %>%
          sample_frac(0.88),
        aes(
          x = x,
          y = y,
          color = colour,
          alpha = alpha
        ),
        label = ">",
        fontface = "bold",
        size = runif(1, 0.5, 0.75)
      )

    # p_triangle <- p_triangle %>%
    #   add_dust()

    file <- tempfile(fileext = ".png")

    ggsave(file, p_triangle, width = save_width, height = save_height, dpi = dpi, bg = "transparent")

    add_blank_triangles <- function(p) {
      p +
        geom_text(
          data = triangle_points %>%
            rowwise() %>%
            mutate(across(c(x, y), ~ .x * rnorm(1, 1, 0.001))) %>%
            ungroup(),
          aes(
            x = x,
            y = y
          ),
          label = ">",
          fontface = "bold",
          size = 0.3,
          colour = "white",
          alpha = 0.4
        )
    }

    p_blank <- p %>%
      add_blank_triangles() %>%
      add_blank_triangles() %>%
      add_blank_triangles() %>%
      add_blank_triangles() %>%
      add_blank_triangles()

    blank_file <- tempfile(fileext = ".png")

    ggsave(blank_file, p_blank, width = save_width, height = save_height, dpi = dpi, bg = "transparent")

    list(
      file = file,
      blank_file = blank_file
    )
  }
)

triangle_files <- triangle_files %>%
  transpose()

triangle_img <- image_read(triangle_files[["file"]][[1]])

walk(triangle_files[["file"]][-1], function(x) {
  img <- image_read(x)

  triangle_img <<- triangle_img %>%
    image_composite(img, operator = "Multiply")
})

blank_triangle_img <- image_read(triangle_files[["blank_file"]][[1]])

walk(triangle_files[["blank_file"]][-1], function(x) {
  img <- image_read(x)

  blank_triangle_img <<- blank_triangle_img %>%
    image_composite(img, "Overlay")
})

# Tiles ----

tiles_data <- crossing(
  x = seq(plot_limits$x[[1]] * 0.75, plot_limits$x[[2]] * 1.25, 0.25),
  y = seq(plot_limits$y[[1]] * 0.75, plot_limits$y[[2]] * 1.25, 0.25)
)

tiles_data$fill <- gen_checkerboard(tiles_data$x, tiles_data$y, fruquency = 0.5)
tiles_data <- tiles_data %>%
  mutate(fill = ifelse(fill == 1, light, "white"),
         colour = light)

p_tiles <- p +
  geom_tile(
    data = tiles_data, aes(x = x, y = y, fill = fill, colour = colour),
    size = 0.3,
    width = 0.5,
    height = 0.5
  )

p_tiles

file <- tempfile(fileext = ".png")

ggsave(file, p_tiles, width = save_width * 1.25, height = save_height * 1.25, dpi = dpi, bg = "white")

tiles_img <- image_read(file)

tiles_img <- tiles_img %>%
  image_crop(geometry = geometry_area(
    width = image_info(background_img)[["width"]],
    height = image_info(background_img)[["height"]],
    x_off = 100,
    y_off = 100
  ))

# Combine -----

tiles_img %>%
  image_composite(background_img) %>%
  image_composite(blank_circle_img) %>%
  image_composite(circle_img) %>%
  image_composite(blank_square_img) %>%
  image_composite(square_img) %>%
  image_composite(blank_triangle_img) %>%
  image_composite(triangle_img) %>%
  image_write(here::here("26", "26_fabric.png"))
