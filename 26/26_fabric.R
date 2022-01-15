library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(ggforce)
library(magick)
library(purrr)

set.seed(12345)

source(here::here("colours.R"))

full_grid <- crossing(x = 1:2, y = 1:2)

save_width <- 1.5 * max(full_grid[["x"]])
save_height <- 1.5 * max(full_grid[["y"]])
dpi <- 1000

p <- ggplot() +
  theme_void() +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_size_identity() +
  scale_alpha_identity()

# Elements:
# Backgrounds DONE
# Circles DONE
# Squares DONE
# Triangles

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

p <- p +
  coord_fixed(
    expand = FALSE,
    xlim = c(
      limits[["xmin"]] + 2,
      limits[["xmax"]] * max(full_grid[["x"]]) * 2 - 2
    ),
    ylim = c(
      limits[["ymin"]] + 2,
      limits[["ymax"]] * max(full_grid[["y"]]) * 2 - 2
    )
  )

# Dust setup -----

dust_grid <- crossing(
  x = seq(
    limits[["xmin"]] + 2,
    limits[["xmax"]] * max(full_grid[["x"]]) * 2 - 2, 0.01
  ),
  y = seq(
    limits[["ymin"]] + 2,
    limits[["ymax"]] * max(full_grid[["y"]]) * 2 - 2, 0.01
  )
) %>%
  sample_frac(0.01)

add_dust <- function(p, dust = dust_grid, frac = 0.001, alpha_min = 0.1, alpha_max = 0.3) {
  dust_grid <- dust_grid %>%
    sample_frac(frac) %>%
    rowwise() %>%
    mutate(across(c(x, y), ~ .x * rnorm(1, 1, 0.1)),
      alpha = runif(1, alpha_min, alpha_max),
      size = runif(1, 0.5, 1)
    ) %>%
    ungroup()

  p +
    geom_text(
      data = dust_grid,
      aes(
        x = x,
        y = y,
        size = size,
        alpha = alpha
      ),
      label = ">",
      fontface = "bold",
      colour = "white"
    )
}

# Finish background -----

background_files <- map_chr(
  1:3,
  function(id) {
    granularity <- runif(1, 0.02, 0.06)
    alpha <- runif(1, 0.3, 0.6)
    size <- runif(1, 0.5, 1)

    background_points <- map2_dfr(
      full_grid[["x"]], full_grid[["y"]],
      function(grid_x, grid_y) {
        backgrounds %>%
          split(.$background_id) %>%
          map_dfr(
            function(backgrounds) {
              crossing(
                x = seq(backgrounds[["xmin"]], backgrounds[["xmax"]], by = granularity),
                y = seq(backgrounds[["ymin"]], backgrounds[["ymax"]], by = granularity)
              ) %>%
                mutate(
                  x = x + 4 * grid_x,
                  y = y + 4 * grid_y,
                  colour = backgrounds[["color"]]
                )
            }
          )
      }
    )

    p <- p +
      geom_text(
        data = background_points,
        aes(
          x = x,
          y = y,
          color = colour
        ),
        label = ">",
        fontface = "bold",
        size = size,
        alpha = alpha
      )

    # p <- p %>%
    #   add_dust()

    file <- tempfile(fileext = ".png")

    ggsave(file, p, width = save_width, height = save_height, dpi = dpi, bg = "white")

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
  1:4,
  function(id) {
    granularity <- runif(1, 0.02, 0.06)
    alpha <- runif(1, 0.3, 0.6)
    size <- runif(1, 0.5, 1)

    circle_points <- map2_dfr(
      full_grid[["x"]], full_grid[["y"]],
      function(grid_x, grid_y) {
        crossing(
          x = seq(circle[["x0"]] - circle[["r"]], circle[["x0"]] + circle[["r"]], by = granularity),
          y = seq(circle[["y0"]] - circle[["r"]], circle[["y0"]] + circle[["r"]], by = granularity)
        ) %>%
          mutate(d = distance(
            x2 = circle[["x0"]],
            x = x,
            y2 = circle[["y0"]],
            y = y
          )) %>%
          filter(d <= 0.99 * circle[["r"]]) %>%
          mutate(
            x = x + 4 * grid_x,
            y = y + 4 * grid_y
          )
      }
    )

    p_circle <- p +
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
        color = red
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

    #
    #     p_blank <- p +
    #       geom_circle(
    #         data = map2_dfr(full_grid[["x"]], full_grid[["y"]], function(grid_x, grid_y) {
    #           circle %>%
    #             mutate(
    #               x0 = x0 + 4 * grid_x,
    #               y0 = y0 + 4 * grid_y
    #             )
    #         }),
    #         aes(x0 = x0, y0 = y0, r = r * 0.87),
    #         colour = "white",
    #         fill = "white"
    #       )

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
  1:4,
  function(id) {
    granularity <- runif(1, 0.04, 0.08)
    alpha <- runif(1, 0.3, 0.6)
    size <- runif(1, 0.5, 1)

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
                y = y
              ) %>%
                mutate(
                  x = x + 4 * grid_x,
                  y = y + 4 * grid_y,
                  colour = squares[["color"]]
                )

              df

              # if (grid_x == min(full_grid[["x"]])) {
              #   df <- df %>%
              #     filter(x != min(x))
              # } else if (grid_x == max(full_grid[["x"]])) {
              #   df <- df %>%
              #     filter(x != max(x))
              # }

              # if (grid_y == min(full_grid[["y"]])) {
              #   df <- df %>%
              #     filter(y != min(y))
              # } else if (grid_y == max(full_grid[["y"]])) {
              #   df <- df %>%
              #     filter(y != max(y))
              # }

              df
            }
          )
      }
    )

    p_square <- p +
      geom_text(
        data = square_points %>%
          filter(colour == darkpink),
        aes(
          x = x,
          y = y,
          color = colour
        ),
        label = ">",
        fontface = "bold",
        size = size,
        alpha = alpha
      ) +
      geom_text(
        data = square_points %>%
          filter(colour == lightpink),
        aes(
          x = x,
          y = y,
          color = colour
        ),
        label = ">",
        fontface = "bold",
        size = size,
        alpha = alpha
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
  1:5,
  function(id) {
    granularity <- runif(1, 0.02, 0.07)
    alpha <- runif(1, 0.2, 0.5)
    size <- runif(1, 0.5, 0.75)

    triangle_points <- map2_dfr(
      full_grid[["x"]], full_grid[["y"]],
      function(grid_x, grid_y) {
        triangles %>%
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
          }, .id = "group") %>%
          mutate(
            x = x + 4 * grid_x,
            y = y + 4 * grid_y
          )
      }
    )

    p_triangle <- p +
      geom_text(
        data = triangle_points,
        aes(
          x = x,
          y = y,
          color = colour
        ),
        label = ">",
        fontface = "bold",
        size = runif(1, 0.5, 0.75),
        alpha = alpha
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

# Combine -----

background_img %>%
  image_composite(blank_circle_img) %>%
  image_composite(circle_img) %>%
  image_composite(blank_square_img) %>%
  image_composite(square_img) %>%
  image_composite(blank_triangle_img) %>%
  image_composite(triangle_img) %>%
  # image_composite(dust_img) %>%
  image_write(here::here("26", "26_fabric.png"))
