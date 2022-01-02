# Thank you Will Chase https://twitter.com/W_R_Chase/status/1359251015183261697

perlin_circle <- function(cx, cy, n = 200, noise_max = 1, octaves = 1, r_min = 1, r_max = 1.1, x_mult = 1, y_mult = 1) {
  dplyr::tibble(
    angle = seq(0, 2 * pi, length.out = n),
    xoff = scales::rescale(cos(angle), from = c(-1, 2), to = c(0, noise_max)),
    yoff = scales::rescale(sin(angle), from = c(-1, 1), to = c(0, noise_max)),
    r = scales::rescale(ambient::fracture(ambient::gen_simplex, ambient::fbm, x = xoff, y = yoff, octaves = 2),
      from = c(-0.5, 0.5), to = c(r_min, r_max)
    ),
    x = x_mult * r * cos(angle) + cx,
    y = y_mult * r * sin(angle) + cy
  )
}

generate_blobby_flower_data <- function(cx = 1, cy = 1, r = 1, petal_color = "pink", pistil_color = "yellow", size = 1) {
  dplyr::tribble(
    ~cx, ~cy, ~n, ~noise_max, ~octaves, ~r_min, ~r_max, ~color, ~section,
    cx, cy, 2000, 1, 1, r, r * 2.5, petal_color, "petal",
    cx, cy, 2000, 1, 2, r / 3, r / 3 * 1.1, pistil_color, "pistil",
  ) %>%
    dplyr::mutate(circle = purrr::pmap(list(cx, cy, n, noise_max, octaves, r_min, r_max), perlin_circle, x_mult = size, y_mult = size)) %>%
    dplyr::select(section, color, circle) %>%
    tidyr::unnest(circle)
}

generate_spiky_flower_data <- function(cx = 1, cy = 1, r = 1, petal_color = "pink", pistil_color = "yellow", size = 1) {
  dplyr::tribble(
    ~cx, ~cy, ~n, ~noise_max, ~octaves, ~r_min, ~r_max, ~color, ~section,
    cx, cy, 2000, 10, 1, r, r * 2.5, petal_color, "petal",
    cx, cy, 2000, 10, 2, r / 3, r / 3 * 1.1, pistil_color, "pistil",
  ) %>%
    dplyr::mutate(circle = purrr::pmap(list(cx, cy, n, noise_max, octaves, r_min, r_max), perlin_circle, x_mult = size, y_mult = size)) %>%
    dplyr::select(section, color, circle) %>%
    tidyr::unnest(circle)
}
