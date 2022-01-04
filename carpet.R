library(dplyr)
library(ggplot2)
library(ggforce)
library(purrr)

circles <- tribble(
  ~x0, ~radius,
  0, 2,
  2.5, 2,
  6, 2,
) %>%
  mutate(
    id = row_number()
  )

set.seed(1234)

circles %>%
  split(.$id) %>%
  map(function(x) {
    n_orig <- sample(1:6, 1)
    n <- ifelse(n_orig == 2, n_orig + 1, n_orig)

    if (n > 1) {
      rings_radius <- seq(0.5, x[["radius"]], length.out = n)
    } else {
      rings_radius <- x[["radius"]]
    }

    if (n_orig == 2) {
      rings_radius <- rings_radius[-1]
    }

    size <- sample(1:5, 1)

    circles <- tibble(x0 = x[["x0"]], radius = rings_radius, size = size)

    if (n > 3) {
      circles <- circles %>%
        mutate(size = case_when(
          size > 1 & row_number() %% 2 == 0 ~ 1L,
          size == 1 & row_number() %% 2 == 0 ~ size + 1L,
          TRUE ~ size
        ))
    }

    circles
  }) %>%
  bind_rows() %>%
  mutate(y0 = 0) %>%
  ggplot() +
  geom_circle(aes(x0 = x0, y0 = y0, r = radius, size = size)) +
  coord_fixed() +
  scale_size_identity()
