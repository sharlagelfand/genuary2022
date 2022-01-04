library(dplyr)
library(tidyr)
library(ambient)
library(ggforce)
library(magick)

source(here::here("colours.R"))
source(here::here("noise.R"))

seed <- 1234

n <- 175

grid <- crossing(
  x = seq(0, 20, length.out = n * 2),
  y = seq(0, 10, length.out = n)
) %>%
  mutate(noise = gen_perlin(x, y, seed = seed))

curl <- curl_noise(gen_checkerboard, seed = seed, x = grid$x, y = grid$y)

grid <- grid %>%
  mutate(angle = atan2(curl$y, curl$x) - atan2(grid$y, grid$x))

p <- ggplot(grid) +
  geom_regon(aes(x0 = x, y0 = y, r = noise / 2, sides = 5, fill = noise, angle = angle), alpha = 0.75) +
  theme_void() +
  coord_fixed() +
  theme(
    plot.background = element_rect(fill = light, color = light)
  )

p_pink <- p +
  scale_fill_gradientn(colors = c(red, darkpink, lightpink, light), guide = "none")

ggsave(here::here("04", "pinks.png"), p_pink, width = 10, height = 5, dpi = 300)

pinks <- image_read(here::here("04", "pinks.png"))

pinks %>%
  add_partial_noise(0.75) %>%
  image_write(here::here("04", "pinks_noisy.png"))

p_greens <- p +
  scale_fill_gradientn(colors = c(darkblue, lightblue, lightgreen, light), guide = "none")

p_greens$data <- p_greens$data %>%
  mutate(
    y = y * -1,
    x = x * -1
  )

ggsave(here::here("04", "bluegreens.png"), p_greens, width = 10, height = 5, dpi = 300)

bluegreens <- image_read(here::here("04", "bluegreens.png"))

bluegreens %>%
  add_partial_noise(0.75) %>%
  image_write(here::here("04", "bluegreens_noisy.png"))
