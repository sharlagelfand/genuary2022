add_partial_noise <- function(clean) {
  noise <- clean %>%
    image_noise()

  # Reduce noise opacity
  noise_data <- image_data(noise, "rgba")
  noise_data[4, , ] <- as.raw(round(as.integer(noise_data[4, , ]) * 0.5))
  noise <- image_read(noise_data)

  image_mosaic(c(clean, noise))
}
