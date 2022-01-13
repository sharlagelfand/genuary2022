add_partial_noise <- function(clean, noise_percent = 0.5, noisetype = "gaussian") {
  noise <- clean %>%
    image_noise(noisetype)

  # Reduce noise opacity
  noise_data <- image_data(noise, "rgba")
  noise_data[4, , ] <- as.raw(round(as.integer(noise_data[4, , ]) * noise_percent))
  noise <- image_read(noise_data)

  image_mosaic(c(clean, noise))
}
