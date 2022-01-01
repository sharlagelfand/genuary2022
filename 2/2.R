# Dithering code via https://www.sumsar.net/blog/2019/01/image-dithering-in-r/ - thank you!
# Original images by Gunta Stolzl

library(imager)

rep_mat <- function(mat, nrow_out, ncol_out) {
  mat[rep(seq_len(nrow(mat)), length.out = nrow_out),
      rep(seq_len(ncol(mat)), length.out = ncol_out)]
}

recursive_bayer_pattern <- function(n) {
  if (n <= 0) {
    return(matrix(0))
  }
  m <- recursive_bayer_pattern(n - 1)
  rbind(
    cbind(4 * m + 0, 4 * m + 2),
    cbind(4 * m + 3, 4 * m + 1)
  )
}

normalized_bayer_pattern <- function(n) {
  pattern <- recursive_bayer_pattern(n)
  (1 + pattern) / (1 + length(pattern))
}

dither_image <- function(image_path, bayer_n = 3) {
  image <- load.image(image_path)
  image_grey <- grayscale(rm.alpha(image))

  bayer_matrix <- rep_mat(
    normalized_bayer_pattern(bayer_n),
    nrow(image_grey), ncol(image_grey)
  )

  bayer_cimg <- as.cimg(bayer_matrix)
  image_bayer <- image_grey > bayer_cimg

  image_bayer_color <- image

  for (rgb_i in 1:3) {
    color_channel <- image_bayer_color[, , 1, rgb_i, drop = FALSE]
    image_bayer_color[, , 1, rgb_i] <- color_channel > bayer_cimg
  }

  plot(image_bayer_color, axes = FALSE)
}

png(here::here("2", "gunta_stolzl_1_dither.png"), width = 5, height = 6, units = "in", res = 300)
dither_image((here::here("2", "gunta_stolzl_1.jpg")))
dev.off()

png(here::here("2", "gunta_stolzl_2_dither.png"), width = 10, height = 4, units = "in", res = 300)
dither_image((here::here("2", "gunta_stolzl_2.jpg")))
dev.off()
