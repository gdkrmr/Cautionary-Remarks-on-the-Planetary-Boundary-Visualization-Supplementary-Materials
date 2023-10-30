

#' check if the point x is inside the rectangle defined by the points a, b, c,
#' d, basic trigonometry! Formulas adapted from here
#' https://math.stackexchange.com/questions/190111/how-to-check-if-a-point-is-inside-a-rectangle
is_inside <- function(x, a, b, d) {
  axab <- sum((x - a) * (b - a))
  baba <- sum((b - a) ^ 2)
  xada <- sum((x - a) * (d - a))
  # we remove the last condition because we don't care about the length of the
  # rectangle because we are only interested in the beam and don't care where it
  # terminates.
  ## dada <- sum((d - a) ^ 2)
  0 < axab  && axab < baba && 0 < xada # && xada < dada
}


#' get the pixels along a line starting from `origin` at `angle` in degrees with
#' a distance lower than dist
get_linear_idx_old <- function(nx, ny, origin = c(nx / 2, ny / 2),
                                  angle = 30, dist = 0.5) {

  idxs <- expand.grid(x = seq_len(nx), y = seq_len(ny))
  idxs <- as.matrix(idxs)
  n_idxs <- nrow(idxs)

  angle <- angle / 360 * 2 * pi

  ## calculate the coordinates of the beam starting from AB as a base and
  ## radiating out into the direction of D. We don't need C.
  oa <- dist * c(cos(0.5 * pi + angle), sin(0.5 * pi + angle))
  a <- origin + oa
  b <- origin - oa
  d <- a + c(cos(angle), sin(angle))

  ## check who is inside the beam
  in_beam <- vector("logical", n_idxs)
  for (i in seq_len(n_idxs)) {
    in_beam[i] <- is_inside(idxs[i, ], a, b, d)
  }

  idx_in_beam <- idxs[in_beam, ]

  return(idx_in_beam)
}

#' This is a vectorized version of the functions above. Much much faster!
get_linear_idx <- function(nx, ny, origin = c(nx / 2, ny / 2),
                           angle = 30, dist = 0.5) {
  idxs <- expand.grid(x = seq_len(nx), y = seq_len(ny))
  idxs <- as.matrix(idxs)
  n_idxs <- nrow(idxs)

  angle <- angle / 360 * 2 * pi

  ## calculate the coordinates of the beam starting from AB as a base and
  ## radiating out into the direction of D. We don't need C.
  oa <- dist * c(cos(0.5 * pi + angle), sin(0.5 * pi + angle))
  a <- origin + oa
  b <- origin - oa
  d <- a + c(cos(angle), sin(angle))

  ## create matrices for vectorized operations
  a <- matrix(a, nrow = n_idxs, ncol = 2, byrow = TRUE)
  b <- matrix(b, nrow = n_idxs, ncol = 2, byrow = TRUE)
  d <- matrix(d, nrow = n_idxs, ncol = 2, byrow = TRUE)

  axab <- rowSums((idxs - a) * (b - a))
  baba <- rowSums((b - a) ^ 2)
  xada <- rowSums((idxs - a) * (d - a))

  # we remove the last condition because we don't care about the length of the
  # rectangle because we are only interested in the beam and don't care where it
  # terminates.
  ## dada <- sum((d - a) ^ 2)
  in_beam <- 0 < axab  & axab < baba & 0 < xada # & xada < dada

  idx_in_beam <- idxs[in_beam, ]

  # sort the pixels by their distance from the center of the beam
  o <- matrix(origin, nrow = nrow(idx_in_beam), ncol = 2, byrow = TRUE)
  dist_from_center <- sqrt(rowSums((idx_in_beam - o) ^ 2))
  idx_in_beam <- idx_in_beam[order(dist_from_center), ]
  sorted_dist_from_center <- sort(dist_from_center)

  return(list(idx = idx_in_beam, dist = sorted_dist_from_center))
}


## Test the functions above

## for (alpha in 0:360) {
##   n <- 30
##   x <- matrix(FALSE, n, n)
##   i <- get_linear_idx(n, n, angle = alpha)
##   x[i] <- TRUE
##   image(x)
## }

## for (alpha in 0:360) {
##   n <- 30
##   x <- matrix(FALSE, n, n)
##   i <- get_linear_idx_old(n, n, angle = alpha)
##   x[i] <- TRUE
##   image(x)
## }


# code for creating a gradient inside of a ggplot bar. From
# https://stackoverflow.com/questions/48210231/creating-a-vertical-color-gradient-for-a-geom-bar-plot
prep_gradient <- function(x, y, spacing = max(y) / 100) {
  stopifnot(length(x) == length(y))
  df <- data.frame(x = x, y = y)
  new_df <- data.frame(x = NULL, y = NULL, z = NULL)
  for (r in seq_len(nrow(df))){
    n <- floor(df[r, "y"] / spacing)
    for (s in c(1:n)){
      tmp <- data.frame(x = df[r, "x"], y = spacing, z = s * spacing)
      new_df <- rbind(new_df, tmp)
    }
    tmp <- data.frame(x = df[r, "x"], y = df[r, "y"] %% spacing, z = df[r, "y"])
    new_df <- rbind(new_df, tmp)
  }
  return(new_df)
}

#' the diagnostics plot shows the figure with a black line at idx.
diagnostic_plot <- function(fig, idx, fill_color = "black") {
  fig2 <- as.raster(fig)
  fig2[idx] <- fill_color
  plot.new()
  rasterImage(fig2, 0, 0, 1, 1, xpd = NA)
}

#' just hide some parameters from ksmooth
smoother <- function(x, smooth) {
  ksmooth(seq_along(x), x, "box", smooth)$y
}

#' get pixels at idx from fig. fig must be a 3d array as read by readPNG with
#' [width, height, channels]. Smooth is the width of a running average smoother.
get_pixels <- function(fig, idx, smooth) {
  ## There must be a more elegant way of doing this
  fig_r <- fig[, , 1][idx]
  fig_g <- fig[, , 2][idx]
  fig_b <- fig[, , 3][idx]
  if (smooth > 1) {
    fig_r <- smoother(fig_r, smooth)
    fig_g <- smoother(fig_g, smooth)
    fig_b <- smoother(fig_b, smooth)
  }
  pixels <- colorspace::sRGB(cbind(fig_r, fig_g, fig_b))
  return(pixels)
}

#' extract a colorbar in hex values from fix starting at origin at angle. The
#' length is cutoff and the bar is smoothed by running an average mean of width
#' smooth. Optionally a diagnostics plot can be produced showing fig and the
#' extracted pixels in black.
bar_from_fig <- function(fig, angle, origin, cutoff = NULL,
                         smooth = 0, plot_diagnostic = FALSE) {
  idx_list <- get_linear_idx(dim(fig)[1], dim(fig)[2], origin, angle)
  idx <- idx_list$idx
  dist <- idx_list$dist
  if (!is.null(cutoff)) {
    idx <- idx[1:cutoff, ]
    dist <- dist[1:cutoff]
  }
  if (plot_diagnostic) {
    diagnostic_plot(fig, idx)
  }
  px_rgb <- get_pixels(fig, idx, smooth)
  px_hex <- hex(px_rgb)
  return(list(rgb = px_rgb, dist = dist, hex = px_hex))
}

#' along a color gradient, take the differences in perception. Colors should be hex strings
coldiff <- function(x, metric = "CIEDE2000") {
  xlab <- as(x, "LAB")
  n <- nrow(xlab@coords)
  res <- numeric(n - 1)
  for (i in 1:(n - 1)) {
    res[i] <- ColorNameR::colordiff(xlab@coords[i, ], xlab@coords[i + 1,], metric = metric)
  }
  return(res)
}


#' plot visual versus lenth on paper
plot_coldist <- function(x, main) {
  coldist <- cumsum(coldiff(x$rgb))
  plot(
    x$dist[-1],
    coldist,
    col = x$hex,
    pch = 16,
    cex = 3,
    ## main = main,
    xlab = NA,
    ylab = NA
    ## bty = "n"
  )
  ## text(x$dist[length(x$dist)] - max(x$dist) * 0.2,
  ##      coldist[length(coldist)] - max(coldist) * 0.02,
  ##      main, adj = 1, cex = 1.5)
  text(0,
       coldist[length(coldist)] - max(coldist) * 0.02,
       main, adj = 0, cex = 2)
}

#' scale the value of index x regarding the holocene mean h and the planetary
#' boundary b as it was done in the original paper
scale_value <- function(x, h, b, base = exp(1), adder = 1) {
  return(log((x - h) / (b - h) + adder, base = base))
}

#' take the raw values and scale the fore the plot
scale_data <- function(x, base = exp(1), adder = 1) {
  x$current_scaled <-
    scale_value(x$current, x$holocene, x$safe_boundary, base = base, adder = adder)
  x$holocene_scaled <- 0
  x$safe_boundary_scaled <-
    scale_value(x$safe_boundary, x$holocene, x$safe_boundary, base = base, adder = adder)
  x$high_risk_boundary_scaled <-
    scale_value(x$high_risk_boundary, x$holocene, x$safe_boundary, base = base, adder = adder)
  return(x)
}

#' rotate vector right, ie [1, 2, 3] -> [3, 1, 2]
rotr <- function(x, n = 1) {
  n <- n %% length(x)
  if (n == 0) {
    return(x)
  } else {
    return(c(tail(x, n), head(x, -n)))
  }
}
