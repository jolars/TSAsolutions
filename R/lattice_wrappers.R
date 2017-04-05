lat_acf <- function(x, ci = 0.95, ...) {
  s <- TSA::acf(x, plot = FALSE, ...)

  nser <- ncol(s$lag)

  clim0 <- qnorm((1 + ci) / 2) / sqrt(s$n.used)

  ylim <- range(s$acf[, 1L:nser, 1L:nser], na.rm = TRUE)
  ylim <- range(c(-clim0, clim0, ylim))

  lattice::xyplot(s$acf ~ s$lag, type = "h", xlab = "Lag", ylab = "ACF",
                  ylim = ylim * 1.1,
                  panel = function(x, y, ...) {
                    panel.abline(h = c(-clim0, 0, clim0), lty = c(3, 1, 3))
                    panel.xyplot(x, y, ...)
                  })
}
