lat_acf <- function(x, ci = 0.95, drop.lag.0 = FALSE, ...) {
  s <- TSA::acf(x, drop.lag.0 = drop.lag.0, plot = FALSE, ...)

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

tacf <- function(..., lag.max = 10) {
  s <- ARMAacf(..., lag.max = lag.max)

  lattice::xyplot(s ~ as.numeric(names(s)), type = "h",
                  ylab = "ACF", xlab = "Lag",
                  panel = function(x, y, ...) {
                    panel.abline(h = 0)
                    panel.xyplot(x, y, ...)
                  })
}
