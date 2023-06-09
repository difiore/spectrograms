##### my_spectro

my_spectro <- function (wave, f, channel = 1, wl = 512, wn = "hanning", zp = 0,
                        ovlp = 0, noisereduction = NULL, fastdisp = FALSE, complex = FALSE,
                        norm = TRUE, correction = "none", fftw = FALSE, dB = "max0",
                        dBref = NULL, plot = TRUE, flog = FALSE, grid = TRUE, osc = FALSE,
                        scale = TRUE, cont = FALSE, collevels = NULL, palette = spectro.colors,
                        contlevels = NULL, colcont = "black", colbg = "white", colgrid = "black",
                        colaxis = "black", collab = "black", cexlab = 1, cexaxis = 1,
                        tlab = "Time (s)", flab = "Frequency (kHz)", alab = "Amplitude",
                        scalelab = "Amplitude\n(dB)", main = NULL, scalefontlab = 1,
                        scalecexlab = 0.75, axisX = TRUE, axisY = TRUE, tlim = NULL,
                        trel = TRUE, flim = NULL, flimd = NULL, widths = c(6, 1),
                        heights = c(3, 1), oma = rep(0, 4), listen = FALSE, ...)
{
  ptm.start <- proc.time()
  if (wl%%2 == 1)
    stop("'wl' has to be an even number.")
  if (!is.null(dB) && all(dB != c("max0", "A", "B", "C", "D")))
    stop("'dB' has to be one of the following character strings: 'max0', 'A', 'B', 'C' or 'D'")
  if (complex) {
    if (plot) {
      plot <- FALSE
      warning("\n'plot' was turned to 'FALSE'")
    }
    if (norm) {
      norm <- FALSE
      warning("\n'norm' was turned to 'FALSE'")
    }
    if (!is.null(dB)) {
      dB <- NULL
      warning("\n'dB' was turned to 'NULL'")
    }
  }
  input <- inputw(wave = wave, f = f, channel = channel)
  if (!is.null(tlim) && trel && osc) {
    wave <- wave0 <- input$w
  }
  else {
    wave <- input$w
  }
  f <- input$f
  rm(input)
  if (!is.null(tlim))
    wave <- cutw(wave, f = f, from = tlim[1], to = tlim[2])
  if (!is.null(flimd)) {
    mag <- round((f/2000)/(flimd[2] - flimd[1]))
    wl <- wl * mag
    if (ovlp == 0)
      ovlp <- 100
    ovlp <- 100 - round(ovlp/mag)
    flim <- flimd
  }
  n <- nrow(wave)
  step <- seq(1, n + 1 - wl, wl - (ovlp * wl/100))
  z <- stdft(wave = wave, f = f, wl = wl, zp = zp, step = step,
             wn = wn, fftw = fftw, scale = norm, complex = complex,
             correction = correction)
  if (!is.null(noisereduction)) {
    if (noisereduction != 1 & noisereduction != 2)
      stop("If not NULL, 'noisereduction' should be 1 or 2")
    noise <- apply(z, MARGIN = noisereduction, FUN = median)
    z <- abs(z - noise)
  }
  if (!is.null(tlim) && trel) {
    X <- seq(tlim[1], tlim[2], length.out = length(step))
  }
  else {
    X <- seq(0, n/f, length.out = length(step))
  }
  xat <- xlabel <- pretty(X)
  if (is.null(flim)) {
    Y <- seq(0, (f/2) - (f/(wl + zp)), by = f/(wl + zp))/1000
  }
  else {
    fl1 <- flim[1] * nrow(z) * 2000/f
    fl2 <- flim[2] * nrow(z) * 2000/f
    z <- z[(fl1:fl2) + 1, ]
    Y <- seq(flim[1], flim[2], length.out = nrow(z))
  }
  yat <- ylabel <- pretty(Y)
  if (flog) {
    Y <- log(Y + 1)
    yat <- log(yat + 1)
  }
  if (!is.null(dB)) {
    if (is.null(dBref)) {
      z <- 20 * log10(z)
    }
    else {
      z <- 20 * log10(z/dBref)
    }
    if (dB != "max0") {
      if (dB == "A")
        z <- dBweight(Y * 1000, dBref = z)$A
      if (dB == "B")
        z <- dBweight(Y * 1000, dBref = z)$B
      if (dB == "C")
        z <- dBweight(Y * 1000, dBref = z)$C
      if (dB == "D")
        z <- dBweight(Y * 1000, dBref = z)$D
    }
  }
  Z <- t(z)
  if (plot) {
    if (!isTRUE(norm) && isTRUE(scale))
      stop("dB colour scale cannot be plot when 'norm' is FALSE")
    maxz <- round(max(z, na.rm = TRUE))
    if (!is.null(dB)) {
      if (is.null(collevels))
        collevels <- seq(maxz - 30, maxz, by = 1)
      if (is.null(contlevels))
        contlevels <- seq(maxz - 30, maxz, by = 10)
    }
    else {
      if (is.null(collevels))
        collevels <- seq(0, maxz, length = 30)
      if (is.null(contlevels))
        contlevels <- seq(0, maxz, length = 3)
    }
    Zlim <- range(Z, finite = TRUE, na.rm = TRUE)
    if (isTRUE(fastdisp) & length(step) > 300) {
      res <- 300
    }
    else {
      res = length(step)
    }
    if (osc & scale) {
      layout(matrix(c(3, 1, 2, 0), ncol = 2, byrow = TRUE),
             widths = widths, heights = heights)
      par(las = 0, oma = oma, col = "white", col = colaxis,
          col.lab = collab, cex.lab = cexlab, cex.axis = cexaxis,
          bg = colbg)
      par(mar = c(0, 1, 4.5, 3))
      dBscale(collevels = collevels, palette = palette,
              fontlab = scalefontlab, cexlab = scalecexlab,
              collab = collab, textlab = scalelab, colaxis = colaxis)
      par(mar = c(5, 4.1, 0, 0))
      if (!is.null(tlim) && trel) {
        wave <- wave0
        from <- tlim[1]
        to <- tlim[2]
      }
      else {
        from <- FALSE
        to <- FALSE
      }
      soscillo(wave = wave, f = f, bty = "u", from = from,
               to = to, fastdisp = fastdisp, collab = collab,
               colaxis = colaxis, colline = colaxis, ylim = c(-max(abs(wave)),
                                                              max(abs(wave))), tickup = max(abs(wave), na.rm = TRUE),
               tlab = tlab, alab = alab, cexlab = cexlab, cexaxis = cexaxis,
               xaxt = {
                 if (!axisX) {
                   "n"
                 }
               }, ...)
      par(mar = c(0, 4.1, 1, 0), las = 1, cex.lab = cexlab +
            0.2)
      filled.contour.modif2(x = X[seq(1, length(X), length = res)],
                            y = Y, z = Z[seq(1, nrow(Z), length = res), ],
                            levels = collevels, nlevels = 20, plot.title = title(main = main,
                                                                                 xlab = "", ylab = flab), plot.axes = {
                                                                                   if (axisY) {
                                                                                     axis(2, at = yat, labels = ylabel)
                                                                                   }
                                                                                   else {
                                                                                     NULL
                                                                                   }
                                                                                 }, color.palette = palette)
      if (grid)
        abline(h = yat, col = colgrid, lty = "dotted")
      if (cont) {
        contour(X, Y, Z, add = TRUE, levels = contlevels,
                nlevels = 5, col = colcont, ...)
      }
      if (colaxis != colgrid)
        abline(h = 0, col = colaxis)
      else abline(h = 0, col = colgrid)
    }
    if (osc == FALSE & scale) {
      layout(matrix(c(2, 1), ncol = 2, byrow = TRUE), widths = widths)
      par(mar = c(5, 1, 4.5, 3), oma = oma, las = 0, col = colaxis,
          col.axis = colaxis, col.lab = collab, bg = colbg,
          cex.axis = cexaxis, cex.lab = cexlab, ...)
      dBscale(collevels = collevels, palette = palette,
              fontlab = scalefontlab, cexlab = scalecexlab,
              collab = collab, textlab = scalelab, colaxis = colaxis)
      par(mar = c(5, 4.1, 1, 0), las = 1, cex = 1, col = colaxis,
          col.axis = colaxis, col.lab = collab, bg = colbg,
          cex.lab = cexlab + 0.2)
      filled.contour.modif2(x = X[seq(1, length(X), length = res)],
                            y = Y, z = Z[seq(1, nrow(Z), length = res), ],
                            levels = collevels, nlevels = 20, plot.title = title(main = main,
                                                                                 xlab = tlab, ylab = flab), plot.axes = {
                                                                                   if (axisX) {
                                                                                     axis(1, at = xat, labels = xlabel)
                                                                                   }
                                                                                   if (axisY) {
                                                                                     axis(2, at = yat, labels = ylabel)
                                                                                   }
                                                                                 }, color.palette = palette)
      if (grid)
        abline(h = yat, col = colgrid, lty = "dotted")
      if (colaxis != colgrid)
        abline(h = 0, col = colaxis)
      else abline(h = 0, col = colgrid)
      if (cont) {
        contour(X, Y, Z, add = TRUE, levels = contlevels,
                nlevels = 5, col = colcont, ...)
      }
    }
    if (osc & scale == FALSE) {
      layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), heights = heights)
      par(mar = c(5.1, 4.1, 0, 2.1), las = 0, oma = oma,
          bg = colbg)
      if (!is.null(tlim) && trel) {
        wave <- wave0
        from <- tlim[1]
        to <- tlim[2]
      }
      else {
        from <- FALSE
        to <- FALSE
      }
      soscillo(wave = wave, f = f, bty = "u", from = from,
               to = to, fastdisp = fastdisp, collab = collab,
               colaxis = colaxis, colline = colaxis, tickup = max(abs(wave),
                                                                  na.rm = TRUE), ylim = c(-max(abs(wave)), max(abs(wave))),
               tlab = tlab, alab = alab, cexlab = cexlab, cexaxis = cexaxis,
               xaxt = {
                 if (!axisX) {
                   "n"
                 }
               }, ...)
      par(mar = c(0, 4.1, 2.1, 2.1), las = 1, cex.lab = cexlab)
      filled.contour.modif2(x = X[seq(1, length(X), length = res)],
                            y = Y, z = Z[seq(1, nrow(Z), length = res), ],
                            levels = collevels, nlevels = 20, plot.title = title(main = main,
                                                                                 xlab = "", ylab = flab), color.palette = palette,
                            plot.axes = {
                              if (axisY) {
                                axis(2, at = yat, labels = ylabel)
                              }
                              else {
                                NULL
                              }
                            }, col.lab = collab, colaxis = colaxis, ...)
      if (grid)
        abline(h = yat, col = colgrid, lty = "dotted")
      if (cont) {
        contour(X, Y, Z, add = TRUE, levels = contlevels,
                nlevels = 5, col = colcont, ...)
      }
      if (colaxis != colgrid)
        abline(h = 0, col = colaxis)
      else abline(h = 0, col = colgrid)
    }
    if (osc == FALSE & scale == FALSE) {
      par(las = 1, col = colaxis, col.axis = colaxis, col.lab = collab,
          bg = colbg, cex.axis = cexaxis, cex.lab = cexlab,
          ...)
      filled.contour.modif2(x = X[seq(1, length(X), length = res)],
                            y = Y, z = Z[seq(1, nrow(Z), length = res), ],
                            levels = collevels, nlevels = 20, plot.title = title(main = main,
                                                                                 xlab = tlab, ylab = flab), plot.axes = {
                                                                                   if (axisX) {
                                                                                     axis(1, at = xat, labels = xlabel)
                                                                                   }
                                                                                   if (axisY) {
                                                                                     axis(2, at = yat, labels = ylabel)
                                                                                   }
                                                                                 }, color.palette = palette, col.lab = collab,
                            colaxis = colaxis)
      if (grid)
        abline(h = yat, col = colgrid, lty = "dotted")
      if (cont) {
        contour(X, Y, Z, add = TRUE, levels = contlevels,
                nlevels = 5, col = colcont, ...)
      }
      if (colaxis != colgrid)
        abline(h = 0, col = colaxis)
      else abline(h = 0, col = colgrid)
    }
    if (listen) {
      listen(wave, f = f)
    }
    ptm <- proc.time() - ptm.start
    if (isTRUE(plot) && ptm[3] > 10)
      cat("This took quite a lot of time to display this graphic, you may set 'fastdisp=TRUE' for a faster, but less accurate, display\n")
    invisible(list(time = X, freq = Y, amp = z))
  }
  else return(list(time = X, freq = Y, amp = z))
}
