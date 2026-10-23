# Dibujo del diagrama con gráficos base de R

PULG_POR_UNIDAD <- 0.8   # escala de referencia para el tamaño del texto

#' @noRd
caja_total <- function(G, tr) {
  N <- G$nodes
  xs <- c(N$x - N$a - 0.3, N$x + N$a + 0.35, unlist(lapply(tr, `[[`, "x")))
  ys <- c(N$y - N$b - 0.25, N$y + N$b + 0.3, unlist(lapply(tr, `[[`, "y")))
  c(range(xs), range(ys))
}

#' @noRd
rellenar <- function(n, cols, degradado) {
  if (!degradado) cols <- rep(cols[2], 2)
  if (cols[1] == cols[2]) {
    if (n$forma == "rect") graphics::rect(n$x - n$a, n$y - n$b, n$x + n$a, n$y + n$b, col = cols[1], border = NA)
    else { t <- seq(0, 2 * pi, length.out = 120); graphics::polygon(n$x + n$a * cos(t), n$y + n$b * sin(t), col = cols[1], border = NA) }
    return(invisible())
  }
  k <- 80
  xs <- seq(n$x - n$a, n$x + n$a, length.out = k + 1)
  pal <- grDevices::colorRampPalette(cols)(k); paso <- xs[2] - xs[1]
  for (i in seq_len(k)) {
    xm <- (xs[i] + xs[i + 1]) / 2
    h <- if (n$forma == "rect") n$b else n$b * sqrt(max(0, 1 - ((xm - n$x) / n$a)^2))
    graphics::rect(xs[i], n$y - h, min(xs[i + 1] + paso * 0.6, n$x + n$a), n$y + h, col = pal[i], border = NA)
  }
}

#' @noRd
punta <- function(x0, y0, x1, y1, col, L = 0.09) {
  th <- atan2(y1 - y0, x1 - x0); a <- 22 * pi / 180
  graphics::polygon(c(x1, x1 - L * cos(th - a), x1 - L * cos(th + a)),
                    c(y1, y1 - L * sin(th - a), y1 - L * sin(th + a)), col = col, border = col)
}

#' @noRd
texto_flecha <- function(G, j, op) {
  e <- G$edges[j, ]
  if (e$tipo == "error") return("1")
  if (!op$mostrar_valores) return(NA_character_)
  s <- formato_valor(e$val, op$estandarizado, op$decimales)
  if (op$estrellas && !e$fijo) s <- paste0(s, estrellas_p(e$p))
  s
}

#' @noRd
dibujar_diagrama <- function(obj) {
  G <- obj$G; tr <- obj$trazos; op <- obj$opciones; pal <- op$paleta
  N <- G$nodes; cj <- caja_total(G, tr)
  op_par <- graphics::par(mar = c(0.3, 0.3, if (is.null(op$titulo)) 0.3 else 2, 0.3))
  on.exit(graphics::par(op_par))
  graphics::plot.new()
  graphics::plot.window(xlim = cj[1:2] + c(-0.25, 0.25), ylim = cj[3:4] + c(-0.25, 0.25), asp = 1)
  usr <- graphics::par("usr"); pin <- graphics::par("pin")
  f <- min(pin[1] / diff(usr[1:2]), pin[2] / diff(usr[3:4])) / PULG_POR_UNIDAD
  if (!is.null(op$titulo)) graphics::title(utf8_real(op$titulo), cex.main = 1, col.main = pal$texto)
  if (!identical(op$estilo, "clasico") && !is.null(op$estilo)) {
    dibujar_moderno(G, tr, op, f, obj$tipo)
    return(invisible(obj))
  }

  # Flechas
  debil <- if (!is.null(G$edges$debil)) G$edges$debil else rep(FALSE, length(tr))
  for (j in seq_along(tr)) {
    g <- tr[[j]]
    graphics::lines(g$x, g$y, lwd = 1.2, col = pal$linea, lty = if (isTRUE(debil[j])) 2 else 1)
    punta(g$x[38], g$y[38], g$x[40], g$y[40], pal$linea)
    if (g$doble) punta(g$x[3], g$y[3], g$x[1], g$y[1], pal$linea)
  }
  # Etiquetas de las flechas (con fondo blanco)
  for (j in seq_along(tr)) {
    lab <- texto_flecha(G, j, op); if (is.na(lab)) next
    cx <- (if (tr[[j]]$tipo == "error") 0.55 else 0.72) * f
    if (tr[[j]]$tipo == "error") {
      g <- tr[[j]]; dx <- g$x[40] - g$x[1]; dy <- g$y[40] - g$y[1]; d <- sqrt(dx^2 + dy^2)
      graphics::text(g$lx - dy / d * 0.09, g$ly + dx / d * 0.09, lab, cex = cx, col = pal$texto)
      next
    }
    w <- graphics::strwidth(lab, cex = cx) * 0.6; h <- graphics::strheight(lab, cex = cx) * 0.75
    graphics::rect(tr[[j]]$lx - w, tr[[j]]$ly - h, tr[[j]]$lx + w, tr[[j]]$ly + h, col = "white", border = NA)
    graphics::text(tr[[j]]$lx, tr[[j]]$ly, lab, cex = cx, col = pal$texto)
  }
  # Figuras
  t <- seq(0, 2 * pi, length.out = 200)
  for (i in seq_len(nrow(N))) {
    n <- N[i, ]
    cols <- switch(n$tipo, lv = pal$latente, ovc = , item = pal$observada, pal$error)
    rellenar(n, cols, op$degradado)
    if (n$forma == "rect") graphics::rect(n$x - n$a, n$y - n$b, n$x + n$a, n$y + n$b, border = pal$linea, lwd = 1.2)
    else graphics::polygon(n$x + n$a * cos(t), n$y + n$b * sin(t), border = pal$linea, lwd = 1.2)
    cx <- f * switch(n$tipo, lv = 0.85, ovc = 0.75, item = 0.68, err = 0.68, dist = 0.7)
    graphics::text(n$x, n$y, utf8_real(n$etiqueta), cex = cx, col = pal$texto)
  }
  # R cuadrada
  r2 <- posiciones_r2(G, tr, op)
  for (i in seq_len(if (is.null(r2)) 0 else nrow(r2)))
    graphics::text(r2$x[i], r2$y[i], formato_valor(r2$valor[i], TRUE, op$decimales),
                   cex = 0.66 * f, col = pal$texto)
  invisible(obj)
}
