# Estilos de dibujo: "clasico" (tipo AMOS), "revista" (grises) y "color"
# (un color por factor). Los dos últimos siguen los requisitos habituales de
# las revistas: letra sin serifa de al menos 8 pt al ancho de impresión,
# líneas de 0.6 a 0.9 pt, relleno plano y sin sombras.

ESTILOS <- c("clasico", "revista", "color")

# Anchos de impresión habituales (pulgadas): una columna ~85 mm, página ~174 mm
ANCHOS <- c(columna = 85, pagina = 174) / 25.4

# Colores Okabe-Ito (distinguibles con daltonismo y legibles en grises)
COLORES_FACTOR <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9")
NEGRO <- "#1A1A1A"
GRIS  <- "#6E6E6E"

# Tamaño relativo del texto principal (ítems y valores) en cada estilo
TEXTO_ITEM <- c(clasico = 0.68, revista = 1, color = 1)

#' @noRd
validar_estilo <- function(estilo) {
  if (!is.character(estilo) || length(estilo) != 1 || !estilo %in% ESTILOS)
    stop("`estilo` debe ser \"clasico\", \"revista\" o \"color\".", call. = FALSE)
  estilo
}

# Las figuras crecen para que quepa el texto, que en los estilos de revista
# es mayor. Se hace antes del acomodo para que el espacio entre ellas se ajuste.
#' @noRd
ajustar_tamanos <- function(G, estilo) {
  if (estilo == "clasico") return(G)
  N <- G$nodes
  for (i in seq_len(nrow(N))) {
    ln <- max(nchar(strsplit(N$etiqueta[i], "\n", fixed = TRUE)[[1]]))
    if (N$forma[i] == "rect") N$a[i] <- max(N$a[i], 0.084 * ln + 0.14)
    else if (N$tipo[i] == "lv") N$a[i] <- max(N$a[i], 0.108 * ln + 0.5)
  }
  G$nodes <- N
  G
}

#' @noRd
tinte <- function(col, k) {
  m <- grDevices::col2rgb(col) / 255
  grDevices::rgb(1 - (1 - m[1]) * k, 1 - (1 - m[2]) * k, 1 - (1 - m[3]) * k)
}

#' @noRd
caja_redonda <- function(x, y, a, b, r, ...) {
  r <- min(r, a, b); t <- seq(0, pi / 2, length.out = 10)
  xs <- c(x + a - r + r * cos(t), x - a + r + r * cos(t + pi / 2),
          x - a + r + r * cos(t + pi), x + a - r + r * cos(t + 3 * pi / 2))
  ys <- c(y + b - r + r * sin(t), y + b - r + r * sin(t + pi / 2),
          y - b + r + r * sin(t + pi), y - b + r + r * sin(t + 3 * pi / 2))
  graphics::polygon(xs, ys, ...)
}

#' @noRd
punta_fina <- function(x0, y0, x1, y1, col, L = 0.1) {
  th <- atan2(y1 - y0, x1 - x0); a <- 20 * pi / 180
  graphics::polygon(c(x1, x1 - L * cos(th - a), x1 - L * 0.75 * cos(th), x1 - L * cos(th + a)),
                    c(y1, y1 - L * sin(th - a), y1 - L * 0.75 * sin(th), y1 - L * sin(th + a)),
                    col = col, border = NA)
}

# Factor al que pertenece cada figura (para el estilo "color")
#' @noRd
factor_de <- function(G, id, bifactor = FALSE) {
  if (id %in% G$lv) return(id)
  if (bifactor && id %in% G$ind_ov) {
    # en un bifactor, el color es el del factor específico (el que tiene menos ítems)
    E <- G$edges[G$edges$tipo %in% c("carga", "cruzada"), ]
    fs <- E$from[E$to == id]
    if (length(fs)) { n <- table(E$from)[fs]; return(names(n)[which.min(n)]) }
  }
  if (id %in% names(G$principal)) return(G$principal[[id]])
  if (id %in% names(G$err_de)) return(factor_de(G, G$err_de[[id]], bifactor))
  NA_character_
}

#' @noRd
dibujar_moderno <- function(G, tr, op, f, tipo = "") {
  N <- G$nodes; color <- op$estilo == "color"; bif <- identical(tipo, "bifactor")
  col_f <- stats::setNames(COLORES_FACTOR[(seq_along(G$lv) - 1) %% length(COLORES_FACTOR) + 1], G$lv)
  borde_de <- function(id) {
    if (!color) return(NEGRO)
    g <- factor_de(G, id, bif); if (is.na(g)) NEGRO else col_f[[g]]
  }
  relleno_de <- function(n) {
    if (!color) return(if (n$tipo == "lv") "#EDEDED" else "white")
    g <- factor_de(G, n$id, bif)
    if (is.na(g) || n$tipo %in% c("err", "dist")) return("white")
    tinte(col_f[[g]], if (n$tipo == "lv") 0.22 else 0.08)
  }
  lw <- function(pt) pt / 0.75   # en R, lwd = 1 equivale a 0.75 pt
  secundaria <- function(tp) tp %in% c("error", "cov", "covres")
  debil <- if (!is.null(G$edges$debil)) G$edges$debil else rep(FALSE, length(tr))

  # Flechas: principales en negro (0.9 pt), errores y correlaciones en gris (0.6 pt)
  for (j in seq_along(tr)) {
    g <- tr[[j]]; col <- if (secundaria(g$tipo)) GRIS else NEGRO
    graphics::lines(g$x, g$y, lwd = lw(if (secundaria(g$tipo)) 0.6 else 0.9), col = col,
                    lty = if (isTRUE(debil[j])) 2 else 1)
    punta_fina(g$x[37], g$y[37], g$x[40], g$y[40], col)
    if (g$doble) punta_fina(g$x[4], g$y[4], g$x[1], g$y[1], col)
  }
  # Valores sobre una "píldora" blanca (sin los "1" de los errores)
  cx <- TEXTO_ITEM[[op$estilo]] * f
  for (j in seq_along(tr)) {
    if (tr[[j]]$tipo == "error") next
    lab <- texto_flecha(G, j, op); if (is.na(lab)) next
    w <- graphics::strwidth(lab, cex = cx) / 2 + 0.05; h <- graphics::strheight(lab, cex = cx) / 2 + 0.04
    caja_redonda(tr[[j]]$lx, tr[[j]]$ly, w, h, h, col = "white", border = NA)
    graphics::text(tr[[j]]$lx, tr[[j]]$ly, lab, cex = cx, col = if (secundaria(tr[[j]]$tipo)) GRIS else NEGRO)
  }
  # Figuras
  t <- seq(0, 2 * pi, length.out = 200)
  for (i in seq_len(nrow(N))) {
    n <- N[i, ]; err <- n$tipo %in% c("err", "dist")
    if (n$forma == "rect") caja_redonda(n$x, n$y, n$a, n$b, 0.05, col = relleno_de(n), border = borde_de(n$id), lwd = lw(0.9))
    else graphics::polygon(n$x + n$a * cos(t), n$y + n$b * sin(t), col = relleno_de(n),
                           border = if (err) GRIS else borde_de(n$id), lwd = lw(if (err) 0.6 else 1))
    k <- switch(n$tipo, lv = 1.18, ovc = 1.08, item = 1, 0.92)
    graphics::text(n$x, n$y, utf8_real(n$etiqueta), cex = k * f, font = if (n$tipo == "lv") 2 else 1,
                   col = if (err) GRIS else NEGRO)
  }
  # R cuadrada en gris y cursiva
  r2 <- posiciones_r2(G, tr, op)
  for (i in seq_len(if (is.null(r2)) 0 else nrow(r2)))
    graphics::text(r2$x[i], r2$y[i], formato_valor(r2$valor[i], TRUE, op$decimales),
                   cex = 0.92 * f, col = GRIS, font = 3)
}
