# Gráfica de sedimentación (scree) con el análisis paralelo

#' Gráfica de sedimentación con análisis paralelo (AFE)
#'
#' Dibuja los autovalores observados, en orden, junto al percentil 95 de los
#' autovalores de datos aleatorios del mismo tamaño (análisis paralelo de
#' Horn). Se retienen los factores cuyo autovalor observado queda por encima
#' de la línea de los aleatorios; el "codo" de la curva (Cattell) es un
#' criterio complementario.
#'
#' Usa los mismos cálculos que [tabla_factorabilidad()], así que la gráfica y
#' la tabla siempre coinciden. Con `tipo = "factores"` (por defecto) se
#' grafican los autovalores de la solución factorial con el método `fm`, como
#' en `psych::fa.parallel()`; con `"componentes"`, los de la matriz de
#' correlaciones, con la línea de Kaiser (autovalor = 1); con `"ambos"`, las
#' dos curvas.
#'
#' @param datos `data.frame` con los ítems (o un modelo de `lavaan`).
#' @param tipo `"factores"`, `"componentes"` o `"ambos"`.
#' @param maximo Número máximo de autovalores que se grafican.
#' @inheritParams tabla_factorabilidad
#' @inheritParams grafica_normalidad
#' @return Invisiblemente, un `data.frame` con los autovalores observados y
#'   aleatorios; el número de factores sugerido está en `attr(, "sugeridos")`.
#' @examples
#' \donttest{
#' if (requireNamespace("psych", quietly = TRUE)) {
#'   grafica_sedimentacion(lavaan::HolzingerSwineford1939[, paste0("x", 1:9)], iteraciones = 50)
#' }
#' }
#' @export
grafica_sedimentacion <- function(datos, variables = NULL, tipo = c("factores", "componentes", "ambos"), fm = "ml", cor = "pearson",
                                  iteraciones = 100, semilla = 123, maximo = 20, archivo = NULL, ancho = "columna", alto = NULL) {
  tipo <- match.arg(tipo)
  tf <- suppressMessages(tabla_factorabilidad(datos, variables, iteraciones = iteraciones, fm = fm, cor = cor, semilla = semilla))
  D <- tf$datos; p <- length(D$autovalores); m <- min(p, maximo); i <- seq_len(m)
  res <- data.frame(numero = i, componentes = D$autovalores[i], aleatorios_componentes = D$aleatorios_comp[i],
                    factores = D$autovalores_reducidos[i], aleatorios_factores = D$aleatorios_fact[i])
  if (is.character(ancho)) {
    if (!ancho %in% names(ANCHOS)) stop("`ancho` debe ser \"columna\", \"pagina\" o un n\u00famero en pulgadas.", call. = FALSE)
    ancho <- ANCHOS[[ancho]]
  }
  if (is.null(alto)) alto <- ancho * 0.8
  ver_f <- tipo %in% c("factores", "ambos"); ver_c <- tipo %in% c("componentes", "ambos")
  dibuja <- function() {
    op <- graphics::par(mar = c(3.4, 3.6, 1.6, 0.8), mgp = c(2.1, 0.6, 0), family = "sans", tcl = -0.3, las = 1)
    on.exit(graphics::par(op))
    y <- c(if (ver_f) c(res$factores, res$aleatorios_factores), if (ver_c) c(res$componentes, res$aleatorios_componentes, 1))
    graphics::plot(NA, xlim = c(1, m), ylim = range(c(0, y)), bty = "l", xaxt = "n",
                   xlab = txt_disp(tx("N\u00famero de factor", "Factor number")), ylab = txt_disp(tx("Autovalor", "Eigenvalue")),
                   cex.lab = 0.95, cex.axis = 0.85)
    graphics::axis(1, at = i, cex.axis = 0.85)
    graphics::abline(h = 0, col = "#BDBDBD", lwd = 0.6)
    ley <- character(0); lty <- integer(0); pch <- integer(0); col <- character(0)
    if (ver_c) {
      graphics::abline(h = 1, lty = 3, col = "#9E9E9E")
      graphics::lines(i, res$componentes, lwd = 1.2); graphics::points(i, res$componentes, pch = 2, cex = 0.8)
      graphics::lines(i, res$aleatorios_componentes, lty = 3, col = "#6E6E6E")
      ley <- c(ley, tx("Componentes, observados", "Components, observed"), tx("Componentes, aleatorios (P95)", "Components, random (95th pct.)"))
      lty <- c(lty, 1, 3); pch <- c(pch, 2, NA); col <- c(col, "#1A1A1A", "#6E6E6E")
    }
    if (ver_f) {
      graphics::lines(i, res$factores, lwd = 1.2); graphics::points(i, res$factores, pch = 16, cex = 0.8)
      graphics::lines(i, res$aleatorios_factores, lty = 2, col = "#6E6E6E")
      ley <- c(ley, tx("Factores, observados", "Factors, observed"), tx("Factores, aleatorios (P95)", "Factors, random (95th pct.)"))
      lty <- c(lty, 1, 2); pch <- c(pch, 16, NA); col <- c(col, "#1A1A1A", "#6E6E6E")
    }
    graphics::legend("topright", legend = txt_disp(ley), lty = lty, pch = pch, col = col, bty = "n", cex = 0.75, pt.cex = 0.8)
    graphics::mtext(txt_disp(tx("Gr\u00e1fica de sedimentaci\u00f3n", "Scree plot")),
                    side = 3, line = 0.3, adj = 0, cex = 0.9, font = 2)
  }
  if (!is.null(archivo)) {
    ext <- tolower(tools::file_ext(archivo))
    con_dispositivo(archivo, ext, ancho, alto, dibuja, res = if (ext %in% c("tiff", "tif")) 600 else 300, pointsize = 9)
  } else dibuja()
  s <- D$factores
  message(tx(paste0("An\u00e1lisis paralelo: ", s[["paralelo_factores"]], " factor(es) (y ", s[["paralelo_componentes"]],
                    " componente(s)) con autovalor por encima de los aleatorios. Kaiser: ", s[["kaiser"]], "; MAP: ", s[["map"]],
                    ". Busca tambi\u00e9n el codo de la curva y que la soluci\u00f3n tenga sentido te\u00f3rico."),
             paste0("Parallel analysis: ", s[["paralelo_factores"]], " factor(s) (and ", s[["paralelo_componentes"]],
                    " component(s)) above the random eigenvalues. Kaiser: ", s[["kaiser"]], "; MAP: ", s[["map"]], ".")))
  attr(res, "sugeridos") <- s
  invisible(res)
}
