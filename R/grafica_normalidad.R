# Gráfica Q-Q chi-cuadrado de las distancias de Mahalanobis

#' Gráfica Q-Q chi-cuadrado (normalidad multivariada)
#'
#' Compara las distancias de Mahalanobis al cuadrado (D²) de los casos,
#' ordenadas, con los cuantiles de la distribución chi-cuadrado con tantos
#' grados de libertad como variables. Si los datos tienen distribución normal
#' multivariada, los puntos caen cerca de la línea de 45°; si se curvan hacia
#' arriba en el extremo derecho, hay colas pesadas o casos atípicos. Los casos
#' atípicos (\eqn{p < .001}) se marcan con su número de fila.
#'
#' Es la versión gráfica de la prueba de Mardia de [tabla_normalidad()] y
#' complementa a [tabla_atipicos()].
#'
#' @param datos `data.frame` o modelo de `lavaan` (se usan sus variables
#'   observadas).
#' @param variables Variables que se usan (por defecto, todas las numéricas).
#' @param alfa Nivel para marcar los atípicos.
#' @param archivo Si se indica, guarda la gráfica (`.png`, `.pdf`, `.tiff`,
#'   `.eps`, `.svg` o `.jpg`).
#' @param ancho,alto Tamaño en pulgadas (o `ancho = "columna"` / `"pagina"`).
#' @return Invisiblemente, un `data.frame` con la fila, D², el cuantil teórico
#'   y si es atípico.
#' @examples
#' library(lavaan)
#' grafica_normalidad(HolzingerSwineford1939[, paste0("x", 1:9)])
#' @export
grafica_normalidad <- function(datos, variables = NULL, alfa = .001, archivo = NULL, ancho = "columna", alto = NULL) {
  filas <- NULL
  if (inherits(datos, "lavaan")) { filas <- lavaan::lavInspect(datos, "case.idx"); if (is.list(filas)) filas <- unlist(filas) }
  d <- obtener_datos(datos, variables)
  if (is.null(filas)) filas <- seq_len(nrow(d))
  ok <- stats::complete.cases(d); d <- d[ok, , drop = FALSE]; filas <- filas[ok]
  n <- nrow(d); k <- ncol(d)
  if (k < 2) stop("Se necesitan al menos dos variables.", call. = FALSE)
  D2 <- stats::mahalanobis(d, colMeans(d), stats::cov(d))
  o <- order(D2); q <- stats::qchisq(stats::ppoints(n), df = k)
  res <- data.frame(fila = filas[o], d2 = D2[o], cuantil = q, atipico = stats::pchisq(D2[o], k, lower.tail = FALSE) < alfa)
  if (is.character(ancho)) {
    if (!ancho %in% names(ANCHOS)) stop("`ancho` debe ser \"columna\", \"pagina\" o un n\u00famero en pulgadas.", call. = FALSE)
    ancho <- ANCHOS[[ancho]]
  }
  if (is.null(alto)) alto <- ancho
  dibuja <- function() {
    op <- graphics::par(mar = c(3.4, 3.6, 1.6, 0.8), mgp = c(2.1, 0.6, 0), family = "sans", tcl = -0.3, las = 1)
    on.exit(graphics::par(op))
    lim <- range(c(0, q, res$d2))
    graphics::plot(res$cuantil, res$d2, pch = 16, cex = 0.55, col = ifelse(res$atipico, "#B03A2E", "#1A1A1A"),
                   xlim = lim, ylim = lim, bty = "l",
                   xlab = if (en()) bquote("Theoretical " * chi^2 * " quantiles (df = " * .(k) * ")")
                          else bquote("Cuantiles te\u00f3ricos " * chi^2 * " (gl = " * .(k) * ")"),
                   ylab = txt_disp(tx("Distancia de Mahalanobis (D\u00b2)", "Mahalanobis distance (D\u00b2)")), cex.lab = 0.95, cex.axis = 0.85)
    graphics::abline(0, 1, lty = 2, col = "#6E6E6E")
    lim_at <- stats::qchisq(1 - alfa, k)
    graphics::abline(h = lim_at, lty = 3, col = "#B03A2E")
    graphics::mtext(txt_disp(tx("Gr\u00e1fica Q-Q chi-cuadrado", "Chi-square Q-Q plot")), side = 3, line = 0.3, adj = 0, cex = 0.9, font = 2)
    if (any(res$atipico)) graphics::text(res$cuantil[res$atipico], res$d2[res$atipico], res$fila[res$atipico],
                                         pos = 2, cex = 0.7, col = "#B03A2E", xpd = TRUE)
  }
  if (!is.null(archivo)) {
    ext <- tolower(tools::file_ext(archivo))
    con_dispositivo(archivo, ext, ancho, alto, dibuja, res = if (ext %in% c("tiff", "tif")) 600 else 300, pointsize = 9)
  } else dibuja()
  nat <- sum(res$atipico)
  message(tx(paste0("Si hay normalidad multivariada, los puntos caen cerca de la l\u00ednea punteada. La l\u00ednea roja marca el l\u00edmite de los at\u00edpicos (p < ",
                    sub("^0", "", format(alfa)), "): ", if (nat) paste0(nat, " caso", if (nat > 1) "s" else "", " por encima.") else "ning\u00fan caso por encima."),
             paste0("Under multivariate normality the points fall near the dashed line. The red line marks the outlier cutoff (p < ",
                    sub("^0", "", format(alfa)), "): ", nat, " case(s) above it.")))
  invisible(res)
}
