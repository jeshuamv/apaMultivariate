# Supuestos: atípicos multivariados y multicolinealidad

#' Tabla de casos atípicos multivariados (distancia de Mahalanobis)
#'
#' Calcula la distancia de Mahalanobis al cuadrado (\eqn{D^2}) de cada caso
#' respecto del centroide, como la salida "Observations farthest from the
#' centroid" de AMOS:
#'
#' * **p1:** probabilidad de que un caso cualquiera tenga una \eqn{D^2} tan
#'   grande (distribución \eqn{\chi^2} con tantos grados de libertad como
#'   variables). Un p1 \eqn{\le} .001 señala un atípico (Kline, 2011).
#' * **p2:** probabilidad de que la *i*-ésima \eqn{D^2} más grande sea tan
#'   grande como la observada; valores pequeños de p2 indican atípicos aun
#'   considerando que se revisan muchos casos.
#'
#' La tabla muestra los casos más alejados; los atípicos (p1 \eqn{\le} `alfa`)
#' quedan en `$atipicos` (número de fila en los datos originales) para
#' excluirlos si se decide hacerlo: `datos[-t$atipicos, ]`. Eliminar casos debe
#' justificarse y reportarse; conviene comparar los resultados con y sin
#' ellos.
#'
#' @param datos Un `data.frame` o un modelo de `lavaan` (se usan sus datos y
#'   los números de fila originales).
#' @param variables Variables que se usan (por omisión, todas las numéricas o
#'   las del modelo).
#' @param alfa Umbral de p1 para considerar atípico un caso (.001 por
#'   omisión).
#' @param n Número de casos que se muestran (los más alejados). Siempre se
#'   muestran todos los atípicos.
#' @param decimales Número de decimales.
#' @param titulo Título de la tabla.
#' @param archivo Si se indica, guarda la tabla.
#' @return Un objeto `tabla_apa`; `$atipicos` tiene los números de fila de los
#'   casos atípicos y `$datos` las distancias de todos los casos.
#' @examples
#' library(lavaan)
#' t <- tabla_atipicos(HolzingerSwineford1939, variables = paste0("x", 1:9))
#' t
#' t$atipicos
#' @export
tabla_atipicos <- function(datos, variables = NULL, alfa = .001, n = 10, decimales = 2, titulo = NULL, archivo = NULL) {
  filas <- NULL
  if (inherits(datos, "lavaan")) {
    filas <- lavaan::lavInspect(datos, "case.idx"); if (is.list(filas)) filas <- unlist(filas)
  }
  d <- obtener_datos(datos, variables)
  if (is.null(filas)) filas <- seq_len(nrow(d))
  ok <- stats::complete.cases(d); d <- d[ok, , drop = FALSE]; filas <- filas[ok]
  N <- nrow(d); k <- ncol(d)
  if (k < 2) stop("Se necesitan al menos dos variables.", call. = FALSE)
  D2 <- stats::mahalanobis(d, colMeans(d), stats::cov(d))
  p1 <- stats::pchisq(D2, k, lower.tail = FALSE)
  o <- order(-D2); r <- seq_len(N)
  # p2: probabilidad de que la r-ésima D2 más grande supere la observada
  p2 <- numeric(N); p2[o] <- stats::pbeta(p1[o], r, N - r + 1)
  at <- which(p1 <= alfa)
  mostrar <- o[seq_len(min(N, max(n, length(at))))]
  cel <- cbind(as.character(filas[mostrar]), f_num(D2[mostrar], decimales), f_p(p1[mostrar]), f_p(p2[mostrar]))
  neg <- matrix(FALSE, nrow(cel), 4); neg[p1[mostrar] <= alfa, ] <- TRUE
  # Atípicos univariados (|z| > 3)
  z <- scale(d); uni <- colSums(abs(z) > 3)
  s_ <- if (length(at) > 1) "s" else ""
  lista_at <- paste(filas[at][order(-D2[at])], collapse = ", ")
  nota <- paste0(tx("D\u00b2 = distancia de Mahalanobis al cuadrado (", "D\u00b2 = squared Mahalanobis distance ("), k, " variables, N = ", N, "); p1 = P(\u03c7\u00b2(", k, ") \u2265 D\u00b2); ",
                 tx("p2 = probabilidad de que la i-\u00e9sima D\u00b2 m\u00e1s grande alcance ese valor. Casos ordenados de m\u00e1s a menos alejado. ",
                    "p2 = probability that the i-th largest D\u00b2 reaches that value. Cases are ordered from farthest to nearest. "),
                 if (length(at)) (if (en()) paste0(length(at), " outlier", s_, " (p1 \u2264 ", f_apa(alfa, 3), "; Kline, 2011) in bold: row", s_, " ", lista_at, ".")
                                  else paste0("En negritas, ", length(at), " caso", s_, " at\u00edpico", s_, " (p1 \u2264 ", f_apa(alfa, 3), "; Kline, 2011): fila", s_, " ", lista_at, "."))
                 else paste0(tx("Ning\u00fan caso tiene p1 \u2264 ", "No case has p1 \u2264 "), f_apa(alfa, 3), " (Kline, 2011)."),
                 if (any(uni > 0)) paste0(tx(" At\u00edpicos univariados (|z| > 3): ", " Univariate outliers (|z| > 3): "),
                                         paste0(names(uni)[uni > 0], " (", uni[uni > 0], ")", collapse = ", "), ".") else
                   tx(" Ninguna variable tiene valores con |z| > 3.", " No variable has values with |z| > 3."))
  t <- nueva_tabla(if (is.null(titulo)) tx("Casos m\u00e1s alejados del centroide (distancia de Mahalanobis)", "Observations farthest from the centroid (Mahalanobis distance)") else titulo,
                   c(tx("Fila", "Row"), "D\u00b2", "p1", "p2"), cel, neg, alinear = c("l", "c", "c", "c"), nota = nota,
                   datos = data.frame(fila = filas, D2 = D2, p1 = p1, p2 = p2, row.names = NULL))
  t$atipicos <- sort(filas[at]); t$univariados <- uni
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

#' Tabla de multicolinealidad (VIF)
#'
#' Para cada variable dependiente del modelo con dos o más predictores,
#' calcula la tolerancia y el factor de inflación de la varianza (VIF) de
#' cada predictor, con la matriz de correlaciones del modelo (sirve también
#' cuando los predictores son variables latentes). Un VIF mayor que 5 (o una
#' tolerancia menor que .20) indica multicolinealidad preocupante; mayor que
#' 10, grave.
#'
#' @param fit Modelo de `lavaan` con regresiones (`~`).
#' @inheritParams tabla_descriptivos
#' @return Un objeto `tabla_apa`. Los valores están en `$datos`.
#' @examples
#' library(lavaan)
#' set.seed(1)
#' d <- data.frame(X1 = rnorm(200)); d$X2 <- .6 * d$X1 + rnorm(200)
#' d$Y <- .3 * d$X1 + .3 * d$X2 + rnorm(200)
#' tabla_vif(sem("Y ~ X1 + X2", data = d))
#' @export
tabla_vif <- function(fit, etiquetas = NULL, decimales = 2, titulo = NULL, archivo = NULL) {
  if (!inherits(fit, "lavaan")) stop("`fit` debe ser un modelo de lavaan.", call. = FALSE)
  pe <- lavaan::parameterEstimates(fit); if ("group" %in% names(pe)) pe <- pe[pe$group == 1, ]
  reg <- pe[pe$op == "~", ]
  R <- lavaan::lavInspect(fit, "cor.all"); if (is.list(R) && !is.matrix(R)) R <- R[[1]]
  dep <- unique(reg$lhs); filas <- list()
  for (y in dep) {
    x <- reg$rhs[reg$lhs == y]
    if (length(x) < 2) next
    vif <- diag(solve(R[x, x]))
    for (i in seq_along(x)) filas[[length(filas) + 1]] <- data.frame(dependiente = y, predictor = x[i], tolerancia = 1 / vif[i],
                                                                     vif = vif[i], stringsAsFactors = FALSE)
  }
  if (!length(filas)) stop("Ninguna variable dependiente tiene dos o m\u00e1s predictores; no hay multicolinealidad que evaluar.", call. = FALSE)
  D <- do.call(rbind, filas); rownames(D) <- NULL
  prev <- ""; cel <- t(vapply(seq_len(nrow(D)), function(i) {
    y <- if (D$dependiente[i] != prev) renombrar(D$dependiente[i], etiquetas) else ""; prev <<- D$dependiente[i]
    c(y, renombrar(D$predictor[i], etiquetas), f_apa(D$tolerancia[i], decimales), f_num(D$vif[i], decimales))
  }, character(4)))
  neg <- matrix(FALSE, nrow(cel), 4); neg[D$vif > 5, 4] <- TRUE
  nota <- paste0(tx("VIF = factor de inflaci\u00f3n de la varianza (1/tolerancia), calculado con las correlaciones del modelo. VIF > 5 (tolerancia < .20) indica multicolinealidad preocupante y VIF > 10, grave. ",
                    "VIF = variance inflation factor (1/tolerance), computed from the model correlations. VIF > 5 (tolerance < .20) indicates problematic multicollinearity and VIF > 10, severe. "),
                 if (any(D$vif > 5)) tx("En negritas, los valores que superan 5.", "Values above 5 are in bold.") else tx("Ning\u00fan predictor supera VIF = 5.", "No predictor exceeds VIF = 5."))
  t <- nueva_tabla(if (is.null(titulo)) tx("Diagn\u00f3stico de multicolinealidad", "Multicollinearity diagnostics") else titulo,
                   c(tx("Variable dependiente", "Dependent variable"), "Predictor", tx("Tolerancia", "Tolerance"), "VIF"), cel, neg,
                   alinear = c("l", "l", "c", "c"), nota = nota, datos = D)
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}
