# Tablas preliminares: descriptivos y correlaciones, datos perdidos,
# normalidad multivariada y confiabilidad (alfa y omega)

# --- Utilidades -------------------------------------------------------------
# Obtiene un data.frame numérico a partir de datos o de un modelo de lavaan
#' @noRd
obtener_datos <- function(x, variables = NULL, quien = "datos") {
  if (inherits(x, "lavaan")) {
    d <- lavaan::lavInspect(x, "data")
    if (is.list(d) && !is.matrix(d)) d <- do.call(rbind, d)
    d <- as.data.frame(d)
    if (!ncol(d)) stop("El modelo no guarda los datos; p\u00e1sale el data.frame.", call. = FALSE)
    colnames(d) <- lavaan::lavNames(x, "ov")[seq_len(ncol(d))]
  } else if (is.data.frame(x) || is.matrix(x)) {
    d <- as.data.frame(x)
  } else stop("`", quien, "` debe ser un data.frame o un modelo de lavaan.", call. = FALSE)
  if (!is.null(variables)) {
    falta <- setdiff(variables, names(d))
    if (length(falta)) stop("No est\u00e1n en los datos: ", paste(falta, collapse = ", "), call. = FALSE)
    d <- d[, variables, drop = FALSE]
  }
  num <- vapply(d, is.numeric, logical(1))
  if (!all(num)) {
    message("Se omiten columnas que no son num\u00e9ricas: ", paste(names(d)[!num], collapse = ", "))
    d <- d[, num, drop = FALSE]
  }
  if (!ncol(d)) stop("No hay variables num\u00e9ricas.", call. = FALSE)
  d
}

# Asimetría y curtosis corregidas por sesgo (las de SPSS)
#' @noRd
asimetria <- function(x) {
  x <- x[!is.na(x)]; n <- length(x); if (n < 3) return(NA_real_)
  m <- mean(x); m2 <- mean((x - m)^2); m3 <- mean((x - m)^3)
  sqrt(n * (n - 1)) / (n - 2) * m3 / m2^1.5
}
#' @noRd
curtosis <- function(x) {
  x <- x[!is.na(x)]; n <- length(x); if (n < 4) return(NA_real_)
  m <- mean(x); m2 <- mean((x - m)^2); g2 <- mean((x - m)^4) / m2^2 - 3
  ((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3))
}

#' @noRd
nota_estrellas <- "* p < .05. ** p < .01. *** p < .001."

# --- Descriptivos y correlaciones --------------------------------------------

#' Tabla de descriptivos y correlaciones
#'
#' La "Tabla 1" de la mayoría de los artículos: para cada variable, número de
#' casos válidos, media, desviación estándar, asimetría y curtosis, y la matriz
#' de correlaciones con asteriscos de significancia.
#'
#' La asimetría y la curtosis están corregidas por sesgo (son las mismas que da
#' SPSS); la curtosis es el exceso sobre la normal (0 = normal). Las
#' correlaciones usan eliminación por pares.
#'
#' @param datos Un `data.frame` o un modelo de `lavaan` (se usan sus datos).
#' @param variables Nombres de las variables (por omisión, todas las numéricas).
#' @param correlaciones Incluir la matriz de correlaciones (`TRUE` por omisión).
#' @param metodo `"pearson"` (por omisión) o `"spearman"`.
#' @param etiquetas Vector con nombres para cambiar cómo se ven las variables.
#' @param decimales Número de decimales.
#' @param titulo Título de la tabla.
#' @param archivo Si se indica, guarda la tabla (`.docx`, `.rtf`, `.html` o
#'   `.csv`).
#' @return Un objeto `tabla_apa`. Los valores numéricos están en `$datos` y
#'   la matriz de correlaciones en `$correlaciones`.
#' @examples
#' library(lavaan)
#' tabla_descriptivos(HolzingerSwineford1939, variables = c("x1", "x2", "x3", "x4"))
#' @export
tabla_descriptivos <- function(datos, variables = NULL, correlaciones = TRUE,
                               metodo = c("pearson", "spearman"), etiquetas = NULL,
                               decimales = 2, titulo = NULL, archivo = NULL) {
  metodo <- match.arg(metodo)
  d <- obtener_datos(datos, variables)
  v <- names(d); k <- length(v)
  n <- vapply(d, function(x) sum(!is.na(x)), numeric(1))
  M <- vapply(d, mean, numeric(1), na.rm = TRUE); DE <- vapply(d, stats::sd, numeric(1), na.rm = TRUE)
  As <- vapply(d, asimetria, numeric(1)); Ku <- vapply(d, curtosis, numeric(1))
  nom <- paste0(seq_len(k), ". ", renombrar(v, etiquetas))
  cel <- cbind(nom, formatC(n, format = "d"), f_num(M, decimales), f_num(DE, decimales),
               f_num(As, decimales), f_num(Ku, decimales))
  cols <- c("Variable", "n", "M", "DE", "Asimetr\u00eda", "Curtosis")
  R <- P <- NULL
  if (correlaciones && k > 1) {
    R <- stats::cor(d, use = "pairwise.complete.obs", method = metodo)
    P <- matrix(NA_real_, k, k, dimnames = list(v, v))
    for (i in seq_len(k)) for (j in seq_len(k)) if (i > j) {
      ok <- stats::complete.cases(d[, c(i, j)])
      if (sum(ok) > 3) P[i, j] <- P[j, i] <- suppressWarnings(
        stats::cor.test(d[ok, i], d[ok, j], method = metodo, exact = FALSE)$p.value)
    }
    cc <- matrix("", k, k - 1)
    for (i in seq_len(k)) for (j in seq_len(k - 1)) {
      if (i == j) cc[i, j] <- "\u2014"
      else if (i > j) cc[i, j] <- paste0(f_apa(R[i, j], decimales), estrellas_p(P[i, j]))
    }
    cel <- cbind(cel, cc); cols <- c(cols, as.character(seq_len(k - 1)))
  }
  nota <- paste0("M = media; DE = desviaci\u00f3n est\u00e1ndar. Asimetr\u00eda y curtosis corregidas por sesgo; ",
                 "valores de asimetr\u00eda entre \u22122 y 2 y de curtosis entre \u22127 y 7 suelen considerarse ",
                 "aceptables (West et al., 1995).",
                 if (!is.null(R)) paste0(" Correlaciones de ", if (metodo == "pearson") "Pearson" else "Spearman",
                                         " con eliminaci\u00f3n por pares. ", nota_estrellas))
  datos_t <- data.frame(variable = v, n = n, M = M, DE = DE, asimetria = As, curtosis = Ku, row.names = NULL)
  t <- nueva_tabla(if (is.null(titulo)) "Estad\u00edsticos descriptivos y correlaciones" else titulo,
                   cols, cel, nota = nota, datos = datos_t)
  t$correlaciones <- R; t$p <- P
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

# --- Datos perdidos ------------------------------------------------------------

#' Tabla de datos perdidos
#'
#' Número y porcentaje de datos perdidos por variable y número de casos
#' completos. Ayuda a justificar cómo se trataron los datos perdidos (por
#' ejemplo, con `missing = "fiml"` en `lavaan`).
#'
#' @inheritParams tabla_descriptivos
#' @param datos Un `data.frame` con los datos originales (antes de eliminar
#'   casos).
#' @param solo_con_perdidos Mostrar solo las variables que tienen datos
#'   perdidos.
#' @return Un objeto `tabla_apa`. Los valores están en `$datos`.
#' @examples
#' d <- airquality[, 1:4]
#' tabla_perdidos(d)
#' @export
tabla_perdidos <- function(datos, variables = NULL, solo_con_perdidos = FALSE, etiquetas = NULL,
                           titulo = NULL, archivo = NULL) {
  if (inherits(datos, "lavaan"))
    stop("Para datos perdidos pasa el data.frame original: el modelo puede haber eliminado casos.", call. = FALSE)
  d <- as.data.frame(datos)
  if (!is.null(variables)) d <- d[, variables, drop = FALSE]
  N <- nrow(d); per <- vapply(d, function(x) sum(is.na(x)), numeric(1))
  v <- names(d); usar <- if (solo_con_perdidos) per > 0 else rep(TRUE, length(v))
  completos <- sum(stats::complete.cases(d))
  cel <- cbind(renombrar(v[usar], etiquetas), formatC(N - per[usar], format = "d"),
               formatC(per[usar], format = "d"), f_num(100 * per[usar] / N, 1))
  cel <- rbind(cel, c("Casos completos", formatC(completos, format = "d"),
                      formatC(N - completos, format = "d"), f_num(100 * (N - completos) / N, 1)))
  total <- 100 * sum(per) / (N * length(v))
  nota <- paste0("N = ", N, ". En la \u00faltima fila, casos sin ning\u00fan dato perdido y casos con al menos uno. ",
                 "Datos perdidos en total: ", f_num(total, 1), " % de las celdas. ",
                 if (sum(per) > 0) "Para estimar el modelo con todos los casos puede usarse m\u00e1xima verosimilitud con informaci\u00f3n completa (missing = \"fiml\" en lavaan)."
                 else "No hay datos perdidos.")
  t <- nueva_tabla(if (is.null(titulo)) "Datos perdidos por variable" else titulo,
                   c("Variable", "V\u00e1lidos", "Perdidos", "% perdidos"), cel, nota = nota,
                   separadores = sum(usar),
                   datos = data.frame(variable = v, validos = N - per, perdidos = per, porcentaje = 100 * per / N,
                                      row.names = NULL))
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

# --- Normalidad multivariada -----------------------------------------------------

#' @noRd
mardia_mv <- function(X) {
  X <- as.matrix(X[stats::complete.cases(X), , drop = FALSE]); n <- nrow(X); p <- ncol(X)
  Xc <- scale(X, scale = FALSE); S <- crossprod(Xc) / n
  D <- Xc %*% solve(S, t(Xc))
  b1 <- sum(D^3) / n^2; b2 <- sum(diag(D)^2) / n
  chi <- n * b1 / 6; gl <- p * (p + 1) * (p + 2) / 6
  z <- (b2 - p * (p + 2)) / sqrt(8 * p * (p + 2) / n)
  list(n = n, p = p, b1 = b1, chi = chi, gl = gl, p_asim = stats::pchisq(chi, gl, lower.tail = FALSE),
       b2 = b2, z = z, p_curt = 2 * stats::pnorm(-abs(z)))
}

#' Tabla de normalidad univariada y multivariada
#'
#' Evaluación de la normalidad como en la salida de AMOS ("Assessment of
#' normality"), que es la que suele pedirse para estimar por máxima
#' verosimilitud:
#'
#' * **Por variable:** mínimo, máximo, asimetría y curtosis con su razón
#'   crítica (c.r., un valor z): \eqn{z = asimetría / \sqrt{6/n}} y
#'   \eqn{z = curtosis / \sqrt{24/n}}. Un |z| mayor que 1.96 (p < .05; en la
#'   práctica, 2) o que 2.58 (p < .01) indica falta de normalidad.
#' * **Multivariada:** coeficiente de curtosis multivariada de Mardia (exceso
#'   sobre el valor esperado con normalidad) y su c.r. Un c.r. menor que 2 en
#'   valor absoluto sugiere normalidad multivariada.
#' * **Pruebas de Mardia** (1970) de asimetría y curtosis multivariadas, en la
#'   segunda parte de la tabla.
#'
#' Si no hay normalidad multivariada, conviene un estimador robusto
#' (`estimator = "MLR"` o `"MLM"`) o bootstrap.
#'
#' La asimetría y la curtosis de esta tabla se calculan como en AMOS (sin la
#' corrección por sesgo de SPSS que usa [tabla_descriptivos()]); con
#' muestras grandes la diferencia es mínima. Para Mardia se usa la covarianza
#' con denominador n (como el paquete `MVN`; `psych::mardia()` usa n - 1).
#' Se usan solo los casos completos.
#'
#' @inheritParams tabla_descriptivos
#' @param datos Un `data.frame` o un modelo de `lavaan` (se usan las
#'   variables observadas del modelo).
#' @return Un objeto `tabla_apa` con dos partes. Los valores están en
#'   `$datos$univariadas` y `$datos$mardia`.
#' @examples
#' library(lavaan)
#' tabla_normalidad(HolzingerSwineford1939, variables = paste0("x", 1:9))
#' @export
tabla_normalidad <- function(datos, variables = NULL, decimales = 2, titulo = NULL, archivo = NULL) {
  d <- obtener_datos(datos, variables); d <- d[stats::complete.cases(d), , drop = FALSE]
  if (ncol(d) < 2) stop("Se necesitan al menos dos variables.", call. = FALSE)
  n <- nrow(d); m <- mardia_mv(d)
  g1 <- vapply(d, function(x) { x <- x - mean(x); mean(x^3) / mean(x^2)^1.5 }, numeric(1))
  g2 <- vapply(d, function(x) { x <- x - mean(x); mean(x^4) / mean(x^2)^2 - 3 }, numeric(1))
  z1 <- g1 / sqrt(6 / n); z2 <- g2 / sqrt(24 / n)
  kmv <- m$b2 - m$p * (m$p + 2)
  cel <- cbind(names(d), f_num(vapply(d, min, numeric(1)), decimales), f_num(vapply(d, max, numeric(1)), decimales),
               f_num(g1, decimales), f_num(z1, 2), f_num(g2, decimales), f_num(z2, 2))
  cel <- rbind(cel, c("Multivariada (Mardia)", "", "", "", "", f_num(kmv, decimales), f_num(m$z, 2)))
  neg <- matrix(FALSE, nrow(cel), ncol(cel))
  neg[seq_len(ncol(d)), 5] <- abs(z1) > 1.96; neg[seq_len(ncol(d)), 7] <- abs(z2) > 1.96; neg[nrow(cel), 7] <- abs(m$z) > 1.96
  fz1 <- names(d)[abs(z1) > 1.96]; fz2 <- names(d)[abs(z2) > 1.96]
  nota <- paste0("c.r. = raz\u00f3n cr\u00edtica (z): asimetr\u00eda/\u221a(6/n) y curtosis/\u221a(24/n); la curtosis es el exceso sobre la normal. ",
                 "En negritas, |c.r.| > 1.96 (p < .05), que indica falta de normalidad; con p < .01 el l\u00edmite es 2.58. ",
                 "Multivariada: coeficiente de curtosis de Mardia y su c.r. N = ", n, " casos completos. ",
                 if (length(fz1)) paste0("Asimetr\u00eda significativa en ", paste(fz1, collapse = ", "), ". ") else "",
                 if (length(fz2)) paste0("Curtosis significativa en ", paste(fz2, collapse = ", "), ". ") else "",
                 if (abs(m$z) < 1.96) "El c.r. multivariado sugiere normalidad multivariada."
                 else "El c.r. multivariado indica que no hay normalidad multivariada; se recomienda un estimador robusto (MLR o MLM) o bootstrap.")
  cel2 <- rbind(c("Asimetr\u00eda multivariada", f_num(m$b1, decimales), paste0("\u03c7\u00b2 = ", f_num(m$chi, 2)),
                  formatC(m$gl, format = "d"), f_p(m$p_asim)),
                c("Curtosis multivariada", f_num(m$b2, decimales), paste0("z = ", f_num(m$z, 2)), "", f_p(m$p_curt)))
  normal <- m$p_asim >= .05 && m$p_curt >= .05
  nota2 <- paste0("Pruebas de Mardia (1970) con ", m$n, " casos y ", m$p, " variables. ",
                  if (normal) "No se rechaza la normalidad multivariada."
                  else "Se rechaza la normalidad multivariada; se recomienda un estimador robusto (MLR o MLM) o bootstrap.")
  t <- nueva_tabla(if (is.null(titulo)) "Evaluaci\u00f3n de la normalidad" else titulo,
                   c("Variable", "M\u00edn.", "M\u00e1x.", "Asimetr\u00eda", "c.r.", "Curtosis", "c.r."), cel, neg,
                   nota = nota, separadores = ncol(d),
                   datos = list(univariadas = data.frame(variable = names(d), asimetria = g1, cr_asimetria = z1,
                                                         curtosis = g2, cr_curtosis = z2, row.names = NULL),
                                mardia = data.frame(prueba = c("asimetria", "curtosis"), coeficiente = c(m$b1, m$b2),
                                                    estadistico = c(m$chi, m$z), gl = c(m$gl, NA), p = c(m$p_asim, m$p_curt),
                                                    curtosis_exceso = c(NA, kmv))),
                   extra = list(nueva_tabla("Pruebas de normalidad multivariada de Mardia",
                                            c("Prueba", "Coeficiente", "Estad\u00edstico", "gl", "p"), cel2, nota = nota2)))
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

# --- Confiabilidad -----------------------------------------------------------------

#' @noRd
alfa_cronbach <- function(X) {
  X <- as.matrix(X[stats::complete.cases(X), , drop = FALSE]); k <- ncol(X)
  C <- stats::cov(X); a <- k / (k - 1) * (1 - sum(diag(C)) / sum(C))
  total <- rowSums(X)
  rit <- vapply(seq_len(k), function(i) stats::cor(X[, i], total - X[, i]), numeric(1))
  list(alfa = a, n = nrow(X), k = k, rit = stats::setNames(rit, colnames(X)))
}

# Intervalo de confianza de Feldt para alfa
#' @noRd
ic_feldt <- function(a, n, k, nivel = .95) {
  g1 <- n - 1; g2 <- (n - 1) * (k - 1); q <- (1 - nivel) / 2
  c(1 - (1 - a) * stats::qf(1 - q, g1, g2), 1 - (1 - a) * stats::qf(q, g1, g2))
}

# Omega de McDonald para la suma de los ítems (cargas sin estandarizar)
#' @noRd
omega_fit <- function(fit, f, items) {
  pe <- lavaan::parameterEstimates(fit, se = FALSE)
  if ("group" %in% names(pe)) pe <- pe[pe$group == 1, ]
  psi <- lavaan::lavInspect(fit, "cov.lv"); if (is.list(psi)) psi <- psi[[1]]
  l <- pe$est[pe$op == "=~" & pe$lhs == f & pe$rhs %in% items]
  th <- pe$est[pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs %in% items]
  rc <- pe$est[pe$op == "~~" & pe$lhs != pe$rhs & pe$lhs %in% items & pe$rhs %in% items]
  v <- sum(l)^2 * psi[f, f]
  v / (v + sum(th) + 2 * sum(rc))
}

#' Tabla de confiabilidad (alfa y omega)
#'
#' Para cada factor: número de ítems, alfa de Cronbach con su intervalo de
#' confianza al 95 % (Feldt), omega de McDonald y el rango de las
#' correlaciones ítem-total corregidas.
#'
#' * Con un modelo de `lavaan` (AFC o SEM), los factores y sus ítems se toman
#'   del modelo y omega se calcula con sus cargas (incluye las covarianzas
#'   entre errores, si las hay). Es la confiabilidad de la suma de los ítems,
#'   igual que `semTools::compRelSEM(fit, obs.var = FALSE)`.
#' * Con un `data.frame`, indique los ítems de cada factor en `factores`;
#'   omega se calcula con un AFC de un factor para cada uno (se necesitan al
#'   menos 3 ítems).
#'
#' @param x Modelo de `lavaan` o `data.frame`.
#' @param factores Solo con `data.frame`: lista con los ítems de cada factor,
#'   p. ej. `list(Visual = c("x1", "x2", "x3"), Verbal = c("x4", "x5", "x6"))`.
#' @inheritParams tabla_descriptivos
#' @return Un objeto `tabla_apa`. Los valores están en `$datos`.
#' @examples
#' library(lavaan)
#' fit <- cfa("visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6", data = HolzingerSwineford1939)
#' tabla_confiabilidad(fit)
#'
#' tabla_confiabilidad(HolzingerSwineford1939,
#'                     factores = list(Visual = c("x1", "x2", "x3"),
#'                                     Velocidad = c("x7", "x8", "x9")))
#' @export
tabla_confiabilidad <- function(x, factores = NULL, etiquetas = NULL, decimales = 2,
                                titulo = NULL, archivo = NULL) {
  if (inherits(x, "lavaan")) {
    s <- lavaan::standardizedSolution(x)
    if ("group" %in% names(s)) s <- s[s$group == 1, ]
    lv <- lavaan::lavNames(x, "lv"); L <- s[s$op == "=~" & !s$rhs %in% lv, ]
    if (!nrow(L)) stop("El modelo no tiene factores con \u00edtems.", call. = FALSE)
    factores <- split(L$rhs, factor(L$lhs, levels = unique(L$lhs)))
    d <- obtener_datos(x)
    om <- vapply(names(factores), function(f) omega_fit(x, f, factores[[f]]), numeric(1))
  } else {
    if (is.null(factores) || !is.list(factores) || is.null(names(factores)))
      stop("Con un data.frame indica los \u00edtems de cada factor: factores = list(F1 = c(...), F2 = c(...)).", call. = FALSE)
    d <- obtener_datos(x, unique(unlist(factores)))
    om <- vapply(names(factores), function(f) {
      it <- factores[[f]]; if (length(it) < 3) return(NA_real_)
      m <- tryCatch(lavaan::cfa(paste("F =~", paste(it, collapse = " + ")), data = d, missing = "listwise"),
                    error = function(e) NULL)
      if (is.null(m)) NA_real_ else omega_fit(m, "F", it)
    }, numeric(1))
  }
  res <- lapply(names(factores), function(f) alfa_cronbach(d[, factores[[f]], drop = FALSE]))
  a <- vapply(res, `[[`, numeric(1), "alfa"); k <- vapply(res, `[[`, numeric(1), "k")
  ic <- t(vapply(res, function(r) ic_feldt(r$alfa, r$n, r$k), numeric(2)))
  rmin <- vapply(res, function(r) min(r$rit), numeric(1)); rmax <- vapply(res, function(r) max(r$rit), numeric(1))
  negativos <- unlist(lapply(res, function(r) names(r$rit)[r$rit < 0]))
  if (length(negativos))
    warning("Correlaci\u00f3n \u00edtem-total negativa en: ", paste(negativos, collapse = ", "),
            ". \u00bfHay \u00edtems redactados en sentido inverso que deban recodificarse?", call. = FALSE)
  nf <- renombrar(names(factores), etiquetas)
  cel <- cbind(nf, formatC(k, format = "d"), f_apa(a, decimales),
               paste0("[", f_apa(ic[, 1], decimales), ", ", f_apa(ic[, 2], decimales), "]"),
               ifelse(is.na(om), "\u2014", f_apa(om, decimales)),
               paste0(f_apa(rmin, decimales), "\u2013", f_apa(rmax, decimales)))
  nota <- paste0("\u03b1 = alfa de Cronbach; IC 95 % de Feldt; \u03c9 = omega de McDonald",
                 if (inherits(x, "lavaan")) ", calculado con las cargas del modelo" else ", calculado con un AFC de un factor",
                 "; r \u00edtem-total = correlaci\u00f3n \u00edtem-total corregida (m\u00ednima\u2013m\u00e1xima). ",
                 "Valores de \u03b1 y \u03c9 \u2265 .70 suelen considerarse aceptables (Nunnally y Bernstein, 1994) y ",
                 "una correlaci\u00f3n \u00edtem-total \u2265 .30 adecuada.",
                 if (any(is.na(om))) " \u2014 = omega no calculable con menos de 3 \u00edtems." else "",
                 " n = ", min(vapply(res, `[[`, numeric(1), "n")), if (length(unique(vapply(res, `[[`, numeric(1), "n"))) > 1) " o m\u00e1s" else "",
                 " casos completos.")
  t <- nueva_tabla(if (is.null(titulo)) "Confiabilidad de las escalas" else titulo,
                   c("Factor", "\u00cdtems", "\u03b1", "IC 95 %", "\u03c9", "r \u00edtem-total"), cel, nota = nota,
                   datos = data.frame(factor = names(factores), items = k, alfa = a, ic_inf = ic[, 1], ic_sup = ic[, 2],
                                      omega = om, rit_min = rmin, rit_max = rmax, row.names = NULL))
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}
