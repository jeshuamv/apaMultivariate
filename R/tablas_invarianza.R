# Invarianza de medición entre grupos

NIVELES_INV <- c(configural = "Configural", metrica = "M\u00e9trica", escalar = "Escalar", estricta = "Estricta")

#' @noRd
nombres_niveles <- function() if (en()) c(configural = "Configural", metrica = "Metric", escalar = "Scalar", estricta = "Strict") else NIVELES_INV

# Parámetros cuya igualdad entre grupos más empeora el ajuste (pruebas de score)
#' @noRd
sugerir_liberar <- function(fit, nivel, cuantos = 3) {
  ts <- tryCatch(suppressWarnings(lavaan::lavTestScore(fit)$uni), error = function(e) NULL)
  if (is.null(ts) || !nrow(ts)) return(character(0))
  pt <- lavaan::parTable(fit)
  par_de <- function(pl) { i <- which(pt$plabel == pl)[1]; if (is.na(i)) return(NA_character_)
    op <- pt$op[i]
    if (op == "~1") paste(pt$lhs[i], "~ 1") else if (op == "|") paste0(pt$lhs[i], " | ", pt$rhs[i]) else paste(pt$lhs[i], op, pt$rhs[i]) }
  ops <- switch(nivel, metrica = "=~", escalar = c("~1", "|"), estricta = "~~", character(0))
  p <- vapply(ts$lhs, par_de, character(1))
  op_de <- vapply(ts$lhs, function(pl) { i <- which(pt$plabel == pl)[1]; if (is.na(i)) "" else pt$op[i] }, character(1))
  ok <- !is.na(p) & op_de %in% ops
  if (!any(ok)) return(character(0))
  d <- data.frame(par = p[ok], chi = ts$X2[ok], stringsAsFactors = FALSE)
  d <- stats::aggregate(chi ~ par, d, max)
  d <- d[order(-d$chi), ]
  utils::head(d$par[d$chi > stats::qchisq(.95, 1)], cuantos)
}

#' Tabla de invarianza de medición
#'
#' Prueba si un instrumento mide lo mismo en distintos grupos (por ejemplo,
#' hombres y mujeres, o dos países) ajustando una secuencia de modelos cada
#' vez más restringidos y comparando cada uno con el anterior:
#'
#' * **Configural:** misma estructura (mismos factores e ítems) en todos los
#'   grupos.
#' * **Métrica (débil):** además, cargas iguales. Permite comparar
#'   correlaciones y efectos entre grupos.
#' * **Escalar (fuerte):** además, interceptos (o umbrales, con ítems
#'   ordinales) iguales. Permite comparar las medias de los factores.
#' * **Estricta:** además, varianzas de los errores iguales.
#'
#' **Criterios** (`criterio`):
#' * `"chen"` (por omisión): no se sostiene la invarianza si el CFI baja más
#'   de .010 y además el RMSEA sube más de .015 o el SRMR sube más de .030 (en
#'   la métrica) o de .010 (en la escalar y la estricta) (Chen, 2007).
#' * `"cfi"`: solo el cambio en el CFI (\eqn{\Delta}CFI < \eqn{-}.010; Cheung y
#'   Rensvold, 2002).
#' * `"chi2"`: prueba \eqn{\Delta\chi^2} (p < .05). Es muy sensible al tamaño
#'   de la muestra.
#'
#' Si un nivel no se sostiene, la nota indica qué parámetros conviene liberar
#' (según pruebas de multiplicadores de Lagrange) para probar la
#' **invarianza parcial**, con el argumento `liberar`, p. ej.
#' `liberar = c("x3 ~ 1")` libera el intercepto del ítem x3.
#'
#' Con ítems ordinales (`ordered = TRUE`) se igualan los umbrales en lugar
#' de los interceptos. Para la identificación propuesta por Wu y Estabrook
#' (2016) puede usarse `semTools::measEq.syntax()`.
#'
#' @param modelo Sintaxis del modelo de `lavaan` (texto) o un modelo ya
#'   estimado con `cfa()` (se toma su sintaxis y su estimador).
#' @param datos `data.frame` con los ítems y la variable de grupo.
#' @param grupo Nombre de la variable que define los grupos.
#' @param niveles Niveles que se prueban, en orden.
#' @param criterio `"chen"`, `"cfi"` o `"chi2"`.
#' @param liberar Parámetros que se dejan libres entre grupos (invarianza
#'   parcial), con la sintaxis de `group.partial` de `lavaan`.
#' @param ... Otros argumentos para [lavaan::cfa()], p. ej.
#'   `estimator = "MLR"` u `ordered = TRUE`.
#' @param titulo Título de la tabla.
#' @param archivo Si se indica, guarda la tabla.
#' @return Un objeto `tabla_apa`. Los valores están en `$datos` y los
#'   modelos estimados en `$modelos`.
#' @examples
#' library(lavaan)
#' modelo <- "visual =~ x1 + x2 + x3
#'            textual =~ x4 + x5 + x6
#'            speed =~ x7 + x8 + x9"
#' tabla_invarianza(modelo, HolzingerSwineford1939, grupo = "school")
#' @export
tabla_invarianza <- function(modelo, datos, grupo, niveles = c("configural", "metrica", "escalar", "estricta"),
                             criterio = c("chen", "cfi", "chi2"), liberar = NULL, ..., titulo = NULL, archivo = NULL) {
  criterio <- match.arg(criterio)
  niveles <- match.arg(niveles, names(NIVELES_INV), several.ok = TRUE)
  niveles <- names(NIVELES_INV)[names(NIVELES_INV) %in% niveles]
  if (length(niveles) < 2) stop("Indica al menos dos niveles para comparar.", call. = FALSE)
  args <- list(...)
  if (inherits(modelo, "lavaan")) {
    sint <- tryCatch(eval(modelo@call$model, parent.frame()), error = function(e) NULL)
    if (!is.character(sint)) stop("No se pudo recuperar la sintaxis del modelo; p\u00e1sala como texto.", call. = FALSE)
    op <- lavaan::lavInspect(modelo, "options")
    if (is.null(args$estimator)) args$estimator <- op$estimator
    if (is.null(args$missing)) args$missing <- op$missing
    if (is.null(args$test) && !identical(op$test, "standard")) args$test <- op$test
    if (is.null(args$se) && !identical(op$se, "standard")) args$se <- op$se
    modelo <- sint
  }
  if (!is.character(modelo)) stop("`modelo` debe ser la sintaxis de lavaan (texto) o un modelo de cfa().", call. = FALSE)
  if (!is.data.frame(datos) || !grupo %in% names(datos)) stop("`grupo` debe ser una columna de `datos`.", call. = FALSE)
  datos <- datos[!is.na(datos[[grupo]]), , drop = FALSE]
  ajustar <- function(eq) do.call("cfa", envir = asNamespace("lavaan"), c(list(model = modelo, data = datos, group = grupo,
                                                       group.equal = eq, group.partial = liberar), args), quote = FALSE)
  conf <- ajustar(character(0))
  ordinal <- isTRUE(lavaan::lavInspect(conf, "categorical"))
  restr <- list(configural = character(0), metrica = "loadings",
                escalar = c("loadings", if (ordinal) "thresholds" else "intercepts"),
                estricta = c("loadings", if (ordinal) "thresholds" else "intercepts", "residuals"))
  fits <- lapply(niveles, function(nv) if (nv == "configural") conf else ajustar(restr[[nv]]))
  names(fits) <- niveles
  I <- as.data.frame(do.call(rbind, lapply(fits, indices)))
  k <- length(fits); D <- matrix(NA_real_, k, 6, dimnames = list(NULL, c("dchi", "dgl", "p", "dcfi", "drmsea", "dsrmr")))
  sostiene <- rep(NA, k)
  for (i in 2:k) {
    D[i, 1:3] <- diferencia_chi(fits[[i]], fits[[i - 1]])
    D[i, 4] <- I$cfi[i] - I$cfi[i - 1]; D[i, 5] <- I$rmsea[i] - I$rmsea[i - 1]; D[i, 6] <- I$srmr[i] - I$srmr[i - 1]
    lim_srmr <- if (niveles[i] == "metrica") .030 else .010
    sostiene[i] <- switch(criterio,
      chen = !(D[i, 4] < -.010 & (D[i, 5] > .015 | D[i, 6] > lim_srmr)),
      cfi  = !(D[i, 4] < -.010),
      chi2 = is.na(D[i, 3]) || D[i, 3] >= .05)
  }
  f_d <- function(x, d = 3) { r <- round(x, d); r[!is.na(r) & r == 0] <- 0
    ifelse(is.na(x), "", sub("^([+-]?)0\\.", "\\1.", formatC(r, format = "f", digits = d, flag = "+"))) }
  # "parcial" solo desde el nivel cuyo tipo de parámetro se liberó
  desde <- if (is.null(liberar)) Inf else min(vapply(liberar, function(l)
    if (grepl("=~", l, fixed = TRUE)) 2 else if (grepl("~ *1$|\\|", l)) 3 else if (grepl("~~", l, fixed = TRUE)) 4 else 2, numeric(1)))
  NV <- nombres_niveles()
  nom <- paste0(NV[niveles], ifelse(match(niveles, names(NIVELES_INV)) >= desde, tx(" parcial", " (partial)"), ""))
  cel <- cbind(nom, f_num(I$chisq, 2), formatC(I$df, format = "d"), f_apa(I$cfi, 3), f_apa(I$rmsea, 3), f_apa(I$srmr, 3),
               ifelse(is.na(D[, 1]), "", paste0(f_num(D[, 1], 2), " (", formatC(D[, 2], format = "d"), ")")), f_p(D[, 3]),
               f_d(D[, 4]), f_d(D[, 5]), f_d(D[, 6]),
               ifelse(is.na(sostiene), "\u2014", ifelse(sostiene, tx("Se sostiene", "Supported"), tx("No se sostiene", "Not supported"))))
  cel[1, 7:11] <- "\u2014"
  # Nivel más alto sostenido (en orden, hasta el primero que falla)
  falla <- which(!is.na(sostiene) & !sostiene)
  hasta <- if (length(falla)) falla[1] - 1 else k
  # los niveles posteriores a una falla no se interpretan
  if (length(falla) && falla[1] < k) { cel[(falla[1] + 1):k, 12] <- tx("No se eval\u00faa", "Not tested"); sostiene[(falla[1] + 1):k] <- NA }
  ng <- lavaan::lavInspect(conf, "ngroups"); nn <- unlist(lavaan::lavInspect(conf, "nobs"))
  gn <- lavaan::lavInspect(conf, "group.label")
  sug <- if (length(falla)) sugerir_liberar(fits[[falla[1]]], niveles[falla[1]]) else character(0)
  crit_txt <- switch(criterio,
    chen = tx("No se sostiene la invarianza si \u0394CFI < \u2212.010 junto con \u0394RMSEA > .015 o \u0394SRMR > .030 (m\u00e9trica) o > .010 (escalar y estricta) (Chen, 2007).",
              "Invariance is not supported if \u0394CFI < \u2212.010 together with \u0394RMSEA > .015 or \u0394SRMR > .030 (metric) or > .010 (scalar and strict) (Chen, 2007)."),
    cfi = tx("No se sostiene la invarianza si \u0394CFI < \u2212.010 (Cheung y Rensvold, 2002).", "Invariance is not supported if \u0394CFI < \u2212.010 (Cheung & Rensvold, 2002)."),
    chi2 = tx("No se sostiene la invarianza si la prueba \u0394\u03c7\u00b2 es significativa (p < .05).", "Invariance is not supported if the \u0394\u03c7\u00b2 test is significant (p < .05)."))
  nota <- paste0(tx("Grupos: ", "Groups: "), paste0(gn, " (n = ", nn, ")", collapse = ", "),
                 tx(". Cada modelo se compara con el anterior. ", ". Each model is compared with the previous one. "),
                 if (any(as.logical(I$robusto))) tx("\u00cdndices y \u0394\u03c7\u00b2 corregidos del estimador robusto. ", "Robust (scaled) indices and \u0394\u03c7\u00b2. ") else "",
                 crit_txt, " ",
                 if (!is.null(liberar)) paste0(tx("Par\u00e1metros liberados (invarianza parcial): ", "Freed parameters (partial invariance): "), paste(liberar, collapse = ", "), ". ") else "",
                 if (hasta == k) (if (en()) paste0(NV[niveles[k]], " invariance is supported.") else paste0("Se sostiene la invarianza ", tolower(NV[niveles[k]]), "."))
                 else paste0(tx("Nivel m\u00e1s alto sostenido: ", "Highest level supported: "), tolower(NV[niveles[hasta]]), ". ",
                             if (length(sug)) paste0(tx("Para probar invarianza parcial, los par\u00e1metros cuya igualdad m\u00e1s empeora el ajuste son, en orden: ",
                                                        "To test partial invariance, the parameters whose equality constraints most worsen fit are, in order: "),
                                                     paste(sug, collapse = "; "),
                                                     tx(". Lib\u00e9relos de uno en uno, empezando por el primero (liberar = \"", ". Free them one at a time, starting with the first (liberar = \""),
                                                     sug[1], tx("\"), y vuelva a revisar la tabla.", "\"), and re-examine the table."))
                             else ""))
  t <- nueva_tabla(if (is.null(titulo)) tx("Invarianza de medici\u00f3n entre grupos", "Measurement invariance across groups") else titulo,
                   c(tx("Modelo", "Model"), "\u03c7\u00b2", tx("gl", "df"), "CFI", "RMSEA", "SRMR", tx("\u0394\u03c7\u00b2 (\u0394gl)", "\u0394\u03c7\u00b2 (\u0394df)"), "p",
                     "\u0394CFI", "\u0394RMSEA", "\u0394SRMR", tx("Decisi\u00f3n", "Decision")),
                   cel, nota = nota, datos = cbind(nivel = niveles, I, D, sostiene = sostiene))
  t$modelos <- fits; t$sugerencias <- sug
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}
