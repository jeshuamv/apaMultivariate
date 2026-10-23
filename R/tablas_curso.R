# Tablas de evaluación del modelo con los criterios habituales en cursos y
# en las salidas de AMOS: criterios de ajuste e identificación, estimaciones,
# hipótesis, índices de modificación

# --- Criterios de ajuste e identificación ----------------------------------------

#' @noRd
evalua <- function(v, exc, acep, mayor = TRUE) {
  if (is.na(v)) return("")
  ok <- function(c) if (mayor) v >= c else v <= c
  if (!is.na(exc) && ok(exc)) "Excelente" else if (ok(acep)) "Aceptable" else "No aceptable"
}

#' Tabla de criterios de ajuste e identificación del modelo
#'
#' Evalúa cada índice de ajuste contra dos puntos de corte (excelente y
#' aceptable), con la fuente de cada criterio, y resume la identificación del
#' modelo: observaciones (\eqn{p}, elementos no redundantes de la matriz de
#' covarianzas), parámetros estimados (\eqn{q}), grados de libertad
#' (\eqn{gl = p - q}) y razón entre el tamaño de la muestra y los parámetros.
#'
#' Criterios usados:
#'
#' | Índice | Excelente | Aceptable | Fuente |
#' |---|---|---|---|
#' | \eqn{\chi^2} | p > .05 | | Hair et al. (2014); Kline (2011) |
#' | \eqn{\chi^2}/gl | \eqn{\le} 2 | \eqn{\le} 3 | Hair et al. (2014) |
#' | RMSEA | \eqn{\le} .05 | \eqn{\le} .06 | Kline (2011); Hu y Bentler (1999) |
#' | SRMR | \eqn{\le} .08 | \eqn{\le} .10 | Hu y Bentler (1999); Hair et al. (2014) |
#' | CFI, TLI | \eqn{\ge} .95 | \eqn{\ge} .90 | Hu y Bentler (1999); Hair et al. (2014) |
#' | GFI | \eqn{\ge} .95 | \eqn{\ge} .90 | Hair et al. (2014) |
#' | PNFI, PRATIO | | \eqn{\ge} .50 | Mulaik et al. (1989) |
#'
#' Con estimadores robustos se usan las versiones corregidas de
#' \eqn{\chi^2}, RMSEA, CFI y TLI.
#'
#' @param fit Modelo de `lavaan`.
#' @param indices Índices que se incluyen. Kline (2011) recomienda reportar
#'   siempre \eqn{\chi^2}, RMSEA, GFI, CFI y SRMR.
#' @param titulo Título de la tabla.
#' @param archivo Si se indica, guarda la tabla.
#' @return Un objeto `tabla_apa` con dos partes (ajuste e identificación).
#'   Los valores están en `$datos`.
#' @examples
#' library(lavaan)
#' fit <- cfa("visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6", data = HolzingerSwineford1939)
#' tabla_criterios_ajuste(fit)
#' @export
tabla_criterios_ajuste <- function(fit, indices = c("chi2", "chi2gl", "rmsea", "srmr", "cfi", "tli", "gfi", "pnfi", "pratio"),
                                   titulo = NULL, archivo = NULL) {
  if (!inherits(fit, "lavaan")) stop("`fit` debe ser un modelo de lavaan.", call. = FALSE)
  indices <- match.arg(indices, several.ok = TRUE)
  I <- indices(fit); fm <- lavaan::fitMeasures(fit)
  gfi <- if ("gfi" %in% names(fm)) fm[["gfi"]] else NA
  bdf <- if ("baseline.df" %in% names(fm)) fm[["baseline.df"]] else NA
  pratio <- I[["df"]] / bdf
  pnfi <- if ("pnfi" %in% names(fm)) fm[["pnfi"]] else NA
  f3 <- function(x) f_apa(x, 3)
  filas <- list(
    chi2 = c("\u03c7\u00b2", paste0(f_num(I[["chisq"]], 2), " (gl = ", formatC(I[["df"]], format = "d"), ", p ",
                                    if (is.na(I[["pvalue"]])) "no calculable" else if (I[["pvalue"]] < .001) "< .001" else paste("=", f_apa(I[["pvalue"]], 3)), ")"),
             "p > .05", "", if (is.na(I[["pvalue"]])) "" else if (I[["pvalue"]] > .05) "Adecuado" else "No adecuado",
             "Hair et al. (2014); Kline (2011)"),
    chi2gl = c("\u03c7\u00b2/gl", f_num(I[["chisq"]] / I[["df"]], 2), "\u2264 2", "\u2264 3",
               evalua(I[["chisq"]] / I[["df"]], 2, 3, FALSE), "Hair et al. (2014)"),
    rmsea = c("RMSEA [IC 90 %]", paste0(f3(I[["rmsea"]]), " [", f3(I[["lo"]]), ", ", f3(I[["hi"]]), "]"),
              "\u2264 .05", "\u2264 .06", evalua(I[["rmsea"]], .05, .06, FALSE), "Kline (2011); Hu y Bentler (1999)"),
    srmr = c("SRMR", f3(I[["srmr"]]), "\u2264 .08", "\u2264 .10", evalua(I[["srmr"]], .08, .10, FALSE),
             "Hu y Bentler (1999); Hair et al. (2014)"),
    cfi = c("CFI", f3(I[["cfi"]]), "\u2265 .95", "\u2265 .90", evalua(I[["cfi"]], .95, .90), "Hu y Bentler (1999); Hair et al. (2014)"),
    tli = c("TLI (NNFI)", f3(I[["tli"]]), "\u2265 .95", "\u2265 .90", evalua(I[["tli"]], .95, .90), "Tabachnick y Fidell (2013)"),
    gfi = c("GFI", f3(gfi), "\u2265 .95", "\u2265 .90", evalua(gfi, .95, .90), "Hair et al. (2014)"),
    pnfi = c("PNFI", f3(pnfi), "", "\u2265 .50", evalua(pnfi, NA, .50), "Mulaik et al. (1989)"),
    pratio = c("PRATIO", f3(pratio), "", "\u2265 .50", evalua(pratio, NA, .50), "Mulaik et al. (1989)"))
  cel <- do.call(rbind, filas[indices])
  nota <- paste0(if (as.logical(I[["robusto"]])) "Se usan los \u00edndices corregidos del estimador robusto (el GFI no tiene versi\u00f3n corregida). " else "",
                 "RMSEA = error cuadr\u00e1tico medio de aproximaci\u00f3n; SRMR = ra\u00edz del residuo cuadr\u00e1tico medio estandarizado; ",
                 "CFI = \u00edndice de ajuste comparativo; TLI = \u00edndice de Tucker-Lewis; GFI = \u00edndice de bondad de ajuste; ",
                 "PNFI = \u00edndice de ajuste normado parsimonioso; PRATIO = raz\u00f3n de parsimonia. La prueba \u03c7\u00b2 es sensible al ",
                 "tama\u00f1o de la muestra; con muestras grandes suele ser significativa aunque el ajuste sea bueno.")
  # Identificación
  q <- fm[["npar"]]; gl <- fm[["df"]]; p <- q + gl; n <- lavaan::lavInspect(fit, "ntotal")
  ident <- if (gl > 0) "Sobreidentificado" else if (gl == 0) "Exactamente identificado" else "Subidentificado"
  razon <- n / q
  cel2 <- rbind(c("Observaciones (p)", formatC(p, format = "d"), "Elementos no redundantes de la matriz de covarianzas"),
                c("Par\u00e1metros estimados (q)", formatC(q, format = "d"), ""),
                c("Grados de libertad (p \u2212 q)", formatC(gl, format = "d"), ident),
                c("Tama\u00f1o de la muestra (N)", formatC(n, format = "d"), ""),
                c("Raz\u00f3n N:q", paste0(f_num(razon, 1), ":1"),
                  if (razon >= 20) "Ideal (\u2265 20:1)" else if (razon >= 10) "Aceptable (\u2265 10:1)" else "Baja (< 10:1)"))
  nota2 <- paste0("gl > 0: modelo sobreidentificado (el deseable); gl = 0: exactamente identificado (\u03c7\u00b2 = 0, el ajuste no puede ",
                  "evaluarse); gl < 0: subidentificado (no puede estimarse). Raz\u00f3n N:q: se recomiendan al menos 10 casos por ",
                  "par\u00e1metro y, de preferencia, 20 (Kline, 2011).")
  t <- nueva_tabla(if (is.null(titulo)) "Evaluaci\u00f3n del ajuste del modelo" else titulo,
                   c("\u00cdndice", "Valor", "Excelente", "Aceptable", "Evaluaci\u00f3n", "Fuente"), cel,
                   alinear = c("l", "c", "c", "c", "c", "l"), nota = nota,
                   datos = list(indices = c(I, gfi = gfi, pnfi = pnfi, pratio = pratio),
                                identificacion = c(p = p, q = q, gl = gl, n = n, razon = razon)),
                   extra = list(nueva_tabla("Identificaci\u00f3n del modelo y tama\u00f1o de la muestra", c("Elemento", "Valor", "Interpretaci\u00f3n"),
                                            cel2, alinear = c("l", "c", "l"), nota = nota2)))
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

# --- Estimaciones al estilo de AMOS --------------------------------------------------

#' Tabla de estimaciones del modelo
#'
#' Todas las estimaciones del modelo, agrupadas como en la salida de AMOS:
#' pesos de regresión (efectos), cargas factoriales, covarianzas, varianzas de
#' las variables exógenas y varianzas de los errores, con la estimación no
#' estandarizada (B), su error estándar (EE), la razón crítica (C.R. =
#' B/EE, que se compara con \eqn{\pm}1.96), el valor *p* y la estimación
#' estandarizada (\eqn{\beta}, o la correlación en las covarianzas).
#'
#' @param fit Modelo de `lavaan`.
#' @param varianzas Incluir varianzas y varianzas de error.
#' @inheritParams tabla_descriptivos
#' @return Un objeto `tabla_apa`. Los valores están en `$datos`.
#' @examples
#' library(lavaan)
#' fit <- sem("ind60 =~ x1 + x2 + x3
#'             dem60 =~ y1 + y2 + y3 + y4
#'             dem60 ~ ind60", data = PoliticalDemocracy)
#' tabla_parametros(fit)
#' @export
tabla_parametros <- function(fit, varianzas = TRUE, etiquetas = NULL, decimales = 2, titulo = NULL, archivo = NULL) {
  if (!inherits(fit, "lavaan")) stop("`fit` debe ser un modelo de lavaan.", call. = FALSE)
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)
  if ("group" %in% names(pe)) pe <- pe[pe$group == 1, ]
  et <- function(v) renombrar(v, etiquetas)
  endo <- unique(c(pe$lhs[pe$op == "~"], pe$rhs[pe$op == "=~"]))
  fila <- function(r, nombre) c(paste0("   ", nombre), f_num(r$est, decimales),
                                if (is.na(r$se) || r$se == 0) "\u2014" else f_num(r$se, decimales),
                                if (is.na(r$z)) "\u2014" else f_num(r$z, 2),
                                if (is.na(r$pvalue)) "\u2014" else f_p(r$pvalue), f_apa(r$std.all, decimales))
  secciones <- list(
    "Pesos de regresi\u00f3n" = list(pe$op == "~", function(r) paste0(et(r$lhs), " \u2190 ", et(r$rhs))),
    "Cargas factoriales" = list(pe$op == "=~", function(r) paste0(et(r$rhs), " \u2190 ", et(r$lhs))),
    "Covarianzas (correlaci\u00f3n en \u03b2)" = list(pe$op == "~~" & pe$lhs != pe$rhs, function(r) paste0(et(r$lhs), " \u2194 ", et(r$rhs))),
    "Varianzas de las variables ex\u00f3genas" = list(varianzas & pe$op == "~~" & pe$lhs == pe$rhs & !pe$lhs %in% endo,
                                                     function(r) et(r$lhs)),
    "Varianzas de los errores (residuales)" = list(varianzas & pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs %in% endo,
                                                   function(r) paste("Error de", et(r$lhs))),
    "Par\u00e1metros definidos" = list(pe$op == ":=", function(r) r$lhs))
  cel <- NULL; sep <- integer(0); orden <- integer(0)
  for (s in names(secciones)) {
    idx <- which(secciones[[s]][[1]]); if (!length(idx)) next
    cel <- rbind(cel, c(s, rep("", 5)), t(vapply(idx, function(i) fila(pe[i, ], secciones[[s]][[2]](pe[i, ])), character(6))))
    sep <- c(sep, nrow(cel)); orden <- c(orden, idx)
  }
  sep <- utils::head(sep, -1)
  nota <- paste0("B = estimaci\u00f3n no estandarizada; EE = error est\u00e1ndar; C.R. = raz\u00f3n cr\u00edtica (B/EE); ",
                 "|C.R.| > 1.96 indica un par\u00e1metro significativo (p < .05); \u03b2 = estimaci\u00f3n estandarizada ",
                 "(en las covarianzas, la correlaci\u00f3n; en los errores, la proporci\u00f3n de varianza no explicada, 1 \u2212 R\u00b2). ",
                 "\u2014 = par\u00e1metro fijo (por ejemplo, la carga de referencia fijada en 1). N = ", lavaan::lavInspect(fit, "ntotal"), ".")
  t <- nueva_tabla(if (is.null(titulo)) "Estimaciones del modelo" else titulo,
                   c("Par\u00e1metro", "B", "EE", "C.R.", "p", "\u03b2"), cel, nota = nota, separadores = sep,
                   datos = pe[orden, c("lhs", "op", "rhs", "est", "se", "z", "pvalue", "std.all")])
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

# --- Hipótesis -------------------------------------------------------------------------

#' Tabla de contraste de hipótesis
#'
#' Resume la decisión sobre cada hipótesis del modelo: el coeficiente, su
#' significancia y si la hipótesis se sostiene o se rechaza.
#'
#' Cada hipótesis se escribe como una trayectoria con flechas: `"X -> Y"`
#' para un efecto directo o `"X -> M -> Y"` para un efecto indirecto
#' (mediación). Si la hipótesis predice un sentido, agréguelo al final:
#' `"X -> Y (+)"` o `"X -> Y (-)"`; entonces se sostiene solo si el efecto es
#' significativo **y** tiene ese signo.
#'
#' Los efectos directos usan el valor *p* de `lavaan`. Los indirectos se
#' evalúan con el intervalo de confianza de [tabla_efectos()] (bootstrap o
#' método delta): son significativos si el intervalo no incluye el 0.
#'
#' @param fit Modelo de `lavaan`.
#' @param hipotesis Vector con nombres, p. ej.
#'   `c(H1 = "SelfEfficacy -> Flow (+)", H2 = "LMS -> Flow -> Satisfaction")`.
#' @param bootstrap Remuestreos para los efectos indirectos (`0` para usar el
#'   método delta).
#' @param alfa Nivel de significancia.
#' @inheritParams tabla_descriptivos
#' @return Un objeto `tabla_apa`. Los valores están en `$datos`.
#' @examples
#' library(lavaan)
#' set.seed(1)
#' d <- data.frame(X = rnorm(200)); d$M <- .5 * d$X + rnorm(200)
#' d$Y <- .4 * d$M + rnorm(200)
#' fit <- sem("M ~ X
#'             Y ~ M + X", data = d)
#' tabla_hipotesis(fit, c(H1 = "X -> M (+)", H2 = "X -> Y (+)", H3 = "X -> M -> Y"),
#'                 bootstrap = 0)
#' @export
tabla_hipotesis <- function(fit, hipotesis, bootstrap = 1000, alfa = .05, etiquetas = NULL, decimales = 2,
                            titulo = NULL, archivo = NULL) {
  if (!inherits(fit, "lavaan")) stop("`fit` debe ser un modelo de lavaan.", call. = FALSE)
  if (is.null(names(hipotesis))) names(hipotesis) <- paste0("H", seq_along(hipotesis))
  ss <- lavaan::standardizedSolution(fit); pe <- lavaan::parameterEstimates(fit)
  if ("group" %in% names(pe)) { pe <- pe[pe$group == 1, ]; ss <- ss[ss$group == 1, ] }
  leer <- function(h) {
    signo <- if (grepl("\\(\\s*\\+\\s*\\)\\s*$", h)) "+" else if (grepl("\\(\\s*[-\u2212]\\s*\\)\\s*$", h)) "-" else ""
    h <- sub("\\(\\s*[-+\u2212]\\s*\\)\\s*$", "", h)
    v <- trimws(strsplit(h, "->|\u2192")[[1]])
    if (length(v) < 2 || any(!nzchar(v))) stop("Hip\u00f3tesis mal escrita: '", h, "'. Usa \"X -> Y\" o \"X -> M -> Y\".", call. = FALSE)
    list(v = v, signo = signo)
  }
  H <- lapply(hipotesis, leer)
  necesita_ind <- any(vapply(H, function(h) length(h$v) > 2, logical(1)))
  efectos <- if (necesita_ind) suppressMessages(tabla_efectos(fit, bootstrap = bootstrap))$datos else NULL
  res <- lapply(names(H), function(nm) {
    h <- H[[nm]]; v <- h$v
    if (length(v) == 2) {
      i <- which(pe$op == "~" & pe$lhs == v[2] & pe$rhs == v[1])
      if (!length(i)) stop(nm, ": el modelo no tiene el efecto ", v[1], " -> ", v[2], ".", call. = FALSE)
      b <- ss$est.std[ss$op == "~" & ss$lhs == v[2] & ss$rhs == v[1]][1]
      list(tipo = "Directo", beta = b, p = pe$pvalue[i[1]], ic = c(NA, NA), sig = pe$pvalue[i[1]] < alfa, est = pe$est[i[1]])
    } else {
      e <- efectos[efectos$efecto == paste(v, collapse = " \u2192 ") & efectos$tipo == "indirecto", ]
      if (!nrow(e)) stop(nm, ": el modelo no tiene la trayectoria indirecta ", paste(v, collapse = " -> "), ".", call. = FALSE)
      list(tipo = "Indirecto", beta = e$beta, p = e$p, ic = c(e$ic_inf, e$ic_sup), sig = e$ic_inf > 0 | e$ic_sup < 0, est = e$B)
    }
  })
  names(res) <- names(H)
  sostiene <- vapply(names(H), function(nm) {
    r <- res[[nm]]; s <- H[[nm]]$signo
    r$sig && (s == "" || (s == "+" && r$est > 0) || (s == "-" && r$est < 0))
  }, logical(1))
  cel <- t(vapply(names(H), function(nm) {
    r <- res[[nm]]; h <- H[[nm]]
    c(nm, paste0(paste(renombrar(h$v, etiquetas), collapse = " \u2192 "), if (nzchar(h$signo)) paste0(" (", h$signo, ")") else ""),
      r$tipo, f_apa(r$beta, decimales),
      if (r$tipo == "Directo") f_p(r$p) else paste0("IC [", f_num(r$ic[1], decimales), ", ", f_num(r$ic[2], decimales), "]"),
      if (sostiene[[nm]]) "Se sostiene" else "Se rechaza")
  }, character(6)))
  nota <- paste0("\u03b2 = coeficiente estandarizado. Efectos directos: se sostienen si p < ", f_apa(alfa, 2),
                 ". Efectos indirectos: se sostienen si el intervalo de confianza al 95 % (",
                 if (necesita_ind && bootstrap > 0) paste0("bootstrap con ", bootstrap, " remuestreos") else "m\u00e9todo delta",
                 ") no incluye el 0. Si la hip\u00f3tesis indica un signo (+ o \u2212), el efecto debe tener ese signo.")
  datos <- data.frame(hipotesis = names(H), trayectoria = vapply(H, function(h) paste(h$v, collapse = " -> "), character(1)),
                      signo = vapply(H, `[[`, character(1), "signo"), tipo = vapply(res, `[[`, character(1), "tipo"),
                      B = vapply(res, `[[`, numeric(1), "est"), beta = vapply(res, `[[`, numeric(1), "beta"),
                      p = vapply(res, function(r) if (is.null(r$p)) NA_real_ else r$p, numeric(1)),
                      sostiene = sostiene, row.names = NULL)
  t <- nueva_tabla(if (is.null(titulo)) "Contraste de hip\u00f3tesis" else titulo,
                   c("Hip\u00f3tesis", "Trayectoria", "Efecto", "\u03b2", "p / IC 95 %", "Decisi\u00f3n"), cel,
                   alinear = c("l", "l", "l", "c", "c", "l"), nota = nota, datos = datos)
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

# --- Índices de modificación ---------------------------------------------------------------

#' Tabla de índices de modificación
#'
#' Lista los parámetros no incluidos en el modelo cuya incorporación más
#' reduciría el \eqn{\chi^2} (índice de modificación, IM) y el cambio
#' esperado en el parámetro (EPC, estandarizado). Un IM mayor que 3.84
#' indica una mejora significativa (p < .05) al liberar ese parámetro.
#'
#' Los cambios al modelo deben justificarse teóricamente; las revistas piden
#' reportar qué modificaciones se hicieron y por qué. Las covarianzas entre
#' errores de ítems del mismo factor suelen ser las más defendibles.
#'
#' @param fit Modelo de `lavaan`.
#' @param n Número máximo de filas.
#' @param minimo IM mínimo que se muestra.
#' @param tipos Tipos de parámetro: `"~~"` (covarianzas), `"=~"` (cargas
#'   cruzadas) y `"~"` (efectos).
#' @inheritParams tabla_descriptivos
#' @return Un objeto `tabla_apa`. Los valores están en `$datos`.
#' @examples
#' library(lavaan)
#' fit <- cfa("visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6
#'             speed =~ x7 + x8 + x9", data = HolzingerSwineford1939)
#' tabla_modificacion(fit, n = 5)
#' @export
tabla_modificacion <- function(fit, n = 10, minimo = 3.84, tipos = c("~~", "=~", "~"), etiquetas = NULL,
                               decimales = 2, titulo = NULL, archivo = NULL) {
  if (!inherits(fit, "lavaan")) stop("`fit` debe ser un modelo de lavaan.", call. = FALSE)
  mi <- lavaan::modindices(fit, sort. = TRUE, standardized = TRUE)
  mi <- mi[mi$op %in% tipos & mi$mi >= minimo, ]
  mi <- utils::head(mi, n)
  et <- function(v) renombrar(v, etiquetas)
  desc <- function(r) switch(r$op,
    "~~" = paste0("Covarianza ", et(r$lhs), " \u2194 ", et(r$rhs)),
    "=~" = paste0("Carga de ", et(r$rhs), " en ", et(r$lhs)),
    "~"  = paste0("Efecto ", et(r$rhs), " \u2192 ", et(r$lhs)), paste(r$lhs, r$op, r$rhs))
  if (!nrow(mi)) {
    cel <- matrix(c("Ning\u00fan par\u00e1metro supera el m\u00ednimo", "", "", "", ""), 1)
  } else {
    cel <- t(vapply(seq_len(nrow(mi)), function(i) {
      r <- mi[i, ]
      c(desc(r), paste(r$lhs, r$op, r$rhs), f_num(r$mi, 2), f_num(r$epc, decimales), f_apa(r$sepc.all, decimales))
    }, character(5)))
  }
  nota <- paste0("IM = \u00edndice de modificaci\u00f3n (disminuci\u00f3n esperada del \u03c7\u00b2 al liberar el par\u00e1metro); ",
                 "EPC = cambio esperado en el par\u00e1metro (no estandarizado y estandarizado). Se muestran los IM \u2265 ",
                 f_num(minimo, 2), ". Liberar un par\u00e1metro solo se justifica si tiene sentido te\u00f3rico; ",
                 "la sintaxis de lavaan para agregarlo est\u00e1 en la segunda columna.")
  t <- nueva_tabla(if (is.null(titulo)) "\u00cdndices de modificaci\u00f3n" else titulo,
                   c("Par\u00e1metro", "Sintaxis lavaan", "IM", "EPC", "EPC est."), cel,
                   alinear = c("l", "l", "c", "c", "c"), nota = nota,
                   datos = mi[, intersect(c("lhs", "op", "rhs", "mi", "epc", "sepc.all"), names(mi))])
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}
