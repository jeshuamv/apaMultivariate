# Tablas de evaluación del modelo con los criterios habituales en cursos y
# en las salidas de AMOS: criterios de ajuste e identificación, estimaciones,
# hipótesis, índices de modificación

# --- Criterios de ajuste e identificación ----------------------------------------

#' @noRd
evalua <- function(v, exc, acep, mayor = TRUE) {
  if (is.na(v)) return("")
  ok <- function(c) if (mayor) v >= c else v <= c
  if (!is.na(exc) && ok(exc)) tx("Excelente", "Excellent") else if (ok(acep)) tx("Aceptable", "Acceptable") else tx("No aceptable", "Not acceptable")
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
#' | RMSEA | \eqn{\le} .06 | \eqn{\le} .08 | Hu y Bentler (1999); Browne y Cudeck (1993) |
#' | SRMR | \eqn{\le} .08 | \eqn{\le} .10 | Hu y Bentler (1999); Hair et al. (2014) |
#' | CFI, TLI | \eqn{\ge} .95 | \eqn{\ge} .90 | Hu y Bentler (1999); Hair et al. (2014) |
#' | GFI | \eqn{\ge} .95 | \eqn{\ge} .90 | Hair et al. (2014) |
#' | PNFI, PRATIO | | \eqn{\ge} .50 | Mulaik et al. (1989) |
#'
#' Con estimadores robustos se usan las versiones corregidas de
#' \eqn{\chi^2}, RMSEA, CFI y TLI.
#'
#' Si tu curso o tu revista usan otros puntos de corte, cámbialos con
#' `cortes`, por ejemplo `cortes = list(rmsea = c(.05, .06))` (primero el de
#' excelente y después el de aceptable). Para un artículo puedes quitar la
#' columna de evaluación con `evaluacion = FALSE`: muchos metodólogos
#' prefieren reportar los valores y los criterios sin calificarlos.
#'
#' @param fit Modelo de `lavaan`.
#' @param indices Índices que se incluyen. Kline (2011) recomienda reportar
#'   siempre \eqn{\chi^2}, RMSEA, GFI, CFI y SRMR.
#' @param cortes Lista para cambiar los puntos de corte de algún índice:
#'   `chi2gl`, `rmsea`, `srmr`, `cfi`, `tli`, `gfi`, `pnfi` o `pratio`, cada
#'   uno con dos valores (excelente, aceptable).
#' @param evaluacion Si es `FALSE`, no se incluye la columna de evaluación.
#' @param ic_rmsea Nivel de confianza del intervalo del RMSEA: `.90` (el
#'   habitual, por defecto) o, si te lo piden, `.95`.
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
                                   cortes = NULL, evaluacion = TRUE, ic_rmsea = .90, titulo = NULL, archivo = NULL) {
  exigir_lavaan(fit, "cfa() o sem()")
  indices <- match.arg(indices, several.ok = TRUE)
  K <- list(chi2gl = c(2, 3), rmsea = c(.06, .08), srmr = c(.08, .10), cfi = c(.95, .90), tli = c(.95, .90),
            gfi = c(.95, .90), pnfi = c(NA, .50), pratio = c(NA, .50))
  if (!is.null(cortes)) {
    mal <- setdiff(names(cortes), names(K))
    if (length(mal)) stop("No reconozco estos \u00edndices en `cortes`: ", paste(mal, collapse = ", "), ". Usa: ", paste(names(K), collapse = ", "), ".", call. = FALSE)
    for (k in names(cortes)) { v <- cortes[[k]]; if (length(v) == 1) v <- c(NA, v); K[[k]] <- v }
  }
  lim <- function(k, i, signo) if (is.na(K[[k]][i])) "" else paste(signo, if (K[[k]][i] < 1) sub("^0", "", formatC(K[[k]][i], format = "f", digits = max(2, nchar(sub("^0\\.", "", format(K[[k]][i])))))) else format(K[[k]][i]))
  I <- indices(fit, ic_rmsea); fm <- medidas_ajuste(fit, ic_rmsea)
  gfi <- if ("gfi" %in% names(fm)) fm[["gfi"]] else NA
  bdf <- if ("baseline.df" %in% names(fm)) fm[["baseline.df"]] else NA
  pratio <- I[["df"]] / bdf
  pnfi <- if ("pnfi" %in% names(fm)) fm[["pnfi"]] else NA
  f3 <- function(x) f_apa(x, 3)
  filas <- list(
    chi2 = c("\u03c7\u00b2", paste0(f_num(I[["chisq"]], 2), tx(" (gl = ", " (df = "), formatC(I[["df"]], format = "d"), ", p ",
                                    if (is.na(I[["pvalue"]])) tx("no calculable", "not computable") else if (I[["pvalue"]] < .001) "< .001" else paste("=", f_apa(I[["pvalue"]], 3)), ")"),
             "p > .05", "", if (is.na(I[["pvalue"]])) "" else if (I[["pvalue"]] > .05) tx("Adecuado", "Adequate") else tx("No adecuado", "Not adequate"),
             "Hair et al. (2014); Kline (2011)"),
    chi2gl = c(tx("\u03c7\u00b2/gl", "\u03c7\u00b2/df"), f_num(I[["chisq"]] / I[["df"]], 2), lim("chi2gl", 1, "\u2264"), lim("chi2gl", 2, "\u2264"),
               evalua(I[["chisq"]] / I[["df"]], K$chi2gl[1], K$chi2gl[2], FALSE), "Hair et al. (2014)"),
    rmsea = c(cab_rmsea(ic_rmsea), paste0(f3(I[["rmsea"]]), " [", f3(I[["lo"]]), ", ", f3(I[["hi"]]), "]"),
              lim("rmsea", 1, "\u2264"), lim("rmsea", 2, "\u2264"), evalua(I[["rmsea"]], K$rmsea[1], K$rmsea[2], FALSE), tx("Hu y Bentler (1999); Browne y Cudeck (1993)", "Hu & Bentler (1999); Browne & Cudeck (1993)")),
    srmr = c("SRMR", f3(I[["srmr"]]), lim("srmr", 1, "\u2264"), lim("srmr", 2, "\u2264"), evalua(I[["srmr"]], K$srmr[1], K$srmr[2], FALSE),
             tx("Hu y Bentler (1999); Hair et al. (2014)", "Hu & Bentler (1999); Hair et al. (2014)")),
    cfi = c("CFI", f3(I[["cfi"]]), lim("cfi", 1, "\u2265"), lim("cfi", 2, "\u2265"), evalua(I[["cfi"]], K$cfi[1], K$cfi[2]), tx("Hu y Bentler (1999); Hair et al. (2014)", "Hu & Bentler (1999); Hair et al. (2014)")),
    tli = c("TLI (NNFI)", f3(I[["tli"]]), lim("tli", 1, "\u2265"), lim("tli", 2, "\u2265"), evalua(I[["tli"]], K$tli[1], K$tli[2]), tx("Tabachnick y Fidell (2013)", "Tabachnick & Fidell (2013)")),
    gfi = c("GFI", f3(gfi), lim("gfi", 1, "\u2265"), lim("gfi", 2, "\u2265"), evalua(gfi, K$gfi[1], K$gfi[2]), "Hair et al. (2014)"),
    pnfi = c("PNFI", f3(pnfi), lim("pnfi", 1, "\u2265"), lim("pnfi", 2, "\u2265"), evalua(pnfi, K$pnfi[1], K$pnfi[2]), "Mulaik et al. (1989)"),
    pratio = c("PRATIO", f3(pratio), lim("pratio", 1, "\u2265"), lim("pratio", 2, "\u2265"), evalua(pratio, K$pratio[1], K$pratio[2]), "Mulaik et al. (1989)"))
  cel <- do.call(rbind, filas[indices])
  cab <- c(tx("\u00cdndice", "Index"), tx("Valor", "Value"), tx("Excelente", "Excellent"), tx("Aceptable", "Acceptable"), tx("Evaluaci\u00f3n", "Evaluation"), tx("Fuente", "Source"))
  ali <- c("l", "c", "c", "c", "c", "l")
  if (!evaluacion) { cel <- cel[, -5, drop = FALSE]; cab <- cab[-5]; ali <- ali[-5] }
  nota <- paste0(if (as.logical(I[["robusto"]])) tx("Se usan los \u00edndices corregidos del estimador robusto (el GFI no tiene versi\u00f3n corregida). ",
                                                    "Robust (scaled) indices are reported (GFI has no robust version). ") else "",
                 tx("RMSEA = error cuadr\u00e1tico medio de aproximaci\u00f3n; SRMR = ra\u00edz del residuo cuadr\u00e1tico medio estandarizado; CFI = \u00edndice de ajuste comparativo; TLI = \u00edndice de Tucker-Lewis; GFI = \u00edndice de bondad de ajuste; PNFI = \u00edndice de ajuste normado parsimonioso; PRATIO = raz\u00f3n de parsimonia. La prueba \u03c7\u00b2 es sensible al tama\u00f1o de la muestra; con muestras grandes suele ser significativa aunque el ajuste sea bueno.",
                    "RMSEA = root mean square error of approximation; SRMR = standardized root mean square residual; CFI = comparative fit index; TLI = Tucker-Lewis index; GFI = goodness-of-fit index; PNFI = parsimony normed fit index; PRATIO = parsimony ratio. The \u03c7\u00b2 test is sensitive to sample size; with large samples it is usually significant even when fit is good."))
  # Identificación
  q <- fm[["npar"]]; gl <- fm[["df"]]; n <- lavaan::lavInspect(fit, "ntotal")
  # Con fixed.x = TRUE (el valor de lavaan), las varianzas y covarianzas de las
  # exógenas observadas no se cuentan como parámetros; AMOS y los libros s\u00ed
  # las cuentan. Se suman a p y q para que coincidan con el conteo a mano.
  nx <- length(lavaan::lavNames(fit, "ov.x"))
  sumx <- if (isTRUE(lavaan::lavInspect(fit, "options")$fixed.x) && nx > 0)
    (nx * (nx + 1) / 2 + if (isTRUE(lavaan::lavInspect(fit, "meanstructure"))) nx else 0) * lavaan::lavInspect(fit, "ngroups") else 0
  q <- q + sumx; p <- q + gl
  ident <- if (gl > 0) tx("Sobreidentificado", "Overidentified") else if (gl == 0) tx("Exactamente identificado", "Just-identified") else tx("Subidentificado", "Underidentified")
  razon <- n / q
  pt <- lavaan::parTable(fit); pt <- pt[pt$group %in% c(0, 1) | is.null(pt$group), ]
  todas <- unique(c(lavaan::lavNames(fit, "ov"), lavaan::lavNames(fit, "lv")))
  endo <- intersect(todas, unique(c(pt$lhs[pt$op == "~"], pt$rhs[pt$op == "=~"])))
  exo <- setdiff(todas, endo)
  lista_v <- function(v) if (length(v)) paste(v, collapse = ", ") else "\u2014"
  cel2 <- rbind(c(tx("Observaciones (p)", "Observations (p)"), formatC(p, format = "d"), tx("Elementos no redundantes de la matriz de covarianzas", "Non-redundant elements of the covariance matrix")),
                c(tx("Par\u00e1metros estimados (q)", "Estimated parameters (q)"), formatC(q, format = "d"), ""),
                c(tx("Grados de libertad (p \u2212 q)", "Degrees of freedom (p \u2212 q)"), formatC(gl, format = "d"), ident),
                c(tx("Tama\u00f1o de la muestra (N)", "Sample size (N)"), formatC(n, format = "d"), ""),
                c(tx("Raz\u00f3n N:q", "N:q ratio"), paste0(f_num(razon, 1), ":1"),
                  if (razon >= 20) "Ideal (\u2265 20:1)" else if (razon >= 10) tx("Aceptable (\u2265 10:1)", "Acceptable (\u2265 10:1)") else tx("Baja (< 10:1)", "Low (< 10:1)")),
                c(tx("Variables ex\u00f3genas", "Exogenous variables"), formatC(length(exo), format = "d"), lista_v(exo)),
                c(tx("Variables end\u00f3genas", "Endogenous variables"), formatC(length(endo), format = "d"), lista_v(endo)))
  nota2 <- paste0(tx("gl > 0: modelo sobreidentificado (el deseable); gl = 0: exactamente identificado (\u03c7\u00b2 = 0, el ajuste no puede evaluarse); gl < 0: subidentificado (no puede estimarse). Raz\u00f3n N:q: se recomiendan al menos 10 casos por par\u00e1metro y, de preferencia, 20 (Kline, 2011). Ex\u00f3genas: no reciben ninguna flecha de otra variable del modelo; end\u00f3genas: reciben al menos una (sin contar los errores).",
              "df > 0: overidentified model (desirable); df = 0: just-identified (\u03c7\u00b2 = 0, fit cannot be evaluated); df < 0: underidentified (cannot be estimated). N:q ratio: at least 10 and preferably 20 cases per parameter are recommended (Kline, 2011). Exogenous: receive no arrow from another variable in the model; endogenous: receive at least one (excluding error terms)."),
                  if (sumx > 0) tx(paste0(" p y q incluyen las ", sumx, " varianzas y covarianzas (y medias, si las hay) de las variables ex\u00f3genas observadas, como en AMOS; lavaan las fija en sus valores de la muestra y no las cuenta, pero los grados de libertad son los mismos."),
                                   paste0(" p and q include the ", sumx, " variances and covariances (and means, if any) of the observed exogenous variables, as in AMOS; lavaan fixes them at their sample values and does not count them, but the degrees of freedom are the same.")) else "")
  t <- nueva_tabla(if (is.null(titulo)) tx("Evaluaci\u00f3n del ajuste del modelo", "Model fit evaluation") else titulo,
                   cab, cel, alinear = ali, nota = nota,
                   datos = list(indices = c(I, gfi = gfi, pnfi = pnfi, pratio = pratio),
                                identificacion = c(p = p, q = q, gl = gl, n = n, razon = razon), exogenas = exo, endogenas = endo),
                   extra = list(nueva_tabla(tx("Identificaci\u00f3n del modelo y tama\u00f1o de la muestra", "Model identification and sample size"), c(tx("Elemento", "Element"), tx("Valor", "Value"), tx("Interpretaci\u00f3n", "Interpretation")),
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
  exigir_un_grupo(fit)
  exigir_lavaan(fit, "cfa() o sem()")
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)
  if ("group" %in% names(pe)) pe <- pe[pe$group == 1, ]
  et <- function(v) renombrar(v, etiquetas)
  endo <- unique(c(pe$lhs[pe$op == "~"], pe$rhs[pe$op == "=~"]))
  fila <- function(r, nombre) c(paste0("   ", nombre), f_num(r$est, decimales),
                                if (is.na(r$se) || r$se == 0) "\u2014" else f_num(r$se, decimales),
                                if (is.na(r$z)) "\u2014" else f_num(r$z, 2),
                                if (is.na(r$pvalue)) "\u2014" else f_p(r$pvalue), f_apa(r$std.all, decimales))
  secciones <- list(
    reg = list(pe$op == "~", function(r) paste0(et(r$lhs), " \u2190 ", et(r$rhs))),
    car = list(pe$op == "=~", function(r) paste0(et(r$rhs), " \u2190 ", et(r$lhs))),
    cov = list(pe$op == "~~" & pe$lhs != pe$rhs, function(r) paste0(et(r$lhs), " \u2194 ", et(r$rhs))),
    var = list(varianzas & pe$op == "~~" & pe$lhs == pe$rhs & !pe$lhs %in% endo,
                                                     function(r) et(r$lhs)),
    err = list(varianzas & pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs %in% endo,
                                                   function(r) paste(tx("Error de", "Error of"), et(r$lhs))),
    def = list(pe$op == ":=", function(r) r$lhs))
  rotulo <- c(reg = tx("Pesos de regresi\u00f3n", "Regression weights"), car = tx("Cargas factoriales", "Factor loadings"),
              cov = tx("Covarianzas (correlaci\u00f3n en \u03b2)", "Covariances (correlation in \u03b2)"),
              var = tx("Varianzas de las variables ex\u00f3genas", "Variances of exogenous variables"),
              err = tx("Varianzas de los errores (residuales)", "Error (residual) variances"), def = tx("Par\u00e1metros definidos", "Defined parameters"))
  cel <- NULL; sep <- integer(0); orden <- integer(0)
  for (s in names(secciones)) {
    idx <- which(secciones[[s]][[1]]); if (!length(idx)) next
    cel <- rbind(cel, c(rotulo[[s]], rep("", 5)), t(vapply(idx, function(i) fila(pe[i, ], secciones[[s]][[2]](pe[i, ])), character(6))))
    sep <- c(sep, nrow(cel)); orden <- c(orden, idx)
  }
  sep <- utils::head(sep, -1)
  nota <- paste0(tx("B = estimaci\u00f3n no estandarizada; EE = error est\u00e1ndar; C.R. = raz\u00f3n cr\u00edtica (B/EE); |C.R.| > 1.96 indica un par\u00e1metro significativo (p < .05); \u03b2 = estimaci\u00f3n estandarizada (en las covarianzas, la correlaci\u00f3n; en los errores, la proporci\u00f3n de varianza no explicada, 1 \u2212 R\u00b2). \u2014 = par\u00e1metro fijo (por ejemplo, la carga de referencia fijada en 1). N = ",
                    "B = unstandardized estimate; SE = standard error; C.R. = critical ratio (B/SE); |C.R.| > 1.96 indicates a significant parameter (p < .05); \u03b2 = standardized estimate (for covariances, the correlation; for errors, the proportion of unexplained variance, 1 \u2212 R\u00b2). \u2014 = fixed parameter (e.g., the reference loading fixed at 1). N = "),
                 lavaan::lavInspect(fit, "ntotal"), ".")
  t <- nueva_tabla(if (is.null(titulo)) tx("Estimaciones del modelo", "Model estimates") else titulo,
                   c(tx("Par\u00e1metro", "Parameter"), "B", tx("EE", "SE"), "C.R.", "p", "\u03b2"), cel, nota = nota, separadores = sep,
                   datos = pe[orden, c("lhs", "op", "rhs", "est", "se", "z", "pvalue", "std.all")])
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

# --- Hipótesis -------------------------------------------------------------------------

#' Tabla de contraste de hipótesis
#'
#' Resume la decisión sobre cada hipótesis del modelo: el coeficiente, su
#' significancia y si la hipótesis se sostiene o no. Un efecto no significativo
#' no demuestra que la hipótesis sea falsa: solo que los datos no la respaldan.
#'
#' Cada hipótesis se escribe como texto. Use `->` para las flechas (también se
#' acepta `→`, pero `->` funciona en cualquier computadora):
#'
#' | Hipótesis | Se escribe |
#' |---|---|
#' | Efecto directo | `"X -> Y"` |
#' | Efecto indirecto por una trayectoria | `"X -> M -> Y"` (o `"X -> M1 -> M2 -> Y"`) |
#' | Efecto indirecto total (todas las trayectorias de X a Y) | `"X -> * -> Y"` |
#' | Efecto total (directo + indirectos) | `"X -> Y [total]"` |
#' | Covarianza o correlación | `"X <-> W"` (o `"X ~~ W"`) |
#'
#' Si la hipótesis predice un sentido, agréguelo al final: `"X -> Y (+)"` o
#' `"X -> Y (-)"`; entonces se sostiene solo si el efecto es significativo
#' **y** tiene ese signo. Con `[total]`, el signo va antes o después:
#' `"X -> Y (+) [total]"`.
#'
#' Los efectos directos y las covarianzas usan el valor *p* (bilateral) de
#' `lavaan`. Los indirectos y los totales se evalúan con el intervalo de
#' confianza de [tabla_efectos()] (bootstrap o método delta): son
#' significativos si el intervalo no incluye el 0. Si el modelo no admite
#' bootstrap (por ejemplo, variables ordinales con WLSMV), se usa el método
#' delta y se avisa.
#'
#' Una covarianza entre dos variables exógenas observadas solo tiene valor *p*
#' si el modelo se estima con `fixed.x = FALSE`; de lo contrario `lavaan` la
#' fija a la de la muestra y la función explica cómo estimarla.
#'
#' @param fit Modelo de `lavaan`.
#' @param hipotesis Vector con nombres, p. ej.
#'   `c(H1 = "SelfEfficacy -> Flow (+)", H2 = "LMS -> Flow -> Satisfaction")`.
#' @param bootstrap Remuestreos para los efectos indirectos y totales (`0`
#'   para usar el método delta).
#' @param alfa Nivel de significancia.
#' @inheritParams tabla_descriptivos
#' @return Un objeto `tabla_apa`. Los valores están en `$datos`.
#' @examples
#' library(lavaan)
#' set.seed(1)
#' d <- data.frame(X = rnorm(200), W = rnorm(200))
#' d$M <- .5 * d$X + rnorm(200)
#' d$Y <- .4 * d$M + .3 * d$W + rnorm(200)
#' fit <- sem("M ~ X
#'             Y ~ M + X + W", data = d)
#' tabla_hipotesis(fit, c(H1 = "X -> M (+)", H2 = "X -> Y (+)", H3 = "X -> M -> Y",
#'                        H4 = "X -> Y (+) [total]"), bootstrap = 0)
#' @export
tabla_hipotesis <- function(fit, hipotesis, bootstrap = 1000, alfa = .05, etiquetas = NULL, decimales = 2,
                            titulo = NULL, archivo = NULL) {
  if (missing(hipotesis)) stop("Escribe tus hip\u00f3tesis, por ejemplo:\n  tabla_hipotesis(fit, c(H1 = \"X -> Y (+)\", H2 = \"X -> M -> Y\"))\n",
                               "(+) o (-) si la hip\u00f3tesis predice el signo; con dos flechas, efecto indirecto; ",
                               "\"X -> * -> Y\", indirecto total; \"X -> Y [total]\", efecto total; \"X <-> W\", covarianza.", call. = FALSE)
  exigir_un_grupo(fit)
  exigir_lavaan(fit, "cfa() o sem()")
  if (is.null(names(hipotesis))) names(hipotesis) <- paste0("H", seq_along(hipotesis))
  vacios <- !nzchar(names(hipotesis)); names(hipotesis)[vacios] <- paste0("H", which(vacios))
  ss <- lavaan::standardizedSolution(fit); pe <- lavaan::parameterEstimates(fit)
  if ("group" %in% names(pe)) { pe <- pe[pe$group == 1, ]; ss <- ss[ss$group == 1, ] }

  leer <- function(h, nm) {
    original <- h
    signo <- ""; total <- FALSE
    repeat {
      if (grepl("\\[\\s*total\\s*\\]\\s*$", h, ignore.case = TRUE)) { total <- TRUE; h <- sub("\\[\\s*total\\s*\\]\\s*$", "", h, ignore.case = TRUE); next }
      if (grepl("\\(\\s*\\+\\s*\\)\\s*$", h)) { signo <- "+"; h <- sub("\\(\\s*\\+\\s*\\)\\s*$", "", h); next }
      if (grepl("\\(\\s*[-\u2212]\\s*\\)\\s*$", h)) { signo <- "-"; h <- sub("\\(\\s*[-\u2212]\\s*\\)\\s*$", "", h); next }
      break
    }
    h <- trimws(h)
    if (grepl("\\([^)]*\\)\\s*$", h))
      stop(nm, ": no reconozco '", sub(".*(\\([^)]*\\))\\s*$", "\\1", h), "' en '", original, "'. Para el signo usa (+) o (-).", call. = FALSE)
    ayuda <- "Usa \"X -> Y\", \"X -> M -> Y\", \"X -> * -> Y\", \"X -> Y [total]\" o \"X <-> W\"."
    if (grepl("<->|\u2194|~~", h)) {
      v <- trimws(strsplit(h, "<->|\u2194|~~")[[1]])
      if (length(v) != 2 || any(!nzchar(v))) stop(nm, ": hip\u00f3tesis mal escrita: '", original, "'. ", ayuda, call. = FALSE)
      if (total) stop(nm, ": [total] no aplica a una covarianza.", call. = FALSE)
      return(list(v = v, signo = signo, clase = "cov"))
    }
    v <- trimws(strsplit(h, "->|\u2192")[[1]])
    if (length(v) < 2 || any(!nzchar(v))) stop(nm, ": hip\u00f3tesis mal escrita: '", original, "'. ", ayuda, call. = FALSE)
    if ("*" %in% v) {
      if (length(v) != 3 || v[2] != "*") stop(nm, ": para el indirecto total escribe \"X -> * -> Y\" (un solo *, en medio).", call. = FALSE)
      if (total) stop(nm, ": usa \"X -> * -> Y\" (indirecto total) o \"X -> Y [total]\" (efecto total), no los dos.", call. = FALSE)
      return(list(v = v[c(1, 3)], signo = signo, clase = "indirecto_total"))
    }
    if (total) {
      if (length(v) != 2) stop(nm, ": el efecto total se escribe con una sola flecha: \"", v[1], " -> ", utils::tail(v, 1), " [total]\".", call. = FALSE)
      return(list(v = v, signo = signo, clase = "total"))
    }
    list(v = v, signo = signo, clase = if (length(v) == 2) "directo" else "indirecto")
  }
  H <- mapply(leer, hipotesis, names(hipotesis), SIMPLIFY = FALSE)

  necesita_ef <- any(vapply(H, function(h) h$clase %in% c("indirecto", "indirecto_total", "total"), logical(1)))
  # Si el modelo no admite bootstrap, tabla_efectos() usa el método delta y
  # avisa; ese aviso se deja pasar y los demás mensajes se omiten
  delta <- bootstrap == 0
  efectos <- if (necesita_ef) withCallingHandlers(tabla_efectos(fit, bootstrap = bootstrap), message = function(m) {
    if (grepl("delta", conditionMessage(m))) { delta <<- TRUE; message(conditionMessage(m)) }
    invokeRestart("muffleMessage")
  })$datos else NULL

  fijado <- function(nm, que) stop(nm, ": ", que, " est\u00e1 fijado en el modelo (no se estima), as\u00ed que no tiene valor p ni se puede contrastar.", call. = FALSE)
  directo <- function(nm, v) {
    i <- which(pe$op == "~" & pe$lhs == v[2] & pe$rhs == v[1])
    if (!length(i)) return(NULL)
    i <- i[1]
    if (is.na(pe$pvalue[i])) fijado(nm, paste0("el efecto ", v[1], " -> ", v[2]))
    b <- ss$est.std[ss$op == "~" & ss$lhs == v[2] & ss$rhs == v[1]][1]
    list(tipo = "Directo", beta = b, p = pe$pvalue[i], ic = c(NA, NA), sig = pe$pvalue[i] < alfa, est = pe$est[i])
  }
  de_efectos <- function(e, tipo) list(tipo = tipo, beta = e$beta[1], p = NA_real_, ic = c(e$ic_inf[1], e$ic_sup[1]),
                                       sig = isTRUE(e$ic_inf[1] > 0 | e$ic_sup[1] < 0), est = e$B[1])

  res <- lapply(names(H), function(nm) {
    h <- H[[nm]]; v <- h$v
    switch(h$clase,
      directo = {
        r <- directo(nm, v)
        if (is.null(r)) {
          hay_ind <- !is.null(efectos) || any(pe$op == "~" & pe$lhs == v[2])
          stop(nm, ": el modelo no tiene el efecto directo ", v[1], " -> ", v[2], ".",
               if (hay_ind) paste0(" Si te refieres al efecto indirecto total escribe \"", v[1], " -> * -> ", v[2], "\"; si al total, \"", v[1], " -> ", v[2], " [total]\".") else "",
               call. = FALSE)
        }
        r
      },
      indirecto = {
        e <- efectos[efectos$efecto == paste(v, collapse = " \u2192 ") & efectos$tipo == "indirecto", ]
        if (!nrow(e)) stop(nm, ": el modelo no tiene la trayectoria indirecta ", paste(v, collapse = " -> "), ".", call. = FALSE)
        de_efectos(e, "Indirecto")
      },
      indirecto_total = {
        e <- efectos[efectos$de == v[1] & efectos$a == v[2] & efectos$tipo == "indirecto_total", ]
        if (!nrow(e)) stop(nm, ": el modelo no tiene efectos indirectos de ", v[1], " sobre ", v[2], ".", call. = FALSE)
        de_efectos(e, "Indirecto total")
      },
      total = {
        e <- efectos[efectos$de == v[1] & efectos$a == v[2] & efectos$tipo == "total", ]
        if (nrow(e)) de_efectos(e, "Total") else {
          # Sin mediación, el efecto total es el directo
          r <- directo(nm, v)
          if (is.null(r)) stop(nm, ": el modelo no tiene ning\u00fan efecto de ", v[1], " sobre ", v[2], ".", call. = FALSE)
          r$tipo <- "Total"; r
        }
      },
      cov = {
        i <- which(pe$op == "~~" & ((pe$lhs == v[1] & pe$rhs == v[2]) | (pe$lhs == v[2] & pe$rhs == v[1])))
        if (!length(i)) stop(nm, ": el modelo no incluye la covarianza ", v[1], " <-> ", v[2], ". Agr\u00e9gala al modelo con ", v[1], " ~~ ", v[2], ".", call. = FALSE)
        i <- i[1]
        if (is.na(pe$pvalue[i])) {
          ex <- lavaan::lavNames(fit, "ov.x")
          if (all(v %in% ex)) stop(nm, ": lavaan no estima la covarianza entre ", v[1], " y ", v[2], " porque son ex\u00f3genas observadas y la fija a la de la muestra (fixed.x = TRUE). ",
                                   "Para contrastarla, estima el modelo con sem(..., fixed.x = FALSE).", call. = FALSE)
          fijado(nm, paste0("la covarianza ", v[1], " <-> ", v[2]))
        }
        b <- ss$est.std[ss$op == "~~" & ((ss$lhs == v[1] & ss$rhs == v[2]) | (ss$lhs == v[2] & ss$rhs == v[1]))][1]
        list(tipo = "Covarianza", beta = b, p = pe$pvalue[i], ic = c(NA, NA), sig = pe$pvalue[i] < alfa, est = pe$est[i])
      })
  })
  names(res) <- names(H)
  sostiene <- vapply(names(H), function(nm) {
    r <- res[[nm]]; s <- H[[nm]]$signo
    isTRUE(r$sig) && (s == "" || (s == "+" && r$est > 0) || (s == "-" && r$est < 0))
  }, logical(1))
  tipo_txt <- function(t) switch(t, Directo = tx("Directo", "Direct"), Indirecto = tx("Indirecto", "Indirect"),
                                 "Indirecto total" = tx("Indirecto total", "Total indirect"), Total = tx("Total", "Total"),
                                 Covarianza = tx("Covarianza", "Covariance"))
  trayecto <- function(h) {
    v <- renombrar(h$v, etiquetas)
    base <- switch(h$clase, cov = paste(v, collapse = " \u2194 "), indirecto_total = paste(v[1], "\u2192 \u2026 \u2192", v[2]),
                   paste(v, collapse = " \u2192 "))
    paste0(base, if (nzchar(h$signo)) paste0(" (", h$signo, ")") else "")
  }
  cel <- t(vapply(names(H), function(nm) {
    r <- res[[nm]]; h <- H[[nm]]
    c(nm, trayecto(h), tipo_txt(r$tipo), f_apa(r$beta, decimales),
      if (!is.na(r$p)) f_p(r$p) else paste0(tx("IC [", "CI ["), f_num(r$ic[1], decimales), ", ", f_num(r$ic[2], decimales), "]"),
      if (sostiene[[nm]]) tx("Se sostiene", "Supported") else tx("No se sostiene", "Not supported"))
  }, character(6)))
  clases <- vapply(H, `[[`, character(1), "clase")
  hay_cov <- any(clases == "cov"); hay_ef <- any(clases %in% c("indirecto", "indirecto_total", "total"))
  metodo <- if (hay_ef) { if (delta) tx("m\u00e9todo delta", "delta method") else paste0(tx("bootstrap con ", "bootstrap with "), bootstrap, tx(" remuestreos", " resamples")) }
  nota <- paste0(tx("\u03b2 = coeficiente estandarizado", "\u03b2 = standardized coefficient"),
                 if (hay_cov) tx(" (en las covarianzas, la correlaci\u00f3n)", " (for covariances, the correlation)") else "",
                 tx(". Efectos directos", ". Direct effects"), if (hay_cov) tx(" y covarianzas", " and covariances") else "",
                 tx(": se sostienen si p < ", " are supported if p < "), f_apa(alfa, 2),
                 if (hay_ef) paste0(tx(". Efectos indirectos y totales: se sostienen si el intervalo de confianza al 95 % (", ". Indirect and total effects are supported if the 95% confidence interval ("),
                                    metodo, tx(", calculado con el efecto no estandarizado) no incluye el 0", ", computed on the unstandardized effect) excludes 0")) else "",
                 tx(". Si la hip\u00f3tesis indica un signo (+ o \u2212), el efecto debe tener ese signo.",
                    ". If the hypothesis specifies a sign (+ or \u2212), the effect must have that sign."))
  datos <- data.frame(hipotesis = names(H),
                      trayectoria = vapply(H, function(h) paste(h$v, collapse = if (h$clase == "cov") " <-> " else if (h$clase == "indirecto_total") " -> * -> " else " -> "), character(1)),
                      signo = vapply(H, `[[`, character(1), "signo"), tipo = vapply(res, `[[`, character(1), "tipo"),
                      B = vapply(res, `[[`, numeric(1), "est"), beta = vapply(res, `[[`, numeric(1), "beta"),
                      p = vapply(res, function(r) if (is.null(r$p)) NA_real_ else r$p, numeric(1)),
                      ic_inf = vapply(res, function(r) r$ic[1], numeric(1)), ic_sup = vapply(res, function(r) r$ic[2], numeric(1)),
                      sostiene = sostiene, row.names = NULL)
  t <- nueva_tabla(if (is.null(titulo)) tx("Contraste de hip\u00f3tesis", "Hypothesis testing") else titulo,
                   c(tx("Hip\u00f3tesis", "Hypothesis"), tx("Trayectoria", "Path"), tx("Efecto", "Effect"), "\u03b2", tx("p / IC 95 %", "p / 95% CI"), tx("Decisi\u00f3n", "Decision")), cel,
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
  exigir_un_grupo(fit)
  exigir_lavaan(fit, "cfa() o sem()")
  igualdad <- any(lavaan::parTable(fit)$op == "==") || any(duplicated(stats::na.omit(
    replace(lavaan::parTable(fit)$label, lavaan::parTable(fit)$label == "", NA))))
  mi <- suppressWarnings(lavaan::modindices(fit, sort. = TRUE, standardized = TRUE))
  mi <- mi[mi$op %in% tipos & mi$mi >= minimo, ]
  mi <- utils::head(mi, n)
  et <- function(v) renombrar(v, etiquetas)
  desc <- function(r) switch(r$op,
    "~~" = paste0(tx("Covarianza ", "Covariance "), et(r$lhs), " \u2194 ", et(r$rhs)),
    "=~" = paste0(tx("Carga de ", "Loading of "), et(r$rhs), tx(" en ", " on "), et(r$lhs)),
    "~"  = paste0(tx("Efecto ", "Effect "), et(r$rhs), " \u2192 ", et(r$lhs)), paste(r$lhs, r$op, r$rhs))
  if (!nrow(mi)) {
    cel <- matrix(c(tx("Ning\u00fan par\u00e1metro supera el m\u00ednimo", "No parameter exceeds the minimum"), "", "", "", ""), 1)
  } else {
    cel <- t(vapply(seq_len(nrow(mi)), function(i) {
      r <- mi[i, ]
      c(desc(r), paste(r$lhs, r$op, r$rhs), f_num(r$mi, 2), f_num(r$epc, decimales), f_apa(r$sepc.all, decimales))
    }, character(5)))
  }
  nota <- paste0(tx("IM = \u00edndice de modificaci\u00f3n (disminuci\u00f3n esperada del \u03c7\u00b2 al liberar el par\u00e1metro); EPC = cambio esperado en el par\u00e1metro (no estandarizado y estandarizado). Se muestran los IM \u2265 ",
                    "MI = modification index (expected decrease in \u03c7\u00b2 if the parameter is freed); EPC = expected parameter change (unstandardized and standardized). MIs \u2265 "),
                 f_num(minimo, 2), tx(". Liberar un par\u00e1metro solo se justifica si tiene sentido te\u00f3rico; la sintaxis de lavaan para agregarlo est\u00e1 en la segunda columna.",
                                      " are shown. Freeing a parameter is justified only on theoretical grounds; the lavaan syntax to add it is in the second column."),
                 if (igualdad) tx(" El modelo tiene restricciones de igualdad: estos IM no las toman en cuenta; para evaluar si conviene liberar una restricci\u00f3n usa lavaan::lavTestScore(fit).",
                                  " The model has equality constraints, which these MIs ignore; use lavaan::lavTestScore(fit) to evaluate releasing a constraint.") else "")
  t <- nueva_tabla(if (is.null(titulo)) tx("\u00cdndices de modificaci\u00f3n", "Modification indices") else titulo,
                   c(tx("Par\u00e1metro", "Parameter"), tx("Sintaxis lavaan", "lavaan syntax"), tx("IM", "MI"), "EPC", tx("EPC est.", "Std. EPC")), cel,
                   alinear = c("l", "l", "c", "c", "c"), nota = nota,
                   datos = mi[, intersect(c("lhs", "op", "rhs", "mi", "epc", "sepc.all"), names(mi))])
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

# --- Residuales de correlación ---------------------------------------------------------

#' Tabla de residuales de correlación
#'
#' Diferencia entre cada correlación observada y la que reproduce el modelo
#' (residual de correlación). Muestra *dónde* ajusta mal el modelo, algo que
#' los índices globales no dicen, y es lo que recomiendan los estándares de
#' reporte de SEM (Kline, 2016; Appelbaum et al., 2018). Residuales mayores
#' que .10 en valor absoluto señalan pares de variables que el modelo explica
#' mal.
#'
#' @param fit Modelo de `lavaan`.
#' @param umbral Valor absoluto a partir del cual se marca un residual (.10
#'   por omisión).
#' @inheritParams tabla_descriptivos
#' @return Un objeto `tabla_apa`; la matriz de residuales está en `$datos`.
#' @examples
#' library(lavaan)
#' fit <- cfa("visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6
#'             speed =~ x7 + x8 + x9", data = HolzingerSwineford1939)
#' tabla_residuales(fit)
#' @export
tabla_residuales <- function(fit, umbral = .10, etiquetas = NULL, decimales = 2, titulo = NULL, archivo = NULL) {
  exigir_un_grupo(fit)
  exigir_lavaan(fit, "cfa() o sem()")
  r <- lavaan::lavResiduals(fit, type = "cor", summary = FALSE)
  if (!is.null(r$cov)) R <- r$cov else R <- r[[1]]$cov
  R <- unclass(R); v <- rownames(R); k <- length(v)
  cc <- matrix("", k, k - 1); neg <- matrix(FALSE, k, k - 1)
  for (i in seq_len(k)) for (j in seq_len(k - 1)) {
    if (i == j) cc[i, j] <- "\u2014"
    else if (i > j) { cc[i, j] <- f_apa(R[i, j], decimales); neg[i, j] <- abs(R[i, j]) > umbral }
  }
  low <- R[lower.tri(R)]; n_alto <- sum(abs(low) > umbral)
  im <- which(abs(R) == max(abs(low)) & lower.tri(R), arr.ind = TRUE)[1, ]
  nom <- paste0(seq_len(k), ". ", renombrar(v, etiquetas))
  srmr <- lavaan::fitMeasures(fit, "srmr")[[1]]
  nota <- paste0(tx("Residual = correlaci\u00f3n observada menos la reproducida por el modelo. En negritas, |residual| > ",
                    "Residual = observed minus model-implied correlation. |Residual| > "),
                 f_apa(umbral, 2), tx(" (Kline, 2016): ", " in bold (Kline, 2016): "), n_alto, tx(" de ", " of "), length(low),
                 tx(" pares. El residual m\u00e1s grande es el de ", " pairs. The largest residual is "),
                 renombrar(v[im[2]], etiquetas), tx(" y ", " with "), renombrar(v[im[1]], etiquetas), " (", f_apa(R[im[1], im[2]], decimales), "). SRMR = ",
                 f_apa(srmr, 3), ".")
  t <- nueva_tabla(if (is.null(titulo)) tx("Residuales de correlaci\u00f3n", "Correlation residuals") else titulo,
                   c("Variable", as.character(seq_len(k - 1))), cbind(nom, cc), cbind(FALSE, neg), nota = nota, datos = R)
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}
