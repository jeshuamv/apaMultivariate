# Modelo estructural: efectos directos, indirectos y totales; párrafo de método

# Todas las trayectorias indirectas (dos o más flechas) del modelo
#' @noRd
trayectorias_indirectas <- function(R) {
  # R: data.frame con columnas de (causa) y a (efecto) de las regresiones
  hijos <- split(R$a, R$de)
  out <- list()
  ir <- function(camino) {
    for (h in hijos[[utils::tail(camino, 1)]]) {
      if (h %in% camino) next                       # evita ciclos
      nuevo <- c(camino, h)
      if (length(nuevo) >= 3) out[[length(out) + 1]] <<- nuevo
      ir(nuevo)
    }
  }
  for (v in unique(R$de)) ir(v)
  out
}

# Valores de todos los efectos para un modelo estimado
#' @noRd
valores_efectos <- function(obj, R, caminos, pares) {
  pe <- lavaan::parameterEstimates(obj, se = FALSE)
  ss <- lavaan::standardizedSolution(obj, se = FALSE)
  if ("group" %in% names(pe)) { pe <- pe[pe$group == 1, ]; ss <- ss[ss$group == 1, ] }
  b <- function(de, a, std) {
    x <- if (std) ss else pe; v <- x[x$op == "~" & x$lhs == a & x$rhs == de, if (std) "est.std" else "est"]
    if (length(v)) v[1] else 0
  }
  un <- function(std) {
    dir <- vapply(seq_len(nrow(R)), function(i) b(R$de[i], R$a[i], std), numeric(1))
    ind <- vapply(caminos, function(cm) prod(vapply(seq_len(length(cm) - 1), function(k) b(cm[k], cm[k + 1], std), numeric(1))), numeric(1))
    tind <- vapply(seq_len(nrow(pares)), function(i) sum(ind[vapply(caminos, function(cm) cm[1] == pares$de[i] && utils::tail(cm, 1) == pares$a[i], logical(1))]), numeric(1))
    tot <- tind + vapply(seq_len(nrow(pares)), function(i) b(pares$de[i], pares$a[i], std), numeric(1))
    c(dir, ind, tind, tot)
  }
  c(un(FALSE), un(TRUE))
}

#' Tabla de efectos directos, indirectos y totales
#'
#' Para un modelo de trayectorias o SEM, reúne todos los efectos:
#'
#' * **Directos:** cada flecha del modelo (`~`).
#' * **Indirectos:** cada trayectoria con mediadores (por ejemplo
#'   X \eqn{\rightarrow} M \eqn{\rightarrow} Y), que se encuentran solas, sin
#'   tener que definirlas con `:=`, y el indirecto total de cada par.
#' * **Totales:** directo + indirectos, para cada par con efecto directo y
#'   mediación.
#'
#' Todos los efectos, incluidos los que no se muestran por repetir otra
#' fila, están en `$datos`.
#'
#' Con `bootstrap` (1000 remuestreos por omisión) los errores estándar y los
#' intervalos de confianza de todos los efectos se obtienen por bootstrap de
#' percentiles, el método recomendado para efectos indirectos (Preacher y
#' Hayes, 2008). Un efecto indirecto es significativo si su intervalo no
#' incluye el 0. Con `bootstrap = 0` se usa el método delta (prueba de Sobel
#' multivariada), que es inmediato pero menos preciso.
#'
#' Si el modelo tiene parámetros definidos con `:=`, se agregan en otra parte
#' de la tabla con los valores que calcula `lavaan`.
#'
#' @param fit Modelo de [lavaan::sem()] con al menos una regresión (`~`).
#' @param bootstrap Número de remuestreos bootstrap (`0` para usar el método
#'   delta). Con 1000 puede tardar de segundos a unos minutos según el
#'   modelo.
#' @param nivel Nivel de confianza de los intervalos.
#' @param semilla Semilla para que el bootstrap sea reproducible.
#' @inheritParams tabla_descriptivos
#' @return Un objeto `tabla_apa`. Los valores están en `$datos`; la segunda
#'   parte de la tabla tiene la \eqn{R^2} de las variables endógenas.
#' @examples
#' library(lavaan)
#' set.seed(1)
#' d <- data.frame(X = rnorm(200)); d$M <- .5 * d$X + rnorm(200)
#' d$Y <- .4 * d$M + .2 * d$X + rnorm(200)
#' fit <- sem("M ~ X
#'             Y ~ M + X", data = d)
#' tabla_efectos(fit, bootstrap = 0)
#' \donttest{
#' tabla_efectos(fit, bootstrap = 500)
#' }
#' @export
tabla_efectos <- function(fit, bootstrap = 1000, nivel = 0.95, semilla = 123, etiquetas = NULL,
                          decimales = 2, titulo = NULL, archivo = NULL) {
  exigir_un_grupo(fit)
  if (!inherits(fit, "lavaan")) stop("`fit` debe ser un modelo de lavaan (sem()).", call. = FALSE)
  pe0 <- lavaan::parameterEstimates(fit, level = nivel)
  if ("group" %in% names(pe0)) pe0 <- pe0[pe0$group == 1, ]
  reg <- pe0[pe0$op == "~", ]
  if (!nrow(reg)) stop("El modelo no tiene efectos (~): no hay nada que descomponer.", call. = FALSE)
  R <- data.frame(de = reg$rhs, a = reg$lhs, stringsAsFactors = FALSE)
  caminos <- trayectorias_indirectas(R)
  pares <- unique(data.frame(de = vapply(caminos, `[`, character(1), 1),
                             a = vapply(caminos, function(cm) utils::tail(cm, 1), character(1)), stringsAsFactors = FALSE))
  nd <- nrow(R); ni <- length(caminos); np <- nrow(pares); m <- nd + ni + 2 * np
  est <- valores_efectos(fit, R, caminos, pares)
  q <- (1 - nivel) / 2

  if (bootstrap > 0) {
    message("Calculando intervalos con ", bootstrap, " remuestreos bootstrap; puede tardar un poco...")
    if (!is.null(semilla)) set.seed(semilla)
    B <- suppressWarnings(lavaan::bootstrapLavaan(fit, R = bootstrap, FUN = function(x) valores_efectos(x, R, caminos, pares)))
    B <- B[stats::complete.cases(B), , drop = FALSE]
    ee <- apply(B, 2, stats::sd)
    li <- apply(B, 2, stats::quantile, probs = q); ls <- apply(B, 2, stats::quantile, probs = 1 - q)
    pval <- rep(NA_real_, 2 * m); pval[seq_len(nd)] <- reg$pvalue
    metodo <- paste0(tx("bootstrap de percentiles con ", "percentile bootstrap with "), nrow(B), tx(" remuestreos", " resamples"))
  } else {
    # Método delta con la matriz de covarianzas de los parámetros
    V <- lavaan::vcov(fit); cf <- lavaan::coef(fit)
    nombre <- function(de, a) { id <- reg$label[reg$lhs == a & reg$rhs == de]
      if (length(id) && nzchar(id[1]) && id[1] %in% names(cf)) id[1] else paste0(a, "~", de) }
    grad <- function(cm) {
      g <- stats::setNames(numeric(length(cf)), names(cf))
      bs <- vapply(seq_len(length(cm) - 1), function(k) { n <- nombre(cm[k], cm[k + 1]); if (n %in% names(cf)) cf[[n]] else 0 }, numeric(1))
      for (k in seq_len(length(cm) - 1)) { n <- nombre(cm[k], cm[k + 1]); if (n %in% names(g)) g[n] <- g[n] + prod(bs[-k]) }
      g
    }
    G <- c(lapply(seq_len(nd), function(i) grad(c(R$de[i], R$a[i]))), lapply(caminos, grad))
    Gt <- lapply(seq_len(np), function(i) Reduce(`+`, G[nd + which(vapply(caminos, function(cm) cm[1] == pares$de[i] && utils::tail(cm, 1) == pares$a[i], logical(1)))]))
    Gtot <- lapply(seq_len(np), function(i) { d <- which(R$de == pares$de[i] & R$a == pares$a[i]); if (length(d)) Gt[[i]] + G[[d]] else Gt[[i]] })
    G <- c(G, Gt, Gtot)
    ee <- c(vapply(G, function(g) sqrt(drop(t(g) %*% V[names(g), names(g)] %*% g)), numeric(1)), rep(NA_real_, m))
    z <- stats::qnorm(1 - q)
    li <- est - z * ee; ls <- est + z * ee
    pval <- 2 * stats::pnorm(-abs(est / ee)); pval[seq_len(nd)] <- reg$pvalue
    metodo <- tx("m\u00e9todo delta", "the delta method")
  }

  et <- function(v) renombrar(v, etiquetas)
  flecha <- function(cm) paste(et(cm), collapse = " \u2192 ")
  nombres <- c(vapply(seq_len(nd), function(i) flecha(c(R$de[i], R$a[i])), character(1)),
               vapply(caminos, flecha, character(1)),
               vapply(seq_len(np), function(i) paste0(et(pares$de[i]), " \u2192 ", et(pares$a[i]), tx(" (indirecto total)", " (total indirect)")), character(1)),
               vapply(seq_len(np), function(i) paste0(et(pares$de[i]), " \u2192 ", et(pares$a[i])), character(1)))
  tipo <- rep(c("directo", "indirecto", "indirecto_total", "total"), c(nd, ni, np, np))
  fila <- function(i) c(paste0("   ", nombres[i]), f_num(est[i], decimales), f_num(ee[i], decimales),
                        paste0("[", f_num(li[i], decimales), ", ", f_num(ls[i], decimales), "]"),
                        f_apa(est[m + i], decimales), if (is.na(pval[i])) "" else f_p(pval[i]))
  cel <- NULL; sep <- integer(0)
  seccion <- function(txt, idx) {
    if (!length(idx)) return()
    cel <<- rbind(cel, c(txt, rep("", 5)), t(vapply(idx, fila, character(6))))
    sep <<- c(sep, nrow(cel))
  }
  # Se omiten filas que repetirían otra: el indirecto total de un par con una
  # sola trayectoria y el total de un par sin efecto directo
  n_cam <- vapply(seq_len(np), function(i) sum(vapply(caminos, function(cm) cm[1] == pares$de[i] && utils::tail(cm, 1) == pares$a[i], logical(1))), numeric(1))
  con_dir <- vapply(seq_len(np), function(i) any(R$de == pares$de[i] & R$a == pares$a[i]), logical(1))
  mostrar <- rep(TRUE, m)
  mostrar[nd + ni + seq_len(np)] <- n_cam > 1
  mostrar[nd + ni + np + seq_len(np)] <- con_dir
  seccion(tx("Efectos directos", "Direct effects"), which(tipo == "directo"))
  seccion(tx("Efectos indirectos", "Indirect effects"), which(tipo %in% c("indirecto", "indirecto_total") & mostrar))
  seccion(tx("Efectos totales", "Total effects"), which(tipo == "total" & mostrar))
  sep <- utils::head(sep, -1)
  neg <- matrix(FALSE, nrow(cel), ncol(cel)); neg[cel[, 2] == "" & nzchar(cel[, 1]), 1] <- FALSE
  # Indirectos significativos (IC sin el 0) en negritas
  sig <- li > 0 | ls < 0
  for (i in which(tipo %in% c("indirecto", "indirecto_total") & sig))
    neg[which(trimws(cel[, 1]) == nombres[i])[1], 4] <- TRUE
  pct <- paste0(round(100 * nivel), tx(" %", "%"))
  ic_txt <- tx(paste("IC", pct), paste(pct, "CI"))
  nota <- paste0(tx("B = coeficiente no estandarizado; EE = error est\u00e1ndar; ", "B = unstandardized coefficient; SE = standard error; "),
                 ic_txt, tx(" por ", " by "), metodo,
                 tx("; \u03b2 = coeficiente estandarizado. Los valores p de los efectos directos son los de lavaan",
                    "; \u03b2 = standardized coefficient. p values for direct effects are from lavaan"),
                 if (bootstrap > 0) tx("; para los indirectos y totales la significancia se juzga por el intervalo",
                                       "; indirect and total effects are judged by their confidence interval") else "",
                 tx(". En negritas, efectos indirectos cuyo intervalo no incluye el 0 (significativos).",
                    ". Indirect effects whose interval excludes 0 (significant) are in bold."),
                 if (!ni) tx(" El modelo no tiene efectos indirectos.", " The model has no indirect effects.") else "")
  r2 <- tryCatch(lavaan::lavInspect(fit, "r2"), error = function(e) numeric(0)); if (is.list(r2)) r2 <- r2[[1]]
  endo <- intersect(unique(R$a), names(r2))
  extra <- list()
  if (length(endo)) extra[[1]] <- nueva_tabla(tx("Varianza explicada de las variables end\u00f3genas", "Variance explained in endogenous variables"), c("Variable", "R\u00b2"),
                                             cbind(et(endo), f_apa(r2[endo], decimales)), alinear = c("l", "c"))
  defs <- pe0[pe0$op == ":=", ]
  if (nrow(defs)) extra[[length(extra) + 1]] <- nueva_tabla(
    tx("Par\u00e1metros definidos en el modelo (:=)", "Defined parameters (:=)"), c(tx("Par\u00e1metro", "Parameter"), tx("Estimaci\u00f3n", "Estimate"), tx("EE", "SE"), ic_txt, "p"),
    cbind(defs$lhs, f_num(defs$est, decimales), f_num(defs$se, decimales),
          paste0("[", f_num(defs$ci.lower, decimales), ", ", f_num(defs$ci.upper, decimales), "]"), f_p(defs$pvalue)),
    nota = tx("Valores calculados por lavaan con el m\u00e9todo de errores est\u00e1ndar del modelo.", "Values computed by lavaan with the model's standard-error method."))
  datos <- data.frame(efecto = nombres, tipo = tipo, B = est[1:m], EE = ee[1:m], ic_inf = li[1:m], ic_sup = ls[1:m],
                      beta = est[(m + 1):(2 * m)], p = pval[1:m], row.names = NULL)
  t <- nueva_tabla(if (is.null(titulo)) tx("Efectos directos, indirectos y totales", "Direct, indirect, and total effects") else titulo,
                   c(tx("Efecto", "Effect"), "B", tx("EE", "SE"), ic_txt, "\u03b2", "p"), cel, neg,
                   alinear = c("l", "c", "c", "c", "c", "c"), nota = nota, separadores = sep, datos = datos, extra = extra)
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

# --- Párrafo de método ------------------------------------------------------------

#' @noRd
nombre_estimador <- function(e) switch(toupper(e),
  ML = tx("m\u00e1xima verosimilitud (ML)", "maximum likelihood (ML)"),
  MLR = tx("m\u00e1xima verosimilitud robusta (MLR), con errores est\u00e1ndar robustos y estad\u00edstico \u03c7\u00b2 corregido de Yuan-Bentler",
           "robust maximum likelihood (MLR), with robust standard errors and the Yuan-Bentler scaled \u03c7\u00b2 statistic"),
  MLM = tx("m\u00e1xima verosimilitud con la correcci\u00f3n de Satorra-Bentler (MLM)", "maximum likelihood with the Satorra-Bentler correction (MLM)"),
  MLMV = tx("m\u00e1xima verosimilitud con correcci\u00f3n de media y varianza (MLMV)", "maximum likelihood with mean and variance adjustment (MLMV)"),
  WLSMV = tx("m\u00ednimos cuadrados ponderados diagonalmente con media y varianza ajustadas (WLSMV), adecuado para \u00edtems ordinales",
             "diagonally weighted least squares with mean and variance adjustment (WLSMV), appropriate for ordinal items"),
  DWLS = tx("m\u00ednimos cuadrados ponderados diagonalmente (DWLS)", "diagonally weighted least squares (DWLS)"),
  ULS = tx("m\u00ednimos cuadrados no ponderados (ULS)", "unweighted least squares (ULS)"),
  ULSMV = tx("m\u00ednimos cuadrados no ponderados con media y varianza ajustadas (ULSMV)", "unweighted least squares with mean and variance adjustment (ULSMV)"),
  GLS = tx("m\u00ednimos cuadrados generalizados (GLS)", "generalized least squares (GLS)"),
  WLS = tx("m\u00ednimos cuadrados ponderados (WLS)", "weighted least squares (WLS)"),
  paste0(tx("el estimador ", "the "), e, tx("", " estimator")))

#' @noRd
cita_paquete <- function(p, autores, cita, titulo, extra = "") {
  v <- utils::packageDescription(p)$Version
  fecha <- utils::packageDescription(p)$`Date/Publication`
  if (is.null(fecha)) fecha <- utils::packageDescription(p)$Date
  anio <- if (!is.null(fecha)) substr(fecha, 1, 4) else format(Sys.Date(), "%Y")
  list(cita = paste0(cita, ", ", anio),
       ref = paste0(autores, " (", anio, "). ", titulo, tx(" (Paquete de R, versi\u00f3n ", " (R package version "), v, ")", extra, "."),
       version = v, anio = anio)
}

#' Párrafo de método y referencias
#'
#' Redacta el párrafo del apartado de análisis de datos con el software, las
#' versiones, el estimador, el manejo de datos perdidos, el tamaño de la
#' muestra y los criterios de ajuste, junto con sus referencias en formato
#' APA. Es un punto de partida: revíselo y adáptelo a su estudio.
#'
#' @param fit Modelo de `lavaan`.
#' @param bootstrap Si se usaron intervalos bootstrap (por ejemplo, en
#'   [tabla_efectos()]), número de remuestreos para mencionarlo.
#' @param paquetes Otros paquetes que se usaron y deben citarse: cualquiera de
#'   `"semTools"`, `"psych"` y `"apaMultivariate"`.
#' @param atipicos Si quitaste casos atípicos, el número de casos quitados o
#'   los datos que devolvió [quitar_atipicos()]; se menciona en el párrafo
#'   con su criterio.
#' @param archivo Si se indica, guarda el texto en `.docx` o `.txt`.
#' @return Un objeto de clase `texto_apa` con `$parrafo` y `$referencias`,
#'   que se imprime en la consola listo para copiar.
#' @examples
#' library(lavaan)
#' fit <- cfa("visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6", data = HolzingerSwineford1939)
#' parrafo_metodo(fit)
#' @export
parrafo_metodo <- function(fit, bootstrap = NULL, paquetes = c("apaMultivariate"), atipicos = NULL, archivo = NULL) {
  if (!inherits(fit, "lavaan")) stop("`fit` debe ser un modelo de lavaan.", call. = FALSE)
  op <- lavaan::lavInspect(fit, "options")
  est <- op$estimator; if (any(op$test %in% "satorra.bentler") && est == "ML") est <- "MLM"
  if (est == "ML" && any(c("yuan.bentler", "yuan.bentler.mplus") %in% op$test)) est <- "MLR"
  if (est == "DWLS" && any(c("scaled.shifted", "mean.var.adjusted") %in% op$test)) est <- "WLSMV"
  n <- lavaan::lavInspect(fit, "ntotal"); ng <- lavaan::lavInspect(fit, "ngroups")
  perd <- switch(op$missing, ml = , ml.x = , fiml = tx(" Los datos perdidos se trataron mediante m\u00e1xima verosimilitud con informaci\u00f3n completa (FIML).",
                                                    " Missing data were handled with full information maximum likelihood (FIML)."),
                 pairwise = tx(" Los datos perdidos se trataron con eliminaci\u00f3n por pares.", " Missing data were handled with pairwise deletion."), "")
  orig <- sum(unlist(lavaan::lavInspect(fit, "norig")))
  if (is.data.frame(atipicos)) atipicos <- length(attr(atipicos, "atipicos"))
  at_txt <- if (!is.null(atipicos) && atipicos > 0) tx(paste0(" Se excluyeron ", atipicos, " casos at\u00edpicos multivariados (distancia de Mahalanobis al cuadrado con p < .001; Kline, 2016)."),
                                                     paste0(" A total of ", atipicos, " multivariate outliers were excluded (squared Mahalanobis distance with p < .001; Kline, 2016).")) else ""
  wis <- identical(op$likelihood, "wishart")
  wis_txt <- if (wis) tx(", con la verosimilitud de Wishart (divisor N \u2212 1, como en AMOS)", ", with the Wishart likelihood (divisor N \u2212 1, as in AMOS)") else ""
  if (op$missing %in% c("listwise", "default") && orig > n) perd <- paste0(tx(" Se excluyeron ", " A total of "), orig - n,
                                                                       tx(" casos con datos perdidos (eliminaci\u00f3n por lista).", " cases with missing data were excluded (listwise deletion)."))
  rver <- paste(R.version$major, R.version$minor, sep = "."); ranio <- R.version$year
  lav <- utils::packageDescription("lavaan")$Version
  tipo <- if (length(lavaan::lavNames(fit, "lv")) && !any(lavaan::parTable(fit)$op == "~")) tx("an\u00e1lisis factorial confirmatorio", "confirmatory factor analysis")
          else if (length(lavaan::lavNames(fit, "lv"))) tx("modelo de ecuaciones estructurales", "structural equation model") else tx("an\u00e1lisis de trayectorias", "path analysis")
  refs <- c(paste0("R Core Team. (", ranio, "). R: A language and environment for statistical computing. R Foundation for Statistical Computing. https://www.R-project.org/"),
            "Rosseel, Y. (2012). lavaan: An R package for structural equation modeling. Journal of Statistical Software, 48(2), 1\u201336. https://doi.org/10.18637/jss.v048.i02",
            "Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. Structural Equation Modeling, 6(1), 1\u201355. https://doi.org/10.1080/10705519909540118")
  citas <- character(0)
  info <- list(
    semTools = list("Jorgensen, T. D., Pornprasertmanit, S., Schoemann, A. M., & Rosseel, Y.", "Jorgensen et al.", "semTools: Useful tools for structural equation modeling", ". https://CRAN.R-project.org/package=semTools"),
    psych = list("Revelle, W.", "Revelle", "psych: Procedures for psychological, psychometric, and personality research", ". Northwestern University. https://CRAN.R-project.org/package=psych"),
    apaMultivariate = list("Moreno Valladares, J.", "Moreno Valladares", tx("apaMultivariate: Tablas y diagramas APA para an\u00e1lisis multivariado", "apaMultivariate: APA-style tables and diagrams for multivariate analysis"), ". https://github.com/jeshuamv/apaMultivariate"))
  for (p in intersect(paquetes, names(info))) if (requireNamespace(p, quietly = TRUE)) {
    ci <- cita_paquete(p, info[[p]][[1]], info[[p]][[2]], info[[p]][[3]], info[[p]][[4]])
    citas[p] <- paste0(p, tx(" (versi\u00f3n ", " (version "), ci$version, "; ", ci$cita, ")"); refs <- c(refs, ci$ref)
  }
  pq <- c(paste0("lavaan (", tx("versi\u00f3n ", "version "), lav, "; Rosseel, 2012)"), unname(citas))
  lista <- if (en()) {
    if (length(pq) == 1) paste("the", pq, "package") else
      paste0("the packages ", paste(pq[-length(pq)], collapse = ", "), if (length(pq) > 2) ", and " else " and ", pq[length(pq)])
  } else if (length(pq) == 1) paste("el paquete", pq) else
    paste0("los paquetes ", paste(pq[-length(pq)], collapse = ", "), " y ", pq[length(pq)])
  parrafo <- if (en()) paste0(
    "Analyses were conducted in R (version ", rver, "; R Core Team, ", ranio, ") using ", lista, ". The ", tipo,
    " was estimated with a sample of ", n, " participants", if (ng > 1) paste0(" in ", ng, " groups") else "",
    " using ", nombre_estimador(est), wis_txt, ".", at_txt, perd,
    " Model fit was evaluated with the \u03c7\u00b2 test, the comparative fit index (CFI), the Tucker-Lewis index (TLI), ",
    "the root mean square error of approximation (RMSEA) with its 90% confidence interval, and the standardized ",
    "root mean square residual (SRMR). CFI and TLI \u2265 .95, RMSEA \u2264 .06, and SRMR \u2264 .08 were considered indicative ",
    "of good fit (Hu & Bentler, 1999).",
    if (!is.null(bootstrap)) paste0(" Indirect effects were tested with 95% percentile bootstrap confidence intervals based on ",
                                    bootstrap, " resamples; an effect was considered significant when its interval did not include zero.") else "")
  else paste0(
    "Los an\u00e1lisis se realizaron en R (versi\u00f3n ", rver, "; R Core Team, ", ranio, ") con ", lista, ". El ", tipo,
    " se estim\u00f3 con una muestra de ", n, " participantes", if (ng > 1) paste0(" en ", ng, " grupos") else "",
    " por ", nombre_estimador(est), wis_txt, ".", at_txt, perd,
    " El ajuste se evalu\u00f3 con la prueba \u03c7\u00b2, el \u00edndice de ajuste comparativo (CFI), el \u00edndice de Tucker-Lewis (TLI), ",
    "el error cuadr\u00e1tico medio de aproximaci\u00f3n (RMSEA) con su intervalo de confianza al 90 % y la ra\u00edz del residuo ",
    "cuadr\u00e1tico medio estandarizado (SRMR). Se consideraron indicadores de buen ajuste CFI y TLI \u2265 .95, RMSEA \u2264 .06 ",
    "y SRMR \u2264 .08 (Hu y Bentler, 1999).",
    if (!is.null(bootstrap)) paste0(" Los efectos indirectos se evaluaron con intervalos de confianza al 95 % obtenidos por bootstrap de percentiles con ",
                                    bootstrap, " remuestreos; un efecto se consider\u00f3 significativo cuando su intervalo no inclu\u00eda el cero.") else "")
  if (nzchar(at_txt)) refs <- c(refs, tx("Kline, R. B. (2016). Principles and practice of structural equation modeling (4.\u00aa ed.). Guilford Press.",
                                     "Kline, R. B. (2016). Principles and practice of structural equation modeling (4th ed.). Guilford Press."))
  refs <- sort(refs)
  t <- structure(list(parrafo = parrafo, referencias = refs), class = "texto_apa")
  if (!is.null(archivo)) {
    ext <- tolower(tools::file_ext(archivo))
    if (ext == "txt") writeLines(enc2utf8(c(parrafo, "", tx("Referencias", "References"), "", refs)), archivo, useBytes = TRUE)
    else if (ext == "docx") {
      cuerpo <- c(docx_par(docx_run(parrafo), despues = 240),
                  docx_par(docx_run(tx("Referencias", "References"), b = TRUE), despues = 120),
                  vapply(refs, function(r) sub("<w:pPr>", "<w:pPr><w:ind w:left=\"720\" w:hanging=\"720\"/>",
                                                docx_par(docx_run(r), despues = 120), fixed = TRUE), character(1)))
      empaquetar_docx(paste(cuerpo, collapse = ""), archivo)
    } else stop("Usa un archivo .docx o .txt.", call. = FALSE)
  }
  t
}

#' @export
print.texto_apa <- function(x, ...) {
  cat("\n", paste(strwrap(x$parrafo, width = 80), collapse = "\n"), "\n\n", tx("Referencias", "References"), "\n\n", sep = "")
  for (r in x$referencias) cat(paste(strwrap(r, width = 80, exdent = 4), collapse = "\n"), "\n\n", sep = "")
  invisible(x)
}
