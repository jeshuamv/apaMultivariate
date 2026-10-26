# Tablas en formato APA para AFE, AFC, SEM y análisis de trayectorias

# --- Formato de números --------------------------------------------------
#' @noRd
f_apa <- function(x, d = 2) {
  r <- round(x, d); r[!is.na(r) & r == 0] <- 0                       # evita "-.00"
  ifelse(is.na(x), "", sub("^(-?)0\\.", "\\1.", formatC(r, format = "f", digits = d)))
}
#' @noRd
f_num <- function(x, d = 2) { r <- round(x, d); r[!is.na(r) & r == 0] <- 0
  ifelse(is.na(x), "", formatC(r, format = "f", digits = d, big.mark = ",")) }
#' @noRd
f_p <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "< .001", f_apa(p, 3)))

#' @noRd
nueva_tabla <- function(titulo, columnas, celdas, negritas = NULL, alinear = NULL, nota = NULL,
                        separadores = integer(0), datos = NULL, extra = list()) {
  celdas <- as.matrix(celdas); celdas[is.na(celdas)] <- ""
  if (is.null(negritas)) negritas <- matrix(FALSE, nrow(celdas), ncol(celdas))
  if (is.null(alinear)) alinear <- c("l", rep("c", ncol(celdas) - 1))
  structure(list(titulo = titulo, columnas = columnas, celdas = celdas, negritas = negritas,
                 alinear = alinear, nota = nota, separadores = separadores, datos = datos,
                 extra = extra), class = "tabla_apa")
}

#' @noRd
renombrar <- function(v, etiquetas) {
  if (is.null(etiquetas)) return(v)
  i <- v %in% names(etiquetas); v[i] <- unlist(etiquetas[v[i]]); v
}

#' Tabla de cargas factoriales
#'
#' Crea la tabla de cargas en formato APA, lista para un artículo o una tesis.
#'
#' * **Análisis exploratorio** (`psych::fa()` o [lavaan::efa()]): matriz de
#'   cargas (de patrón si la rotación es oblicua) con los ítems ordenados por
#'   factor y por carga, comunalidades (\eqn{h^2}), varianza explicada y, si la
#'   rotación es oblicua, las correlaciones entre factores.
#' * **AFC, SEM** ([lavaan::cfa()], [lavaan::sem()]): cargas estandarizadas
#'   con su error estándar, valor *p* y \eqn{R^2} de cada ítem.
#'
#' @param x Análisis exploratorio o modelo de `lavaan`.
#' @param umbral Solo en exploratorios: carga mínima que se considera
#'   importante (por omisión .30).
#' @param ocultar_bajas Solo en exploratorios: si `TRUE` (por omisión), deja en
#'   blanco las cargas menores que el umbral.
#' @param etiquetas Vector con nombres para cambiar cómo se ven los ítems y
#'   factores, p. ej. `c(MR1 = "Neuroticismo")`.
#' @param decimales Número de decimales.
#' @param nfactores Para `lavaan::efa()` con varias soluciones.
#' @param titulo Título de la tabla (hay uno por omisión).
#' @param archivo Si se indica, guarda la tabla (`.rtf` para Word, `.html` o
#'   `.csv`) con [guardar_tabla()].
#' @return Un objeto `tabla_apa` que se imprime en la consola. Sus valores
#'   numéricos están en `$datos`.
#' @examples
#' library(lavaan)
#' fit <- cfa("visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6", data = HolzingerSwineford1939)
#' tabla_cargas(fit)
#'
#' afe <- efa(data = HolzingerSwineford1939[, 7:15], nfactors = 3)
#' tabla_cargas(afe)
#' @export
tabla_cargas <- function(x, umbral = 0.30, ocultar_bajas = TRUE, etiquetas = NULL, decimales = 2,
                         nfactores = NULL, titulo = NULL, archivo = NULL) {
  t <- if (es_afe(x)) tabla_cargas_afe(x, umbral, ocultar_bajas, etiquetas, decimales, nfactores, titulo)
       else if (inherits(x, "lavaan")) tabla_cargas_lavaan(x, etiquetas, decimales, titulo)
       else stop("`x` debe ser un modelo de lavaan o un an\u00e1lisis exploratorio de psych::fa() o lavaan::efa().",
                 call. = FALSE)
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

#' @noRd
tabla_cargas_afe <- function(x, umbral, ocultar_bajas, etiquetas, decimales, nfactores, titulo) {
  A <- leer_afe(x, nfactores); L <- A$L; fac <- colnames(L)
  principal <- fac[apply(abs(L), 1, which.max)]
  ord <- order(match(principal, fac), -abs(L[cbind(seq_len(nrow(L)), match(principal, fac))]))
  L <- L[ord, , drop = FALSE]; principal <- principal[ord]; h2 <- A$h2[rownames(L)]
  cel <- matrix(f_apa(L, decimales), nrow(L))
  neg <- matrix(FALSE, nrow(L), ncol(L))
  for (i in seq_len(nrow(L))) neg[i, match(principal[i], fac)] <- abs(L[i, principal[i]]) >= umbral
  if (ocultar_bajas) cel[abs(L) < umbral] <- ""
  cel <- cbind(renombrar(rownames(L), etiquetas), cel, f_apa(h2, decimales))
  neg <- cbind(FALSE, neg, FALSE)
  pie <- rbind(c(tx("Varianza explicada", "Explained variance"), f_num(A$ss, decimales), ""),
               c(tx("% de varianza", "% of variance"), f_num(100 * A$prop, 1), ""),
               c(tx("% acumulado", "Cumulative %"), f_num(100 * A$acum, 1), ""))
  cel <- rbind(cel, pie); neg <- rbind(neg, matrix(FALSE, 3, ncol(neg)))
  oblicua <- !is.null(A$Phi)
  nota <- paste0(if (oblicua) tx("Matriz de patr\u00f3n. ", "Pattern matrix. ") else "", tx("M\u00e9todo: ", "Method: "), A$metodo, ". ",
                 if (!is.null(A$n) && !is.na(A$n)) paste0("N = ", A$n, ". ") else "",
                 if (ocultar_bajas) paste0(tx("Se omiten cargas menores que ", "Loadings below "), f_apa(umbral, 2), tx(". ", " are omitted. ")) else "",
                 tx("En negritas, la carga m\u00e1s alta de cada \u00edtem. h\u00b2 = comunalidad.",
                    "The highest loading of each item is in bold. h\u00b2 = communality."))
  extra <- list()
  if (oblicua) {
    P <- A$Phi; k <- ncol(P); cc <- matrix("", k, k)
    for (i in seq_len(k)) for (j in seq_len(k)) cc[i, j] <- if (i == j) "\u2014" else if (j < i) f_apa(P[i, j], decimales) else ""
    nf <- renombrar(fac, etiquetas)
    extra[[1]] <- nueva_tabla(tx("Correlaciones entre factores", "Factor correlations"), c("Factor", nf), cbind(nf, cc))
  }
  datos <- data.frame(item = rownames(L), L, h2 = h2, row.names = NULL, check.names = FALSE)
  nueva_tabla(if (is.null(titulo)) tx("Cargas factoriales del an\u00e1lisis factorial exploratorio", "Exploratory factor analysis loadings") else titulo,
              c(tx("\u00cdtem", "Item"), renombrar(fac, etiquetas), "h\u00b2"), cel, neg, nota = nota,
              separadores = nrow(L), datos = datos, extra = extra)
}

#' @noRd
tabla_cargas_lavaan <- function(fit, etiquetas, decimales, titulo) {
  exigir_un_grupo(fit)
  s <- lavaan::standardizedSolution(fit)
  if ("group" %in% names(s)) s <- s[s$group == 1, ]
  s <- s[s$op == "=~", ]
  if (!nrow(s)) stop("El modelo no tiene cargas factoriales (=~).", call. = FALSE)
  r2 <- tryCatch(lavaan::lavInspect(fit, "r2"), error = function(e) numeric(0))
  if (is.list(r2)) r2 <- r2[[1]]
  fac_prev <- ""
  cel <- t(vapply(seq_len(nrow(s)), function(j) {
    f <- if (s$lhs[j] != fac_prev) renombrar(s$lhs[j], etiquetas) else ""
    fac_prev <<- s$lhs[j]
    c(f, renombrar(s$rhs[j], etiquetas), f_apa(s$est.std[j], decimales), f_apa(s$se[j], 3), f_p(s$pvalue[j]),
      if (s$rhs[j] %in% names(r2)) f_apa(r2[[s$rhs[j]]], decimales) else "")
  }, character(6)))
  datos <- data.frame(factor = s$lhs, item = s$rhs, carga = s$est.std, ee = s$se, p = s$pvalue,
                      r2 = unname(r2[s$rhs]))
  nueva_tabla(if (is.null(titulo)) tx("Cargas factoriales estandarizadas", "Standardized factor loadings") else titulo,
              c("Factor", tx("\u00cdtem", "Item"), "\u03bb", tx("EE", "SE"), "p", "R\u00b2"), cel,
              alinear = c("l", "l", "c", "c", "c", "c"),
              nota = paste0(tx("\u03bb = carga factorial estandarizada; EE = error est\u00e1ndar; R\u00b2 = proporci\u00f3n de varianza del \u00edtem explicada por su factor. N = ",
                               "\u03bb = standardized factor loading; SE = standard error; R\u00b2 = proportion of item variance explained by its factor. N = "),
                            lavaan::lavInspect(fit, "ntotal"), "."),
              datos = datos)
}

#' Tabla de índices de ajuste
#'
#' Reúne los índices de ajuste de uno o varios modelos de `lavaan` en una
#' tabla APA, para compararlos lado a lado. Si el modelo se estimó con un
#' estimador robusto (`MLM`, `MLR`, `WLSMV`, ...), se usan las versiones
#' corregidas de los índices.
#'
#' @param ... Uno o varios modelos de `lavaan`. Si se les pone nombre, ese
#'   nombre aparece en la tabla: `tabla_ajuste("Modelo 1" = fit1, "Modelo 2" = fit2)`.
#' @param extra Índices adicionales: cualquiera de `"gfi"`, `"agfi"`,
#'   `"nfi"`, `"pnfi"`, `"aic"`, `"bic"`.
#' @param ic_rmsea Nivel de confianza del intervalo del RMSEA: `.90` (el
#'   habitual, por defecto) o, si te lo piden, `.95`.
#' @param titulo Título de la tabla.
#' @param archivo Si se indica, guarda la tabla (`.rtf`, `.html` o `.csv`).
#' @return Un objeto `tabla_apa`. Los valores numéricos están en `$datos`.
#' @examples
#' library(lavaan)
#' m1 <- cfa("g =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9", data = HolzingerSwineford1939)
#' m3 <- cfa("visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6
#'             speed =~ x7 + x8 + x9", data = HolzingerSwineford1939)
#' tabla_ajuste("Un factor" = m1, "Tres factores" = m3, extra = c("aic", "bic"))
#' @export
tabla_ajuste <- function(..., extra = NULL, ic_rmsea = .90, titulo = NULL, archivo = NULL) {
  fits <- list(...)
  if (length(fits) == 1 && is.list(fits[[1]]) && !inherits(fits[[1]], "lavaan")) fits <- fits[[1]]
  if (!length(fits) || !all(vapply(fits, inherits, logical(1), "lavaan")))
    stop("Pasa uno o varios modelos de lavaan.", call. = FALSE)
  nms <- names(fits); if (is.null(nms)) nms <- rep("", length(fits))
  nms[nms == ""] <- paste(tx("Modelo", "Model"), which(nms == ""))
  extras_ok <- c("gfi", "agfi", "nfi", "pnfi", "aic", "bic")
  if (!all(extra %in% extras_ok)) stop("`extra` acepta: ", paste(extras_ok, collapse = ", "), ".", call. = FALSE)

  robusto <- FALSE
  filas <- lapply(fits, function(f) {
    fm <- medidas_ajuste(f, ic_rmsea)
    usa <- function(base, alt) { for (a in c(alt, base)) if (a %in% names(fm) && !is.na(fm[[a]])) return(fm[[a]]); NA }
    rob <- "chisq.scaled" %in% names(fm) && !is.na(fm[["chisq.scaled"]])
    if (rob) robusto <<- TRUE
    v <- c(chisq = usa("chisq", if (rob) "chisq.scaled"), df = fm[["df"]],
           pvalue = usa("pvalue", if (rob) "pvalue.scaled"),
           cfi = usa("cfi", if (rob) c("cfi.robust", "cfi.scaled")),
           tli = usa("tli", if (rob) c("tli.robust", "tli.scaled")),
           rmsea = usa("rmsea", if (rob) c("rmsea.robust", "rmsea.scaled")),
           lo = usa("rmsea.ci.lower", if (rob) c("rmsea.ci.lower.robust", "rmsea.ci.lower.scaled")),
           hi = usa("rmsea.ci.upper", if (rob) c("rmsea.ci.upper.robust", "rmsea.ci.upper.scaled")),
           srmr = usa("srmr", NULL))
    for (e in extra) v[e] <- usa(e, NULL)
    v
  })
  D <- as.data.frame(do.call(rbind, filas)); D <- cbind(modelo = nms, D)
  cel <- cbind(nms, f_num(D$chisq, 2), formatC(D$df, format = "d"), f_p(D$pvalue),
               f_num(D$chisq / D$df, 2), f_apa(D$cfi, 3), f_apa(D$tli, 3),
               paste0(f_apa(D$rmsea, 3), " [", f_apa(D$lo, 3), ", ", f_apa(D$hi, 3), "]"), f_apa(D$srmr, 3))
  cols <- c(tx("Modelo", "Model"), "\u03c7\u00b2", tx("gl", "df"), "p", tx("\u03c7\u00b2/gl", "\u03c7\u00b2/df"), "CFI", "TLI", cab_rmsea(ic_rmsea), "SRMR")
  for (e in extra) {
    cel <- cbind(cel, if (e %in% c("aic", "bic")) f_num(D[[e]], 2) else f_apa(D[[e]], 3))
    cols <- c(cols, toupper(e))
  }
  nota <- paste0(if (robusto) tx("Se usan los \u00edndices corregidos del estimador robusto. ", "Robust (scaled) fit indices are reported. ") else "",
                 tx("Criterios de buen ajuste (Hu y Bentler, 1999): CFI y TLI \u2265 .95 (\u2265 .90 aceptable), RMSEA \u2264 .06 (\u2264 .08 aceptable), SRMR \u2264 .08. Con muestras grandes la prueba \u03c7\u00b2 suele ser significativa aunque el ajuste sea bueno.",
                    "Good-fit criteria (Hu & Bentler, 1999): CFI and TLI \u2265 .95 (\u2265 .90 acceptable), RMSEA \u2264 .06 (\u2264 .08 acceptable), SRMR \u2264 .08. With large samples the \u03c7\u00b2 test is usually significant even when fit is good."))
  t <- nueva_tabla(if (is.null(titulo)) tx("\u00cdndices de ajuste", "Fit indices") else titulo, cols, cel, nota = nota, datos = D)
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

#' Tabla de confiabilidad y validez (CR, AVE y Fornell-Larcker)
#'
#' Calcula para cada factor de un AFC la confiabilidad compuesta (CR, también
#' llamada omega) y la varianza promedio extraída (AVE), y arma la matriz de
#' Fornell y Larcker (1981): raíz cuadrada del AVE en la diagonal (en
#' negritas) y correlaciones entre factores debajo.
#'
#' Se usan las fórmulas clásicas con las cargas estandarizadas \eqn{\lambda}:
#' \deqn{CR = (\sum\lambda)^2 / [(\sum\lambda)^2 + \sum(1-\lambda^2)]}
#' \deqn{AVE = \sum\lambda^2 / n}
#' Otros programas usan variantes que pueden dar valores ligeramente
#' distintos; con `metodo = "semtools"` se usan `semTools::compRelSEM()` y
#' `semTools::AVE()` (requiere el paquete `semTools`).
#'
#' @param fit Modelo de [lavaan::cfa()] o [lavaan::sem()] con al menos un
#'   factor.
#' @param metodo `"clasico"` (fórmulas de Fornell y Larcker, por omisión) o
#'   `"semtools"`.
#' @param etiquetas Vector con nombres para cambiar cómo se ven los factores.
#' @param decimales Número de decimales.
#' @param titulo Título de la tabla.
#' @param archivo Si se indica, guarda la tabla (`.rtf`, `.html` o `.csv`).
#' @return Un objeto `tabla_apa`. Los valores numéricos (CR, AVE, raíz del AVE
#'   y si cumple Fornell-Larcker) están en `$datos`.
#' @examples
#' library(lavaan)
#' fit <- cfa("visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6
#'             speed =~ x7 + x8 + x9", data = HolzingerSwineford1939)
#' tabla_validez(fit)
#' @export
tabla_validez <- function(fit, metodo = c("clasico", "semtools"), etiquetas = NULL, decimales = 2,
                          titulo = NULL, archivo = NULL) {
  exigir_un_grupo(fit)
  metodo <- match.arg(metodo)
  if (!inherits(fit, "lavaan")) stop("`fit` debe ser un modelo de lavaan (cfa() o sem()).", call. = FALSE)
  s <- lavaan::standardizedSolution(fit)
  if ("group" %in% names(s)) s <- s[s$group == 1, ]
  lv <- lavaan::lavNames(fit, "lv")
  s <- s[s$op == "=~" & !s$rhs %in% lv, ]
  fac <- unique(s$lhs)
  if (!length(fac)) stop("El modelo no tiene factores con \u00edtems.", call. = FALSE)
  cr  <- vapply(fac, function(f) { l <- s$est.std[s$lhs == f]; sum(l)^2 / (sum(l)^2 + sum(1 - l^2)) }, numeric(1))
  ave <- vapply(fac, function(f) mean(s$est.std[s$lhs == f]^2), numeric(1))
  if (metodo == "semtools") {
    if (!requireNamespace("semTools", quietly = TRUE))
      stop("Para metodo = \"semtools\" instala el paquete: install.packages(\"semTools\").", call. = FALSE)
    num <- function(v) vapply(fac, function(f) as.numeric(unlist(v[f]))[1], numeric(1))
    cr  <- num(semTools::compRelSEM(fit, tau.eq = FALSE))
    ave <- num(semTools::AVE(fit))
  }
  R <- lavaan::lavInspect(fit, "cor.lv"); if (is.list(R)) R <- R[[1]]
  R <- R[fac, fac, drop = FALSE]
  k <- length(fac); cc <- matrix("", k, k); neg <- matrix(FALSE, k, k)
  for (i in seq_len(k)) for (j in seq_len(i)) {
    if (i == j) { cc[i, j] <- f_apa(sqrt(ave[i]), decimales); neg[i, j] <- TRUE }
    else cc[i, j] <- f_apa(R[i, j], decimales)
  }
  maxr <- vapply(seq_len(k), function(i) if (k > 1) max(abs(R[i, -i])) else 0, numeric(1))
  cumple <- sqrt(ave) > maxr
  nf <- renombrar(fac, etiquetas)
  cel <- cbind(nf, f_apa(cr, decimales), f_apa(ave, decimales), cc)
  nota <- paste0(if (metodo == "semtools") tx("CR y AVE calculados con semTools. ", "CR and AVE computed with semTools. ") else "",
                 tx("CR = confiabilidad compuesta; AVE = varianza promedio extra\u00edda. En negritas, la ra\u00edz cuadrada del AVE; debajo de la diagonal, correlaciones entre factores. Criterios: CR > .70, AVE > .50 (Hair et al., 2014) y ra\u00edz del AVE mayor que las correlaciones del factor (Fornell y Larcker, 1981). ",
                    "CR = composite reliability; AVE = average variance extracted. The square root of the AVE is in bold on the diagonal; factor correlations are below the diagonal. Criteria: CR > .70, AVE > .50 (Hair et al., 2014), and square root of the AVE greater than the factor's correlations (Fornell & Larcker, 1981). "),
                 if (all(cumple)) tx("Todos los factores cumplen el criterio de Fornell-Larcker.", "All factors meet the Fornell-Larcker criterion.")
                 else paste0(tx("No cumplen el criterio de Fornell-Larcker: ", "Factors not meeting the Fornell-Larcker criterion: "), paste(nf[!cumple], collapse = ", "), "."))
  datos <- data.frame(factor = fac, CR = cr, AVE = ave, raiz_AVE = sqrt(ave), r_maxima = maxr,
                      fornell_larcker = cumple, row.names = NULL)
  t <- nueva_tabla(if (is.null(titulo)) tx("Confiabilidad compuesta, varianza promedio extra\u00edda y correlaciones entre factores",
                                          "Composite reliability, average variance extracted, and factor correlations") else titulo,
                   c("Factor", "CR", "AVE", nf), cel, cbind(matrix(FALSE, k, 3), neg),
                   nota = nota, datos = datos)
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}
