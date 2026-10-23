# Tablas del modelo de medición: HTMT, comparación de modelos, sesgo de
# método común y factorabilidad para el AFE

# Factores e ítems de un modelo de lavaan (solo factores de primer orden)
#' @noRd
factores_de <- function(fit) {
  pt <- lavaan::parTable(fit); lv <- lavaan::lavNames(fit, "lv")
  L <- pt[pt$op == "=~" & !pt$rhs %in% lv & pt$group %in% c(0, 1), ]
  split(L$rhs, factor(L$lhs, levels = unique(L$lhs)))
}

# Matriz de correlaciones observadas (grupo 1) de un modelo de lavaan
#' @noRd
cor_observada <- function(fit) {
  S <- lavaan::lavInspect(fit, "sampstat")
  if (!is.null(S$cov)) S <- S$cov else S <- S[[1]]$cov
  stats::cov2cor(unclass(S))
}

# --- HTMT ---------------------------------------------------------------------

#' @noRd
calcular_htmt <- function(R, factores, tipo) {
  R <- abs(R); k <- length(factores); H <- matrix(NA_real_, k, k, dimnames = list(names(factores), names(factores)))
  media <- function(v) if (tipo == "htmt2") exp(mean(log(v))) else mean(v)
  mono <- vapply(factores, function(it) { m <- R[it, it]; media(m[lower.tri(m)]) }, numeric(1))
  for (i in seq_len(k)) for (j in seq_len(k)) if (i > j)
    H[i, j] <- media(R[factores[[i]], factores[[j]]]) / sqrt(mono[i] * mono[j])
  H
}

#' Tabla HTMT de validez discriminante
#'
#' Calcula la razón heterorrasgo-monorrasgo (HTMT; Henseler et al., 2015)
#' entre cada par de factores. Es el criterio de validez discriminante que hoy
#' piden más revisores, porque el de Fornell-Larcker detecta mal la falta de
#' validez discriminante. Valores menores que .85 (criterio estricto) o .90
#' (criterio flexible) indican validez discriminante.
#'
#' @param x Modelo de `lavaan` (se usan sus factores e ítems y la matriz de
#'   correlaciones observada) o `data.frame`.
#' @param factores Solo con `data.frame`: lista con los ítems de cada factor.
#' @param tipo `"htmt"` (Henseler et al., 2015, por omisión) o `"htmt2"`
#'   (media geométrica; Roemer et al., 2021), que es la que calcula por
#'   omisión `semTools::htmt()`.
#' @param umbral Valor máximo aceptable (`.85` por omisión).
#' @inheritParams tabla_descriptivos
#' @return Un objeto `tabla_apa`. La matriz HTMT está en `$datos`.
#' @examples
#' library(lavaan)
#' fit <- cfa("visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6
#'             speed =~ x7 + x8 + x9", data = HolzingerSwineford1939)
#' tabla_htmt(fit)
#' @export
tabla_htmt <- function(x, factores = NULL, tipo = c("htmt", "htmt2"), umbral = 0.85, etiquetas = NULL,
                       decimales = 2, titulo = NULL, archivo = NULL) {
  tipo <- match.arg(tipo)
  if (inherits(x, "lavaan")) {
    factores <- factores_de(x); R <- cor_observada(x)
  } else {
    if (is.null(factores) || !is.list(factores) || is.null(names(factores)))
      stop("Con un data.frame indica los \u00edtems de cada factor: factores = list(F1 = c(...), F2 = c(...)).", call. = FALSE)
    d <- obtener_datos(x, unique(unlist(factores)))
    R <- stats::cor(d, use = "pairwise.complete.obs")
  }
  if (length(factores) < 2) stop("Se necesitan al menos dos factores.", call. = FALSE)
  if (any(lengths(factores) < 2)) stop("Cada factor necesita al menos dos \u00edtems para calcular el HTMT.", call. = FALSE)
  H <- calcular_htmt(R, factores, tipo)
  k <- length(factores); nf <- renombrar(names(factores), etiquetas)
  cc <- matrix("", k, k - 1); neg <- matrix(FALSE, k, k - 1)
  for (i in seq_len(k)) for (j in seq_len(k - 1)) {
    if (i == j) cc[i, j] <- "\u2014"
    else if (i > j) { cc[i, j] <- f_apa(H[i, j], decimales); neg[i, j] <- H[i, j] >= umbral }
  }
  malos <- which(H >= umbral, arr.ind = TRUE)
  pares <- if (nrow(malos)) paste(apply(malos, 1, function(z) paste(nf[z[1]], "y", nf[z[2]])), collapse = "; ") else ""
  nota <- paste0("HTMT = raz\u00f3n heterorrasgo-monorrasgo",
                 if (tipo == "htmt") " (Henseler et al., 2015)" else " con medias geom\u00e9tricas (HTMT2; Roemer et al., 2021)",
                 ". Valores menores que .85 (criterio estricto) o .90 (flexible) indican validez discriminante. ",
                 if (nrow(malos)) paste0("En negritas, valores \u2265 ", f_apa(umbral, 2), ": ", pares, ".")
                 else paste0("Todos los pares son menores que ", f_apa(umbral, 2), "."))
  t <- nueva_tabla(if (is.null(titulo)) "Validez discriminante: raz\u00f3n heterorrasgo-monorrasgo (HTMT)" else titulo,
                   c("Factor", nf[-k]), cbind(nf, cc), cbind(FALSE, neg), nota = nota, datos = H)
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

# --- Comparación de modelos --------------------------------------------------------

#' @noRd
indices <- function(f) {
  fm <- lavaan::fitMeasures(f)
  rob <- "chisq.scaled" %in% names(fm) && !is.na(fm[["chisq.scaled"]])
  usa <- function(base, alt) { for (a in c(alt, base)) if (a %in% names(fm) && !is.na(fm[[a]])) return(fm[[a]]); NA_real_ }
  c(chisq = usa("chisq", if (rob) "chisq.scaled"), df = fm[["df"]],
    pvalue = usa("pvalue", if (rob) "pvalue.scaled"),
    cfi = usa("cfi", if (rob) c("cfi.robust", "cfi.scaled")),
    tli = usa("tli", if (rob) c("tli.robust", "tli.scaled")),
    rmsea = usa("rmsea", if (rob) c("rmsea.robust", "rmsea.scaled")),
    lo = usa("rmsea.ci.lower", if (rob) c("rmsea.ci.lower.robust", "rmsea.ci.lower.scaled")),
    hi = usa("rmsea.ci.upper", if (rob) c("rmsea.ci.upper.robust", "rmsea.ci.upper.scaled")),
    srmr = usa("srmr", NULL), aic = usa("aic", NULL), bic = usa("bic", NULL), robusto = rob)
}

#' @noRd
diferencia_chi <- function(a, b) {
  r <- tryCatch(suppressWarnings(lavaan::lavTestLRT(a, b)), error = function(e) NULL)
  if (is.null(r) || nrow(r) < 2) return(c(NA, NA, NA))
  c(r[2, "Chisq diff"], r[2, "Df diff"], r[2, "Pr(>Chisq)"])
}

#' Tabla de comparación de modelos
#'
#' Compara modelos alternativos (por ejemplo, de uno, dos y tres factores, o
#' de primer y segundo orden): índices de ajuste, AIC y BIC, y las
#' diferencias entre cada modelo y el anterior (o el primero): \eqn{\Delta\chi^2}
#' con su prueba, \eqn{\Delta}CFI y \eqn{\Delta}RMSEA.
#'
#' La prueba \eqn{\Delta\chi^2} se calcula con [lavaan::lavTestLRT()], que con
#' estimadores robustos aplica la corrección de Satorra-Bentler. Solo es
#' válida para **modelos anidados** (uno es una versión restringida del
#' otro); para modelos no anidados use `anidados = FALSE` y compare AIC y BIC
#' (menor es mejor).
#'
#' @param ... Dos o más modelos de `lavaan`, con nombre:
#'   `tabla_comparacion("Un factor" = m1, "Tres factores" = m3)`.
#' @param referencia `"anterior"` (cada modelo contra el anterior, por
#'   omisión) o `"primero"` (todos contra el primero).
#' @param anidados Si `FALSE`, no se calcula \eqn{\Delta\chi^2}.
#' @param titulo Título de la tabla.
#' @param archivo Si se indica, guarda la tabla (`.docx`, `.rtf`, `.html` o
#'   `.csv`).
#' @return Un objeto `tabla_apa`. Los valores están en `$datos`.
#' @examples
#' library(lavaan)
#' HS <- HolzingerSwineford1939
#' m1 <- cfa("g =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9", data = HS)
#' m3 <- cfa("visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6
#'             speed =~ x7 + x8 + x9", data = HS)
#' tabla_comparacion("Un factor" = m1, "Tres factores" = m3)
#' @export
tabla_comparacion <- function(..., referencia = c("anterior", "primero"), anidados = TRUE,
                              titulo = NULL, archivo = NULL) {
  referencia <- match.arg(referencia)
  fits <- list(...)
  if (length(fits) == 1 && is.list(fits[[1]]) && !inherits(fits[[1]], "lavaan")) fits <- fits[[1]]
  if (length(fits) < 2 || !all(vapply(fits, inherits, logical(1), "lavaan")))
    stop("Pasa dos o m\u00e1s modelos de lavaan.", call. = FALSE)
  nms <- names(fits); if (is.null(nms)) nms <- rep("", length(fits))
  nms[nms == ""] <- paste("Modelo", which(nms == ""))
  I <- as.data.frame(do.call(rbind, lapply(fits, indices)))
  n <- length(fits); dif <- matrix(NA_real_, n, 5, dimnames = list(NULL, c("dchi", "dgl", "p", "dcfi", "drmsea")))
  for (i in 2:n) {
    r <- if (referencia == "anterior") i - 1 else 1
    if (anidados) dif[i, 1:3] <- diferencia_chi(fits[[i]], fits[[r]])
    dif[i, 4] <- I$cfi[i] - I$cfi[r]; dif[i, 5] <- I$rmsea[i] - I$rmsea[r]
  }
  f_d <- function(x, d = 3) { r <- round(x, d); r[!is.na(r) & r == 0] <- 0
    ifelse(is.na(x), "", sub("^([+-]?)0\\.", "\\1.", formatC(r, format = "f", digits = d, flag = "+"))) }
  cel <- cbind(nms, f_num(I$chisq, 2), formatC(I$df, format = "d"), f_apa(I$cfi, 3), f_apa(I$tli, 3),
               f_apa(I$rmsea, 3), f_apa(I$srmr, 3), f_num(I$aic, 1), f_num(I$bic, 1))
  cols <- c("Modelo", "\u03c7\u00b2", "gl", "CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC")
  if (anidados) {
    cel <- cbind(cel, ifelse(is.na(dif[, 1]), "", paste0(f_num(dif[, 1], 2), " (", formatC(dif[, 2], format = "d"), ")")),
                 f_p(dif[, 3]))
    cols <- c(cols, "\u0394\u03c7\u00b2 (\u0394gl)", "p")
  }
  cel <- cbind(cel, f_d(dif[, 4]), f_d(dif[, 5])); cols <- c(cols, "\u0394CFI", "\u0394RMSEA")
  cel[1, (ncol(cel) - if (anidados) 3 else 1):ncol(cel)] <- "\u2014"
  mejor_aic <- nms[which.min(I$aic)]; mejor_bic <- nms[which.min(I$bic)]
  nota <- paste0(if (any(as.logical(I$robusto))) "\u00cdndices corregidos del estimador robusto. " else "",
                 "Diferencias respecto ", if (referencia == "anterior") "del modelo anterior" else paste0("de ", nms[1]), ". ",
                 if (anidados) "\u0394\u03c7\u00b2 v\u00e1lida solo para modelos anidados. " else "",
                 "Un \u0394CFI menor que \u2212.010 junto con un \u0394RMSEA mayor que .015 indica un ajuste ",
                 "apreciablemente peor (Chen, 2007). AIC y BIC: menor es mejor; el AIC m\u00e1s bajo es el de ",
                 mejor_aic, if (mejor_bic == mejor_aic) " y tambi\u00e9n el BIC." else paste0(" y el BIC m\u00e1s bajo, el de ", mejor_bic, "."))
  t <- nueva_tabla(if (is.null(titulo)) "Comparaci\u00f3n de modelos alternativos" else titulo, cols, cel, nota = nota,
                   datos = cbind(modelo = nms, I, dif))
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

# --- Sesgo de método común --------------------------------------------------------------

#' Tabla de sesgo de método común
#'
#' Dos pruebas habituales del sesgo de método común (Podsakoff et al., 2003)
#' cuando todos los datos vienen de un mismo cuestionario:
#'
#' * **Factor único de Harman:** porcentaje de varianza que explica el primer
#'   componente sin rotar de todos los ítems. Si es menor que 50 %, no hay un
#'   factor general que domine.
#' * **AFC de un factor:** todos los ítems en un solo factor. Si ajusta
#'   claramente peor que el modelo propuesto, el sesgo de método no explica
#'   las relaciones entre ítems.
#'
#' Ambas pruebas son poco sensibles: un resultado favorable no descarta el
#' sesgo, pero uno desfavorable sí lo señala.
#'
#' @param fit Modelo de `lavaan` propuesto (AFC o SEM). Se usan sus ítems,
#'   datos y estimador.
#' @param titulo Título de la tabla.
#' @param archivo Si se indica, guarda la tabla.
#' @return Un objeto `tabla_apa`. Los valores están en `$datos`.
#' @examples
#' library(lavaan)
#' fit <- cfa("visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6
#'             speed =~ x7 + x8 + x9", data = HolzingerSwineford1939)
#' tabla_metodo_comun(fit)
#' @export
tabla_metodo_comun <- function(fit, titulo = NULL, archivo = NULL) {
  if (!inherits(fit, "lavaan")) stop("`fit` debe ser un modelo de lavaan.", call. = FALSE)
  fac <- factores_de(fit); items <- unique(unlist(fac))
  if (length(items) < 3) stop("Se necesitan al menos tres \u00edtems.", call. = FALSE)
  R <- cor_observada(fit)[items, items]
  ev <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
  pct <- 100 * ev[1] / length(items)
  op <- lavaan::lavInspect(fit, "options")
  d <- obtener_datos(fit)[, items, drop = FALSE]
  uno <- tryCatch(lavaan::cfa(paste("Metodo =~", paste(items, collapse = " + ")), data = d,
                              estimator = op$estimator, missing = op$missing),
                  error = function(e) NULL)
  if (is.null(uno)) stop("No se pudo estimar el AFC de un factor.", call. = FALSE)
  i1 <- indices(uno); i0 <- indices(fit)
  dchi <- diferencia_chi(uno, fit)
  peor <- (i1[["cfi"]] - i0[["cfi"]]) < -0.01
  cel <- rbind(
    c("Factor \u00fanico de Harman", paste0("Primer componente: ", f_num(pct, 1), " % de la varianza"),
      "< 50 %", if (pct < 50) "No hay un factor general dominante" else "Posible sesgo de m\u00e9todo"),
    c("AFC de un factor",
      paste0("CFI = ", f_apa(i1[["cfi"]], 3), ", RMSEA = ", f_apa(i1[["rmsea"]], 3),
             if (!is.na(dchi[1])) paste0("; \u0394\u03c7\u00b2(", formatC(dchi[2], format = "d"), ") = ", f_num(dchi[1], 2),
                                         ", p ", if (dchi[3] < .001) "< .001" else paste("=", f_apa(dchi[3], 3))) else ""),
      paste0("Peor que el modelo propuesto (CFI = ", f_apa(i0[["cfi"]], 3), ")"),
      if (peor) "El sesgo de m\u00e9todo no explica los datos" else "Posible sesgo de m\u00e9todo"))
  nota <- paste0("Pruebas seg\u00fan Podsakoff et al. (2003). Harman: an\u00e1lisis de componentes principales sin rotar de los ",
                 length(items), " \u00edtems. AFC de un factor: todos los \u00edtems en un factor, comparado con el modelo propuesto ",
                 "(\u0394\u03c7\u00b2 y \u0394CFI). Estas pruebas son poco sensibles: un resultado favorable no descarta por completo ",
                 "el sesgo de m\u00e9todo com\u00fan.")
  t <- nueva_tabla(if (is.null(titulo)) "Pruebas de sesgo de m\u00e9todo com\u00fan" else titulo,
                   c("Prueba", "Resultado", "Criterio", "Conclusi\u00f3n"), cel, alinear = rep("l", 4), nota = nota,
                   datos = list(harman = pct, autovalores = ev, un_factor = i1, propuesto = i0,
                                diferencia = stats::setNames(dchi, c("dchi", "dgl", "p"))))
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

# --- Factorabilidad y número de factores -----------------------------------------------

#' @noRd
kmo <- function(R) {
  Ri <- solve(R); A <- -stats::cov2cor(Ri); diag(A) <- 0
  R0 <- R; diag(R0) <- 0
  list(global = sum(R0^2) / (sum(R0^2) + sum(A^2)),
       msa = colSums(R0^2) / (colSums(R0^2) + colSums(A^2)))
}

#' @noRd
mapa_velicer <- function(R) {
  p <- ncol(R); e <- eigen(R, symmetric = TRUE); L <- e$vectors %*% diag(sqrt(pmax(e$values, 0)))
  f0 <- (sum(R^2) - p) / (p * (p - 1))
  fm <- vapply(seq_len(p - 1), function(m) {
    C <- R - L[, 1:m, drop = FALSE] %*% t(L[, 1:m, drop = FALSE])
    d <- diag(C); if (any(d <= 0)) return(NA_real_)
    P <- C / sqrt(outer(d, d)); (sum(P^2) - p) / (p * (p - 1))
  }, numeric(1))
  v <- c(f0, fm); which.min(v) - 1
}

#' @noRd
autovalores_reducidos <- function(R) {
  smc <- 1 - 1 / diag(solve(R)); Rr <- R; diag(Rr) <- smc
  eigen(Rr, symmetric = TRUE, only.values = TRUE)$values
}

#' Tabla de factorabilidad y número de factores (AFE)
#'
#' Reúne lo que suele reportarse antes de un análisis factorial exploratorio:
#'
#' * **Factorabilidad:** índice KMO global y por ítem (MSA), prueba de
#'   esfericidad de Bartlett y determinante de la matriz de correlaciones.
#' * **Número de factores** según cuatro criterios: autovalores mayores que 1
#'   (Kaiser), análisis paralelo de Horn con componentes y con factores
#'   (autovalores de la matriz reducida con correlaciones múltiples al
#'   cuadrado) y la prueba MAP de Velicer.
#' * **Autovalores** observados junto al percentil 95 de los aleatorios.
#'
#' El análisis paralelo usa datos aleatorios normales, por lo que sus
#' resultados pueden variar un poco; fije `semilla` para que sean
#' reproducibles.
#'
#' @param datos `data.frame` con los ítems (o un modelo de `lavaan`).
#' @param iteraciones Número de conjuntos de datos aleatorios del análisis
#'   paralelo.
#' @param semilla Semilla para el análisis paralelo (`NULL` para no fijarla).
#' @inheritParams tabla_descriptivos
#' @return Un objeto `tabla_apa` con tres partes. Los valores están en
#'   `$datos`.
#' @examples
#' library(lavaan)
#' tabla_factorabilidad(HolzingerSwineford1939[, paste0("x", 1:9)], iteraciones = 50)
#' @export
tabla_factorabilidad <- function(datos, variables = NULL, iteraciones = 100, semilla = 123, etiquetas = NULL,
                                 titulo = NULL, archivo = NULL) {
  d <- obtener_datos(datos, variables); d <- d[stats::complete.cases(d), , drop = FALSE]
  n <- nrow(d); p <- ncol(d)
  if (p < 3) stop("Se necesitan al menos tres variables.", call. = FALSE)
  R <- stats::cor(d)
  if (det(R) <= 0) stop("La matriz de correlaciones es singular (hay variables redundantes).", call. = FALSE)
  K <- kmo(R)
  chi <- -(n - 1 - (2 * p + 5) / 6) * log(det(R)); gl <- p * (p - 1) / 2
  pb <- stats::pchisq(chi, gl, lower.tail = FALSE)
  ev <- eigen(R, symmetric = TRUE, only.values = TRUE)$values; evr <- autovalores_reducidos(R)
  if (!is.null(semilla)) { viejo <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv())
                           set.seed(semilla); on.exit(if (!is.null(viejo)) assign(".Random.seed", viejo, envir = globalenv()), add = TRUE) }
  sim <- replicate(iteraciones, {
    Rs <- stats::cor(matrix(stats::rnorm(n * p), n, p))
    c(eigen(Rs, symmetric = TRUE, only.values = TRUE)$values, autovalores_reducidos(Rs))
  })
  q95 <- apply(sim, 1, stats::quantile, probs = .95)
  pc95 <- q95[1:p]; fa95 <- q95[(p + 1):(2 * p)]
  primero_bajo <- function(o, a) { b <- which(o <= a); if (length(b)) b[1] - 1 else length(o) }
  n_kaiser <- sum(ev > 1); n_pac <- primero_bajo(ev, pc95); n_paf <- primero_bajo(evr, fa95); n_map <- mapa_velicer(R)
  nivel <- function(k) if (k >= .90) "excelente" else if (k >= .80) "meritorio" else if (k >= .70) "mediano" else
    if (k >= .60) "mediocre" else if (k >= .50) "miserable" else "inaceptable"
  vmin <- names(K$msa)[which.min(K$msa)]
  cel1 <- rbind(c("KMO global", f_apa(K$global, 2), paste0("\u2265 .60 (", nivel(K$global), ")")),
                c("MSA m\u00ednimo por \u00edtem", paste0(f_apa(min(K$msa), 2), " (", renombrar(vmin, etiquetas), ")"), "\u2265 .50"),
                c("Prueba de Bartlett", paste0("\u03c7\u00b2(", formatC(gl, format = "d"), ") = ", f_num(chi, 2), ", p ",
                                               if (pb < .001) "< .001" else paste("=", f_apa(pb, 3))), "p < .05"),
                c("Determinante", formatC(det(R), format = "e", digits = 2), "> 0.00001"))
  bajos <- names(K$msa)[K$msa < .50]
  nota1 <- paste0("KMO = medida de adecuaci\u00f3n muestral de Kaiser-Meyer-Olkin (calificaci\u00f3n de Kaiser, 1974); ",
                  "MSA = adecuaci\u00f3n de cada \u00edtem. N = ", n, " casos completos. ",
                  if (length(bajos)) paste0("\u00cdtems con MSA < .50, candidatos a eliminarse: ", paste(renombrar(bajos, etiquetas), collapse = ", "), ".")
                  else "Todos los \u00edtems tienen MSA \u2265 .50.")
  cel2 <- rbind(c("Autovalores mayores que 1 (Kaiser)", n_kaiser),
                c("An\u00e1lisis paralelo, componentes (Horn, 1965)", n_pac),
                c("An\u00e1lisis paralelo, factores", n_paf),
                c("MAP de Velicer (1976)", n_map))
  nota2 <- paste0("An\u00e1lisis paralelo con ", iteraciones, " conjuntos de datos aleatorios y el percentil 95. ",
                  "El criterio de Kaiser tiende a sobrestimar el n\u00famero de factores; se recomienda dar m\u00e1s peso al ",
                  "an\u00e1lisis paralelo y a la MAP, y a la interpretabilidad de la soluci\u00f3n.")
  m <- min(p, 10)
  cel3 <- cbind(as.character(seq_len(m)), f_num(ev[1:m], 2), f_num(pc95[1:m], 2), f_num(evr[1:m], 2), f_num(fa95[1:m], 2))
  neg3 <- cbind(FALSE, ev[1:m] > pc95[1:m], FALSE, evr[1:m] > fa95[1:m], FALSE)
  extra <- list(nueva_tabla("N\u00famero de factores sugerido por cada criterio", c("Criterio", "Factores"), cel2,
                            alinear = c("l", "c"), nota = nota2),
                nueva_tabla("Autovalores observados y aleatorios (percentil 95)",
                            c("N.\u00ba", "Componentes", "Aleatorios", "Factores", "Aleatorios"), cel3, neg3,
                            nota = "En negritas, autovalores observados mayores que los aleatorios. Factores: autovalores de la matriz reducida (correlaciones m\u00faltiples al cuadrado en la diagonal)."))
  t <- nueva_tabla(if (is.null(titulo)) "Factorabilidad de la matriz de correlaciones" else titulo,
                   c("Indicador", "Valor", "Criterio"), cel1, alinear = c("l", "c", "l"), nota = nota1,
                   datos = list(kmo = K$global, msa = K$msa, bartlett = c(chi = chi, gl = gl, p = pb), determinante = det(R),
                                autovalores = ev, autovalores_reducidos = evr, aleatorios_comp = pc95, aleatorios_fact = fa95,
                                factores = c(kaiser = n_kaiser, paralelo_componentes = n_pac, paralelo_factores = n_paf, map = n_map)),
                   extra = extra)
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}
