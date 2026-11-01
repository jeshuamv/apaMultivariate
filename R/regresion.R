# Regresión lineal múltiple: coeficientes, supuestos, casos influyentes y
# comparación de todos los subconjuntos de predictores

#' @noRd
exigir_lm <- function(fit) {
  objeto_existe(fit, "fit", "Primero estima la regresi\u00f3n, por ejemplo: fit <- lm(y ~ x1 + x2, data = datos)")
  if (!inherits(fit, "lm") || inherits(fit, "glm") || inherits(fit, "mlm"))
    stop("`fit` debe ser una regresi\u00f3n lineal estimada con lm(), por ejemplo: fit <- lm(y ~ x1 + x2, data = datos).", call. = FALSE)
  if (anyNA(stats::coef(fit)))
    stop("Hay coeficientes que no se pudieron estimar (NA): ", paste(names(which(is.na(stats::coef(fit)))), collapse = ", "),
         ". Suele deberse a colinealidad perfecta (un predictor es combinaci\u00f3n exacta de otros); qu\u00edtalo del modelo.", call. = FALSE)
  if (!is.null(fit$weights)) stop("Las regresiones con pesos (weights) no est\u00e1n disponibles.", call. = FALSE)
  invisible(TRUE)
}

#' @noRd
nombre_coef <- function(v, etiquetas) {
  v[v == "(Intercept)"] <- tx("(Intercepto)", "(Intercept)")
  renombrar(v, etiquetas)
}

# Errores estándar HC3 (MacKinnon y White, 1985): los recomendados por
# Long y Ervin (2000), sobre todo con muestras pequeñas.
#' @noRd
vcov_hc3 <- function(fit) {
  X <- stats::model.matrix(fit); e <- stats::residuals(fit); h <- stats::hatvalues(fit)
  B <- solve(crossprod(X)); w <- e^2 / (1 - h)^2
  B %*% crossprod(X * sqrt(w)) %*% B
}

#' @noRd
vif_lm <- function(fit) {
  X <- stats::model.matrix(fit); X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  if (ncol(X) < 2) return(stats::setNames(rep(NA_real_, ncol(X)), colnames(X)))
  diag(solve(stats::cor(X)))
}

#' Tabla de coeficientes de una regresión lineal múltiple (APA)
#'
#' Reúne en una tabla lo que se reporta de una regresión múltiple:
#' coeficientes no estandarizados (*B*) con su error estándar e intervalo de
#' confianza, coeficientes estandarizados (\eqn{\beta}), *t*, *p*, la
#' correlación semiparcial al cuadrado (\eqn{sr^2}, la varianza que aporta
#' cada predictor por sí solo) y el VIF. En la nota van *N*, \eqn{R^2},
#' \eqn{R^2} ajustada y la prueba *F* del modelo.
#'
#' * `robusto = TRUE` usa errores estándar robustos a la heterocedasticidad
#'   HC3 (cuando [tabla_supuestos_regresion()] muestra varianza no constante).
#' * `bootstrap = 2000` (por ejemplo) calcula los intervalos de confianza de
#'   *B* por bootstrap de percentiles, remuestreando casos; es útil con
#'   residuos no normales o muestras pequeñas.
#'
#' Si hay predictores categóricos (factores) o interacciones, \eqn{\beta} se
#' calcula con la desviación estándar de cada columna del modelo (variables
#' ficticias incluidas), así que conviene interpretarlo con cuidado.
#'
#' @param fit Regresión estimada con [stats::lm()].
#' @param ic Nivel de confianza de los intervalos.
#' @param robusto Si es `TRUE`, errores estándar HC3.
#' @param bootstrap Número de remuestreos para los intervalos por bootstrap
#'   (0 = sin bootstrap).
#' @param semilla Semilla del bootstrap.
#' @inheritParams tabla_descriptivos
#' @param decimales Número de decimales de *B*, *EE* y los intervalos.
#' @return Un objeto `tabla_apa`. Los valores están en `$datos` y los del
#'   modelo (\eqn{R^2}, *F*, ...) en `attr($datos, "modelo")`.
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   fit <- lm(y ~ x1 + x2 + x3 + x4, data = MASS::cement)   # datos de Hald
#'   tabla_regresion(fit)
#' }
#' @export
tabla_regresion <- function(fit, etiquetas = NULL, decimales = 2, ic = .95, robusto = FALSE, bootstrap = 0, semilla = 123,
                            titulo = NULL, archivo = NULL) {
  exigir_lm(fit)
  X <- stats::model.matrix(fit); y <- stats::model.response(stats::model.frame(fit))
  n <- nrow(X); gl <- stats::df.residual(fit); b <- stats::coef(fit)
  if (gl < 1) stop("No quedan grados de libertad: hay tantos coeficientes como casos.", call. = FALSE)
  s <- summary(fit); r2 <- s$r.squared; r2a <- s$adj.r.squared
  con_int <- "(Intercept)" %in% names(b); k <- sum(names(b) != "(Intercept)")
  Fm <- if (!is.null(s$fstatistic)) s$fstatistic else c(value = NA, numdf = NA, dendf = NA)
  pF <- stats::pf(Fm[1], Fm[2], Fm[3], lower.tail = FALSE)
  ee_ols <- sqrt(diag(stats::vcov(fit))); t_ols <- b / ee_ols
  ee <- if (robusto) sqrt(diag(vcov_hc3(fit))) else ee_ols
  tt <- b / ee; p <- 2 * stats::pt(-abs(tt), gl)
  q <- stats::qt(1 - (1 - ic) / 2, gl); li <- b - q * ee; ls <- b + q * ee
  if (bootstrap > 0) {
    if (!is.null(semilla)) { viejo <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv())
                             set.seed(semilla); on.exit(if (!is.null(viejo)) assign(".Random.seed", viejo, envir = globalenv()), add = TRUE) }
    B <- replicate(bootstrap, { i <- sample.int(n, n, replace = TRUE); stats::lm.fit(X[i, , drop = FALSE], y[i])$coefficients })
    B <- B[, colSums(is.na(B)) == 0, drop = FALSE]
    li <- apply(B, 1, stats::quantile, probs = (1 - ic) / 2); ls <- apply(B, 1, stats::quantile, probs = 1 - (1 - ic) / 2)
    validos <- ncol(B)
  }
  sdx <- apply(X, 2, stats::sd); beta <- b * sdx / stats::sd(y); beta[names(b) == "(Intercept)"] <- NA
  sr2 <- t_ols^2 * (1 - r2) / gl; sr2[names(b) == "(Intercept)"] <- NA
  pr2 <- t_ols^2 / (t_ols^2 + gl); pr2[names(b) == "(Intercept)"] <- NA
  vif <- stats::setNames(rep(NA_real_, length(b)), names(b)); v <- vif_lm(fit); vif[names(v)] <- v
  D <- data.frame(predictor = names(b), B = b, EE = ee, li = li, ls = ls, beta = beta, t = tt, p = p, sr2 = sr2,
                  r2_parcial = pr2, vif = vif, tolerancia = 1 / vif, stringsAsFactors = FALSE)
  rownames(D) <- NULL
  attr(D, "modelo") <- c(n = n, k = k, r2 = r2, r2_ajustada = r2a, F = unname(Fm[1]), gl1 = unname(Fm[2]), gl2 = unname(Fm[3]), p = unname(pF))
  pct <- paste0(round(100 * ic), " %")
  cel <- cbind(nombre_coef(D$predictor, etiquetas), f_num(D$B, decimales), f_num(D$EE, decimales),
               paste0("[", f_num(D$li, decimales), ", ", f_num(D$ls, decimales), "]"),
               f_apa(D$beta, 2), f_num(D$t, 2), f_p(D$p), f_apa(D$sr2, 3), f_num(D$vif, 2))
  neg <- matrix(FALSE, nrow(cel), ncol(cel)); neg[, 9] <- !is.na(D$vif) & D$vif > 5
  muestra <- n / max(k, 1)
  nota <- paste0("N = ", n, ". ", "R\u00b2 = ", f_apa(r2, 3), ", R\u00b2 ", tx("ajustada", "adjusted"), " = ", f_apa(r2a, 3),
                 if (!is.na(Fm[1])) paste0(", F(", Fm[2], ", ", Fm[3], ") = ", f_num(Fm[1], 2), ", p ", if (pF < .001) "< .001" else paste("=", f_apa(pF, 3))), ". ",
                 tx("B = coeficiente no estandarizado; EE = error est\u00e1ndar", "B = unstandardized coefficient; SE = standard error"),
                 if (robusto) tx(" robusto a la heterocedasticidad (HC3)", " robust to heteroscedasticity (HC3)") else "",
                 "; IC = ", tx("intervalo de confianza", "confidence interval"),
                 if (bootstrap > 0) paste0(tx(" por bootstrap de percentiles (", " from percentile bootstrap ("), validos, tx(" remuestreos)", " resamples)")) else "",
                 tx("; \u03b2 = coeficiente estandarizado; sr\u00b2 = correlaci\u00f3n semiparcial al cuadrado (varianza que explica cada predictor por s\u00ed solo); VIF = factor de inflaci\u00f3n de la varianza.",
                    "; \u03b2 = standardized coefficient; sr\u00b2 = squared semipartial correlation (variance uniquely explained by each predictor); VIF = variance inflation factor."),
                 if (any(D$vif > 10, na.rm = TRUE)) tx(" Hay VIF mayores que 10 (en negritas): la multicolinealidad es grave y los coeficientes individuales son inestables y dif\u00edciles de interpretar, aunque el R\u00b2 del modelo s\u00ed es v\u00e1lido.",
                                                       " Some VIF exceed 10 (in bold): multicollinearity is severe and the individual coefficients are unstable, although the model R\u00b2 is still valid.")
                 else if (any(D$vif > 5, na.rm = TRUE)) tx(" En negritas, VIF mayores que 5 (multicolinealidad preocupante).", " VIF above 5 are in bold (problematic multicollinearity).") else "",
                 if (k >= 1 && muestra < 10) paste0(tx(" Con ", " With "), f_num(muestra, 1),
                                                    tx(" casos por predictor (se recomiendan al menos 10 a 15), las estimaciones son poco estables.",
                                                       " cases per predictor (at least 10 to 15 are recommended), the estimates are unstable.")) else "",
                 if (any(grepl(":", D$predictor)) || any(!vapply(stats::model.frame(fit)[-1], is.numeric, logical(1))))
                   tx(" Para variables ficticias e interacciones, \u03b2 usa la desviaci\u00f3n est\u00e1ndar de cada columna del modelo.",
                      " For dummy variables and interactions, \u03b2 uses the standard deviation of each model column.") else "")
  t <- nueva_tabla(if (is.null(titulo)) paste0(tx("Regresi\u00f3n lineal m\u00faltiple para predecir ", "Multiple linear regression predicting "),
                                               renombrar(names(stats::model.frame(fit))[1], etiquetas)) else titulo,
                   c("Predictor", "B", tx("EE", "SE"), paste0("IC ", pct), "\u03b2", "t", "p", "sr\u00b2", "VIF"), cel, neg,
                   alinear = c("l", rep("c", 8)), nota = nota, datos = D)
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

#' Tabla de supuestos de la regresión lineal
#'
#' Evalúa los supuestos de una regresión con [stats::lm()] y dice en una
#' columna qué significa cada resultado:
#'
#' * **Normalidad de los residuos:** prueba de Shapiro-Wilk.
#' * **Homocedasticidad:** prueba de Breusch-Pagan en su versión
#'   estudentizada de Koenker (la de `lmtest::bptest()`).
#' * **Linealidad y especificación:** prueba RESET de Ramsey con los
#'   predichos al cuadrado y al cubo (la de `lmtest::resettest()`).
#' * **Independencia de los errores:** estadístico de Durbin-Watson (solo
#'   tiene sentido si el orden de los casos importa, por ejemplo en el
#'   tiempo).
#' * **Multicolinealidad:** VIF más alto.
#' * **Casos influyentes:** distancia de Cook (detalles en
#'   [tabla_influencia()]).
#'
#' Con muestras pequeñas estas pruebas tienen poca potencia: que no sean
#' significativas no demuestra que el supuesto se cumpla. Revisa también
#' las gráficas de residuos: `plot(fit)`.
#'
#' @inheritParams tabla_regresion
#' @param alfa Nivel de significancia de las pruebas.
#' @return Un objeto `tabla_apa`. Los valores están en `$datos`.
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   fit <- lm(y ~ x1 + x2 + x3 + x4, data = MASS::cement)
#'   tabla_supuestos_regresion(fit)
#' }
#' @export
tabla_supuestos_regresion <- function(fit, alfa = .05, etiquetas = NULL, titulo = NULL, archivo = NULL) {
  exigir_lm(fit)
  X <- stats::model.matrix(fit); e <- stats::residuals(fit); n <- length(e); yhat <- stats::fitted(fit)
  k <- ncol(X) - ("(Intercept)" %in% colnames(X))
  # Normalidad
  if (n >= 3 && n <= 5000) { sw <- stats::shapiro.test(e); norm <- c(est = unname(sw$statistic), gl = NA, p = sw$p.value) }
  else norm <- c(est = NA, gl = NA, p = NA)
  # Breusch-Pagan (Koenker): n * R2 de e^2 sobre los predictores
  u <- e^2
  aux <- stats::lm.fit(X, u); r2a <- 1 - sum(aux$residuals^2) / sum((u - mean(u))^2)
  bp <- c(est = n * r2a, gl = k, p = stats::pchisq(n * r2a, k, lower.tail = FALSE))
  # RESET (potencias 2 y 3 de los predichos)
  X2 <- cbind(X, yhat^2, yhat^3); f2 <- stats::lm.fit(X2, stats::model.response(stats::model.frame(fit)))
  rango2 <- f2$rank - fit$rank
  sse1 <- sum(e^2); sse2 <- sum(f2$residuals^2); gl2 <- n - f2$rank
  reset <- if (rango2 > 0 && gl2 > 0) { Fr <- ((sse1 - sse2) / rango2) / (sse2 / gl2); c(est = Fr, gl = rango2, p = stats::pf(Fr, rango2, gl2, lower.tail = FALSE)) }
           else c(est = NA, gl = NA, p = NA)
  # Durbin-Watson
  dw <- sum(diff(e)^2) / sum(e^2)
  # VIF
  v <- vif_lm(fit); vmax <- if (all(is.na(v))) NA else max(v, na.rm = TRUE); vcual <- if (is.na(vmax)) "" else names(v)[which.max(v)]
  # Cook
  ck <- stats::cooks.distance(fit); c4 <- sum(ck > 4 / n, na.rm = TRUE); c1 <- sum(ck > 1, na.rm = TRUE)
  cmax <- max(ck, na.rm = TRUE); ccual <- rownames(stats::model.frame(fit))[which.max(ck)]
  pv <- function(x) if (is.na(x)) "\u2014" else f_p(x)
  si <- function(p, bien, mal) if (is.na(p)) tx("No se pudo calcular.", "Could not be computed.") else if (p < alfa) mal else bien
  filas <- rbind(
    c(tx("Normalidad de los residuos", "Normality of residuals"), "Shapiro-Wilk", if (is.na(norm[1])) "\u2014" else paste0("W = ", f_apa(norm[1], 3)), pv(norm[3]),
      if (n > 5000) tx("Con m\u00e1s de 5000 casos no se aplica; con muestras grandes la normalidad importa poco (teorema central del l\u00edmite).", "Not applied with more than 5000 cases; with large samples normality matters little.")
      else si(norm[3], tx("No se rechaza la normalidad de los residuos.", "Normality of residuals is not rejected."),
              tx("Los residuos se apartan de la normalidad: revisa la gr\u00e1fica Q-Q y considera intervalos por bootstrap (bootstrap = 2000).", "Residuals depart from normality: check the Q-Q plot and consider bootstrap intervals (bootstrap = 2000)."))),
    c(tx("Homocedasticidad", "Homoscedasticity"), "Breusch-Pagan", paste0("\u03c7\u00b2(", k, ") = ", f_num(bp[1], 2)), pv(bp[3]),
      si(bp[3], tx("No se rechaza la homocedasticidad (varianza constante de los residuos).", "Homoscedasticity is not rejected."),
         tx("La varianza de los residuos no es constante: usa errores est\u00e1ndar robustos (tabla_regresion(fit, robusto = TRUE)).", "Residual variance is not constant: use robust standard errors (tabla_regresion(fit, robusto = TRUE))."))),
    c(tx("Linealidad y especificaci\u00f3n", "Linearity and specification"), "RESET", if (is.na(reset[1])) "\u2014" else paste0("F(", reset[2], ", ", gl2, ") = ", f_num(reset[1], 2)), pv(reset[3]),
      si(reset[3], tx("No se rechaza la especificaci\u00f3n lineal (no se detectan relaciones no lineales ni variables omitidas).", "Linear specification is not rejected."),
         tx("Posible relaci\u00f3n no lineal o variables omitidas: revisa la gr\u00e1fica de residuos contra predichos y considera t\u00e9rminos cuadr\u00e1ticos.", "Possible nonlinearity or omitted variables: check residuals vs. fitted and consider quadratic terms."))),
    c(tx("Independencia de los errores", "Independence of errors"), "Durbin-Watson", paste0("DW = ", f_num(dw, 2)), "\u2014",
      paste0(if (dw >= 1.5 && dw <= 2.5) tx("Entre 1.5 y 2.5: sin autocorrelaci\u00f3n importante. ", "Between 1.5 and 2.5: no relevant autocorrelation. ")
             else tx("Fuera de 1.5 a 2.5: posible autocorrelaci\u00f3n. ", "Outside 1.5 to 2.5: possible autocorrelation. "),
             tx("Solo tiene sentido si el orden de los casos importa (por ejemplo, mediciones en el tiempo).", "Only meaningful if the order of cases matters (e.g., measurements over time)."))),
    c(tx("Multicolinealidad", "Multicollinearity"), tx("VIF m\u00e1ximo", "Highest VIF"), if (is.na(vmax)) "\u2014" else paste0(f_num(vmax, 2), " (", renombrar(vcual, etiquetas), ")"), "\u2014",
      if (is.na(vmax)) tx("Un solo predictor: no aplica.", "Single predictor: not applicable.")
      else if (vmax > 10) tx("Grave (VIF > 10): los coeficientes individuales son inestables; considera quitar o combinar predictores muy correlacionados.", "Severe (VIF > 10): individual coefficients are unstable; consider removing or combining highly correlated predictors.")
      else if (vmax > 5) tx("Preocupante (VIF > 5).", "Problematic (VIF > 5).") else tx("Aceptable (VIF \u2264 5).", "Acceptable (VIF \u2264 5).")),
    c(tx("Casos influyentes", "Influential cases"), tx("Distancia de Cook", "Cook's distance"), paste0(tx("m\u00e1x. = ", "max = "), f_apa(cmax, 2), tx(" (caso ", " (case "), ccual, ")"), "\u2014",
      paste0(c4, tx(" caso(s) con D > 4/N", " case(s) with D > 4/N"), " (", f_apa(4 / n, 2), ")", tx(" y ", " and "), c1, tx(" con D > 1. ", " with D > 1. "),
             if (c1 > 0) tx("Hay casos muy influyentes: rev\u00edsalos con tabla_influencia(fit).", "There are highly influential cases: inspect them with tabla_influencia(fit).")
             else if (c4 > 0) tx("Rev\u00edsalos con tabla_influencia(fit).", "Inspect them with tabla_influencia(fit).") else ""))
  )
  neg <- matrix(FALSE, nrow(filas), ncol(filas))
  pp <- c(norm[3], bp[3], reset[3]); neg[1:3, 4] <- !is.na(pp) & pp < alfa
  neg[5, 3] <- !is.na(vmax) && vmax > 5; neg[6, 3] <- c1 > 0
  nota <- paste0("N = ", n, tx(". Pruebas con \u03b1 = ", ". Tests with \u03b1 = "), f_apa(alfa, 2),
                 tx("; en negritas, los resultados que indican un problema. Breusch-Pagan en la versi\u00f3n estudentizada de Koenker; RESET con los predichos al cuadrado y al cubo.",
                    "; results indicating a problem are in bold. Koenker's studentized Breusch-Pagan test; RESET with squared and cubed fitted values."),
                 if (n < 50) tx(" Con una muestra peque\u00f1a, estas pruebas tienen poca potencia: un resultado no significativo no demuestra que el supuesto se cumpla; revisa tambi\u00e9n las gr\u00e1ficas de residuos (plot(fit)).",
                                " With a small sample these tests have low power: a nonsignificant result does not show that the assumption holds; also inspect the residual plots (plot(fit)).") else "")
  D <- data.frame(supuesto = c("normalidad", "homocedasticidad", "reset", "durbin_watson", "vif", "cook"),
                  estadistico = c(norm[1], bp[1], reset[1], dw, vmax, cmax), gl = c(NA, k, reset[2], NA, NA, NA),
                  p = c(norm[3], bp[3], reset[3], NA, NA, NA), stringsAsFactors = FALSE)
  t <- nueva_tabla(if (is.null(titulo)) tx("Supuestos de la regresi\u00f3n lineal", "Linear regression assumptions") else titulo,
                   c(tx("Supuesto", "Assumption"), tx("Prueba", "Test"), tx("Estad\u00edstico", "Statistic"), "p", tx("Interpretaci\u00f3n", "Interpretation")),
                   filas, neg, alinear = c("l", "l", "c", "c", "l"), nota = nota, datos = D)
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

#' Tabla de casos influyentes de una regresión
#'
#' Muestra los casos que superan algún punto de corte habitual de
#' influencia en una regresión con [stats::lm()]:
#'
#' * residuo estudentizado: \eqn{|r| > 2};
#' * apalancamiento (*h*): \eqn{h > 2p/N};
#' * distancia de Cook: \eqn{D > 4/N} (y, muy influyente, \eqn{D > 1});
#' * DFFITS: \eqn{|DFFITS| > 2\sqrt{p/N}};
#' * DFBETAS: \eqn{|DFBETAS| > 2/\sqrt{N}} (se muestra el mayor);
#' * COVRATIO: \eqn{|COVRATIO - 1| > 3p/N};
#'
#' donde *p* es el número de coeficientes (con el intercepto). Si algún caso
#' supera el corte de Cook, se agrega una tabla de sensibilidad con los
#' coeficientes con y sin el caso más influyente.
#'
#' Que un caso supere un corte no basta para eliminarlo: revisa si es un
#' error de captura o un caso legítimo, y reporta lo que decidas.
#'
#' @inheritParams tabla_regresion
#' @param todos Si es `TRUE`, muestra todos los casos.
#' @param maximo Número máximo de casos que se muestran (los de mayor
#'   distancia de Cook).
#' @return Un objeto `tabla_apa`. Los valores de todos los casos están en
#'   `$datos`.
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   fit <- lm(y ~ x1 + x2 + x3 + x4, data = MASS::cement)
#'   tabla_influencia(fit)
#' }
#' @export
tabla_influencia <- function(fit, todos = FALSE, maximo = 20, etiquetas = NULL, titulo = NULL, archivo = NULL) {
  exigir_lm(fit)
  im <- stats::influence.measures(fit)$infmat
  n <- nrow(im); p <- length(stats::coef(fit))
  dfb <- im[, grepl("^dfb", colnames(im)), drop = FALSE]
  cnb <- names(stats::coef(fit))
  dfbmax <- apply(abs(dfb), 1, max); dfbcual <- cnb[apply(abs(dfb), 1, which.max)]
  D <- data.frame(caso = rownames(stats::model.frame(fit)), rstudent = stats::rstudent(fit), h = im[, "hat"], cook = im[, "cook.d"],
                  dffits = im[, "dffit"], dfbetas = dfbmax, dfbetas_coef = dfbcual, covratio = im[, "cov.r"], stringsAsFactors = FALSE)
  rownames(D) <- NULL
  cortes <- c(rstudent = 2, h = 2 * p / n, cook = 4 / n, dffits = 2 * sqrt(p / n), dfbetas = 2 / sqrt(n), covratio = 3 * p / n)
  M <- cbind(abs(D$rstudent) > cortes[["rstudent"]], D$h > cortes[["h"]], D$cook > cortes[["cook"]],
             abs(D$dffits) > cortes[["dffits"]], D$dfbetas > cortes[["dfbetas"]], abs(D$covratio - 1) > cortes[["covratio"]])
  D$criterios <- rowSums(M)
  sel <- if (todos) seq_len(n) else which(D$criterios > 0)
  sel <- sel[order(-D$cook[sel])]
  recorte <- length(sel) > maximo; sel <- utils::head(sel, maximo)
  cel <- if (length(sel)) cbind(D$caso[sel], f_num(D$rstudent[sel], 2), f_apa(D$h[sel], 2), f_apa(D$cook[sel], 2), f_num(D$dffits[sel], 2),
                                paste0(f_apa(D$dfbetas[sel], 2), " (", nombre_coef(D$dfbetas_coef[sel], etiquetas), ")"), f_num(D$covratio[sel], 2),
                                as.character(D$criterios[sel]))
         else matrix(c(tx("Ning\u00fan caso supera los cortes", "No case exceeds the cutoffs"), rep("", 7)), 1)
  neg <- matrix(FALSE, nrow(cel), ncol(cel))
  if (length(sel)) { neg[, 2:7] <- M[sel, , drop = FALSE]; neg[, 4] <- neg[, 4] | D$cook[sel] > 1 }
  nota <- paste0("N = ", n, tx(", p = ", ", p = "), p, tx(" coeficientes. Puntos de corte: |r| > 2; h > 2p/N = ", " coefficients. Cutoffs: |r| > 2; h > 2p/N = "), f_apa(cortes[["h"]], 2),
                 "; D > 4/N = ", f_apa(cortes[["cook"]], 2), tx(" (muy influyente si D > 1)", " (highly influential if D > 1)"),
                 "; |DFFITS| > 2\u221a(p/N) = ", f_apa(cortes[["dffits"]], 2), "; |DFBETAS| > 2/\u221aN = ", f_apa(cortes[["dfbetas"]], 2),
                 tx(" (entre par\u00e9ntesis, el coeficiente m\u00e1s afectado)", " (most affected coefficient in parentheses)"),
                 "; |COVRATIO \u2212 1| > 3p/N = ", f_apa(cortes[["covratio"]], 2),
                 tx(". r = residuo estudentizado; h = apalancamiento; D = distancia de Cook. En negritas, los valores que superan el corte; Criterios = cu\u00e1ntos supera. ",
                    ". r = studentized residual; h = leverage; D = Cook's distance. Values above the cutoff are in bold; Criteria = how many are exceeded. "),
                 if (recorte) paste0(tx("Se muestran los ", "Showing the "), maximo, tx(" casos con mayor D de Cook. ", " cases with the largest Cook's D. ")) else "",
                 tx("Un caso influyente no se elimina solo por superar un corte: revisa si es un error de captura y reporta los resultados con y sin \u00e9l.",
                    "An influential case should not be removed only for exceeding a cutoff: check for data-entry errors and report results with and without it."))
  t <- nueva_tabla(if (is.null(titulo)) tx("Diagn\u00f3stico de casos influyentes", "Influential case diagnostics") else titulo,
                   c(tx("Caso", "Case"), "r", "h", "D", "DFFITS", tx("DFBETAS m\u00e1x.", "Max DFBETAS"), "COVRATIO", tx("Criterios", "Criteria")),
                   cel, neg, nota = nota, datos = D)
  i <- which.max(D$cook)
  if (D$cook[i] > cortes[["cook"]] && n - 1 > p) {
    mf <- stats::model.frame(fit)
    f2 <- stats::lm(stats::formula(fit), data = mf[-i, , drop = FALSE])
    b1 <- stats::coef(fit); b2 <- stats::coef(f2)[names(b1)]
    p1 <- summary(fit)$coefficients[, 4]; p2 <- summary(f2)$coefficients[names(b1), 4]
    ee1 <- summary(fit)$coefficients[, 2]
    cambio <- (b2 - b1) / ee1          # cambio en errores estándar (como DFBETAS)
    r21 <- summary(fit)$r.squared; r22 <- summary(f2)$r.squared
    sens <- cbind(nombre_coef(names(b1), etiquetas), f_num(b1, 2), f_p(p1), f_num(b2, 2), f_p(p2), f_num(cambio, 2))
    sens <- rbind(sens, c("R\u00b2", f_apa(r21, 3), "", f_apa(r22, 3), "", ""))
    negs <- matrix(FALSE, nrow(sens), ncol(sens)); negs[seq_along(b1), 6] <- abs(cambio) > 1 | ((p1 < .05) != (p2 < .05))
    t$extra <- list(nueva_tabla(paste0(tx("Sensibilidad: coeficientes con y sin el caso ", "Sensitivity: coefficients with and without case "), D$caso[i]),
                                c("", tx("B (todos)", "B (all)"), "p", paste0("B (", tx("sin ", "without "), D$caso[i], ")"), "p", tx("Cambio (EE)", "Change (SE)")),
                                sens, negs,
                                nota = tx(paste0("Se quit\u00f3 el caso con mayor distancia de Cook (D = ", f_apa(D$cook[i], 2), "). Cambio (EE) = diferencia entre los coeficientes, en errores est\u00e1ndar del modelo completo. En negritas, coeficientes que cambian m\u00e1s de un error est\u00e1ndar o cuya significancia (p < .05) cambia. Si las conclusiones no cambian, los resultados son robustos a ese caso."),
                                          paste0("The case with the largest Cook's distance (D = ", f_apa(D$cook[i], 2), ") was removed. Change (SE) = difference between coefficients in standard errors of the full model. In bold, coefficients that change by more than one standard error or whose significance (p < .05) changes. If conclusions hold, results are robust to that case."))))
    attr(t$datos, "sin_caso") <- f2
  }
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

#' Comparar todos los subconjuntos de predictores de una regresión
#'
#' Estima la regresión con cada combinación posible de los predictores del
#' modelo (con *k* predictores hay \eqn{2^k - 1}) y las compara con:
#' \eqn{R^2}, \eqn{R^2} ajustada, *Cp* de Mallows, AICc, BIC y PRESS (suma
#' de los errores de predicción al dejar fuera cada caso, equivalente a la
#' validación cruzada dejando uno fuera). Se ordenan por el criterio que
#' elijas y en negritas va el mejor valor de cada columna.
#'
#' Elegir predictores con los mismos datos aprovecha el azar: el modelo
#' "ganador" suele funcionar peor en otra muestra. Usa esta tabla como
#' exploración y prefiere el modelo que respalde la teoría.
#'
#' Todos los modelos se estiman con los mismos casos (los del modelo
#' completo). No admite interacciones.
#'
#' @inheritParams tabla_regresion
#' @param criterio Criterio para ordenar: `"AICc"`, `"BIC"`, `"PRESS"`,
#'   `"Cp"` o `"R2_ajustada"`.
#' @param mostrar Número de modelos que se muestran (por defecto, todos si
#'   son 15 o menos).
#' @return Un objeto `tabla_apa`. Los valores de todos los modelos están en
#'   `$datos`.
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   fit <- lm(y ~ x1 + x2 + x3 + x4, data = MASS::cement)
#'   tabla_subconjuntos(fit)
#' }
#' @export
tabla_subconjuntos <- function(fit, criterio = c("AICc", "BIC", "PRESS", "Cp", "R2_ajustada"), mostrar = NULL, etiquetas = NULL,
                               titulo = NULL, archivo = NULL) {
  exigir_lm(fit); criterio <- match.arg(criterio)
  tl <- attr(stats::terms(fit), "term.labels")
  if (any(grepl(":", tl))) stop("tabla_subconjuntos() no admite interacciones; usa un modelo solo con efectos principales.", call. = FALSE)
  if (length(tl) < 2) stop("Se necesitan al menos dos predictores.", call. = FALSE)
  if (length(tl) > 12) stop("Con m\u00e1s de 12 predictores hay demasiadas combinaciones (", 2^length(tl) - 1, "); reduce la lista con criterios te\u00f3ricos.", call. = FALSE)
  mf <- stats::model.frame(fit); yn <- names(mf)[1]; n <- nrow(mf)
  if (!attr(stats::terms(fit), "intercept")) stop("El modelo no tiene intercepto; tabla_subconjuntos() lo necesita.", call. = FALSE)
  mse <- sum(stats::residuals(fit)^2) / stats::df.residual(fit)
  subs <- unlist(lapply(seq_along(tl), function(m) utils::combn(tl, m, simplify = FALSE)), recursive = FALSE)
  filas <- lapply(subs, function(s) {
    f <- stats::lm(stats::reformulate(s, yn), data = mf)
    sm <- summary(f); kk <- length(stats::coef(f)) - 1; K <- kk + 2
    sse <- sum(stats::residuals(f)^2); aic <- stats::AIC(f)
    h <- stats::hatvalues(f); press <- sum((stats::residuals(f) / (1 - h))^2)
    data.frame(predictores = paste(s, collapse = " + "), m = length(s), k = kk, R2 = sm$r.squared, R2_ajustada = sm$adj.r.squared,
               Cp = sse / mse - n + 2 * (kk + 1), AICc = if (n - K - 1 > 0) aic + 2 * K * (K + 1) / (n - K - 1) else NA,
               BIC = stats::BIC(f), PRESS = press, stringsAsFactors = FALSE)
  })
  D <- do.call(rbind, filas)
  D <- D[order(if (criterio == "R2_ajustada") -D$R2_ajustada else D[[criterio]], D$k), ]; rownames(D) <- NULL
  if (is.null(mostrar)) mostrar <- if (nrow(D) <= 15) nrow(D) else 15
  V <- utils::head(D, mostrar)
  nombres <- vapply(V$predictores, function(x) paste(renombrar(strsplit(x, " + ", fixed = TRUE)[[1]], etiquetas), collapse = ", "), character(1))
  cel <- cbind(nombres, formatC(V$k, format = "d"), f_apa(V$R2, 3), f_apa(V$R2_ajustada, 3), f_num(V$Cp, 2),
               ifelse(is.na(V$AICc), "\u2014", f_num(V$AICc, 2)), f_num(V$BIC, 2), f_num(V$PRESS, 2))
  neg <- matrix(FALSE, nrow(cel), ncol(cel))
  neg[, 4] <- abs(V$R2_ajustada - max(D$R2_ajustada)) < 1e-12
  neg[, 6] <- !is.na(V$AICc) & abs(V$AICc - min(D$AICc, na.rm = TRUE)) < 1e-9
  neg[, 7] <- abs(V$BIC - min(D$BIC)) < 1e-9
  neg[, 8] <- abs(V$PRESS - min(D$PRESS)) < 1e-9
  cerca <- sum(D$AICc - min(D$AICc, na.rm = TRUE) <= 2, na.rm = TRUE)
  nota <- paste0("N = ", n, tx(". Modelos ordenados por ", ". Models ordered by "), sub("R2_ajustada", tx("R\u00b2 ajustada", "adjusted R\u00b2"), criterio),
                 if (mostrar < nrow(D)) paste0(tx("; se muestran ", "; showing "), mostrar, tx(" de ", " of "), nrow(D)) else "",
                 tx(". k = n\u00famero de coeficientes sin el intercepto; Cp = Cp de Mallows (un buen modelo tiene Cp cercano a k + 1); AICc = AIC corregido para muestras peque\u00f1as; PRESS = suma de los errores de predicci\u00f3n dejando fuera cada caso. Menores AICc, BIC y PRESS indican un mejor modelo; en negritas, el mejor valor de cada columna. ",
                    ". k = number of coefficients excluding the intercept; Cp = Mallows' Cp (a good model has Cp close to k + 1); AICc = small-sample corrected AIC; PRESS = predicted residual sum of squares (leave-one-out). Lower AICc, BIC and PRESS indicate a better model; the best value of each column is in bold. "),
                 if (cerca > 1) paste0(cerca, tx(" modelos est\u00e1n a 2 puntos o menos del AICc m\u00e1s bajo: tienen un apoyo parecido y la teor\u00eda debe decidir. ",
                                                 " models are within 2 points of the lowest AICc: they have similar support and theory should decide. ")) else "",
                 tx("Elegir predictores con los mismos datos aprovecha el azar; tr\u00e1talo como exploratorio y, si puedes, valida el modelo en otra muestra.",
                    "Selecting predictors on the same data capitalizes on chance; treat this as exploratory and, if possible, validate in another sample."))
  t <- nueva_tabla(if (is.null(titulo)) tx("Comparaci\u00f3n de todos los subconjuntos de predictores", "All-subsets comparison of predictors") else titulo,
                   c(tx("Predictores", "Predictors"), "k", "R\u00b2", tx("R\u00b2 aj.", "Adj. R\u00b2"), "Cp", "AICc", "BIC", "PRESS"),
                   cel, neg, alinear = c("l", rep("c", 7)), nota = nota, datos = D)
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}
