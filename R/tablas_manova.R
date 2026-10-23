# MANOVA de un factor

#' @noRd
box_m <- function(Y, g) {
  g <- droplevels(as.factor(g)); k <- nlevels(g); p <- ncol(Y); N <- nrow(Y)
  ni <- as.numeric(table(g))
  S <- lapply(levels(g), function(l) stats::cov(Y[g == l, , drop = FALSE]))
  Sp <- Reduce(`+`, Map(function(s, n) (n - 1) * s, S, ni)) / (N - k)
  M <- (N - k) * log(det(Sp)) - sum(mapply(function(s, n) (n - 1) * log(det(s)), S, ni))
  c1 <- (2 * p^2 + 3 * p - 1) / (6 * (p + 1) * (k - 1)) * (sum(1 / (ni - 1)) - 1 / (N - k))
  chi <- M * (1 - c1); gl <- p * (p + 1) * (k - 1) / 2
  c(M = M, chi = chi, gl = gl, p = stats::pchisq(chi, gl, lower.tail = FALSE))
}

# Comparaciones por pares de una variable entre grupos (Tukey HSD o Games-Howell)
#' @noRd
comparaciones_pares <- function(y, g, metodo, nivel = .95) {
  lv <- levels(g); k <- length(lv)
  m <- tapply(y, g, mean); v <- tapply(y, g, stats::var); n <- tapply(y, g, length)
  N <- length(y); mse <- sum((n - 1) * v) / (N - k)
  pares <- utils::combn(lv, 2)
  out <- lapply(seq_len(ncol(pares)), function(j) {
    a <- pares[1, j]; b <- pares[2, j]; dif <- m[[b]] - m[[a]]
    if (metodo == "tukey") {
      se <- sqrt(mse / 2 * (1 / n[[a]] + 1 / n[[b]])); gl <- N - k
    } else {
      se <- sqrt((v[[a]] / n[[a]] + v[[b]] / n[[b]]) / 2)
      gl <- (v[[a]] / n[[a]] + v[[b]] / n[[b]])^2 / ((v[[a]] / n[[a]])^2 / (n[[a]] - 1) + (v[[b]] / n[[b]])^2 / (n[[b]] - 1))
    }
    q <- abs(dif) / se; crit <- stats::qtukey(nivel, k, gl)
    dp <- sqrt(((n[[a]] - 1) * v[[a]] + (n[[b]] - 1) * v[[b]]) / (n[[a]] + n[[b]] - 2))
    data.frame(grupo1 = a, grupo2 = b, diferencia = dif, ic_inf = dif - crit * se, ic_sup = dif + crit * se,
               p = stats::ptukey(q, k, gl, lower.tail = FALSE), d = dif / dp, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

#' Tabla de MANOVA de un factor
#'
#' Análisis de varianza multivariado de un factor: ¿difieren los grupos en el
#' conjunto de variables dependientes? La tabla tiene cinco partes:
#'
#' 1. **Pruebas multivariadas:** traza de Pillai, lambda de Wilks, traza de
#'    Hotelling-Lawley y raíz mayor de Roy, con su F aproximada, grados de
#'    libertad, *p* y eta cuadrada parcial (\eqn{\eta^2_p}). La traza de
#'    Pillai es la más robusta cuando no se cumple la igualdad de matrices de
#'    covarianza. En un MANOVA de un factor, la eta cuadrada y la eta
#'    cuadrada parcial coinciden.
#' 2. **Supuestos:** igualdad de matrices de covarianza (M de Box, que se
#'    evalúa con \eqn{\alpha} = .001 por ser muy sensible a la falta de
#'    normalidad), razón entre el grupo más grande y el más pequeño (< 1.5),
#'    normalidad multivariada en cada grupo (Mardia), atípicos multivariados
#'    dentro de cada grupo (Mahalanobis, p < .001) y multicolinealidad entre
#'    las variables dependientes (r < .90).
#' 3. **ANOVA univariados** de seguimiento para cada variable dependiente,
#'    con la corrección de Bonferroni (\eqn{\alpha} / número de variables).
#' 4. **Comparaciones post hoc** entre pares de grupos para cada variable
#'    dependiente (con tres o más grupos): diferencia de medias, intervalo de
#'    confianza al 95 %, *p* ajustado y *d* de Cohen. Con `posthoc = "auto"`
#'    se usa Tukey HSD si se cumple la igualdad de matrices de covarianza y
#'    Games-Howell (que no supone varianzas iguales) si no se cumple.
#' 5. **Descriptivos por grupo:** media y desviación estándar.
#'
#' Con dos grupos, las cuatro pruebas equivalen a la \eqn{T^2} de Hotelling,
#' que se informa en la nota, y no hacen falta comparaciones post hoc.
#'
#' @param datos `data.frame`.
#' @param dependientes Nombres de las variables dependientes (dos o más,
#'   métricas).
#' @param grupo Nombre de la variable independiente (el factor).
#' @param posthoc `"auto"` (por omisión), `"tukey"`, `"games-howell"` o
#'   `"ninguno"`.
#' @param alfa Nivel de significancia.
#' @param decimales Número de decimales.
#' @param titulo Título de la tabla.
#' @param archivo Si se indica, guarda la tabla.
#' @return Un objeto `tabla_apa` con varias partes; los valores están en
#'   `$datos` y el modelo de `stats::manova()` en `$modelo`.
#' @examples
#' tabla_manova(iris, c("Petal.Length", "Sepal.Length"), "Species")
#' @export
tabla_manova <- function(datos, dependientes, grupo, posthoc = c("auto", "tukey", "games-howell", "ninguno"),
                         alfa = .05, decimales = 2, titulo = NULL, archivo = NULL) {
  posthoc <- match.arg(posthoc)
  if (!is.data.frame(datos)) stop("`datos` debe ser un data.frame.", call. = FALSE)
  falta <- setdiff(c(dependientes, grupo), names(datos))
  if (length(falta)) stop("No est\u00e1n en los datos: ", paste(falta, collapse = ", "), call. = FALSE)
  if (length(dependientes) < 2) stop("Se necesitan dos o m\u00e1s variables dependientes.", call. = FALSE)
  d <- datos[stats::complete.cases(datos[, c(dependientes, grupo)]), c(dependientes, grupo)]
  g <- droplevels(as.factor(d[[grupo]])); Y <- as.matrix(d[, dependientes])
  k <- nlevels(g); p <- ncol(Y); N <- nrow(Y); ni <- table(g)
  if (k < 2) stop("El factor necesita al menos dos grupos.", call. = FALSE)
  mod <- stats::manova(Y ~ g)
  s <- min(p, k - 1)
  pruebas <- c(Pillai = "Pillai", Wilks = "Wilks", "Hotelling-Lawley" = "Hotelling-Lawley", Roy = "Roy")
  res <- t(vapply(pruebas, function(te) {
    st <- summary(mod, test = te)$stats[1, ]
    c(valor = st[[2]], F = st[[3]], gl1 = st[[4]], gl2 = st[[5]], p = st[[6]])
  }, numeric(5)))
  eta <- c(Pillai = res["Pillai", "valor"] / s,
           Wilks = 1 - res["Wilks", "valor"]^(1 / s),
           "Hotelling-Lawley" = (res["Hotelling-Lawley", "valor"] / s) / (1 + res["Hotelling-Lawley", "valor"] / s),
           Roy = res["Roy", "valor"] / (1 + res["Roy", "valor"]))
  nombres <- c(tx("Traza de Pillai", "Pillai's trace"), tx("Lambda de Wilks", "Wilks' lambda"),
               tx("Traza de Hotelling-Lawley", "Hotelling-Lawley trace"), tx("Ra\u00edz mayor de Roy", "Roy's largest root"))
  cel <- cbind(nombres, f_num(res[, "valor"], 3), f_num(res[, "F"], 2), f_num(res[, "gl1"], 0), f_num(res[, "gl2"], 0),
               f_p(res[, "p"]), f_apa(eta, decimales))
  t2 <- if (k == 2) res["Hotelling-Lawley", "valor"] * (N - 2) else NA
  nota <- paste0(tx("F aproximada; \u03b7\u00b2p = eta cuadrada parcial (en un MANOVA de un factor coincide con la eta cuadrada). La traza de Pillai es la prueba m\u00e1s robusta si no se cumple la igualdad de matrices de covarianza. Variables dependientes: ",
                    "Approximate F; \u03b7\u00b2p = partial eta squared (equal to eta squared in a one-way MANOVA). Pillai's trace is the most robust test when covariance matrices are unequal. Dependent variables: "),
                 paste(dependientes, collapse = ", "), "; ", tx("factor: ", "factor: "), grupo, " (", k, tx(" grupos, N = ", " groups, N = "), N, ").",
                 if (!is.na(t2)) paste0(tx(" Con dos grupos las cuatro pruebas equivalen a la T\u00b2 de Hotelling: T\u00b2 = ", " With two groups the four tests are equivalent to Hotelling's T\u00b2: T\u00b2 = "), f_num(t2, 2), ".") else "")

  # Supuestos
  bm <- box_m(Y, g)
  razon <- max(ni) / min(ni)
  mard <- lapply(levels(g), function(l) { Yl <- Y[g == l, , drop = FALSE]
    if (nrow(Yl) > p + 1) mardia_mv(Yl) else NULL })
  atip <- unlist(lapply(levels(g), function(l) {
    idx <- which(g == l); Yl <- Y[idx, , drop = FALSE]
    if (length(idx) <= p + 1) return(integer(0))
    D2 <- stats::mahalanobis(Yl, colMeans(Yl), stats::cov(Yl))
    as.integer(rownames(d)[idx][D2 > stats::qchisq(.999, p)]) }))
  R <- stats::cor(Y); rmax <- max(abs(R[upper.tri(R)]))
  si <- tx("Se cumple", "Met"); no <- tx("No se cumple", "Not met")
  fp <- function(x) if (x < .001) "< .001" else paste("=", f_apa(x, 3))
  filas_normal <- t(vapply(seq_along(mard), function(i) {
    m <- mard[[i]]
    if (is.null(m)) return(c(paste0("   ", levels(g)[i]), tx("n insuficiente", "insufficient n"), "p > .05", ""))
    ok <- m$p_asim >= .05 && m$p_curt >= .05
    c(paste0("   ", levels(g)[i]), paste0(tx("Asimetr\u00eda p ", "Skewness p "), fp(m$p_asim), tx("; curtosis p ", "; kurtosis p "), fp(m$p_curt)),
      "p > .05", if (ok) si else no)
  }, character(4)))
  cel2 <- rbind(
    c(tx("Igualdad de matrices de covarianza (M de Box)", "Equality of covariance matrices (Box's M)"),
      paste0("M = ", f_num(bm[["M"]], 2), ", \u03c7\u00b2(", f_num(bm[["gl"]], 0), ") = ", f_num(bm[["chi"]], 2), ", p ", fp(bm[["p"]])),
      "p > .001", if (bm[["p"]] > .001) si else no),
    c(tx("Raz\u00f3n de tama\u00f1os de grupo (mayor/menor)", "Group size ratio (largest/smallest)"), f_num(razon, 2), "< 1.5", if (razon < 1.5) si else no),
    c(tx("Normalidad multivariada por grupo (Mardia)", "Multivariate normality by group (Mardia)"), "", "", ""),
    filas_normal,
    c(tx("At\u00edpicos multivariados por grupo (Mahalanobis)", "Multivariate outliers by group (Mahalanobis)"),
      if (length(atip)) paste0(length(atip), tx(" (filas ", " (rows "), paste(sort(atip), collapse = ", "), ")") else tx("Ninguno", "None"),
      paste0("D\u00b2 > ", f_num(stats::qchisq(.999, p), 2), " (p < .001)"), if (length(atip)) tx("Revisar", "Review") else si),
    c(tx("Multicolinealidad entre variables dependientes", "Multicollinearity among dependent variables"),
      paste0(tx("|r| m\u00e1xima = ", "Largest |r| = "), f_apa(rmax, 2)), "< .90", if (rmax < .90) si else no))
  nota2 <- tx("La M de Box es muy sensible a la falta de normalidad, por eso se eval\u00faa con \u03b1 = .001. Si no se cumple la igualdad de matrices de covarianza, use la traza de Pillai, procure grupos de tama\u00f1o similar y un nivel de significancia m\u00e1s estricto. La linealidad entre pares de variables dependientes se revisa con diagramas de dispersi\u00f3n; la independencia de las observaciones, con el dise\u00f1o del estudio.",
              "Box's M is highly sensitive to non-normality and is therefore evaluated at \u03b1 = .001. If covariance matrices are unequal, report Pillai's trace, keep group sizes similar, and use a stricter significance level. Linearity between pairs of dependent variables is checked with scatterplots; independence of observations, through the study design.")

  # ANOVA univariados
  au <- stats::summary.aov(mod)
  alfa_b <- alfa / p
  uni <- t(vapply(seq_len(p), function(j) {
    tb <- au[[j]]; ss <- tb[["Sum Sq"]]
    c(F = tb[["F value"]][1], gl1 = tb[["Df"]][1], gl2 = tb[["Df"]][2], p = tb[["Pr(>F)"]][1], eta = ss[1] / (ss[1] + ss[2]))
  }, numeric(5)))
  cel3 <- cbind(dependientes, f_num(uni[, "F"], 2), f_num(uni[, "gl1"], 0), f_num(uni[, "gl2"], 0), f_p(uni[, "p"]), f_apa(uni[, "eta"], decimales))
  neg3 <- matrix(FALSE, p, 6); neg3[uni[, "p"] < alfa_b, 5] <- TRUE
  nota3 <- paste0(tx("\u03b7\u00b2p = eta cuadrada parcial. Con la correcci\u00f3n de Bonferroni, cada ANOVA se eval\u00faa con \u03b1 = ",
                     "\u03b7\u00b2p = partial eta squared. With the Bonferroni correction, each ANOVA is evaluated at \u03b1 = "),
                  f_apa(alfa, 2), "/", p, " = ", f_apa(alfa_b, 3), tx("; en negritas, los significativos.", "; significant results are in bold."))

  # Post hoc
  extra_ph <- list(); ph <- NULL
  if (k >= 3 && posthoc != "ninguno") {
    metodo <- if (posthoc == "auto") (if (bm[["p"]] > .001) "tukey" else "games-howell") else posthoc
    ph <- do.call(rbind, lapply(dependientes, function(v) cbind(variable = v, comparaciones_pares(d[[v]], g, metodo))))
    prev <- ""
    cel5 <- t(vapply(seq_len(nrow(ph)), function(i) {
      v <- if (ph$variable[i] != prev) ph$variable[i] else ""; prev <<- ph$variable[i]
      c(v, paste(ph$grupo2[i], "\u2212", ph$grupo1[i]), f_num(ph$diferencia[i], decimales),
        paste0("[", f_num(ph$ic_inf[i], decimales), ", ", f_num(ph$ic_sup[i], decimales), "]"), f_p(ph$p[i]), f_num(ph$d[i], 2))
    }, character(6)))
    neg5 <- matrix(FALSE, nrow(cel5), 6); neg5[ph$p < alfa, 5] <- TRUE
    nombre_m <- if (metodo == "tukey") "Tukey HSD" else "Games-Howell"
    nota5 <- paste0(tx("Comparaciones por pares con ", "Pairwise comparisons using "), nombre_m,
                    if (posthoc == "auto") tx(if (metodo == "tukey") " (se cumple la igualdad de matrices de covarianza)" else " (no se cumple la igualdad de matrices de covarianza; Games-Howell no supone varianzas iguales)",
                                              if (metodo == "tukey") " (covariance matrices are equal)" else " (covariance matrices are unequal; Games-Howell does not assume equal variances)") else "",
                    tx(". Diferencia = media del segundo grupo menos la del primero; IC 95 % y p ajustados por comparaciones m\u00faltiples; d = d de Cohen (0.20 peque\u00f1o, 0.50 mediano, 0.80 grande); en negritas, p < ",
                       ". Difference = mean of the second group minus the first; 95% CI and p adjusted for multiple comparisons; d = Cohen's d (0.20 small, 0.50 medium, 0.80 large); p < "),
                    f_apa(alfa, 2), tx(".", " in bold."))
    extra_ph <- list(nueva_tabla(tx("Comparaciones post hoc entre grupos", "Post hoc comparisons between groups"),
                                 c("Variable", tx("Comparaci\u00f3n", "Comparison"), tx("Diferencia", "Difference"), tx("IC 95 %", "95% CI"), "p", "d"),
                                 cel5, neg5, alinear = c("l", "l", "c", "c", "c", "c"), nota = nota5,
                                 separadores = cumsum(table(factor(ph$variable, levels = dependientes)))[-p]))
  }

  # Descriptivos por grupo
  desc <- vapply(levels(g), function(l) vapply(dependientes, function(v) {
    x <- d[g == l, v]; paste0(f_num(mean(x), decimales), " (", f_num(stats::sd(x), decimales), ")") }, character(1)), character(p))
  desc <- matrix(desc, nrow = p)
  cel4 <- rbind(cbind(dependientes, desc), c("n", as.character(as.numeric(ni))))

  t <- nueva_tabla(if (is.null(titulo)) tx("MANOVA de un factor: pruebas multivariadas", "One-way MANOVA: multivariate tests") else titulo,
                   c(tx("Prueba", "Test"), tx("Valor", "Value"), "F", tx("gl1", "df1"), tx("gl2", "df2"), "p", "\u03b7\u00b2p"), cel, nota = nota,
                   datos = list(multivariadas = cbind(as.data.frame(res), eta_p = eta), box_m = bm, razon = razon,
                                univariadas = data.frame(variable = dependientes, uni, row.names = NULL), posthoc = ph, atipicos = sort(atip)),
                   extra = c(list(
                     nueva_tabla(tx("Supuestos del MANOVA", "MANOVA assumptions"),
                                 c(tx("Supuesto", "Assumption"), tx("Resultado", "Result"), tx("Criterio", "Criterion"), tx("Conclusi\u00f3n", "Conclusion")), cel2,
                                 alinear = rep("l", 4), nota = nota2),
                     nueva_tabla(tx("ANOVA univariados de seguimiento", "Follow-up univariate ANOVAs"),
                                 c("Variable", "F", tx("gl1", "df1"), tx("gl2", "df2"), "p", "\u03b7\u00b2p"), cel3, neg3, nota = nota3)),
                     extra_ph,
                     list(nueva_tabla(tx("Medias (desviaciones est\u00e1ndar) por grupo", "Means (standard deviations) by group"), c("Variable", levels(g)), cel4,
                                      separadores = p, nota = tx("Valores: M (DE).", "Values: M (SD).")))))
  t$modelo <- mod
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}
