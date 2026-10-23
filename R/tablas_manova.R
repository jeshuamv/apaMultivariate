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

#' Tabla de MANOVA de un factor
#'
#' Análisis de varianza multivariado de un factor: ¿difieren los grupos en el
#' conjunto de variables dependientes? La tabla tiene cuatro partes:
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
#' 4. **Descriptivos por grupo:** media y desviación estándar.
#'
#' Con dos grupos, las cuatro pruebas equivalen a la \eqn{T^2} de Hotelling,
#' que se informa en la nota.
#'
#' @param datos `data.frame`.
#' @param dependientes Nombres de las variables dependientes (dos o más,
#'   métricas).
#' @param grupo Nombre de la variable independiente (el factor).
#' @param alfa Nivel de significancia.
#' @param decimales Número de decimales.
#' @param titulo Título de la tabla.
#' @param archivo Si se indica, guarda la tabla.
#' @return Un objeto `tabla_apa` con cuatro partes; los valores están en
#'   `$datos` y el modelo de `stats::manova()` en `$modelo`.
#' @examples
#' tabla_manova(iris, c("Petal.Length", "Sepal.Length"), "Species")
#' @export
tabla_manova <- function(datos, dependientes, grupo, alfa = .05, decimales = 2, titulo = NULL, archivo = NULL) {
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
  nombres <- c("Traza de Pillai", "Lambda de Wilks", "Traza de Hotelling-Lawley", "Ra\u00edz mayor de Roy")
  cel <- cbind(nombres, f_num(res[, "valor"], 3), f_num(res[, "F"], 2), f_num(res[, "gl1"], 0), f_num(res[, "gl2"], 0),
               f_p(res[, "p"]), f_apa(eta, decimales))
  t2 <- if (k == 2) res["Hotelling-Lawley", "valor"] * (N - 2) else NA
  nota <- paste0("F aproximada; \u03b7\u00b2p = eta cuadrada parcial (en un MANOVA de un factor coincide con la eta cuadrada). ",
                 "La traza de Pillai es la prueba m\u00e1s robusta si no se cumple la igualdad de matrices de covarianza. ",
                 "Variables dependientes: ", paste(dependientes, collapse = ", "), "; factor: ", grupo, " (", k, " grupos, N = ", N, ").",
                 if (!is.na(t2)) paste0(" Con dos grupos las cuatro pruebas equivalen a la T\u00b2 de Hotelling: T\u00b2 = ", f_num(t2, 2), ".") else "")

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
  filas_normal <- t(vapply(seq_along(mard), function(i) {
    m <- mard[[i]]
    if (is.null(m)) return(c(paste0("   ", levels(g)[i]), "n insuficiente", "p > .05", ""))
    ok <- m$p_asim >= .05 && m$p_curt >= .05
    c(paste0("   ", levels(g)[i]), paste0("Asimetr\u00eda p ", if (m$p_asim < .001) "< .001" else paste("=", f_apa(m$p_asim, 3)),
                                        "; curtosis p ", if (m$p_curt < .001) "< .001" else paste("=", f_apa(m$p_curt, 3))),
      "p > .05", if (ok) "Se cumple" else "No se cumple")
  }, character(4)))
  cel2 <- rbind(
    c("Igualdad de matrices de covarianza (M de Box)", paste0("M = ", f_num(bm[["M"]], 2), ", \u03c7\u00b2(", f_num(bm[["gl"]], 0), ") = ",
                                                              f_num(bm[["chi"]], 2), ", p ", if (bm[["p"]] < .001) "< .001" else paste("=", f_apa(bm[["p"]], 3))),
      "p > .001", if (bm[["p"]] > .001) "Se cumple" else "No se cumple"),
    c("Raz\u00f3n de tama\u00f1os de grupo (mayor/menor)", f_num(razon, 2), "< 1.5", if (razon < 1.5) "Se cumple" else "No se cumple"),
    c("Normalidad multivariada por grupo (Mardia)", "", "", ""),
    filas_normal,
    c("At\u00edpicos multivariados por grupo (Mahalanobis)", if (length(atip)) paste0(length(atip), " (filas ", paste(sort(atip), collapse = ", "), ")") else "Ninguno",
      paste0("D\u00b2 > ", f_num(stats::qchisq(.999, p), 2), " (p < .001)"), if (length(atip)) "Revisar" else "Se cumple"),
    c("Multicolinealidad entre variables dependientes", paste0("|r| m\u00e1xima = ", f_apa(rmax, 2)), "< .90", if (rmax < .90) "Se cumple" else "No se cumple"))
  nota2 <- paste0("La M de Box es muy sensible a la falta de normalidad, por eso se eval\u00faa con \u03b1 = .001. Si no se cumple la igualdad de ",
                  "matrices de covarianza, use la traza de Pillai, procure grupos de tama\u00f1o similar y un nivel de significancia m\u00e1s estricto. ",
                  "La linealidad entre pares de variables dependientes se revisa con diagramas de dispersi\u00f3n; la independencia de las ",
                  "observaciones, con el dise\u00f1o del estudio.")

  # ANOVA univariados
  au <- stats::summary.aov(mod)
  alfa_b <- alfa / p
  uni <- t(vapply(seq_len(p), function(j) {
    tb <- au[[j]]; ss <- tb[["Sum Sq"]]
    c(F = tb[["F value"]][1], gl1 = tb[["Df"]][1], gl2 = tb[["Df"]][2], p = tb[["Pr(>F)"]][1], eta = ss[1] / (ss[1] + ss[2]))
  }, numeric(5)))
  cel3 <- cbind(dependientes, f_num(uni[, "F"], 2), f_num(uni[, "gl1"], 0), f_num(uni[, "gl2"], 0), f_p(uni[, "p"]), f_apa(uni[, "eta"], decimales))
  neg3 <- matrix(FALSE, p, 6); neg3[uni[, "p"] < alfa_b, 5] <- TRUE
  nota3 <- paste0("\u03b7\u00b2p = eta cuadrada parcial. Con la correcci\u00f3n de Bonferroni, cada ANOVA se eval\u00faa con \u03b1 = ",
                  f_apa(alfa, 2), "/", p, " = ", f_apa(alfa_b, 3), "; en negritas, los significativos.")

  # Descriptivos por grupo
  desc <- vapply(levels(g), function(l) vapply(dependientes, function(v) {
    x <- d[g == l, v]; paste0(f_num(mean(x), decimales), " (", f_num(stats::sd(x), decimales), ")") }, character(1)), character(p))
  desc <- matrix(desc, nrow = p)
  cel4 <- rbind(cbind(dependientes, desc), c("n", as.character(as.numeric(ni))))

  t <- nueva_tabla(if (is.null(titulo)) "MANOVA de un factor: pruebas multivariadas" else titulo,
                   c("Prueba", "Valor", "F", "gl1", "gl2", "p", "\u03b7\u00b2p"), cel, nota = nota,
                   datos = list(multivariadas = cbind(as.data.frame(res), eta_p = eta), box_m = bm, razon = razon,
                                univariadas = data.frame(variable = dependientes, uni, row.names = NULL), atipicos = sort(atip)),
                   extra = list(
                     nueva_tabla("Supuestos del MANOVA", c("Supuesto", "Resultado", "Criterio", "Conclusi\u00f3n"), cel2,
                                 alinear = rep("l", 4), nota = nota2),
                     nueva_tabla("ANOVA univariados de seguimiento", c("Variable", "F", "gl1", "gl2", "p", "\u03b7\u00b2p"), cel3, neg3,
                                 nota = nota3),
                     nueva_tabla("Medias (desviaciones est\u00e1ndar) por grupo", c("Variable", levels(g)), cel4,
                                 separadores = p, nota = "Valores: M (DE).")))
  t$modelo <- mod
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}
