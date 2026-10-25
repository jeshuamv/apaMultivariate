# Moderación y mediación moderada al estilo de PROCESS (Hayes, 2022):
# regresiones por mínimos cuadrados y bootstrap de percentiles

#' @noRd
ajustar_moderacion <- function(d, modelo, cov) {
  f <- function(lhs, rhs) stats::as.formula(paste(lhs, "~", paste(c(rhs, cov), collapse = " + ")))
  switch(as.character(modelo),
    "1"  = list(y = stats::lm(f(".Y", c(".X", ".W", ".X:.W")), d)),
    "7"  = list(m = stats::lm(f(".M", c(".X", ".W", ".X:.W")), d), y = stats::lm(f(".Y", c(".M", ".X")), d)),
    "14" = list(m = stats::lm(f(".M", ".X"), d), y = stats::lm(f(".Y", c(".M", ".X", ".W", ".M:.W")), d)))
}

# Efectos indirectos condicionales e índice de mediación moderada
#' @noRd
indirectos_condicionales <- function(ml, modelo, wv) {
  if (modelo == 7) {
    a1 <- stats::coef(ml$m)[[".X"]]; a3 <- stats::coef(ml$m)[[".X:.W"]]; b <- stats::coef(ml$y)[[".M"]]
    c((a1 + a3 * wv) * b, indice = a3 * b)
  } else {
    a <- stats::coef(ml$m)[[".X"]]; b1 <- stats::coef(ml$y)[[".M"]]; b3 <- stats::coef(ml$y)[[".M:.W"]]
    c(a * (b1 + b3 * wv), indice = a * b3)
  }
}

#' Tabla de moderación y mediación moderada
#'
#' Análisis de moderación y de mediación moderada con variables observadas,
#' como el macro PROCESS de Hayes (2022), con regresiones por mínimos
#' cuadrados y bootstrap de percentiles. Se usan sobre todo con puntajes de
#' escalas (sumas o promedios de los ítems).
#'
#' * **Modelo 1, moderación simple:** W modera el efecto de X sobre Y. La
#'   tabla da los coeficientes, el cambio en \eqn{R^2} por la interacción, el
#'   efecto condicional de X en distintos valores de W (pendientes simples) y
#'   la región de significancia de Johnson-Neyman.
#' * **Modelo 7, mediación moderada en la primera etapa:** W modera el efecto
#'   de X sobre el mediador M (X \eqn{\rightarrow} M \eqn{\rightarrow} Y).
#' * **Modelo 14, mediación moderada en la segunda etapa:** W modera el efecto
#'   del mediador M sobre Y.
#'
#' En los modelos 7 y 14 la tabla da los efectos indirectos condicionales en
#' distintos valores de W y el **índice de mediación moderada** (Hayes, 2015)
#' con su intervalo bootstrap: si no incluye el 0, el efecto indirecto
#' depende de W.
#'
#' Los valores de W son la media y una desviación estándar arriba y abajo
#' (o los dos valores, si W es dicotómica). Por omisión X y W (y M en el
#' modelo 14) se centran en la media, lo que facilita interpretar los
#' coeficientes principales; los efectos condicionales no cambian.
#'
#' @param datos `data.frame`.
#' @param x,w,y Nombres de la variable independiente, el moderador y la
#'   dependiente.
#' @param m Nombre del mediador (modelos 7 y 14).
#' @param modelo `1`, `7` o `14` (numeración de PROCESS).
#' @param covariables Nombres de covariables (opcional).
#' @param centrar Centrar en la media las variables que forman productos.
#' @param bootstrap Remuestreos bootstrap para los efectos indirectos
#'   (5000 por omisión, como PROCESS).
#' @param semilla Semilla para el bootstrap.
#' @inheritParams tabla_descriptivos
#' @return Un objeto `tabla_apa`; los valores están en `$datos` y las
#'   regresiones en `$modelos`. Se puede graficar con [grafica_interaccion()].
#' @examples
#' set.seed(1)
#' d <- data.frame(X = rnorm(200), W = rnorm(200))
#' d$Y <- .3 * d$X + .2 * d$W + .25 * d$X * d$W + rnorm(200)
#' tabla_moderacion(d, x = "X", w = "W", y = "Y")
#' \donttest{
#' d$M <- .4 * d$X + .3 * d$X * d$W + rnorm(200)
#' d$Y2 <- .5 * d$M + rnorm(200)
#' tabla_moderacion(d, x = "X", w = "W", m = "M", y = "Y2", modelo = 7, bootstrap = 1000)
#' }
#' @export
tabla_moderacion <- function(datos, x, w, y, m = NULL, modelo = 1, covariables = NULL, centrar = TRUE,
                             bootstrap = 5000, semilla = 123, etiquetas = NULL, decimales = 2,
                             titulo = NULL, archivo = NULL) {
  modelo <- as.numeric(modelo)
  if (!modelo %in% c(1, 7, 14)) stop("`modelo` debe ser 1, 7 o 14 (numeraci\u00f3n de PROCESS).", call. = FALSE)
  if (modelo != 1 && is.null(m)) stop("Los modelos 7 y 14 necesitan un mediador: m = \"...\".", call. = FALSE)
  vars <- c(x, w, y, m, covariables)
  falta <- setdiff(vars, names(datos))
  if (length(falta)) stop("No est\u00e1n en los datos: ", paste(falta, collapse = ", "), call. = FALSE)
  d0 <- datos[stats::complete.cases(datos[, vars]), vars, drop = FALSE]
  d <- data.frame(.X = d0[[x]], .W = d0[[w]], .Y = d0[[y]])
  if (!is.null(m)) d$.M <- d0[[m]]
  for (cv in covariables) d[[cv]] <- d0[[cv]]
  medias <- c(X = mean(d$.X), W = mean(d$.W), M = if (!is.null(m)) mean(d$.M) else NA)
  centrados <- if (!centrar) character(0) else if (modelo == 14) c("M", "W") else c("X", "W")
  for (v in centrados) d[[paste0(".", v)]] <- d[[paste0(".", v)]] - medias[[v]]
  ml <- ajustar_moderacion(d, modelo, covariables)
  n <- nrow(d)
  et <- function(v) renombrar(v, etiquetas)
  nom <- c(.X = et(x), .W = et(w), .M = if (!is.null(m)) et(m) else "", .Y = et(y))
  prod_txt <- function(a, b) paste(nom[[a]], "\u00d7", nom[[b]])

  # Valores de W (escala original) y su versión centrada
  wo <- d0[[w]]; dic <- length(unique(wo)) == 2
  w_orig <- if (dic) sort(unique(wo)) else mean(wo) + c(-1, 0, 1) * stats::sd(wo)
  w_lab <- if (dic) format(w_orig) else c(tx("\u22121 DE", "\u22121 SD"), tx("Media", "Mean"), tx("+1 DE", "+1 SD"))
  wv <- w_orig - if ("W" %in% centrados) medias[["W"]] else 0

  # Tabla de coeficientes
  fila_coef <- function(fit, term, nombre) {
    cs <- summary(fit)$coefficients; ci <- stats::confint(fit)
    c(paste0("   ", nombre), f_num(cs[term, 1], decimales), f_num(cs[term, 2], decimales), f_num(cs[term, 3], 2),
      f_p(cs[term, 4]), paste0("[", f_num(ci[term, 1], decimales), ", ", f_num(ci[term, 2], decimales), "]"))
  }
  nombre_term <- function(term) switch(term, "(Intercept)" = tx("Constante", "Constant"), ".X" = nom[[".X"]], ".W" = nom[[".W"]],
                                       ".M" = nom[[".M"]], ".X:.W" = prod_txt(".X", ".W"), ".M:.W" = prod_txt(".M", ".W"), term)
  bloque <- function(fit, dep) {
    terms <- rownames(summary(fit)$coefficients)
    princ <- intersect(c(".X", ".M", ".W", ".X:.W", ".M:.W"), terms)
    terms <- c(princ, setdiff(terms, c(princ, "(Intercept)")), "(Intercept)")
    s <- summary(fit); fst <- s$fstatistic
    list(cel = rbind(c(paste0(tx("Variable dependiente: ", "Outcome: "), dep), rep("", 5)),
                     t(vapply(terms, function(tm) fila_coef(fit, tm, nombre_term(tm)), character(6)))),
         r2 = paste0(dep, ": R\u00b2 = ", f_apa(s$r.squared, 3), ", F(", fst[2], ", ", fst[3], ") = ", f_num(fst[1], 2), ", p ",
                     { pv <- stats::pf(fst[1], fst[2], fst[3], lower.tail = FALSE); if (pv < .001) "< .001" else paste("=", f_apa(pv, 3)) }))
  }
  bl <- if (modelo == 1) list(bloque(ml$y, nom[[".Y"]])) else list(bloque(ml$m, nom[[".M"]]), bloque(ml$y, nom[[".Y"]]))
  cel <- do.call(rbind, lapply(bl, `[[`, "cel"))
  sep <- if (length(bl) == 2) nrow(bl[[1]]$cel) else integer(0)

  # Cambio en R2 por la interacción
  modint <- if (modelo == 14) ml$y else if (modelo == 7) ml$m else ml$y
  term_int <- if (modelo == 14) ".M:.W" else ".X:.W"
  sin <- stats::update(modint, stats::as.formula(paste(". ~ . -", term_int)))
  av <- stats::anova(sin, modint)
  dr2 <- summary(modint)$r.squared - summary(sin)$r.squared
  int_txt <- paste0(tx("Interacci\u00f3n: \u0394R\u00b2 = ", "Interaction: \u0394R\u00b2 = "), f_apa(dr2, 3), ", F(", av$Df[2], ", ", av$Res.Df[2], ") = ",
                    f_num(av$F[2], 2), ", p ", if (av$`Pr(>F)`[2] < .001) "< .001" else paste("=", f_apa(av$`Pr(>F)`[2], 3)))
  cent_txt <- if (length(centrados)) paste0(tx("Variables centradas en la media: ", "Mean-centered variables: "),
                                            paste(nom[paste0(".", centrados)], collapse = ", "), ". ") else ""
  nota <- paste0(tx("B = coeficiente no estandarizado; EE = error est\u00e1ndar; IC 95 % basado en t. ", "B = unstandardized coefficient; SE = standard error; 95% CI based on t. "),
                 paste(vapply(bl, `[[`, character(1), "r2"), collapse = "; "), ". ", int_txt, ". ", cent_txt, "N = ", n, ".")

  extra <- list(); datos_t <- list(modelos = ml)
  if (modelo == 1) {
    b <- stats::coef(ml$y); V <- stats::vcov(ml$y); gl <- stats::df.residual(ml$y); tc <- stats::qt(.975, gl)
    ef <- b[[".X"]] + b[[".X:.W"]] * wv
    se <- sqrt(V[".X", ".X"] + 2 * wv * V[".X", ".X:.W"] + wv^2 * V[".X:.W", ".X:.W"])
    tt <- ef / se; pp <- 2 * stats::pt(-abs(tt), gl)
    cel2 <- cbind(paste0(w_lab, if (!dic) paste0(" (", f_num(w_orig, decimales), ")") else ""), f_num(ef, decimales), f_num(se, decimales),
                  f_num(tt, 2), f_p(pp), paste0("[", f_num(ef - tc * se, decimales), ", ", f_num(ef + tc * se, decimales), "]"))
    # Johnson-Neyman
    jn_txt <- ""; jn <- numeric(0)
    if (!dic) {
      A <- b[[".X:.W"]]^2 - tc^2 * V[".X:.W", ".X:.W"]; B <- 2 * (b[[".X"]] * b[[".X:.W"]] - tc^2 * V[".X", ".X:.W"])
      C <- b[[".X"]]^2 - tc^2 * V[".X", ".X"]; disc <- B^2 - 4 * A * C
      if (disc >= 0 && abs(A) > 1e-12) {
        raices <- sort((-B + c(-1, 1) * sqrt(disc)) / (2 * A))
        rango <- range(d$.W); jn <- raices[raices >= rango[1] & raices <= rango[2]]
      }
      desc <- if ("W" %in% centrados) medias[["W"]] else 0
      jn_txt <- if (length(jn)) paste0(tx(" Johnson-Neyman: el efecto de ", " Johnson-Neyman: the effect of "), nom[[".X"]],
                                       tx(" cambia de significancia (p = .05) cuando ", " changes significance (p = .05) when "), nom[[".W"]], " = ",
                                       paste(f_num(jn + desc, decimales), collapse = tx(" y ", " and ")), ".")
                else paste0(tx(" Johnson-Neyman: no hay puntos de transici\u00f3n dentro del rango observado de ", " Johnson-Neyman: no transition points within the observed range of "), nom[[".W"]], ".")
      jn <- jn + desc
    }
    extra[[1]] <- nueva_tabla(paste0(tx("Efecto condicional de ", "Conditional effect of "), nom[[".X"]], tx(" sobre ", " on "), nom[[".Y"]],
                                     tx(" seg\u00fan ", " at values of "), nom[[".W"]]),
                              c(nom[[".W"]], tx("Efecto", "Effect"), tx("EE", "SE"), "t", "p", tx("IC 95 %", "95% CI")), cel2,
                              nota = paste0(tx("Pendientes simples. ", "Simple slopes. "),
                                            if (!dic) tx("Valores de W: media y \u00b11 DE. ", "Moderator values: mean and \u00b11 SD. ") else "", jn_txt))
    datos_t$condicionales <- data.frame(w = w_orig, efecto = ef, ee = se, t = tt, p = pp); datos_t$johnson_neyman <- jn
  } else {
    est <- indirectos_condicionales(ml, modelo, wv)
    message("Calculando intervalos con ", bootstrap, " remuestreos bootstrap; puede tardar un poco...")
    if (!is.null(semilla)) set.seed(semilla)
    B <- t(replicate(bootstrap, {
      db <- d[sample.int(n, n, replace = TRUE), , drop = FALSE]
      indirectos_condicionales(ajustar_moderacion(db, modelo, covariables), modelo, wv)
    }))
    ee <- apply(B, 2, stats::sd); li <- apply(B, 2, stats::quantile, .025); ls <- apply(B, 2, stats::quantile, .975)
    k <- length(wv)
    filas <- c(paste0(w_lab, if (!dic) paste0(" (", f_num(w_orig, decimales), ")") else ""), tx("\u00cdndice de mediaci\u00f3n moderada", "Index of moderated mediation"))
    cel2 <- cbind(filas, f_num(est, decimales), f_num(ee, decimales), paste0("[", f_num(li, decimales), ", ", f_num(ls, decimales), "]"))
    neg2 <- matrix(FALSE, nrow(cel2), 4); neg2[li > 0 | ls < 0, 4] <- TRUE
    sig_ind <- li[k + 1] > 0 | ls[k + 1] < 0
    trayecto <- paste(nom[[".X"]], "\u2192", nom[[".M"]], "\u2192", nom[[".Y"]])
    nota2 <- paste0(tx("Efecto indirecto de ", "Indirect effect of "), trayecto, tx(" en valores de ", " at values of "), nom[[".W"]],
                    if (!dic) tx(" (media y \u00b11 DE)", " (mean and \u00b11 SD)") else "",
                    tx(". EE e IC 95 % por bootstrap de percentiles con ", ". Bootstrap SE and 95% percentile CI based on "), bootstrap,
                    tx(" remuestreos; en negritas, intervalos que no incluyen el 0. ", " resamples; intervals excluding 0 are in bold. "),
                    tx("\u00cdndice de mediaci\u00f3n moderada (Hayes, 2015): ", "Index of moderated mediation (Hayes, 2015): "),
                    if (sig_ind) tx("su intervalo no incluye el 0, por lo que el efecto indirecto depende de ", "its interval excludes 0, so the indirect effect depends on ")
                    else tx("su intervalo incluye el 0, por lo que no hay evidencia de que el efecto indirecto dependa de ", "its interval includes 0, so there is no evidence that the indirect effect depends on "),
                    nom[[".W"]], ".")
    extra[[1]] <- nueva_tabla(tx("Efectos indirectos condicionales", "Conditional indirect effects"),
                              c(nom[[".W"]], tx("Efecto", "Effect"), tx("EE boot", "Boot SE"), tx("IC 95 % boot", "Boot 95% CI")), cel2, neg2, nota = nota2)
    datos_t$indirectos <- data.frame(w = c(w_orig, NA), efecto = est, ee = ee, ic_inf = li, ic_sup = ls, row.names = c(w_lab, "indice"))
  }
  titulos <- c("1" = tx("Moderaci\u00f3n: coeficientes de regresi\u00f3n", "Moderation: regression coefficients"),
               "7" = tx("Mediaci\u00f3n moderada (modelo 7): coeficientes de regresi\u00f3n", "Moderated mediation (Model 7): regression coefficients"),
               "14" = tx("Mediaci\u00f3n moderada (modelo 14): coeficientes de regresi\u00f3n", "Moderated mediation (Model 14): regression coefficients"))
  t <- nueva_tabla(if (is.null(titulo)) titulos[[as.character(modelo)]] else titulo,
                   c(tx("Predictor", "Predictor"), "B", tx("EE", "SE"), "t", "p", tx("IC 95 %", "95% CI")), cel,
                   nota = nota, separadores = sep, datos = datos_t, extra = extra)
  t$modelos <- ml
  t$moderacion <- list(modelo = modelo, nombres = nom, w_orig = w_orig, w_lab = w_lab, wv = wv, centrados = centrados,
                       medias = medias, d = d, dic = dic)
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}

#' Gráfica de la interacción (pendientes simples)
#'
#' Dibuja la relación entre el predictor y la variable dependiente para
#' distintos valores del moderador (media y \eqn{\pm}1 DE, o los dos valores
#' de un moderador dicotómico), a partir de una tabla creada con
#' [tabla_moderacion()]. En los modelos 7 y 14 se grafica la trayectoria
#' moderada (X \eqn{\rightarrow} M o M \eqn{\rightarrow} Y). Sigue el estilo de
#' revista: escala de grises, tipos de línea distintos y letra sin serifa.
#'
#' @param t Tabla creada con [tabla_moderacion()].
#' @param archivo Si se indica, guarda la gráfica (`.pdf`, `.png`, `.tiff`,
#'   `.eps` o `.svg`).
#' @param ancho Ancho: `"columna"` (85 mm, por omisión), `"pagina"` (174 mm)
#'   o un número en pulgadas.
#' @param alto Alto en pulgadas (por omisión, 0.8 del ancho).
#' @return Invisiblemente, los valores graficados.
#' @examples
#' set.seed(1)
#' d <- data.frame(X = rnorm(200), W = rnorm(200))
#' d$Y <- .3 * d$X + .2 * d$W + .25 * d$X * d$W + rnorm(200)
#' t <- tabla_moderacion(d, x = "X", w = "W", y = "Y")
#' grafica_interaccion(t)
#' @export
grafica_interaccion <- function(t, archivo = NULL, ancho = "columna", alto = NULL) {
  if (!inherits(t, "tabla_apa") || is.null(t$moderacion)) stop("`t` debe crearse con tabla_moderacion().", call. = FALSE)
  M <- t$moderacion; nom <- M$nombres
  fit <- if (M$modelo == 7) t$modelos$m else t$modelos$y
  pred <- if (M$modelo == 14) ".M" else ".X"
  dep <- if (M$modelo == 7) ".M" else ".Y"
  xv <- M$d[[pred]]; rango <- mean(xv) + c(-1, 1) * stats::sd(xv)
  base <- as.list(colMeans(M$d[, setdiff(names(M$d), c(pred, ".W")), drop = FALSE]))
  desc_x <- if (substr(pred, 2, 2) %in% M$centrados) M$medias[[substr(pred, 2, 2)]] else 0
  lineas <- lapply(M$wv, function(wv) {
    nd <- as.data.frame(c(stats::setNames(list(rango), pred), list(.W = wv), base))
    stats::predict(fit, nd)
  })
  if (is.character(ancho)) {
    if (!ancho %in% names(ANCHOS)) stop("`ancho` debe ser \"columna\", \"pagina\" o un n\u00famero en pulgadas.", call. = FALSE)
    ancho <- ANCHOS[[ancho]]
  }
  if (is.null(alto)) alto <- ancho * 0.8
  dibuja <- function() {
    op <- graphics::par(mar = c(3.2, 3.4, 0.6, 0.6), mgp = c(2, 0.6, 0), family = "sans", cex = 1, tcl = -0.3)
    on.exit(graphics::par(op))
    yl <- range(unlist(lineas)); yl <- yl + c(-1, 1) * 0.15 * diff(yl)
    xl <- rango + desc_x + c(-1, 1) * 0.18 * diff(rango)
    graphics::plot(rango + desc_x, lineas[[1]], type = "n", xlim = xl, ylim = yl, xlab = nom[[pred]], ylab = nom[[dep]],
                   xaxt = "n", bty = "l", las = 1, cex.lab = 1, cex.axis = 0.9)
    graphics::axis(1, at = rango + desc_x, labels = txt_disp(c(tx("Bajo (\u22121 DE)", "Low (\u22121 SD)"), tx("Alto (+1 DE)", "High (+1 SD)"))), cex.axis = 0.9)
    lt <- c(2, 1, 3)[seq_along(lineas)]; cols <- c("#1A1A1A", "#1A1A1A", "#6E6E6E")[seq_along(lineas)]
    if (length(lineas) == 2) { lt <- c(2, 1); cols <- c("#1A1A1A", "#1A1A1A") }
    for (i in seq_along(lineas)) graphics::lines(rango + desc_x, lineas[[i]], lty = lt[i], lwd = 1.6, col = cols[i])
    graphics::points(rep(rango + desc_x, length(lineas)), unlist(lineas), pch = 16, cex = 0.6)
    etq <- if (M$dic) paste(nom[[".W"]], "=", M$w_lab) else paste(nom[[".W"]], M$w_lab)
    graphics::legend("topleft", legend = txt_disp(etq), lty = lt, lwd = 1.6, col = cols, bty = "n", cex = 0.85, seg.len = 2.5)
  }
  if (!is.null(archivo)) {
    ext <- tolower(tools::file_ext(archivo))
    con_dispositivo(archivo, ext, ancho, alto, dibuja, res = if (ext %in% c("tiff", "tif")) 600 else 300, pointsize = 9)
  } else dibuja()
  invisible(data.frame(w = rep(M$w_orig, each = 2), x = rep(rango + desc_x, length(lineas)), y = unlist(lineas)))
}
