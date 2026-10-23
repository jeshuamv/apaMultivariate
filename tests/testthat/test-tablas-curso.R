library(lavaan)
HS <- HolzingerSwineford1939
v9 <- paste0("x", 1:9)
fit3 <- cfa("visual =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6
             speed =~ x7 + x8 + x9", data = HS)
pd <- sem("ind60 =~ x1 + x2 + x3
           dem60 =~ y1 + y2 + y3 + y4
           dem65 =~ y5 + y6 + y7 + y8
           dem60 ~ ind60
           dem65 ~ ind60 + dem60", data = PoliticalDemocracy)

test_that("criterios de ajuste e identificacion", {
  t <- tabla_criterios_ajuste(fit3)
  fm <- fitMeasures(fit3)
  expect_equal(unname(t$datos$identificacion["q"]), fm[["npar"]])
  expect_equal(unname(t$datos$identificacion["p"]), 9 * 10 / 2)
  expect_equal(unname(t$datos$indices["pnfi"]), fm[["pnfi"]], tolerance = 1e-8)
  expect_equal(unname(t$datos$indices["pratio"]), fm[["df"]] / fm[["baseline.df"]], tolerance = 1e-8)
  expect_equal(nrow(t$celdas), 9)
  expect_equal(unname(t$celdas[t$celdas[, 1] == "CFI", 5]), evalua(fm[["cfi"]], .95, .90))
  expect_match(t$extra[[1]]$celdas[3, 3], "Sobreidentificado")
  expect_equal(evalua(.07, .05, .06, FALSE), "No aceptable")
  expect_equal(evalua(.055, .05, .06, FALSE), "Aceptable")
  expect_equal(evalua(.96, .95, .90), "Excelente")
  ji <- sem("y ~ x1 + x2", data = data.frame(y = rnorm(50), x1 = rnorm(50), x2 = rnorm(50)))
  expect_match(tabla_criterios_ajuste(ji)$extra[[1]]$celdas[3, 3], "Exactamente")
  expect_equal(nrow(tabla_criterios_ajuste(fit3, indices = c("chi2", "rmsea"))$celdas), 2)
})

test_that("estimaciones al estilo AMOS", {
  t <- tabla_parametros(pd)
  pe <- parameterEstimates(pd, standardized = TRUE)
  expect_equal(nrow(t$datos), sum(pe$op %in% c("~", "=~", "~~")))
  primeras <- trimws(t$celdas[, 1])
  expect_true(all(c("Pesos de regresión", "Cargas factoriales", "Varianzas de los errores (residuales)") %in% primeras))
  fila <- which(primeras == "dem60 ← ind60")
  expect_equal(unname(t$celdas[fila, 4]), formatC(pe$z[pe$op == "~" & pe$lhs == "dem60"], format = "f", digits = 2))
  expect_equal(unname(t$celdas[which(primeras == "x1 ← ind60"), 3]), "—")
  expect_false(any(grepl("Varianzas", tabla_parametros(pd, varianzas = FALSE)$celdas[, 1])))
})

test_that("hipotesis directas, indirectas y con signo", {
  t <- tabla_hipotesis(pd, c(H1 = "ind60 -> dem60 (+)", H2 = "ind60 -> dem65 (-)",
                             H3 = "ind60 -> dem60 -> dem65"), bootstrap = 0)
  expect_identical(unname(t$datos$sostiene), c(TRUE, FALSE, TRUE))
  expect_equal(t$datos$signo, c("+", "-", ""))
  expect_equal(t$datos$tipo, c("Directo", "Directo", "Indirecto"))
  expect_error(tabla_hipotesis(pd, c(H1 = "dem65 -> ind60")), "no tiene el efecto")
  expect_error(tabla_hipotesis(pd, c(H1 = "ind60")), "mal escrita")
  t2 <- tabla_hipotesis(pd, c("ind60 → dem60"), bootstrap = 0)
  expect_equal(t2$datos$hipotesis, "H1")
})

test_that("indices de modificacion", {
  t <- tabla_modificacion(fit3, n = 4)
  mi <- modindices(fit3, sort. = TRUE)
  expect_equal(t$datos$mi[1], mi$mi[1], tolerance = 1e-8)
  expect_lte(nrow(t$celdas), 4)
  expect_true(all(t$datos$mi >= 3.84))
  t2 <- tabla_modificacion(fit3, minimo = 1e6)
  expect_match(t2$celdas[1, 1], "Ning")
})

test_that("normalidad al estilo AMOS", {
  t <- tabla_normalidad(HS, variables = v9)
  x <- HS$x1 - mean(HS$x1); n <- nrow(HS)
  g1 <- mean(x^3) / mean(x^2)^1.5; g2 <- mean(x^4) / mean(x^2)^2 - 3
  expect_equal(t$datos$univariadas$asimetria[1], g1, tolerance = 1e-10)
  expect_equal(t$datos$univariadas$cr_asimetria[1], g1 / sqrt(6 / n), tolerance = 1e-10)
  expect_equal(t$datos$univariadas$cr_curtosis[1], g2 / sqrt(24 / n), tolerance = 1e-10)
  expect_equal(t$datos$mardia$curtosis_exceso[2], t$datos$mardia$coeficiente[2] - 9 * 11, tolerance = 1e-10)
  expect_true(any(t$negritas[, 5]))
  expect_length(t$extra, 1)
  expect_equal(nrow(tabla_normalidad(fit3)$datos$univariadas), 9)
})

test_that("atipicos multivariados: D2, p1 y filas originales", {
  t <- tabla_atipicos(HS, variables = v9)
  D2 <- mahalanobis(HS[, v9], colMeans(HS[, v9]), cov(HS[, v9]))
  expect_equal(t$datos$D2, unname(D2), tolerance = 1e-10)
  expect_equal(t$atipicos, sort(which(pchisq(D2, 9, lower.tail = FALSE) <= .001)))
  f <- cfa("visual =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed =~ x7 + x8 + x9", data = HS[-(1:3), ])
  ta <- tabla_atipicos(f)
  expect_equal(max(ta$datos$fila), nrow(HS) - 3)
  expect_true(all(ta$datos$p2 >= 0 & ta$datos$p2 <= 1))
  expect_gte(nrow(tabla_atipicos(HS, variables = v9, n = 1)$celdas), length(t$atipicos))
})

test_that("VIF coincide con car", {
  skip_if_not_installed("car")
  set.seed(1); d <- data.frame(X1 = rnorm(200)); d$X2 <- .8 * d$X1 + rnorm(200, 0, .4); d$X3 <- rnorm(200)
  d$Y <- .3 * d$X1 + .3 * d$X2 + rnorm(200)
  t <- tabla_vif(sem("Y ~ X1 + X2 + X3", data = d))
  expect_equal(unname(t$datos$vif), unname(car::vif(lm(Y ~ X1 + X2 + X3, d))), tolerance = 1e-8)
  expect_error(tabla_vif(sem("Y ~ X1", data = d)), "dos o m")
  expect_equal(nrow(tabla_vif(pd)$datos), 2)
})

test_that("MANOVA coincide con el ejemplo de clase (iris)", {
  t <- tabla_manova(iris, c("Petal.Length", "Sepal.Length"), "Species")
  m <- manova(cbind(Petal.Length, Sepal.Length) ~ Species, iris)
  expect_equal(t$datos$multivariadas["Wilks", "valor"], summary(m, test = "Wilks")$stats[1, 2], tolerance = 1e-10)
  expect_equal(round(t$datos$multivariadas["Wilks", "eta_p"], 4), 0.8003)
  expect_equal(round(t$datos$multivariadas["Pillai", "eta_p"], 4), 0.4943)
  skip_if_not_installed("rstatix")
  bm <- rstatix::box_m(iris[, c("Petal.Length", "Sepal.Length")], iris$Species)
  expect_equal(unname(t$datos$box_m["chi"]), unname(bm$statistic), tolerance = 1e-6)
  expect_length(t$extra, 3)
  expect_equal(nrow(t$datos$univariadas), 2)
})

test_that("MANOVA con dos grupos informa T2 y valida entradas", {
  t <- tabla_manova(subset(iris, Species != "setosa"), c("Petal.Length", "Sepal.Length"), "Species")
  expect_match(t$nota, "T²")
  expect_error(tabla_manova(iris, "Petal.Length", "Species"), "dos o m")
  expect_error(tabla_manova(iris, c("a", "b"), "Species"), "No est")
})

test_that("diagrama no estandarizado muestra varianzas como AMOS", {
  set.seed(3); d <- data.frame(X = rnorm(100), W = rnorm(100, 0, 2)); d$M <- .5 * d$X + rnorm(100)
  d$Y <- .4 * d$M + .3 * d$W + rnorm(100)
  f <- sem("M ~ X
            Y ~ M + W", data = d)
  dd <- diagrama_sem(f, estandarizado = FALSE, dibujar = FALSE)
  pe <- parameterEstimates(f)
  expect_equal(unname(dd$G$r2["X"]), pe$est[pe$op == "~~" & pe$lhs == "X" & pe$rhs == "X"], tolerance = 1e-8)
  err_M <- dd$G$de_err[["M"]]
  expect_equal(unname(dd$G$r2[err_M]), pe$est[pe$op == "~~" & pe$lhs == "M" & pe$rhs == "M"], tolerance = 1e-8)
  expect_false("M" %in% names(dd$G$r2))
  expect_silent(guardar_diagrama(dd, tempfile(fileext = ".png")))
})

test_that("todas las tablas nuevas se guardan en Word", {
  skip_if_not_installed("zip")
  f <- tempfile(fileext = ".docx")
  guardar_tablas(tabla_criterios_ajuste(pd), tabla_parametros(pd), tabla_normalidad(fit3), tabla_atipicos(fit3),
                 tabla_vif(pd), tabla_modificacion(fit3), tabla_manova(iris, c("Petal.Length", "Sepal.Length"), "Species"),
                 archivo = f)
  expect_true(file.size(f) > 3000)
})
