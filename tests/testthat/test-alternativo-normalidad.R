library(lavaan)

test_that("tabla_comparacion no compara modelos con distintas variables o casos", {
  a <- sem("Examination ~ Education\n Fertility ~ Education + Examination + Catholic", data = swiss)
  b <- sem("Fertility ~ Education + Catholic", data = swiss)
  c1 <- sem("Examination ~ Education\n Fertility ~ Education + Examination + Catholic", data = swiss[-1, ])
  t <- tabla_comparacion(A = a, B = b)
  expect_true(all(is.na(t$datos$dchi)))
  expect_equal(unname(t$celdas[2, 8]), "—")
  expect_match(t$nota, "mismas variables")
  expect_match(tabla_comparacion(A = a, C = c1)$nota, "mismos casos")
  d <- sem("Examination ~ Education\n Fertility ~ Education + Catholic", data = swiss)
  expect_false(is.na(tabla_comparacion(A = a, D = d)$datos$dchi[2]))
})

test_that("modelo_alternativo quita paso a paso y prueba el cambio", {
  f <- sem("Examination ~ Education\n Fertility ~ Education + Examination + Catholic", data = swiss)
  alt <- suppressMessages(modelo_alternativo(f, dibujar = FALSE))
  expect_s3_class(alt, "modelo_alternativo")
  expect_equal(alt$cambios$trayectoria, "Examination → Fertility")
  pe <- parameterEstimates(alt$fit); expect_false(any(pe$op == "~" & pe$lhs == "Fertility" & pe$rhs == "Examination"))
  # la prueba anidada es la del modelo con esa trayectoria fijada en 0
  f0 <- sem("Examination ~ Education\n Fertility ~ Education + 0*Examination + Catholic", data = swiss)
  expect_equal(unname(alt$prueba["dchi"]), unname(lavTestLRT(f0, f)[2, "Chisq diff"]), tolerance = 1e-8)
  expect_output(print(alt), "modelo_alt <-")
  # conserva estimador y likelihood
  g <- sem("Examination ~ Education\n Fertility ~ Education + Examination + Catholic", data = swiss,
           estimator = "MLM", likelihood = "wishart")
  ag <- suppressMessages(modelo_alternativo(g, dibujar = FALSE))
  expect_equal(lavInspect(ag$fit, "options")$likelihood, "wishart")
  expect_true("satorra.bentler" %in% lavInspect(ag$fit, "options")$test)
})

test_that("modelo_alternativo: agregar por IM, variables fuera y covarianzas automaticas", {
  set.seed(3); n <- 300
  d <- data.frame(x = rnorm(n), z = rnorm(n)); d$m <- .5 * d$x + rnorm(n); d$w <- .4 * d$x + rnorm(n)
  d$y <- .5 * d$m + .4 * d$x + rnorm(n)
  f <- sem("m ~ x\n w ~ x\n y ~ m + w", data = d)
  a <- suppressMessages(modelo_alternativo(f, agregar = 1, dibujar = FALSE))
  expect_true(any(grepl("x → y", a$cambios$trayectoria)))
  expect_true(any(a$cambios$cambio == "Se quita"))
  pt <- parTable(a$fit)
  cv <- pt[pt$op == "~~" & pt$lhs != pt$rhs & pt$lhs %in% c("w", "y") & pt$rhs %in% c("w", "y"), ]
  if (nrow(cv)) expect_equal(cv$free, 0)
  expect_error(modelo_alternativo(sem("x1 ~~ x2", data = HolzingerSwineford1939)), "ni factores")
})

test_that("grafica_normalidad marca los mismos atipicos que el procedimiento de clase", {
  X <- HolzingerSwineford1939[, paste0("x", 1:9)]
  f <- tempfile(fileext = ".png")
  r <- suppressMessages(grafica_normalidad(X, archivo = f))
  expect_true(file.exists(f))
  D2 <- mahalanobis(X, colMeans(X), var(X))
  expect_setequal(r$fila[r$atipico], which(D2 > qchisq(.999, 9)))
  expect_equal(r$cuantil, qchisq(ppoints(nrow(X)), 9))
})

test_that("parrafo_metodo menciona wishart y los atipicos quitados", {
  d2 <- suppressMessages(quitar_atipicos(HolzingerSwineford1939, paste0("x", 1:9)))
  f <- cfa("v =~ x1 + x2 + x3", data = d2, likelihood = "wishart")
  p <- parrafo_metodo(f, atipicos = d2)
  expect_match(p$parrafo, "Wishart"); expect_match(p$parrafo, "Se excluyeron 2 casos atípicos")
  expect_true(any(grepl("^Kline", p$referencias)))
})

test_that("modelos_rivales genera y compara los rivales de un AFC", {
  HS <- HolzingerSwineford1939
  f <- cfa("visual =~ x1 + x2 + x3\n textual =~ x4 + x5 + x6\n speed =~ x7 + x8 + x9", data = HS)
  r <- modelos_rivales(f, dibujar = FALSE)
  expect_s3_class(r, "modelos_rivales")
  expect_setequal(names(r$fits), c("original", "un_factor", "independientes", "segundo_orden", "bifactor"))
  uno <- cfa("G =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9", data = HS)
  expect_equal(fitMeasures(r$fits$un_factor, "chisq"), fitMeasures(uno, "chisq"), tolerance = 1e-6)
  ort <- cfa("visual =~ x1 + x2 + x3\n textual =~ x4 + x5 + x6\n speed =~ x7 + x8 + x9", data = HS, orthogonal = TRUE)
  expect_equal(fitMeasures(r$fits$independientes, "chisq"), fitMeasures(ort, "chisq"), tolerance = 1e-6)
  # con 3 factores el de segundo orden es equivalente
  expect_equal(fitMeasures(r$fits$segundo_orden, "chisq"), fitMeasures(f, "chisq"), tolerance = 1e-4)
  expect_match(r$comparacion$nota, "equivalente")
  # con dos factores no se estiman segundo orden ni bifactor
  f2 <- cfa("visual =~ x1 + x2 + x3\n textual =~ x4 + x5 + x6", data = HS)
  r2 <- modelos_rivales(f2, dibujar = FALSE)
  expect_false(any(c("segundo_orden", "bifactor") %in% names(r2$fits)))
  expect_error(modelos_rivales(sem("x1 ~ x2", data = HS)), "trayectorias")
})

test_that("modelo_alternativo en un AFC quita cargas bajas sin romper factores", {
  HS <- HolzingerSwineford1939
  f <- cfa("v =~ x1 + x2 + x3\n t =~ x4 + x5 + x6 + x1\n s =~ x7 + x8 + x9 + x3", data = HS)
  a <- suppressMessages(modelo_alternativo(f, dibujar = FALSE))
  expect_true(all(grepl("carga cruzada", a$cambios$cambio)))
  fac <- factores_de(a$fit)
  expect_true(all(lengths(fac) >= 3))
  b <- suppressMessages(modelo_alternativo(cfa("v =~ x1 + x2 + x3\n t =~ x4 + x5 + x6\n s =~ x7 + x8 + x9", data = HS),
                                           agregar = 1, dibujar = FALSE))
  expect_equal(nrow(b$cambios), 1); expect_match(b$cambios$cambio, "Se agrega")
})

test_that("comparar_soluciones compara AFE con distinto numero de factores", {
  skip_if_not_installed("psych"); skip_if_not_installed("GPArotation")
  X <- HolzingerSwineford1939[, paste0("x", 1:9)]
  r <- comparar_soluciones(X, 2:4, dibujar = FALSE)
  expect_s3_class(r, "comparacion_afe")
  expect_equal(r$tabla$datos$k, 2:4)
  a3 <- psych::fa(X, 3, rotate = "oblimin", fm = "ml")
  expect_equal(r$tabla$datos$rmsea[2], unname(a3$RMSEA[1]), tolerance = 1e-8)
  expect_equal(r$tabla$datos$sin[1], 1)          # con 2 factores, x2 no carga
})
