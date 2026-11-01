test_that("tabla_regresion reproduce lm, HC3 y los datos de Hald", {
  skip_if_not_installed("MASS")
  fit <- lm(y ~ x1 + x2 + x3 + x4, data = MASS::cement)
  t <- tabla_regresion(fit)
  expect_s3_class(t, "tabla_apa")
  D <- t$datos
  expect_equal(D$B, unname(coef(fit)))
  expect_equal(D$p, unname(summary(fit)$coefficients[, 4]))
  expect_equal(D$li, unname(confint(fit)[, 1]))
  expect_equal(round(D$vif[-1], 2), c(38.50, 254.42, 46.87, 282.51))
  expect_equal(unname(attr(D, "modelo")["r2"]), summary(fit)$r.squared)
  # beta = coeficiente con variables estandarizadas
  z <- as.data.frame(scale(MASS::cement))
  expect_equal(D$beta[-1], unname(coef(lm(y ~ x1 + x2 + x3 + x4, data = z))[-1]))
  # HC3 a mano: (X'X)^-1 X' diag(e^2/(1-h)^2) X (X'X)^-1
  X <- model.matrix(fit); e <- resid(fit); h <- hatvalues(fit); B <- solve(crossprod(X))
  hc3 <- sqrt(diag(B %*% t(X) %*% diag(e^2 / (1 - h)^2) %*% X %*% B))
  expect_equal(tabla_regresion(fit, robusto = TRUE)$datos$EE, unname(hc3))
  tb <- tabla_regresion(fit, bootstrap = 200)
  expect_true(all(tb$datos$li < tb$datos$ls))
  expect_match(tb$nota, "bootstrap")
  expect_match(t$nota, "VIF mayores que 10")
  expect_error(tabla_regresion(glm(am ~ wt, binomial, mtcars)), "lm\\(\\)")
})

test_that("tabla_supuestos_regresion coincide con Breusch-Pagan y RESET", {
  skip_if_not_installed("MASS")
  fit <- lm(y ~ x1 + x2 + x3 + x4, data = MASS::cement)
  s <- tabla_supuestos_regresion(fit)$datos
  expect_equal(s$estadistico[1], unname(shapiro.test(resid(fit))$statistic))
  expect_equal(round(s$estadistico[2], 4), 1.4636)     # lmtest::bptest(fit)
  expect_equal(round(s$estadistico[3], 5), 0.04317)    # lmtest::resettest(fit)
  expect_equal(round(s$estadistico[4], 4), 2.0526)     # Durbin-Watson
})

test_that("tabla_influencia marca casos y agrega la sensibilidad", {
  skip_if_not_installed("MASS")
  fit <- lm(y ~ x1 + x2 + x3 + x4, data = MASS::cement)
  t <- tabla_influencia(fit)
  expect_equal(t$datos$cook, unname(cooks.distance(fit)))
  expect_equal(t$celdas[1, 1], "8")
  expect_length(t$extra, 1)
  expect_match(t$extra[[1]]$titulo, "caso 8")
})

test_that("tabla_subconjuntos ordena los 15 modelos de Hald", {
  skip_if_not_installed("MASS")
  fit <- lm(y ~ x1 + x2 + x3 + x4, data = MASS::cement)
  t <- tabla_subconjuntos(fit)
  expect_equal(nrow(t$datos), 15)
  expect_equal(t$datos$predictores[1], "x1 + x2")
  completo <- t$datos[t$datos$k == 4, ]
  expect_equal(completo$Cp, 5)                         # Cp = p en el modelo completo
  expect_error(tabla_subconjuntos(lm(y ~ x1 * x2, MASS::cement)), "interacciones")
})

test_that("comparar_soluciones compara rotaciones y metodos", {
  skip_if_not_installed("psych"); skip_if_not_installed("GPArotation")
  X <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  r <- comparar_soluciones(X, 3, rotate = c("none", "varimax", "promax"), fm = "pa", dibujar = FALSE)
  expect_equal(names(r$soluciones), c("none", "varimax", "promax"))
  # la rotacion no cambia la varianza comun ni el ajuste
  expect_equal(length(unique(round(r$tabla$datos$var, 8))), 1)
  expect_equal(length(unique(round(r$tabla$datos$rmsea, 8))), 1)
  expect_length(r$tabla$extra, 1)
  r2 <- comparar_soluciones(X, 3, fm = c("pa", "ml"), rotate = "promax", dibujar = FALSE)
  expect_equal(names(r2$soluciones), c("pa", "ml"))
})

test_that("grafica_sedimentacion coincide con tabla_factorabilidad", {
  skip_if_not_installed("psych")
  X <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  f <- tempfile(fileext = ".png")
  g <- suppressMessages(grafica_sedimentacion(X, tipo = "ambos", iteraciones = 20, archivo = f))
  expect_true(file.exists(f))
  tf <- tabla_factorabilidad(X, iteraciones = 20)
  expect_equal(g$factores, tf$datos$autovalores_reducidos)
  expect_equal(attr(g, "sugeridos"), tf$datos$factores)
})

test_that("se detecta la columna de numeros de fila de write.csv", {
  X <- lavaan::HolzingerSwineford1939[1:50, paste0("x", 1:4)]
  f <- tempfile(fileext = ".csv"); write.csv(X, f, row.names = TRUE)
  d <- suppressMessages(leer_datos(f))
  expect_equal(names(d), paste0("x", 1:4))
  d2 <- read.csv(f)
  r <- capture.output(rv <- revisar_datos(d2))
  expect_true(any(rv$estado == "problema" & grepl("números de fila", rv$texto)))
  r <- capture.output(rv <- revisar_datos(cbind(id = 1:50, X)))
  expect_true(any(grepl("identificador", rv$texto)))
  r <- capture.output(rv <- revisar_datos(X))
  expect_false(any(grepl("identificador|números de fila", rv$texto)))
})

test_that("correlaciones policoricas en el AFE", {
  skip_if_not_installed("psych"); skip_if_not_installed("GPArotation")
  b <- psych::bfi[1:600, 1:10]; b <- b[complete.cases(b), ]
  t <- tabla_factorabilidad(b, iteraciones = 10, cor = "policorica")
  expect_equal(unname(t$datos$kmo), unname(psych::KMO(psych::polychoric(b)$rho)$MSA))
  expect_match(t$nota, "polic")
  r <- comparar_soluciones(b, 2, cor = "policorica", dibujar = FALSE)
  a <- psych::fa(b, 2, cor = "poly", fm = "ml", rotate = "oblimin")
  expect_equal(r$tabla$datos$var, sum(a$communality) / 10, tolerance = 1e-4)
  expect_error(tabla_factorabilidad(lavaan::HolzingerSwineford1939[, 7:15], cor = "policorica"), "ordinales")
  expect_match(tabla_factorabilidad(b[, 1:5] * 0 + (b[, 1:5] > 3), iteraciones = 5)$nota, "polic")
})

test_that("tabla_hipotesis no dice 'se rechaza'", {
  fit <- lavaan::sem("Fertility ~ Education + Agriculture", data = swiss)
  h <- tabla_hipotesis(fit, c(H1 = "Education -> Fertility (-)", H2 = "Agriculture -> Fertility (-)"))
  expect_false(any(grepl("rechaza", h$celdas)))
  expect_true(any(grepl("No se sostiene", h$celdas)))
})
