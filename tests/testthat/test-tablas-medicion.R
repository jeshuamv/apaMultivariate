library(lavaan)
HS <- HolzingerSwineford1939
v9 <- paste0("x", 1:9)
mod3 <- "visual =~ x1 + x2 + x3
         textual =~ x4 + x5 + x6
         speed =~ x7 + x8 + x9"
fit3 <- cfa(mod3, data = HS)

test_that("HTMT y HTMT2 coinciden con semTools", {
  skip_if_not_installed("semTools")
  h1 <- tabla_htmt(fit3)$datos; h2 <- tabla_htmt(fit3, tipo = "htmt2")$datos
  s1 <- unclass(semTools::htmt(mod3, HS, htmt2 = FALSE)); s2 <- unclass(semTools::htmt(mod3, HS))
  expect_equal(h1[lower.tri(h1)], s1[lower.tri(s1)], tolerance = 1e-6)
  expect_equal(h2[lower.tri(h2)], s2[lower.tri(s2)], tolerance = 1e-6)
})

test_that("HTMT con data.frame y aviso de valores altos", {
  t <- tabla_htmt(HS, factores = list(A = c("x4", "x5"), B = c("x6", "x1")), umbral = 0.5)
  expect_true(any(t$negritas))
  expect_match(t$nota, "A y B|B y A")
  expect_error(tabla_htmt(HS), "factores")
  expect_error(tabla_htmt(HS, factores = list(A = c("x1", "x2"))), "dos factores")
})

test_that("comparacion de modelos: delta chi2 igual a lavTestLRT", {
  m1 <- cfa("g =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9", data = HS)
  t <- tabla_comparacion("Uno" = m1, "Tres" = fit3)
  lr <- lavTestLRT(m1, fit3)
  expect_equal(t$datos$dchi[2], lr[2, "Chisq diff"], tolerance = 1e-8)
  expect_equal(t$datos$dcfi[2], fitMeasures(fit3, "cfi")[[1]] - fitMeasures(m1, "cfi")[[1]], tolerance = 1e-8)
  expect_match(t$celdas[2, "ΔCFI" == t$columnas], "^\\+\\.")
  expect_false("p" %in% tabla_comparacion(m1, fit3, anidados = FALSE)$columnas)
  expect_error(tabla_comparacion(m1), "dos o m")
})

test_that("comparacion con estimador robusto usa la diferencia escalada", {
  m1 <- cfa("g =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9", data = HS, estimator = "MLR")
  m3 <- cfa(mod3, data = HS, estimator = "MLR")
  t <- tabla_comparacion(m1, m3)
  expect_equal(t$datos$dchi[2], lavTestLRT(m1, m3)[2, "Chisq diff"], tolerance = 1e-8)
  expect_match(t$nota, "robusto")
})

test_that("sesgo de metodo comun", {
  t <- tabla_metodo_comun(fit3)
  ev <- eigen(cor(HS[, v9]))$values
  expect_equal(t$datos$harman, 100 * ev[1] / 9, tolerance = 1e-8)
  expect_equal(nrow(t$celdas), 2)
  expect_match(t$celdas[2, 4], "no explica")
})

test_that("factorabilidad coincide con psych (KMO, MSA, Bartlett, MAP)", {
  skip_if_not_installed("psych")
  t <- tabla_factorabilidad(HS[, v9], iteraciones = 30)
  k <- psych::KMO(HS[, v9])
  expect_equal(t$datos$kmo, k$MSA, tolerance = 1e-10)
  expect_equal(unname(t$datos$msa), unname(k$MSAi), tolerance = 1e-10)
  b <- psych::cortest.bartlett(cor(HS[, v9]), n = nrow(HS))
  expect_equal(unname(t$datos$bartlett["chi"]), b$chisq, tolerance = 1e-8)
  vs <- psych::vss(HS[, v9], n = 8, plot = FALSE)
  expect_equal(unname(t$datos$factores["map"]), which.min(vs$map))
  expect_equal(unname(t$datos$factores["kaiser"]), sum(eigen(cor(HS[, v9]))$values > 1))
  expect_length(t$extra, 2)
})

test_that("factorabilidad es reproducible con semilla", {
  a <- tabla_factorabilidad(HS[, v9], iteraciones = 20, semilla = 1)$datos$aleatorios_comp
  b <- tabla_factorabilidad(HS[, v9], iteraciones = 20, semilla = 1)$datos$aleatorios_comp
  expect_equal(a, b)
})

test_that("las tablas del grupo 2 se guardan en un solo docx", {
  skip_if_not_installed("zip")
  f <- tempfile(fileext = ".docx")
  m1 <- cfa("g =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9", data = HS)
  guardar_tablas(tabla_htmt(fit3), tabla_comparacion(m1, fit3), tabla_metodo_comun(fit3),
                 tabla_factorabilidad(HS[, v9], iteraciones = 20), archivo = f)
  expect_true(file.size(f) > 2000)
})
