library(lavaan)
HS <- HolzingerSwineford1939
mod <- "visual =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
        speed =~ x7 + x8 + x9"

test_that("invarianza coincide con los modelos de lavaan y lavTestLRT", {
  t <- tabla_invarianza(mod, HS, "school")
  c0 <- cfa(mod, HS, group = "school")
  c1 <- cfa(mod, HS, group = "school", group.equal = "loadings")
  c2 <- cfa(mod, HS, group = "school", group.equal = c("loadings", "intercepts"))
  expect_equal(t$datos$chisq[1:3], unname(sapply(list(c0, c1, c2), fitMeasures, "chisq")), tolerance = 1e-8)
  lr <- lavTestLRT(c0, c1, c2)
  expect_equal(t$datos$dchi[2:3], lr[2:3, "Chisq diff"], tolerance = 1e-8)
  expect_equal(t$datos$dcfi[3], fitMeasures(c2, "cfi")[[1]] - fitMeasures(c1, "cfi")[[1]], tolerance = 1e-8)
  expect_identical(t$datos$sostiene[2:3], c(TRUE, FALSE))
  expect_equal(unname(t$celdas[4, 12]), "No se evalúa")
  expect_true(length(t$sugerencias) >= 1)
  expect_match(t$nota, "uno en uno")
  expect_length(t$modelos, 4)
})

test_that("criterios cfi y chi2", {
  t1 <- tabla_invarianza(mod, HS, "school", niveles = c("configural", "metrica", "escalar"), criterio = "cfi")
  expect_identical(t1$datos$sostiene[2:3], c(TRUE, FALSE))
  t2 <- tabla_invarianza(mod, HS, "school", niveles = c("configural", "metrica"), criterio = "chi2")
  expect_true(t2$datos$sostiene[2])
  expect_match(t2$nota, "p < .05")
})

test_that("invarianza parcial", {
  t <- tabla_invarianza(mod, HS, "school", liberar = "x3 ~ 1")
  expect_equal(unname(t$celdas[2:3, 1]), c("Métrica", "Escalar parcial"))
  ref <- cfa(mod, HS, group = "school", group.equal = c("loadings", "intercepts"), group.partial = "x3 ~ 1")
  expect_equal(t$datos$chisq[3], fitMeasures(ref, "chisq")[[1]], tolerance = 1e-8)
  expect_match(t$nota, "x3 ~ 1")
})

test_that("acepta un modelo ya estimado y conserva su estimador", {
  fm <- cfa(mod, data = HS, estimator = "MLR")
  t <- tabla_invarianza(fm, HS, "school", niveles = c("configural", "metrica"))
  expect_equal(lavInspect(t$modelos[[1]], "options")$estimator, "ML")
  expect_true("yuan.bentler.mplus" %in% lavInspect(t$modelos[[1]], "options")$test ||
              "yuan.bentler" %in% lavInspect(t$modelos[[1]], "options")$test)
  expect_match(t$nota, "robusto")
})

test_that("errores claros", {
  expect_error(tabla_invarianza(mod, HS, "no_existe"), "columna")
  expect_error(tabla_invarianza(mod, HS, "school", niveles = "configural"), "dos niveles")
})
