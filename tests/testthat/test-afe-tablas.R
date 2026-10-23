library(lavaan)
hs <- HolzingerSwineford1939[, paste0("x", 1:9)]
fit3 <- cfa("visual =~ x1 + x2 + x3\n textual =~ x4 + x5 + x6\n speed =~ x7 + x8 + x9",
            data = HolzingerSwineford1939)

test_that("AFE con lavaan::efa", {
  fe <- efa(data = hs, nfactors = 2:3)
  expect_error(diagrama_sem(fe, dibujar = FALSE), "nfactores")
  d <- diagrama_sem(fe, nfactores = 3, dibujar = FALSE)
  expect_equal(d$tipo, "afe")
  expect_equal(length(d$G$lv), 3)
  expect_false(anyNA(d$G$nodes$x))
  expect_true(all(abs(d$G$edges$val[d$G$edges$tipo == "cruzada"]) >= 0.30))
})

test_that("AFE con psych::fa", {
  skip_if_not_installed("psych")
  f <- psych::fa(hs, nfactors = 3, rotate = "oblimin", fm = "ml")
  d <- diagrama_sem(f, umbral = 0.40, dibujar = FALSE)
  expect_equal(d$tipo, "afe")
  expect_equal(sum(d$G$edges$tipo == "cov"), 3)          # oblicua: correlaciones
  fv <- psych::fa(hs, nfactors = 3, rotate = "varimax", fm = "ml")
  expect_equal(sum(diagrama_sem(fv, dibujar = FALSE)$G$edges$tipo == "cov"), 0)
  t <- tabla_cargas(f)
  expect_s3_class(t, "tabla_apa")
  expect_equal(nrow(t$datos), 9)
  expect_length(t$extra, 1)                                # correlaciones entre factores
  expect_true(any(t$negritas))
})

test_that("tablas de AFC: cargas, ajuste y validez", {
  t1 <- tabla_cargas(fit3)
  expect_equal(nrow(t1$datos), 9)
  t2 <- tabla_ajuste("Tres factores" = fit3)
  expect_equal(t2$datos$modelo, "Tres factores")
  expect_equal(unname(t2$celdas[1, 3]), "24")
  t3 <- tabla_validez(fit3)
  expect_equal(nrow(t3$datos), 3)
  expect_true(all(t3$datos$CR > 0 & t3$datos$CR < 1))
  expect_equal(sum(t3$negritas), 3)
  l <- t3$datos$raiz_AVE[1]; expect_equal(l, sqrt(t3$datos$AVE[1]))
})

test_that("ajuste con estimador robusto usa los indices corregidos", {
  fr <- cfa("visual =~ x1 + x2 + x3\n textual =~ x4 + x5 + x6", data = HolzingerSwineford1939, estimator = "MLR")
  t <- tabla_ajuste(fr)
  expect_match(t$nota, "robusto")
})

test_that("formato APA de los numeros", {
  expect_equal(f_apa(c(0.456, -0.004, 0.5)), c(".46", ".00", ".50"))
  expect_equal(f_p(c(0.0001, 0.0234)), c("< .001", ".023"))
})

test_that("guardar tablas en rtf, html y csv", {
  t <- tabla_validez(fit3)
  for (ext in c("rtf", "html", "csv")) {
    f <- tempfile(fileext = paste0(".", ext)); guardar_tabla(t, f)
    expect_true(file.exists(f) && file.size(f) > 100)
  }
  f <- tempfile(fileext = ".rtf"); guardar_tabla(t, f); rtf <- paste(readLines(f), collapse = "")
  expect_match(rtf, "^\\{\\\\rtf1"); expect_match(rtf, "\\\\u237\\?")   # í codificada
  expect_error(guardar_tabla(t, tempfile(fileext = ".docx")), "rtf")
})
