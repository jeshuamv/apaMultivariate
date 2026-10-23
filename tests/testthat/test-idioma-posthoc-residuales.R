library(lavaan)
HS <- HolzingerSwineford1939
mod <- "visual =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
        speed =~ x7 + x8 + x9"
fit3 <- cfa(mod, data = HS)

test_that("idioma_sem cambia y restaura el idioma", {
  viejo <- idioma_sem("en")
  on.exit(idioma_sem(viejo))
  expect_equal(idioma_sem(), "en")
  expect_error(idioma_sem("fr"), "es")
  idioma_sem("es"); expect_equal(idioma_sem(), "es")
})

test_that("las tablas salen en ingles", {
  viejo <- idioma_sem("en"); on.exit(idioma_sem(viejo))
  t <- tabla_ajuste(fit3)
  expect_equal(t$titulo, "Fit indices")
  expect_true(all(c("Model", "df", "RMSEA [90% CI]") %in% t$columnas))
  expect_match(t$nota, "Hu & Bentler")
  expect_equal(tabla_cargas(fit3)$columnas[4], "SE")
  expect_match(tabla_validez(fit3)$nota, "composite reliability")
  expect_equal(tabla_descriptivos(HS[, 7:10])$columnas[4], "SD")
  expect_match(tabla_criterios_ajuste(fit3)$celdas[5, 5], "Excellent|Acceptable|Not acceptable")
  ti <- tabla_invarianza(mod, HS, "school", niveles = c("configural", "metrica", "escalar"))
  expect_equal(unname(ti$celdas[, 1]), c("Configural", "Metric", "Scalar"))
  expect_true(all(ti$celdas[2:3, 12] %in% c("Supported", "Not supported")))
  set.seed(1); d <- data.frame(X = rnorm(100)); d$M <- .5 * d$X + rnorm(100); d$Y <- .4 * d$M + rnorm(100)
  te <- tabla_efectos(sem("M ~ X\n Y ~ M + X", data = d), bootstrap = 0)
  expect_true(all(c("Direct effects", "Indirect effects", "Total effects") %in% te$celdas[, 1]))
  expect_equal(te$columnas[4], "95% CI")
  tm <- tabla_manova(iris, c("Petal.Length", "Sepal.Length"), "Species")
  expect_equal(tm$celdas[1, 1], "Pillai's trace")
  p <- parrafo_metodo(fit3, paquetes = character(0))
  expect_match(p$parrafo, "^Analyses were conducted in R")
  expect_match(p$parrafo, "confirmatory factor analysis")
  expect_output(print(p), "References")
})

test_that("Word y RTF en ingles: Table y Note", {
  skip_if_not_installed("zip")
  viejo <- idioma_sem("en"); on.exit(idioma_sem(viejo))
  f <- tempfile(fileext = ".docx"); guardar_tabla(tabla_ajuste(fit3), f)
  ex <- tempfile(); zip::unzip(f, exdir = ex)
  xml <- paste(readLines(file.path(ex, "word", "document.xml"), warn = FALSE, encoding = "UTF-8"), collapse = "")
  expect_match(xml, ">Table 1<"); expect_match(xml, ">Note.<"); expect_match(paste(readLines(file.path(ex, "word", "styles.xml"), warn = FALSE), collapse = ""), "en-US")
  r <- tempfile(fileext = ".rtf"); guardar_tabla(tabla_ajuste(fit3), r)
  expect_match(paste(readLines(r), collapse = ""), "Table 1")
})

test_that("en espanol no cambia nada", {
  viejo <- idioma_sem("es"); on.exit(idioma_sem(viejo))
  expect_equal(tabla_ajuste(fit3)$titulo, "Índices de ajuste")
  expect_equal(tabla_cargas(fit3)$columnas[4], "EE")
})

test_that("post hoc de MANOVA coincide con TukeyHSD y Games-Howell", {
  cp <- comparaciones_pares(iris$Sepal.Length, iris$Species, "tukey")
  tk <- TukeyHSD(aov(Sepal.Length ~ Species, iris))$Species
  expect_equal(cp$diferencia, unname(tk[, "diff"]), tolerance = 1e-10)
  expect_equal(cp$ic_inf, unname(tk[, "lwr"]), tolerance = 1e-8)
  expect_equal(cp$p, unname(tk[, "p adj"]), tolerance = 1e-6)
  skip_if_not_installed("rstatix")
  gh <- comparaciones_pares(iris$Sepal.Length, iris$Species, "games-howell")
  rg <- as.data.frame(rstatix::games_howell_test(iris, Sepal.Length ~ Species))
  expect_equal(gh$ic_inf, rg$conf.low, tolerance = 1e-6)
  expect_equal(abs(gh$d), abs(as.data.frame(rstatix::cohens_d(iris, Sepal.Length ~ Species))$effsize), tolerance = 1e-8)
})

test_that("MANOVA elige el post hoc y lo omite con dos grupos", {
  t <- tabla_manova(iris, c("Petal.Length", "Sepal.Length"), "Species")
  expect_length(t$extra, 4)
  expect_match(t$extra[[3]]$nota, "Games-Howell")
  expect_equal(nrow(t$datos$posthoc), 6)
  tt <- tabla_manova(iris, c("Petal.Length", "Sepal.Length"), "Species", posthoc = "tukey")
  expect_match(tt$extra[[3]]$nota, "Tukey")
  expect_length(tabla_manova(iris, c("Petal.Length", "Sepal.Length"), "Species", posthoc = "ninguno")$extra, 3)
  expect_length(tabla_manova(subset(iris, Species != "setosa"), c("Petal.Length", "Sepal.Length"), "Species")$extra, 3)
})

test_that("residuales de correlacion", {
  t <- tabla_residuales(fit3)
  chk <- cor(HS[, paste0("x", 1:9)]) - cov2cor(fitted(fit3)$cov)
  expect_equal(unname(t$datos[rownames(chk), colnames(chk)]), unname(unclass(chk)), tolerance = 1e-10)
  expect_equal(sum(t$negritas), sum(abs(chk[lower.tri(chk)]) > .10))
  expect_match(t$nota, "x2 y x7")
})
