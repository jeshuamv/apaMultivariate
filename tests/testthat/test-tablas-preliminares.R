library(lavaan)
HS <- HolzingerSwineford1939
v9 <- paste0("x", 1:9)
fit3 <- cfa("visual =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6
             speed =~ x7 + x8 + x9", data = HS)

test_that("descriptivos coinciden con psych (tipo 2, como SPSS)", {
  skip_if_not_installed("psych")
  t <- tabla_descriptivos(HS, variables = v9)
  ps <- psych::describe(HS[, v9], type = 2)
  expect_equal(t$datos$asimetria, ps$skew, tolerance = 1e-8)
  expect_equal(t$datos$curtosis, ps$kurtosis, tolerance = 1e-8)
  expect_equal(t$datos$M, ps$mean, tolerance = 1e-10)
  expect_equal(unname(t$correlaciones), unname(cor(HS[, v9])), tolerance = 1e-10)
  expect_equal(ncol(t$celdas), 6 + 8)
  expect_match(t$celdas[2, 7], "\\*\\*\\*$")
})

test_that("descriptivos acepta un modelo de lavaan y omite columnas no numericas", {
  expect_equal(nrow(tabla_descriptivos(fit3)$celdas), 9)
  expect_message(t <- tabla_descriptivos(HS[, c("school", "x1", "x2")]), "school")
  expect_equal(nrow(t$celdas), 2)
})

test_that("datos perdidos", {
  t <- tabla_perdidos(airquality)
  expect_equal(t$datos$perdidos[1:2], c(37, 7))
  expect_equal(unname(t$celdas[nrow(t$celdas), 2]), "111")
  expect_equal(nrow(tabla_perdidos(airquality, solo_con_perdidos = TRUE)$celdas), 3)
  expect_error(tabla_perdidos(fit3), "data.frame")
})

test_that("Mardia: formula original (denominador n), relacionada con psych (n - 1)", {
  skip_if_not_installed("psych")
  t <- tabla_normalidad(HS, variables = v9)
  pm <- psych::mardia(HS[, v9], plot = FALSE)
  n <- nrow(HS)
  expect_equal(t$datos$mardia$coeficiente[1], as.numeric(pm$b1p) * (n / (n - 1))^3, tolerance = 1e-8)
  expect_equal(t$datos$mardia$coeficiente[2], as.numeric(pm$b2p) * (n / (n - 1))^2, tolerance = 1e-8)
  expect_equal(t$datos$mardia$gl[1], 9 * 10 * 11 / 6)
})

test_that("alfa, IC de Feldt y r item-total coinciden con psych", {
  skip_if_not_installed("psych")
  t <- tabla_confiabilidad(fit3)
  for (k in 1:3) {
    it <- v9[(3 * k - 2):(3 * k)]
    a <- suppressWarnings(psych::alpha(HS[, it]))
    expect_equal(t$datos$alfa[k], a$total$raw_alpha, tolerance = 1e-8)
    expect_equal(t$datos$rit_min[k], min(a$item.stats$r.drop), tolerance = 1e-8)
    expect_equal(round(t$datos$ic_inf[k], 2), round(a$feldt$lower.ci[[1]], 2))
  }
})

test_that("omega coincide con semTools::compRelSEM(obs.var = FALSE)", {
  skip_if_not_installed("semTools")
  fit2 <- cfa("visual =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed =~ x7 + x8 + x9
               x7 ~~ x8", data = HS)
  for (f in list(fit3, fit2)) {
    om <- unlist(semTools::compRelSEM(f, obs.var = FALSE))
    expect_equal(tabla_confiabilidad(f)$datos$omega, as.numeric(om), tolerance = 1e-6)
  }
})

test_that("confiabilidad con data.frame y factores", {
  t <- tabla_confiabilidad(HS, factores = list(V = c("x1", "x2", "x3"), Dos = c("x4", "x5")))
  expect_true(is.na(t$datos$omega[2]))
  expect_error(tabla_confiabilidad(HS), "factores")
  d <- HS[, v9]; d$x3 <- -d$x3
  expect_warning(tabla_confiabilidad(d, factores = list(V = c("x1", "x2", "x3"))), "inverso")
})

test_that("guarda en .docx una o varias tablas", {
  skip_if_not_installed("zip")
  f <- tempfile(fileext = ".docx")
  guardar_tabla(tabla_descriptivos(HS, variables = v9), f)
  expect_true(file.exists(f))
  cont <- zip::zip_list(f)$filename
  expect_true(all(c("[Content_Types].xml", "word/document.xml", "word/styles.xml") %in% cont))
  f2 <- tempfile(fileext = ".docx")
  guardar_tablas(tabla_ajuste(fit3), tabla_cargas(fit3), tabla_validez(fit3), archivo = f2, numero = 2)
  ex <- tempfile(); zip::unzip(f2, exdir = ex)
  xml <- paste(readLines(file.path(ex, "word", "document.xml"), warn = FALSE, encoding = "UTF-8"), collapse = "")
  expect_match(xml, "Tabla 2"); expect_match(xml, "Tabla 4")
  expect_match(xml, "<w:i/>", fixed = TRUE)
  expect_error(guardar_tablas(tabla_ajuste(fit3), archivo = tempfile(fileext = ".rtf")), "docx")
  expect_error(guardar_tablas("hola", archivo = f2), "tabla_")
})

test_that("una tabla muy ancha pasa a pagina horizontal", {
  skip_if_not_installed("zip")
  f <- tempfile(fileext = ".docx")
  d <- as.data.frame(matrix(rnorm(300 * 14), 300)); names(d) <- paste0("variable_", 1:14)
  guardar_tabla(tabla_descriptivos(d), f)
  ex <- tempfile(); zip::unzip(f, exdir = ex)
  xml <- paste(readLines(file.path(ex, "word", "document.xml"), warn = FALSE), collapse = "")
  expect_match(xml, "landscape")
})
