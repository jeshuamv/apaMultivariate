library(lavaan)
hs <- HolzingerSwineford1939[, c("id", paste0("x", 1:9))]

test_that("leer_datos lee CSV con coma y con punto y coma (decimales con coma, latin1)", {
  f1 <- tempfile(fileext = ".csv"); utils::write.csv(hs, f1, row.names = FALSE)
  d1 <- suppressMessages(leer_datos(f1))
  expect_equal(dim(d1), dim(hs)); expect_equal(d1$x1, hs$x1)
  f2 <- tempfile(fileext = ".csv")
  con <- file(f2, encoding = "latin1")
  utils::write.table(data.frame(`Ítem 1` = c(1.5, 2.25), grupo = c("Sí", "No"), check.names = FALSE),
                     con, sep = ";", dec = ",", row.names = FALSE)
  d2 <- suppressMessages(leer_datos(f2))
  expect_equal(names(d2), c("Item_1", "grupo"))
  expect_equal(d2$Item_1, c(1.5, 2.25))
  expect_equal(d2$grupo, c("Sí", "No"))
  expect_message(leer_datos(f2), "nombre")
})

test_that("leer_datos lee Excel y SPSS sin etiquetas", {
  skip_if_not_installed("haven")
  f <- tempfile(fileext = ".sav")
  x <- hs[1:20, 1:4]; x$sexo <- haven::labelled(rep(1:2, 10), c(Hombre = 1, Mujer = 2))
  haven::write_sav(x, f)
  d <- suppressMessages(leer_datos(f))
  expect_true(is.numeric(d$sexo)); expect_null(attr(d$sexo, "labels")); expect_false(inherits(d$sexo, "haven_labelled"))
  expect_s3_class(d, "data.frame"); expect_false(inherits(d, "tbl_df"))
  skip_if_not_installed("writexl")
  fx <- tempfile(fileext = ".xlsx"); writexl::write_xlsx(hs[1:30, ], fx)
  expect_equal(suppressMessages(leer_datos(fx))$x5, hs$x5[1:30])
})

test_that("leer_datos: codigos de perdido y errores claros", {
  f <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = c(1, 99, 3), b = c(-9, 2, 2)), f, row.names = FALSE)
  d <- suppressMessages(leer_datos(f, perdidos = c(99, -9)))
  expect_equal(sum(is.na(d)), 2)
  expect_error(leer_datos("no_existe.xlsx"), "No encuentro")
  g <- tempfile(fileext = ".pdf"); file.create(g)
  expect_error(leer_datos(g), "extensi")
})

test_that("nombres_limpios", {
  expect_equal(nombres_limpios(c("Ítem 1", "3er item", "a-b", "ok")), c("Item_1", "x3er_item", "a_b", "ok"))
  expect_equal(anyDuplicated(nombres_limpios(c("a b", "a_b"))), 0)
})

test_that("revisar_datos: variables faltantes, sugerencias y sintaxis", {
  r <- capture.output(v <- revisar_datos(hs, "F =~ x1 + x2 + X3"))
  expect_equal(sum(v$estado == "problema"), 1)
  expect_match(v$texto[1], "quisiste decir 'x3'")
  capture.output(s <- revisar_datos(hs, "F =~ x1 + x2 +"))
  expect_match(s$texto[1], "sintaxis")
  capture.output(ok <- revisar_datos(hs, "visual =~ x1 + x2 + x3"))
  expect_false(any(ok$estado == "problema"))
  expect_true(any(grepl("hay que corregir", capture.output(print(v)))))
})

test_that("revisar_datos: texto, constantes, codigos, Likert y N", {
  set.seed(2)
  d <- data.frame(a = sample(1:5, 150, TRUE), b = sample(c(1:4, 99), 150, TRUE), c = 1, t = "x")
  capture.output(v <- revisar_datos(d))
  txt <- paste(v$texto, collapse = " ")
  expect_match(txt, "no son num")
  expect_match(txt, "mismo valor")
  expect_match(txt, "dato perdido")
  expect_match(txt, "Likert")
  expect_equal(v$estado[nrow(v)], "aviso")   # N = 150
})

test_that("plantilla crea el script sin sobrescribir", {
  old <- setwd(tempdir()); on.exit(setwd(old))
  unlink(c("analisis_afc.R", "analisis_afc_2.R"))
  a <- suppressMessages(plantilla("CFA", abrir = FALSE))
  b <- suppressMessages(plantilla("afc", abrir = FALSE))
  expect_match(a, "analisis_afc\\.R$"); expect_match(b, "analisis_afc_2\\.R$")
  expect_true(any(grepl("CAMBIA AQU", readLines(a, encoding = "UTF-8"))))
  for (tp in c("afe", "trayectorias", "sem", "moderacion", "manova"))
    expect_silent(parse(suppressMessages(plantilla(tp, archivo = tempfile(fileext = ".R"), abrir = FALSE)), encoding = "UTF-8"))
  expect_error(plantilla("anova"), "No hay plantilla")
  expect_message(plantilla(), "Plantillas disponibles")
})
