# Errores comunes de quien empieza: el mensaje debe decir qué hacer

datos <- lavaan::HolzingerSwineford1939
modelo <- "visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6"
fit <- lavaan::cfa(modelo, data = datos)
reg <- lm(x1 ~ x2 + x3, datos)

test_that("pasar el texto del modelo o los datos en lugar del modelo estimado", {
  expect_error(tabla_ajuste(modelo), "texto del modelo")
  expect_error(tabla_cargas(modelo), "texto del modelo")
  expect_error(diagrama_sem(modelo, dibujar = FALSE), "texto del modelo")
  expect_error(tabla_parametros(datos), "los datos")
  expect_error(tabla_ajuste(reg), "tabla_regresion")
  expect_error(tabla_regresion(fit), "tabla_parametros")
  expect_error(tabla_regresion("x1 ~ x2"), "lm\\(modelo")
  expect_error(revisar_datos(modelo, datos), "al rev")
  expect_error(revisar_datos(fit), "revisar_modelo")
})

test_that("objetos con el nombre mal escrito sugieren el correcto", {
  assign("fit_prueba_usab", fit, envir = globalenv()); on.exit(rm("fit_prueba_usab", envir = globalenv()))
  expect_error(tabla_parametros(fit_prueba_usb), "Quisiste decir 'fit_prueba_usab'")
  expect_error(tabla_ajuste(fit_prueba_usb), "No existe un objeto")
})

test_that("guardar: nombre por defecto, carpeta inexistente y confirmacion", {
  viejo <- setwd(tempdir()); on.exit(setwd(viejo))
  expect_message(guardar_tablas(tabla_ajuste(fit)), "tablas_apa.docx")
  expect_true(file.exists("tablas_apa.docx"))
  expect_error(guardar_tabla(tabla_ajuste(fit), "no_existe_123/t.docx"), "No existe la carpeta")
  expect_error(guardar_tabla(tabla_ajuste(fit)), "nombre del archivo")
})

test_that("leer_datos sugiere el archivo cuando falta la extension", {
  viejo <- setwd(tempdir()); on.exit(setwd(viejo))
  write.csv(datos[1:10, 7:9], "mis_datos_usab.csv", row.names = FALSE)
  expect_error(leer_datos("mis_datos_usab"), "mis_datos_usab.csv")
  writeLines("no es excel", "roto_usab.xlsx")
  expect_error(leer_datos("roto_usab.xlsx"), "No pude leer")
})

test_that("plantilla, idioma e hipotesis aceptan variantes", {
  expect_true(file.exists(plantilla(" AFC ", archivo = tempfile(fileext = ".R"), abrir = FALSE)))
  expect_true(file.exists(plantilla("Regresión", archivo = tempfile(fileext = ".R"), abrir = FALSE)))
  viejo <- idioma_sem("inglés"); on.exit(idioma_sem(viejo))
  expect_equal(idioma_sem(), "en")
  expect_error(tabla_hipotesis(lavaan::sem("x1 ~ x2", datos)), "Escribe tus hip")
})
