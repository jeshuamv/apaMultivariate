library(lavaan)

hs3 <- "visual =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
        speed =~ x7 + x8 + x9"
fit_afc <- cfa(hs3, data = HolzingerSwineford1939)
fit_so  <- cfa(paste(hs3, "\n g =~ visual + textual + speed"), data = HolzingerSwineford1939)
fit_sem <- sem("ind60 =~ x1 + x2 + x3
                dem60 =~ y1 + y2 + y3 + y4
                dem65 =~ y5 + y6 + y7 + y8
                dem60 ~ ind60
                dem65 ~ ind60 + dem60
                y1 ~~ y5", data = PoliticalDemocracy)
set.seed(1)
dp <- data.frame(X = rnorm(200)); dp$M <- 0.5 * dp$X + rnorm(200); dp$Y <- 0.4 * dp$M + 0.2 * dp$X + rnorm(200)
fit_path <- sem("M ~ X\n Y ~ M + X", data = dp)

sin_na <- function(d) !anyNA(d$G$nodes$x) && !anyNA(d$G$nodes$y)

test_that("detecta el tipo de modelo y elige acomodo", {
  d <- diagrama_sem(fit_afc, dibujar = FALSE)
  expect_s3_class(d, "diagrama_sem")
  expect_equal(d$tipo, "afc"); expect_equal(d$acomodo, "fila"); expect_true(sin_na(d))
  expect_equal(diagrama_sem(fit_so, dibujar = FALSE)$tipo, "segundo_orden")
  expect_equal(diagrama_sem(fit_sem, dibujar = FALSE)$tipo, "sem")
  expect_equal(diagrama_sem(fit_path, dibujar = FALSE)$tipo, "path")
})

test_that("crea los errores numerados y las flechas", {
  d <- diagrama_sem(fit_afc, dibujar = FALSE)
  N <- d$G$nodes
  expect_equal(sum(N$tipo == "err"), 9)
  expect_true(all(paste0("e", 1:9) %in% N$etiqueta))
  expect_equal(sum(d$G$edges$tipo == "carga"), 9)
  expect_equal(sum(d$G$edges$tipo == "cov"), 3)
})

test_that("todos los acomodos validos funcionan", {
  for (a in c("fila", "dos_filas", "arbol")) expect_true(sin_na(diagrama_sem(fit_afc, acomodo = a, dibujar = FALSE)))
  for (a in c("izq_der", "arriba_abajo")) expect_true(sin_na(diagrama_sem(fit_sem, acomodo = a, dibujar = FALSE)))
  expect_error(diagrama_sem(fit_path, acomodo = "fila", dibujar = FALSE), "no aplica")
})

test_that("oculta correlaciones segun la opcion", {
  d <- diagrama_sem(fit_afc, correlaciones = FALSE, dibujar = FALSE)
  expect_equal(sum(d$G$edges$tipo == "cov"), 0)
  expect_equal(nrow(d$correlaciones_ocultas), 3)
  d <- diagrama_sem(fit_afc, correlaciones = 0.45, dibujar = FALSE)
  expect_true(all(abs(d$G$edges$val[d$G$edges$tipo == "cov"]) >= 0.45))
})

test_that("etiquetas, lados y mover", {
  d <- diagrama_sem(fit_afc, etiquetas = c(visual = "Visual"), dibujar = FALSE)
  expect_true("Visual" %in% d$G$nodes$etiqueta)
  d1 <- diagrama_sem(fit_afc, dibujar = FALSE)
  d2 <- diagrama_sem(fit_afc, mover = list(visual = c(1, 2)), dibujar = FALSE)
  expect_equal(d2$G$nodes$x[d2$G$nodes$id == "x1"] - d1$G$nodes$x[d1$G$nodes$id == "x1"], 1)
  d3 <- diagrama_sem(fit_afc, lados = c(visual = "abajo"), dibujar = FALSE)
  expect_lt(d3$G$nodes$y[d3$G$nodes$id == "x1"], d3$G$nodes$y[d3$G$nodes$id == "visual"])
  expect_error(diagrama_sem(fit_afc, lados = c(visual = "atras"), dibujar = FALSE))
  expect_error(diagrama_sem(fit_afc, mover = list(nada = c(1, 1)), dibujar = FALSE))
})

test_that("solucion no estandarizada y estrellas", {
  d <- diagrama_sem(fit_afc, estandarizado = FALSE, estrellas = TRUE, dibujar = FALSE)
  expect_false(d$opciones$estandarizado)
  expect_equal(texto_flecha(d$G, which(d$G$edges$to == "x1" & d$G$edges$tipo == "carga"), d$opciones), "1.00")
  j <- which(d$G$edges$to == "x2" & d$G$edges$tipo == "carga")
  expect_match(texto_flecha(d$G, j, d$opciones), "\\*")
})

test_that("guarda en varios formatos", {
  d <- diagrama_sem(fit_sem, dibujar = FALSE)
  for (ext in c("png", "pdf", "jpg")) {
    f <- tempfile(fileext = paste0(".", ext))
    guardar_diagrama(d, f, res = 72)
    expect_true(file.exists(f) && file.size(f) > 1000)
  }
  expect_error(guardar_diagrama(d, tempfile(fileext = ".doc")), "Formato")
})

test_that("paletas", {
  expect_s3_class(paleta_sem("azul"), "paleta_sem")
  expect_equal(paleta_sem(latente = "red")$latente, c("red", "red"))
  expect_error(paleta_sem("arcoiris"), "Paleta desconocida")
  expect_error(diagrama_sem(fit_afc, paleta = "azul", dibujar = FALSE))
})

test_that("rechaza objetos que no son lavaan", {
  expect_error(diagrama_sem(lm(mpg ~ wt, mtcars)), "lavaan")
})
