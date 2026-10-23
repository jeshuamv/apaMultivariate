library(lavaan)

fit3 <- cfa("visual =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6
             speed =~ x7 + x8 + x9", data = HolzingerSwineford1939)

test_that("los tres estilos dibujan sin error", {
  for (e in c("clasico", "revista", "color")) {
    f <- tempfile(fileext = ".png")
    d <- diagrama_sem(fit3, estilo = e, dibujar = FALSE)
    expect_equal(d$opciones$estilo, e)
    expect_silent(guardar_diagrama(d, f))
    expect_true(file.exists(f))
  }
})

test_that("un estilo desconocido da un error claro", {
  expect_error(diagrama_sem(fit3, estilo = "moderno", dibujar = FALSE), "clasico")
})

test_that("los estilos de revista agrandan las cajas para el texto", {
  fit <- cfa("F =~ item_largo_1 + item_largo_2 + item_largo_3",
             data = setNames(HolzingerSwineford1939[, c("x1", "x2", "x3")],
                             c("item_largo_1", "item_largo_2", "item_largo_3")))
  a_cl <- diagrama_sem(fit, dibujar = FALSE)$G$nodes
  a_rv <- diagrama_sem(fit, estilo = "revista", dibujar = FALSE)$G$nodes
  expect_true(all(a_rv$a[a_rv$forma == "rect"] > a_cl$a[a_cl$forma == "rect"]))
})

test_that("el ancho de revista informa el tamano del texto y avisa si es pequeno", {
  d <- diagrama_sem(fit3, estilo = "revista", dibujar = FALSE)
  expect_message(guardar_diagrama(d, tempfile(fileext = ".pdf"), ancho = "pagina"), "174 x")
  expect_gte(tamano_texto(d, ANCHOS[["pagina"]], 1, 12, 1) , 0)
  expect_warning(suppressMessages(guardar_diagrama(d, tempfile(fileext = ".pdf"), ancho = "columna")), "8 pt")
  dc <- diagrama_sem(fit3, dibujar = FALSE)
  expect_warning(suppressMessages(guardar_diagrama(dc, tempfile(fileext = ".pdf"), ancho = "pagina")), "revista")
  expect_error(guardar_diagrama(d, tempfile(fileext = ".pdf"), ancho = "doble"), "pagina")
})

test_that("el texto de revista a pagina completa cumple 8 pt en un AFC de 3 factores", {
  d <- diagrama_sem(fit3, estilo = "revista", dibujar = FALSE)
  pt <- suppressMessages(guardar_diagrama(d, f <- tempfile(fileext = ".pdf"), ancho = "pagina"))
  cj <- caja_total(d$G, d$trazos)
  W <- diff(cj[1:2]) + 0.5; H <- diff(cj[3:4]) + 0.5
  expect_gte(tamano_texto(d, ANCHOS[["pagina"]], ANCHOS[["pagina"]] * H / W, W, H), 8)
})

test_that("guarda en los formatos de revista", {
  d <- diagrama_sem(fit3, estilo = "color", dibujar = FALSE)
  for (x in c("pdf", "eps", "tiff", "png", "jpg")) {
    f <- tempfile(fileext = paste0(".", x))
    suppressMessages(guardar_diagrama(d, f, ancho = "pagina"))
    expect_true(file.exists(f) && file.size(f) > 1000)
  }
  expect_error(guardar_diagrama(d, tempfile(fileext = ".bmp")), "Formato")
})

test_that("el estilo color funciona en bifactor, AFE y trayectorias", {
  set.seed(1); n <- 300
  dx <- data.frame(X = rnorm(n)); dx$M <- .5 * dx$X + rnorm(n); dx$Y <- .4 * dx$M + rnorm(n)
  fp <- sem("M ~ X\n Y ~ M + X", data = dx)
  expect_silent(guardar_diagrama(diagrama_sem(fp, estilo = "color", dibujar = FALSE), tempfile(fileext = ".png")))
  skip_if_not_installed("psych")
  afe <- suppressWarnings(psych::fa(HolzingerSwineford1939[, paste0("x", 1:9)], nfactors = 3, rotate = "varimax"))
  expect_silent(guardar_diagrama(diagrama_sem(afe, estilo = "color", dibujar = FALSE), tempfile(fileext = ".png")))
})

test_that("no se escribe -.00", {
  expect_equal(formato_valor(-0.001, TRUE, 2), ".00")
  expect_equal(formato_valor(-0.25, TRUE, 2), "-.25")
})
