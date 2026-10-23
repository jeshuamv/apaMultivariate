library(lavaan)
set.seed(3); n <- 300
d <- data.frame(X = rnorm(n), W = rnorm(n))
d$M <- .5 * d$X + rnorm(n); d$Y <- .4 * d$M + .2 * d$X + .3 * d$W + rnorm(n); d$Z <- .3 * d$Y + rnorm(n)
fit <- sem("M ~ X
            Y ~ M + X + W
            Z ~ Y", data = d)

test_that("encuentra todas las trayectorias indirectas", {
  R <- data.frame(de = c("X", "M", "X", "W", "Y"), a = c("M", "Y", "Y", "Y", "Z"))
  cm <- vapply(trayectorias_indirectas(R), paste, character(1), collapse = ">")
  expect_setequal(cm, c("X>M>Y", "X>M>Y>Z", "X>Y>Z", "M>Y>Z", "W>Y>Z"))
})

test_that("indirectos y metodo delta coinciden con := de lavaan", {
  t <- tabla_efectos(fit, bootstrap = 0)
  ref <- sem("M ~ a*X
              Y ~ b*M + c*X + w*W
              Z ~ e*Y
              i1 := a*b
              i2 := a*b*e
              i3 := w*e
              tot := c + a*b
              ti := c*e + a*b*e", data = d)
  pe <- parameterEstimates(ref); pe <- pe[pe$op == ":=", ]
  D <- t$datos
  chk <- function(efecto, tipo, def) {
    fila <- D[D$efecto == efecto & D$tipo == tipo, ]
    expect_equal(fila$B, pe$est[pe$lhs == def], tolerance = 1e-8)
    expect_equal(fila$EE, pe$se[pe$lhs == def], tolerance = 1e-6)
  }
  chk("X → M → Y", "indirecto", "i1")
  chk("X → M → Y → Z", "indirecto", "i2")
  chk("W → Y → Z", "indirecto", "i3")
  chk("X → Y", "total", "tot")
  chk("X → Z (indirecto total)", "indirecto_total", "ti")
  # coeficientes estandarizados = producto de los estandarizados
  ss <- standardizedSolution(fit)
  b <- function(l, r) ss$est.std[ss$op == "~" & ss$lhs == l & ss$rhs == r]
  expect_equal(D$beta[D$efecto == "X → M → Y"], b("M", "X") * b("Y", "M"), tolerance = 1e-8)
})

test_that("se omiten filas repetidas", {
  t <- tabla_efectos(fit, bootstrap = 0)
  primera <- trimws(t$celdas[, 1])
  expect_false("W → Z" %in% primera)                       # total sin efecto directo
  expect_false("X → Y (indirecto total)" %in% primera)     # una sola trayectoria
  expect_true("X → Z (indirecto total)" %in% primera)
  expect_length(t$extra, 1)                                     # R2
})

test_that("bootstrap da intervalos parecidos a los de lavaan", {
  skip_on_cran()
  t <- suppressMessages(tabla_efectos(fit, bootstrap = 300, semilla = 1))
  fila <- t$datos[t$datos$efecto == "X → M → Y" & t$datos$tipo == "indirecto", ]
  set.seed(1)
  ref <- sem("M ~ a*X
              Y ~ b*M + c*X + W
              Z ~ Y
              ind := a*b", data = d, se = "bootstrap", bootstrap = 300)
  pb <- parameterEstimates(ref, boot.ci.type = "perc"); pb <- pb[pb$lhs == "ind", ]
  expect_lt(abs(fila$ic_inf - pb$ci.lower), 0.03)
  expect_lt(abs(fila$ic_sup - pb$ci.upper), 0.03)
  expect_true(is.na(fila$p))
  expect_true(t$negritas[which(trimws(t$celdas[, 1]) == "X → M → Y"), 4])
})

test_that("parametros definidos con := aparecen aparte y modelos sin efectos dan error", {
  ref <- sem("M ~ a*X
              Y ~ b*M + X
              ind := a*b", data = d)
  t <- tabla_efectos(ref, bootstrap = 0)
  expect_length(t$extra, 2)
  expect_error(tabla_efectos(cfa("F =~ x1 + x2 + x3", data = HolzingerSwineford1939), bootstrap = 0), "no tiene efectos")
})

test_that("SEM con variables latentes", {
  m <- sem("ind60 =~ x1 + x2 + x3
            dem60 =~ y1 + y2 + y3 + y4
            dem65 =~ y5 + y6 + y7 + y8
            dem60 ~ ind60
            dem65 ~ ind60 + dem60", data = PoliticalDemocracy)
  t <- tabla_efectos(m, bootstrap = 0)
  expect_true("ind60 → dem60 → dem65" %in% t$datos$efecto)
})

test_that("parrafo de metodo", {
  HS <- HolzingerSwineford1939; HS$x1[1:5] <- NA
  f1 <- cfa("visual =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6", data = HS, estimator = "MLR", missing = "fiml")
  p <- parrafo_metodo(f1, paquetes = "semTools", bootstrap = 1000)
  expect_s3_class(p, "texto_apa")
  expect_match(p$parrafo, "MLR"); expect_match(p$parrafo, "FIML"); expect_match(p$parrafo, "1000 remuestreos")
  expect_match(p$parrafo, "los paquetes lavaan .* y semTools")
  expect_match(p$parrafo, packageDescription("lavaan")$Version, fixed = TRUE)
  expect_true(any(grepl("^Rosseel", p$referencias)))
  f2 <- cfa("visual =~ x1 + x2 + x3", data = HS)
  expect_match(parrafo_metodo(f2, paquetes = character(0))$parrafo, "Se excluyeron 5 casos")
  expect_output(print(p), "Referencias")
  skip_if_not_installed("zip")
  f <- tempfile(fileext = ".docx"); parrafo_metodo(f1, archivo = f); expect_true(file.exists(f))
  f <- tempfile(fileext = ".txt"); parrafo_metodo(f1, archivo = f); expect_match(readLines(f, encoding = "UTF-8")[1], "^Los an")
})
