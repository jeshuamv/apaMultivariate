library(lavaan)
set.seed(1); n <- 300
d <- data.frame(X = rnorm(n, 3, 1), W = rnorm(n, 4, 1.5), Z = rnorm(n))
d$Y <- 1 + .3 * d$X + .2 * d$W + .25 * (d$X - 3) * (d$W - 4) + .1 * d$Z + rnorm(n)
d$M <- .4 * d$X + .3 * (d$X - 3) * (d$W - 4) + rnorm(n)
d$Y2 <- .5 * d$M + .1 * d$X + rnorm(n)
d$Y3 <- .5 * d$M + .2 * (d$M - mean(d$M)) * (d$W - 4) + rnorm(n)

test_that("moderacion simple: coeficientes, pendientes y Johnson-Neyman", {
  t <- tabla_moderacion(d, x = "X", w = "W", y = "Y", covariables = "Z")
  dc <- d; dc$X <- dc$X - mean(dc$X); dc$W <- dc$W - mean(dc$W)
  m <- lm(Y ~ X * W + Z, dc); b <- coef(m); V <- vcov(m)
  w <- c(-1, 0, 1) * sd(d$W)
  expect_equal(t$datos$condicionales$efecto, unname(b["X"] + b["X:W"] * w), tolerance = 1e-10)
  expect_equal(unname(coef(t$modelos$y)[".X:.W"]), unname(b["X:W"]), tolerance = 1e-10)
  jn <- t$datos$johnson_neyman - mean(d$W)
  tj <- (b["X"] + b["X:W"] * jn) / sqrt(V["X", "X"] + 2 * jn * V["X", "X:W"] + jn^2 * V["X:W", "X:W"])
  expect_equal(abs(unname(tj)), rep(qt(.975, df.residual(m)), length(jn)), tolerance = 1e-6)
  expect_match(t$nota, "ΔR²")
  expect_length(t$extra, 1)
  expect_equal(unname(trimws(t$celdas[nrow(t$celdas), 1])), "Constante")
})

test_that("sin centrar, las pendientes simples no cambian", {
  a <- tabla_moderacion(d, x = "X", w = "W", y = "Y")
  b <- tabla_moderacion(d, x = "X", w = "W", y = "Y", centrar = FALSE)
  expect_equal(a$datos$condicionales$efecto, b$datos$condicionales$efecto, tolerance = 1e-10)
})

test_that("moderador dicotomico usa sus dos valores", {
  dd <- d; dd$G <- rbinom(n, 1, .5); dd$Y4 <- .3 * dd$X + .5 * dd$X * dd$G + rnorm(n)
  t <- tabla_moderacion(dd, x = "X", w = "G", y = "Y4")
  expect_equal(t$datos$condicionales$w, c(0, 1))
  m0 <- lm(Y4 ~ X, subset(dd, G == 0))
  expect_equal(t$datos$condicionales$efecto[1], unname(coef(m0)["X"]), tolerance = 1e-8)
})

test_that("modelo 7: indice de mediacion moderada igual a lavaan", {
  t <- suppressMessages(tabla_moderacion(d, x = "X", w = "W", m = "M", y = "Y2", modelo = 7, bootstrap = 300))
  dc <- d; dc$X <- dc$X - mean(d$X); dc$W <- dc$W - mean(d$W); dc$XW <- dc$X * dc$W
  f <- sem("M ~ a1*X + W + a3*XW
            Y2 ~ b*M + X
            idx := a3*b", data = dc)
  pe <- parameterEstimates(f)
  expect_equal(t$datos$indirectos["indice", "efecto"], pe$est[pe$lhs == "idx"], tolerance = 1e-6)
  expect_true(t$datos$indirectos["indice", "ic_inf"] > 0)
  expect_equal(nrow(t$datos$indirectos), 4)
})

test_that("modelo 14 y errores", {
  t <- suppressMessages(tabla_moderacion(d, x = "X", w = "W", m = "M", y = "Y3", modelo = 14, bootstrap = 200))
  a <- coef(t$modelos$m)[[".X"]]; b1 <- coef(t$modelos$y)[[".M"]]; b3 <- coef(t$modelos$y)[[".M:.W"]]
  expect_equal(t$datos$indirectos["indice", "efecto"], a * b3, tolerance = 1e-10)
  expect_error(tabla_moderacion(d, x = "X", w = "W", y = "Y", modelo = 7), "mediador")
  expect_error(tabla_moderacion(d, x = "X", w = "W", y = "Y", modelo = 4), "1, 7 o 14")
  expect_error(tabla_moderacion(d, x = "X", w = "Q", y = "Y"), "No est")
})

test_that("grafica de la interaccion", {
  t <- tabla_moderacion(d, x = "X", w = "W", y = "Y")
  f <- tempfile(fileext = ".png")
  v <- grafica_interaccion(t, archivo = f)
  expect_true(file.exists(f))
  expect_equal(nrow(v), 6)
  t14 <- suppressMessages(tabla_moderacion(d, x = "X", w = "W", m = "M", y = "Y3", modelo = 14, bootstrap = 50))
  expect_silent(grafica_interaccion(t14, archivo = tempfile(fileext = ".pdf")))
  expect_error(grafica_interaccion(tabla_ajuste(cfa("F =~ x1 + x2 + x3", data = HolzingerSwineford1939))), "tabla_moderacion")
})

test_that("bifactor: omegaH y omegaHS iguales a semTools", {
  skip_if_not_installed("semTools")
  set.seed(5); n <- 500; G <- rnorm(n); S <- matrix(rnorm(n * 3), n)
  X <- sapply(1:12, function(i) .6 * G + .4 * S[, (i - 1) %/% 4 + 1] + rnorm(n, 0, sqrt(.48)))
  colnames(X) <- paste0("i", 1:12); X <- as.data.frame(scale(X))
  fb <- cfa("g =~ i1+i2+i3+i4+i5+i6+i7+i8+i9+i10+i11+i12
             A =~ i1+i2+i3+i4
             B =~ i5+i6+i7+i8
             C =~ i9+i10+i11+i12", data = X, orthogonal = TRUE, std.lv = TRUE)
  t <- tabla_bifactor(fb)
  om <- unlist(semTools::compRelSEM(fb, obs.var = FALSE))
  expect_equal(c(t$datos$omegaH, unname(t$datos$omegaHS)), unname(as.numeric(om[c("g", "A", "B", "C")])), tolerance = 1e-4)
  expect_equal(t$datos$puc, 1 - 3 * 6 / 66, tolerance = 1e-10)
  expect_true(all(t$datos$fd <= 1))
  expect_equal(t$datos$general, "g")
})

test_that("segundo orden: omegaH de Schmid-Leiman igual a semTools (nivel 2)", {
  skip_if_not_installed("semTools")
  Z <- as.data.frame(scale(HolzingerSwineford1939[, paste0("x", 1:9)]))
  fs <- cfa("visual =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6
             speed =~ x7 + x8 + x9
             g =~ visual + textual + speed", data = Z, std.lv = TRUE)
  t <- tabla_bifactor(fs)
  expect_equal(t$datos$tipo, "segundo_orden")
  expect_equal(t$datos$omegaH, unname(unlist(semTools::compRelSEM(fs, obs.var = FALSE, higher = "g"))["g"]), tolerance = 1e-4)
  expect_match(t$nota, "Schmid-Leiman")
})

test_that("caso Heywood: aviso y sin H ni FD", {
  Z <- as.data.frame(scale(HolzingerSwineford1939[, paste0("x", 1:9)]))
  f2 <- suppressWarnings(cfa("g =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
             visual =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6
             speed =~ x7 + x8 + x9", data = Z, orthogonal = TRUE, std.lv = TRUE))
  expect_warning(t <- tabla_bifactor(f2), "Heywood")
  expect_true(all(t$celdas[, 6:7] == "—"))
  expect_match(t$nota, "Heywood")
})

test_that("en ingles", {
  viejo <- idioma_sem("en"); on.exit(idioma_sem(viejo))
  t <- tabla_moderacion(d, x = "X", w = "W", y = "Y")
  expect_equal(t$titulo, "Moderation: regression coefficients")
  expect_match(t$extra[[1]]$nota, "Johnson-Neyman")
  expect_equal(t$extra[[1]]$celdas[2, 1], paste0("Mean (", formatC(mean(d$W), format = "f", digits = 2), ")"))
})
