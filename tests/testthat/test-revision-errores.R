# Errores encontrados en la revisión exhaustiva de la versión 0.12.1

test_that("tabla_influencia y tabla_subconjuntos funcionan con transformaciones", {
  x1 <- 1:5   # un objeto global con el nombre de una columna no debe estorbar
  f <- lm(log(mpg) ~ log(wt) + hp, mtcars)
  t <- tabla_influencia(f)
  i <- which.max(cooks.distance(f))
  f2 <- lm(log(mpg) ~ log(wt) + hp, mtcars[-i, ])
  expect_equal(unname(as.numeric(t$extra[[1]]$celdas[2, 4])), round(unname(coef(f2)[2]), 2))
  expect_match(t$extra[[1]]$celdas[4, 4], sub("^0", "", formatC(summary(f2)$r.squared, format = "f", digits = 3)), fixed = TRUE)
  expect_silent(suppressMessages(tabla_influencia(lm(mpg ~ poly(wt, 2) + factor(cyl), mtcars))))
  s <- tabla_subconjuntos(lm(log(mpg) ~ log(wt) + hp + qsec, mtcars))
  expect_equal(s$datos$R2[s$datos$predictores == "log(wt)"], summary(lm(log(mpg) ~ log(wt), mtcars))$r.squared)
})

test_that("parrafo_metodo nombra bien los estimadores corregidos", {
  HS <- lavaan::HolzingerSwineford1939
  m <- "a =~ x1 + x2 + x3\n b =~ x4 + x5 + x6"
  expect_match(parrafo_metodo(lavaan::cfa(m, HS, estimator = "MLMV"))$parrafo, "(MLMV)", fixed = TRUE)
  expect_match(parrafo_metodo(lavaan::cfa(m, HS, estimator = "MLR"))$parrafo, "(MLR)", fixed = TRUE)
  d <- HS; for (v in paste0("x", 1:6)) d[[v]] <- cut(d[[v]], 4, labels = FALSE)
  expect_match(parrafo_metodo(lavaan::cfa(m, d, ordered = TRUE, estimator = "WLSM"))$parrafo, "(WLSM)", fixed = TRUE)
  expect_match(parrafo_metodo(lavaan::cfa(m, d, ordered = TRUE))$parrafo, "(WLSMV)", fixed = TRUE)
})

test_that("tabla_confiabilidad con un factor de un solo indicador", {
  f <- lavaan::sem("F1 =~ x1 + x2 + x3\n F2 =~ x4\n F1 ~ F2", lavaan::HolzingerSwineford1939)
  expect_no_warning(t <- tabla_confiabilidad(f))
  expect_equal(unname(t$celdas[2, 3:6]), rep("—", 4))
  expect_true(is.na(t$datos$alfa[2]))
})

test_that("tabla_manova reporta atípicos aunque los nombres de fila sean texto", {
  d <- iris; rownames(d) <- paste0("caso", 1:150); d[5, 1:4] <- c(9, 1, 7, 0.1)
  expect_no_warning(t <- tabla_manova(d, names(d)[1:4], "Species"))
  expect_true(5 %in% t$datos$atipicos)
})

test_that("las funciones con semilla no alteran el generador aleatorio del usuario", {
  set.seed(99); a <- runif(1)
  set.seed(99); invisible(tabla_factorabilidad(lavaan::HolzingerSwineford1939[, 7:15], iteraciones = 5)); b <- runif(1)
  expect_identical(a, b)
  f <- lm(mpg ~ wt + hp, mtcars)
  expect_identical(tabla_regresion(f, bootstrap = 50)$datos$li, tabla_regresion(f, bootstrap = 50)$datos$li)
})

test_that("modelo_alternativo sin cambios avisa una sola vez y guardar_tablas acepta el archivo sin nombre", {
  set.seed(1); d <- data.frame(X = rnorm(200)); d$M <- .5 * d$X + rnorm(200); d$Y <- .5 * d$M + rnorm(200)
  msgs <- character(0)
  withCallingHandlers(modelo_alternativo(lavaan::sem("M ~ X\n Y ~ M", d), dibujar = FALSE),
                      message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_equal(sum(grepl("No hay cambios", msgs)), 1)
  f <- lavaan::cfa("a =~ x1 + x2 + x3", lavaan::HolzingerSwineford1939)
  arch <- file.path(tempdir(), "dos_tablas.docx")
  suppressMessages(guardar_tablas(tabla_ajuste(f), tabla_cargas(f), arch))
  expect_true(file.exists(arch))
})

test_that("tabla_efectos(matrices = TRUE): matrices estandarizadas como AMOS", {
  set.seed(5); d <- data.frame(X = rnorm(200), W = rnorm(200))
  d$M <- .5 * d$X + rnorm(200); d$Y <- .4 * d$M + .3 * d$W + rnorm(200)
  f <- lavaan::sem("M ~ X\n Y ~ M + W", d)
  t <- tabla_efectos(f, bootstrap = 0, matrices = TRUE)
  M <- t$matrices
  s <- lavaan::standardizedSolution(f); b <- function(de, a) s$est.std[s$op == "~" & s$rhs == de & s$lhs == a]
  expect_equal(M$totales["Y", "X"], b("X", "M") * b("M", "Y"))      # solo indirecto: no sale en la tabla principal
  expect_equal(M$totales["Y", "W"], b("W", "Y"))                    # solo directo
  expect_equal(M$indirectos["Y", "X"], M$totales["Y", "X"])
  expect_equal(M$directos["M", "X"], b("X", "M"))
  expect_length(t$extra, 4)                                          # R² + 3 matrices
  expect_length(tabla_efectos(f, bootstrap = 0)$extra, 1)
  expect_false(is.null(tabla_efectos(f, bootstrap = 0)$matrices))
  # Latentes: total = directo + indirecto
  fl <- lavaan::sem("ind60 =~ x1 + x2 + x3\n dem60 =~ y1 + y2 + y3 + y4\n dem65 =~ y5 + y6 + y7 + y8\n dem60 ~ ind60\n dem65 ~ ind60 + dem60",
                    lavaan::PoliticalDemocracy)
  te <- tabla_efectos(fl, bootstrap = 0)
  expect_equal(te$matrices$totales["dem65", "ind60"], te$datos$beta[te$datos$tipo == "total"])
})

test_that("parrafo_metodo(ic_rmsea) menciona el nivel del intervalo", {
  f <- lavaan::cfa("a =~ x1 + x2 + x3\n b =~ x4 + x5 + x6", lavaan::HolzingerSwineford1939)
  expect_match(parrafo_metodo(f)$parrafo, "al 90 %", fixed = TRUE)
  expect_match(parrafo_metodo(f, ic_rmsea = .95)$parrafo, "al 95 %", fixed = TRUE)
  expect_error(parrafo_metodo(f, ic_rmsea = 95), "entre 0 y 1")
})

test_that("tabla_ajuste(n = TRUE) agrega la columna N de cada modelo", {
  m <- "visual =~ x1 + x2 + x3\n textual =~ x4 + x5 + x6"
  HS <- lavaan::HolzingerSwineford1939
  f1 <- lavaan::cfa(m, HS); f2 <- lavaan::cfa(m, HS[1:250, ])
  t0 <- tabla_ajuste(A = f1, B = f2, extra = "pnfi")
  t1 <- tabla_ajuste(A = f1, B = f2, extra = "pnfi", n = TRUE)
  expect_equal(t1$columnas[1:3], c("Modelo", "N", "χ²"))
  expect_equal(unname(t1$celdas[, 2]), c("301", "250"))
  expect_equal(ncol(t1$celdas), ncol(t0$celdas) + 1)
  expect_equal(t0$datos$n, c(301, 250))                 # siempre en $datos
  expect_equal(unname(t1$celdas[, -2]), unname(t0$celdas))  # lo demás no cambia
  fg <- lavaan::cfa(m, HS, group = "school")
  expect_equal(tabla_ajuste(fg, n = TRUE)$datos$n, 301)    # multigrupo: N total
  expect_error(tabla_ajuste(f1, n = "si"), "TRUE o FALSE")
})

test_that("tabla_ajuste(cortes) usa los puntos de corte indicados en la nota", {
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3\n textual =~ x4 + x5 + x6\n speed =~ x7 + x8 + x9",
                     data = lavaan::HolzingerSwineford1939)
  t0 <- tabla_ajuste(fit)
  expect_match(t0$nota, "Hu y Bentler")
  t1 <- tabla_ajuste(fit, extra = "pnfi", cortes = list(rmsea = c(.05, .06), srmr = c(.08, .10), pnfi = .50))
  expect_no_match(t1$nota, "Hu y Bentler")
  expect_match(t1$nota, "RMSEA ≤ .05 excelente (≤ .06 aceptable)", fixed = TRUE)
  expect_match(t1$nota, "SRMR ≤ .08 excelente (≤ .10 aceptable)", fixed = TRUE)
  expect_match(t1$nota, "PNFI ≥ .50 aceptable", fixed = TRUE)
  expect_error(tabla_ajuste(fit, cortes = list(xyz = .1)), "No reconozco")
  expect_error(tabla_ajuste(fit, cortes = c(rmsea = .05)), "lista con nombres")
  # tabla_criterios_ajuste sigue igual
  k <- tabla_criterios_ajuste(fit, indices = "rmsea", cortes = list(rmsea = c(.05, .06)))
  expect_true(any(grepl(".05", k$celdas, fixed = TRUE)))
})

test_that("tabla_normalidad(criterio = 'curtosis', limite = 2) decide solo con la curtosis de Mardia", {
  d <- lavaan::PoliticalDemocracy
  t0 <- tabla_normalidad(d, variables = c(paste0("y", 1:8), paste0("x", 1:3)))
  expect_match(t0$extra[[1]]$nota, "Se rechaza")
  t1 <- tabla_normalidad(d, variables = c(paste0("y", 1:8), paste0("x", 1:3)), limite = 2, criterio = "curtosis")
  expect_match(t1$nota, "se cumple el supuesto de normalidad multivariada")
  expect_match(t1$nota, "|c.r.| > 2,", fixed = TRUE)
  expect_match(t1$extra[[1]]$nota, "se reporta como referencia")
  expect_error(tabla_normalidad(d, variables = c("y1", "y2"), limite = -1), "positivo")
  expect_error(tabla_normalidad(d, variables = c("y1", "y2"), criterio = "otro"))
})

test_that("diagrama_sem: los R2 de los ítems de un factor van en la misma esquina", {
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3\n textual =~ x4 + x5 + x6\n speed =~ x7 + x8 + x9",
                     data = lavaan::HolzingerSwineford1939)
  pos <- NULL
  trace("posiciones_r2", exit = quote(assign("pos", returnValue(), envir = globalenv())),
        print = FALSE, where = asNamespace("apaMultivariate"))
  on.exit(untrace("posiciones_r2", where = asNamespace("apaMultivariate")))
  f <- tempfile(fileext = ".png"); diagrama_sem(fit, archivo = f)
  N <- get("pos", envir = globalenv())
  expect_true(is.data.frame(N))
  expect_setequal(N$id, paste0("x", 1:9))
  for (g in list(paste0("x", 1:3), paste0("x", 4:6), paste0("x", 7:9)))
    expect_length(unique(round(N$y[N$id %in% g], 6)), 1)
})
