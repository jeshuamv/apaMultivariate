# Extracción de la estructura del modelo a partir de un objeto lavaan.
# Devuelve una lista "G" con nodos, flechas y metadatos que usan el resto
# de las funciones internas.

#' @noRd
formato_valor <- function(x, estandarizado, decimales) {
  x <- round(x, decimales); x[x == 0] <- 0   # evita "-.00"
  s <- formatC(x, format = "f", digits = decimales)
  if (estandarizado) s <- sub("^(-?)0\\.", "\\1.", s)   # APA: sin cero inicial
  s
}

#' @noRd
estrellas_p <- function(p) {
  ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
}

#' @noRd
tam_lv <- function(lab) {
  ln <- max(nchar(strsplit(lab, "\n", fixed = TRUE)[[1]]))
  c(max(0.95, 0.075 * ln + 0.45), if (grepl("\n", lab, fixed = TRUE)) 0.48 else 0.4)
}

#' @noRd
tam_ov <- function(lab, b) c(max(0.42, 0.062 * nchar(lab) + 0.12), b)

#' @noRd
extraer_estructura <- function(fit, estandarizado = TRUE, grupo = 1, etiquetas = NULL,
                               prefijo_error = "e", mostrar_errores = TRUE) {
  if (!inherits(fit, "lavaan"))
    stop("`fit` debe ser un modelo estimado con lavaan (cfa(), sem() o lavaan()).", call. = FALSE)
  if (!isTRUE(lavaan::lavInspect(fit, "converged")))
    warning("El modelo no convergi\u00f3; el diagrama puede no ser confiable.", call. = FALSE)

  ng <- lavaan::lavInspect(fit, "ngroups")
  if (grupo < 1 || grupo > ng) stop("`grupo` debe estar entre 1 y ", ng, ".", call. = FALSE)

  if (estandarizado) {
    s <- lavaan::standardizedSolution(fit)
    s$valor <- s$est.std
  } else {
    s <- lavaan::parameterEstimates(fit)
    s$valor <- s$est
  }
  if (!"pvalue" %in% names(s)) s$pvalue <- NA_real_
  if (!"se" %in% names(s)) s$se <- NA_real_
  if ("group" %in% names(s)) s <- s[s$group == grupo, ]
  s <- s[s$op %in% c("=~", "~", "~~"), ]

  r2 <- tryCatch(lavaan::lavInspect(fit, "r2"), error = function(e) numeric(0))
  if (is.list(r2)) r2 <- r2[[grupo]]

  lv <- lavaan::lavNames(fit, "lv")
  L  <- s[s$op == "=~", ]
  R  <- s[s$op == "~", ]
  fijo0 <- abs(s$valor) < 1e-8 & (is.na(s$se) | s$se == 0)
  C  <- s[s$op == "~~" & s$lhs != s$rhs & !fijo0, ]

  ind_ov  <- unique(L$rhs[!L$rhs %in% lv])
  ov_all  <- lavaan::lavNames(fit, "ov")
  core_ov <- setdiff(ov_all, ind_ov)
  lv1  <- unique(L$lhs[L$rhs %in% ind_ov])
  lvH  <- unique(L$lhs[L$rhs %in% lv])
  endo <- unique(c(R$lhs, L$rhs))

  etq <- function(v) {
    propia <- !is.null(etiquetas) && v %in% names(etiquetas)
    e <- if (propia) as.character(etiquetas[[v]]) else v
    if (v %in% lv && !propia && nchar(e) > 12) e <- gsub("_", "\n", e, fixed = TRUE)
    e
  }

  filas <- list()
  agregar <- function(id, tipo, forma, lab, ab) {
    filas[[length(filas) + 1]] <<- data.frame(id = id, etiqueta = lab, tipo = tipo, forma = forma,
                                             a = ab[1], b = ab[2], stringsAsFactors = FALSE)
  }
  for (v in lv)      { l <- etq(v); agregar(v, "lv",   "elip", l, tam_lv(l)) }
  for (v in core_ov) { l <- etq(v); agregar(v, "ovc",  "rect", l, tam_ov(l, 0.2)) }
  for (v in ind_ov)  { l <- etq(v); agregar(v, "item", "rect", l, tam_ov(l, 0.17)) }

  err_de <- character(0)
  if (mostrar_errores) {
    k <- 0
    for (v in c(ind_ov, core_ov)) if (v %in% endo) {
      k <- k + 1; id <- paste0(".err", k); lab <- paste0(prefijo_error, k)
      agregar(id, "err", "circ", lab, c(0.2, 0.2)); err_de[id] <- v
    }
    for (v in lv) if (v %in% endo) {
      k <- k + 1; id <- paste0(".err", k); lab <- paste0(prefijo_error, k)
      agregar(id, "dist", "circ", lab, c(0.24, 0.24)); err_de[id] <- v
    }
  }
  nodes <- do.call(rbind, filas)
  nodes$x <- NA_real_; nodes$y <- NA_real_
  de_err <- stats::setNames(names(err_de), err_de)

  aristas <- list()
  agregar_a <- function(f, t, tipo, val, p, fijo) {
    aristas[[length(aristas) + 1]] <<- data.frame(from = f, to = t, tipo = tipo, val = val,
                                                 p = p, fijo = fijo, debil = FALSE, stringsAsFactors = FALSE)
  }
  es_fijo <- function(x) is.na(x$se) | x$se == 0
  principal <- vapply(ind_ov, function(i) {
    x <- L[L$rhs == i, ]; x$lhs[which.max(abs(x$valor))] }, character(1))
  for (j in seq_len(nrow(L))) {
    tipo <- if (L$rhs[j] %in% lv) "carga2" else if (principal[L$rhs[j]] == L$lhs[j]) "carga" else "cruzada"
    agregar_a(L$lhs[j], L$rhs[j], tipo, L$valor[j], L$pvalue[j], es_fijo(L[j, ]))
  }
  for (j in seq_len(nrow(R))) agregar_a(R$rhs[j], R$lhs[j], "reg", R$valor[j], R$pvalue[j], es_fijo(R[j, ]))
  for (j in seq_len(nrow(C))) {
    a <- C$lhs[j]; b <- C$rhs[j]
    tipo <- if (a %in% endo && b %in% endo) "covres" else "cov"
    if (tipo == "covres" && !is.na(de_err[a]) && !is.na(de_err[b])) { a <- de_err[[a]]; b <- de_err[[b]] }
    agregar_a(a, b, tipo, C$valor[j], C$pvalue[j], es_fijo(C[j, ]))
  }
  for (e in names(err_de)) agregar_a(e, err_de[[e]], "error", NA_real_, NA_real_, TRUE)
  edges <- if (length(aristas)) do.call(rbind, aristas) else
    data.frame(from = character(), to = character(), tipo = character(), val = numeric(),
               p = numeric(), fijo = logical(), debil = logical(), stringsAsFactors = FALSE)

  list(nodes = nodes, edges = edges, r2 = r2, lv = lv, lv1 = lv1, lvH = lvH,
       core_ov = core_ov, ind_ov = ind_ov, principal = principal,
       err_de = err_de, de_err = de_err, lados = list())
}

#' @noRd
detectar_tipo <- function(G) {
  E <- G$edges
  if (!length(G$lv)) return("path")
  regs <- E[E$tipo == "reg", ]
  if (nrow(regs) && any(c(regs$from, regs$to) %in% G$lv)) return("sem")
  if (length(G$core_ov) && nrow(regs)) return("sem")
  if (length(G$lvH)) return("segundo_orden")
  if (es_bifactor(G)) return("bifactor")
  "afc"
}

#' @noRd
es_bifactor <- function(G) {
  L <- G$edges[G$edges$tipo %in% c("carga", "cruzada"), ]
  if (length(G$lv1) < 3 || !nrow(L)) return(FALSE)
  cnt <- table(L$from)
  max(cnt) >= 0.8 * length(G$ind_ov) && sum(L$tipo == "cruzada") + sum(L$tipo == "carga") > length(G$ind_ov) * 1.5
}
