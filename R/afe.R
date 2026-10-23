# Análisis factorial exploratorio: lectura de psych::fa() y lavaan::efa()

#' @noRd
es_afe <- function(x) {
  if (inherits(x, "fa") || inherits(x, "efaList")) return(TRUE)
  if (inherits(x, "lavaan")) {
    pt <- lavaan::parTable(x)
    return("efa" %in% names(pt) && any(nzchar(pt$efa)))
  }
  FALSE
}

# Convierte un AFE a un formato común:
# L (cargas, ítems x factores), h2, Phi (correlaciones entre factores o NULL),
# varianza explicada y datos del método.
#' @noRd
leer_afe <- function(x, nfactores = NULL) {
  if (inherits(x, "efaList")) {
    x <- Filter(function(f) inherits(f, "lavaan"), unclass(x))
    nf <- vapply(x, function(f) length(lavaan::lavNames(f, "lv")), numeric(1))
    if (length(x) > 1 && is.null(nfactores))
      stop("El objeto contiene soluciones con ", paste(nf, collapse = ", "),
           " factores. Indica cu\u00e1l dibujar con `nfactores`, p. ej. nfactores = ", nf[length(nf)], ".",
           call. = FALSE)
    i <- if (is.null(nfactores)) 1 else match(nfactores, nf)
    if (is.na(i)) stop("No hay una soluci\u00f3n con ", nfactores, " factores.", call. = FALSE)
    x <- x[[i]]
  }
  if (inherits(x, "fa")) {
    L <- unclass(x$loadings); L <- matrix(L, nrow = nrow(L), dimnames = dimnames(x$loadings))
    Phi <- x$Phi
    if (!is.null(Phi)) dimnames(Phi) <- list(colnames(L), colnames(L))
    h2 <- if (!is.null(x$communality)) x$communality else rowSums(L^2)
    metodo <- paste0("psych::fa (", x$fm, ", rotaci\u00f3n ", x$rotation, ")")
    n <- x$n.obs
  } else if (inherits(x, "lavaan")) {
    s <- lavaan::standardizedSolution(x)
    s <- s[s$op == "=~", ]
    fac <- unique(s$lhs); its <- unique(s$rhs)
    L <- matrix(0, length(its), length(fac), dimnames = list(its, fac))
    for (j in seq_len(nrow(s))) L[s$rhs[j], s$lhs[j]] <- s$est.std[j]
    Phi <- lavaan::lavInspect(x, "cor.lv")
    if (all(abs(Phi[upper.tri(Phi)]) < 1e-8)) Phi <- NULL
    h2 <- lavaan::lavInspect(x, "r2")[its]
    rot <- tryCatch(lavaan::lavInspect(x, "options")$rotation, error = function(e) "")
    metodo <- paste0("lavaan::efa (rotaci\u00f3n ", rot, ")")
    n <- lavaan::lavInspect(x, "nobs")
  } else stop("No se reconoce el objeto de an\u00e1lisis exploratorio.", call. = FALSE)

  # Varianza explicada (igual que psych: con Phi en rotaciones oblicuas)
  ss <- if (is.null(Phi)) colSums(L^2) else diag(Phi %*% t(L) %*% L)
  p <- nrow(L)
  list(L = L, h2 = h2, Phi = Phi, ss = ss, prop = ss / p, acum = cumsum(ss / p),
       metodo = metodo, n = n)
}

# Estructura "G" del diagrama a partir de un AFE
#' @noRd
estructura_afe <- function(A, umbral, etiquetas, prefijo_error, mostrar_errores) {
  L <- A$L; fac <- colnames(L); its <- rownames(L)
  etq <- function(v) if (!is.null(etiquetas) && v %in% names(etiquetas)) as.character(etiquetas[[v]]) else v
  principal <- stats::setNames(fac[apply(abs(L), 1, which.max)], its)
  # orden: por factor y, dentro de cada factor, de mayor a menor carga
  carga_p <- abs(L[cbind(its, principal)])
  ord <- order(match(principal, fac), -carga_p)
  its <- its[ord]; principal <- principal[its]

  filas <- list()
  agregar <- function(id, tipo, forma, lab, ab)
    filas[[length(filas) + 1]] <<- data.frame(id = id, etiqueta = lab, tipo = tipo, forma = forma,
                                             a = ab[1], b = ab[2], stringsAsFactors = FALSE)
  for (f in fac) { l <- etq(f); agregar(f, "lv", "elip", l, tam_lv(l)) }
  for (v in its) { l <- etq(v); agregar(v, "item", "rect", l, tam_ov(l, 0.17)) }
  err_de <- character(0)
  if (mostrar_errores) for (k in seq_along(its)) {
    id <- paste0(".err", k); agregar(id, "err", "circ", paste0(prefijo_error, k), c(0.2, 0.2)); err_de[id] <- its[k]
  }
  nodes <- do.call(rbind, filas); nodes$x <- NA_real_; nodes$y <- NA_real_

  ar <- list()
  agregar_a <- function(f, t, tipo, val, debil = FALSE)
    ar[[length(ar) + 1]] <<- data.frame(from = f, to = t, tipo = tipo, val = val, p = NA_real_,
                                       fijo = TRUE, debil = debil, stringsAsFactors = FALSE)
  debiles <- character(0)
  for (v in its) for (f in fac) {
    l <- L[v, f]
    if (f == principal[[v]]) {
      agregar_a(f, v, "carga", l, debil = abs(l) < umbral)
      if (abs(l) < umbral) debiles <- c(debiles, v)
    } else if (abs(l) >= umbral) agregar_a(f, v, "cruzada", l)
  }
  if (!is.null(A$Phi)) for (i in seq_along(fac)) for (j in seq_along(fac)) if (j > i)
    agregar_a(fac[i], fac[j], "cov", A$Phi[i, j])
  for (e in names(err_de)) agregar_a(e, err_de[[e]], "error", NA_real_)
  edges <- do.call(rbind, ar)

  list(nodes = nodes, edges = edges, r2 = A$h2[its], lv = fac, lv1 = fac, lvH = character(0),
       core_ov = character(0), ind_ov = its, principal = principal,
       err_de = err_de, de_err = stats::setNames(names(err_de), err_de), lados = list(),
       debiles = debiles)
}
