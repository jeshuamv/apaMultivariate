# Trazo de cada flecha (polilínea) y posición de su etiqueta

#' @noRd
choca_con <- function(px, py, otros, rango = 3:38, holgura = NULL) {
  if (!nrow(otros)) return(FALSE)
  any(vapply(seq_len(nrow(otros)), function(q) {
    ha <- if (is.null(holgura)) otros$a[q] * 0.95 else otros$a[q] + holgura
    hb <- if (is.null(holgura)) otros$b[q] * 0.95 else otros$b[q] + holgura
    any(abs(px[rango] - otros$x[q]) < ha & abs(py[rango] - otros$y[q]) < hb) }, logical(1)))
}

#' @noRd
trazar <- function(G) {
  N <- G$nodes; E <- G$edges
  cen <- c(mean(N$x), mean(N$y))
  out <- vector("list", nrow(E))
  for (j in seq_len(nrow(E))) {
    a <- as.list(N[N$id == E$from[j], ]); b <- as.list(N[N$id == E$to[j], ])
    otros <- N[!N$id %in% c(E$from[j], E$to[j]), ]
    curvo <- E$tipo[j] %in% c("cov", "covres")
    reciproca <- E$tipo[j] == "reg" && any(E$from == E$to[j] & E$to == E$from[j] & E$tipo == "reg")
    mx <- (a$x + b$x) / 2; my <- (a$y + b$y) / 2; dx <- b$x - a$x; dy <- b$y - a$y

    if (curvo || reciproca) {
      ks <- if (reciproca) 0.12 else if (E$tipo[j] == "covres") c(0.35, 0.5, 0.7) else c(0.22, 0.35, 0.5, 0.7)
      for (k in ks) {
        c1 <- c(mx - k * dy, my + k * dx); c2 <- c(mx + k * dy, my - k * dx)
        if (reciproca) ctrl <- c1 else {
          ids <- c(E$from[j], E$to[j])
          if (E$tipo[j] == "covres" && all(ids %in% names(G$err_de))) ids <- unname(G$err_de[ids])
          if (E$tipo[j] == "covres") ref <- c(mean(N$x[N$id %in% ids]), mean(N$y[N$id %in% ids])) else {
            its <- names(G$principal)[G$principal %in% ids]
            its <- c(its, stats::na.omit(G$de_err[its]))
            I <- N[N$id %in% its, ]
            ref <- if (nrow(I) && identical(G$curva_ref, "items")) c(mean(I$x), mean(I$y)) else cen
          }
          if (all(abs(ref - c(mx, my)) < 1e-6)) ref <- cen
          ctrl <- if (sum((c1 - ref)^2) > sum((c2 - ref)^2)) c1 else c2
        }
        p <- bezier(borde(a, ctrl[1], ctrl[2]), ctrl, borde(b, ctrl[1], ctrl[2]))
        if (!choca_con(p$x, p$y, otros, 4:37)) break
      }
    } else {
      p1 <- borde(a, b$x, b$y); p2 <- borde(b, a$x, a$y)
      t <- seq(0, 1, length.out = 40)
      p <- list(x = p1[1] + t * (p2[1] - p1[1]), y = p1[2] + t * (p2[2] - p1[2]))
      # Un efecto que atraviesa otra caja se curva hacia el lado con menos cruces
      if (E$tipo[j] == "reg" && choca_con(p$x, p$y, otros, holgura = 0.12)) {
        S <- do.call(rbind, lapply(setdiff(seq_len(nrow(E)), j), function(q) {
          if (any(c(E$from[q], E$to[q]) %in% c(E$from[j], E$to[j]))) return(NULL)
          u <- N[N$id == E$from[q], ]; v <- N[N$id == E$to[q], ]; c(u$x, u$y, v$x, v$y) }))
        ncruces <- function(q) if (is.null(S)) 0 else
          sum(apply(S, 1, function(z) any(vapply(seq(1, 37, 3), function(i)
            cruza(c(q$x[i], q$y[i]), c(q$x[i + 3], q$y[i + 3]), z[1:2], z[3:4]), logical(1)))))
        mejor <- Inf
        for (k in c(0.15, 0.25, 0.35, 0.5)) for (sg in c(1, -1)) {
          ctrl <- c(mx - sg * k * dy, my + sg * k * dx)
          q <- bezier(borde(a, ctrl[1], ctrl[2]), ctrl, borde(b, ctrl[1], ctrl[2]))
          if (choca_con(q$x, q$y, otros, holgura = 0.08)) next
          sc <- ncruces(q) + k
          if (sc < mejor) { mejor <- sc; p <- q }
        }
      }
    }
    li <- round((if (E$tipo[j] %in% c("carga", "cruzada")) 0.55 else 0.5) * 39) + 1
    out[[j]] <- list(x = p$x, y = p$y, lx = p$x[li], ly = p$y[li], doble = curvo, tipo = E$tipo[j])
  }
  out
}

# Elige la esquina de cada variable donde su R2 estorbe menos
#' @noRd
posiciones_r2 <- function(G, tr, op) {
  N <- G$nodes
  if (!(op$mostrar_r2 && op$estandarizado && length(G$r2))) return(NULL)
  ids <- intersect(names(G$r2), N$id)
  if (!length(ids)) return(NULL)
  px <- unlist(lapply(tr, `[[`, "x")); py <- unlist(lapply(tr, `[[`, "y"))
  lx <- vapply(tr, `[[`, numeric(1), "lx"); ly <- vapply(tr, `[[`, numeric(1), "ly")
  out <- NULL
  for (v in ids) {
    n <- N[N$id == v, ]
    cand <- rbind(c(1, 1), c(-1, 1), c(1, -1), c(-1, -1))
    mejor <- -Inf; sel <- NULL
    for (k in seq_len(nrow(cand))) {
      cx <- n$x + cand[k, 1] * (n$a + 0.14); cy <- n$y + cand[k, 2] * (n$b + 0.1)
      d_lin <- min(sqrt((px - cx)^2 / 0.5 + (py - cy)^2), 9)
      d_lab <- if (length(lx)) min(sqrt((lx - cx)^2 / 0.5 + (ly - cy)^2)) else 9
      otros <- N[N$id != v, ]
      # holgura entre el texto de la R2 (aprox. 0.26 x 0.16) y las demás figuras
      d_nod <- if (nrow(otros)) min(pmax(abs(otros$x - cx) - otros$a - 0.13, abs(otros$y - cy) - otros$b - 0.08)) else 9
      sc <- min(d_lin, d_lab * 0.9, d_nod + 0.25) - 0.04 * (k - 1)   # leve preferencia: arriba a la derecha
      if (!is.null(out)) sc <- min(sc, min(sqrt((out$x - cx)^2 / 0.5 + (out$y - cy)^2)))
      if (k == 1 && sc >= 0.22) { sel <- c(cx, cy); break }   # arriba a la derecha si está libre
      if (sc > mejor) { mejor <- sc; sel <- c(cx, cy) }
    }
    out <- rbind(out, data.frame(id = v, x = sel[1], y = sel[2], valor = G$r2[[v]]))
  }
  out
}
