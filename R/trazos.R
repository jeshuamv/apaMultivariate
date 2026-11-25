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
  # Primero las curvas (covarianzas); luego las rectas pueden esquivarlas
  for (j in order(!E$tipo %in% c("cov", "covres"))) {
    a <- as.list(N[N$id == E$from[j], ]); b <- as.list(N[N$id == E$to[j], ])
    otros <- N[!N$id %in% c(E$from[j], E$to[j]), ]
    curvo <- E$tipo[j] %in% c("cov", "covres")
    reciproca <- E$tipo[j] == "reg" && any(E$from == E$to[j] & E$to == E$from[j] & E$tipo == "reg")
    mx <- (a$x + b$x) / 2; my <- (a$y + b$y) / 2; dx <- b$x - a$x; dy <- b$y - a$y

    if (curvo || reciproca) {
      ks <- if (reciproca) 0.2 else if (E$tipo[j] == "covres") c(0.35, 0.5, 0.7) else c(0.22, 0.35, 0.5, 0.7)
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
      # Puntos de las flechas ya trazadas, para no encimarse con ellas
      hechos <- Filter(Negate(is.null), out)
      HX <- unlist(lapply(hechos, `[[`, "x")); HY <- unlist(lapply(hechos, `[[`, "y"))
      encima <- function(q) if (!length(HX)) 0 else
        sum(vapply(8:32, function(i) min((HX - q$x[i])^2 + (HY - q$y[i])^2) < 0.12^2, logical(1)))
      # Un efecto que atraviesa otra caja (o corre encima de otra flecha) se curva
      # hacia el lado con menos cruces
      if (E$tipo[j] %in% c("reg", "cruzada") &&
          (choca_con(p$x, p$y, otros, holgura = 0.12) || encima(p) > 3)) {
        S <- do.call(rbind, lapply(setdiff(seq_len(nrow(E)), j), function(q) {
          if (any(c(E$from[q], E$to[q]) %in% c(E$from[j], E$to[j]))) return(NULL)
          u <- N[N$id == E$from[q], ]; v <- N[N$id == E$to[q], ]; c(u$x, u$y, v$x, v$y) }))
        ncruces <- function(q) if (is.null(S)) 0 else
          sum(apply(S, 1, function(z) any(vapply(seq(1, 37, 3), function(i)
            cruza(c(q$x[i], q$y[i]), c(q$x[i + 3], q$y[i + 3]), z[1:2], z[3:4]), logical(1)))))
        mejor <- if (choca_con(p$x, p$y, otros, holgura = 0.08)) Inf else ncruces(p) + 0.4 * encima(p)
        for (k in c(0.15, 0.25, 0.35, 0.5)) for (sg in c(1, -1)) {
          ctrl <- c(mx - sg * k * dy, my + sg * k * dx)
          q <- bezier(borde(a, ctrl[1], ctrl[2]), ctrl, borde(b, ctrl[1], ctrl[2]))
          if (choca_con(q$x, q$y, otros, holgura = 0.08)) next
          sc <- ncruces(q) + k + 0.4 * encima(q)
          if (sc < mejor) { mejor <- sc; p <- q }
        }
      }
    }
    li <- round((if (E$tipo[j] %in% c("carga", "cruzada")) 0.55 else 0.5) * 39) + 1
    out[[j]] <- list(x = p$x, y = p$y, lx = p$x[li], ly = p$y[li], doble = curvo, tipo = E$tipo[j])
  }
  out
}

# Elige la esquina de cada variable donde su R2 estorbe menos. Los ítems de un
# mismo factor usan todos la misma esquina (como en AMOS), para que cada valor
# se lea junto a su ítem y no se confunda con el del vecino.
#' @noRd
posiciones_r2 <- function(G, tr, op) {
  N <- G$nodes
  if (!(op$mostrar_r2 && length(G$r2))) return(NULL)
  ids <- intersect(names(G$r2)[!is.na(G$r2)], N$id)      # sin R² calculable (p. ej., caso Heywood): no se escribe "NA"
  if (!length(ids)) return(NULL)
  px <- unlist(lapply(tr, `[[`, "x")); py <- unlist(lapply(tr, `[[`, "y"))
  lx <- vapply(tr, `[[`, numeric(1), "lx"); ly <- vapply(tr, `[[`, numeric(1), "ly")
  out <- NULL
  esquinas_rect <- rbind(c(1, 1), c(-1, 1), c(1, -1), c(-1, -1))
  esquinas_circ <- rbind(c(0, 1), c(0, -1), c(1, 1), c(-1, 1), c(1, -1), c(-1, -1))
  lugar <- function(n, k, cand) c(n$x + cand[k, 1] * (n$a + 0.14), n$y + cand[k, 2] * (n$b + if (cand[k, 1] == 0) 0.13 else 0.1))
  libre <- function(v, n, cxy, otros_r2) {
    cx <- cxy[1]; cy <- cxy[2]
    d_lin <- min(sqrt((px - cx)^2 / 0.5 + (py - cy)^2), 9)
    d_lab <- if (length(lx)) min(sqrt((lx - cx)^2 / 0.5 + (ly - cy)^2)) else 9
    otros <- N[N$id != v, ]
    # holgura entre el texto de la R2 (aprox. 0.26 x 0.16) y las demás figuras
    d_nod <- if (nrow(otros)) min(pmax(abs(otros$x - cx) - otros$a - 0.13, abs(otros$y - cy) - otros$b - 0.08)) else 9
    sc <- min(d_lin, d_lab * 0.9, d_nod + 0.25)
    if (!is.null(otros_r2) && nrow(otros_r2)) sc <- min(sc, min(sqrt((otros_r2$x - cx)^2 / 0.5 + (otros_r2$y - cy)^2)))
    sc
  }
  # 1) Ítems (rectángulos) agrupados por su factor: una sola esquina por grupo
  E <- G$edges
  cargas <- if (!is.null(E) && nrow(E)) E[E$tipo == "carga", , drop = FALSE] else NULL
  hechos <- character(0)
  if (!is.null(cargas) && nrow(cargas)) {
    rect_ids <- ids[N$forma[match(ids, N$id)] == "rect"]
    fac <- cargas$from[match(rect_ids, cargas$to)]
    rect_ids <- rect_ids[!is.na(fac)]; fac <- fac[!is.na(fac)]
    for (f in unique(fac)) {
      grupo <- rect_ids[fac == f]
      mejor <- -Inf; kk <- 1
      for (k in seq_len(nrow(esquinas_rect))) {
        pos <- lapply(grupo, function(v) lugar(N[N$id == v, ], k, esquinas_rect))
        sc <- min(vapply(seq_along(grupo), function(i) libre(grupo[i], N[N$id == grupo[i], ], pos[[i]], out), numeric(1))) - 0.04 * (k - 1)
        if (k == 1 && sc >= 0.22) { kk <- 1; break }    # posición preferida (arriba a la derecha) si está libre
        if (sc > mejor) { mejor <- sc; kk <- k }
      }
      for (v in grupo) {
        xy <- lugar(N[N$id == v, ], kk, esquinas_rect)
        out <- rbind(out, data.frame(id = v, x = xy[1], y = xy[2], valor = G$r2[[v]]))
      }
      hechos <- c(hechos, grupo)
    }
  }
  # 2) Las demás variables (factores, observadas de trayectorias, errores), una por una
  for (v in setdiff(ids, hechos)) {
    n <- N[N$id == v, ]
    circ <- n$forma == "circ"
    # en los círculos de error se prefiere arriba o abajo del círculo, para que
    # el valor no se confunda con el de otra figura
    cand <- if (circ) esquinas_circ else esquinas_rect
    mejor <- -Inf; sel <- NULL
    for (k in seq_len(nrow(cand))) {
      cxy <- lugar(n, k, cand)
      sc <- libre(v, n, cxy, out) - 0.04 * (k - 1)   # leve preferencia: arriba a la derecha
      if ((k == 1 || (circ && k == 2)) && sc >= 0.22) { sel <- cxy; break }   # posición preferida si está libre
      if (sc > mejor) { mejor <- sc; sel <- cxy }
    }
    out <- rbind(out, data.frame(id = v, x = sel[1], y = sel[2], valor = G$r2[[v]]))
  }
  out[match(ids, out$id), , drop = FALSE]
}
