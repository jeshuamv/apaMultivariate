# Estrategias de acomodo (posición de cada elemento del diagrama)

LADOS <- list(arriba = c(0, 1), abajo = c(0, -1), izquierda = c(-1, 0), derecha = c(1, 0))

ACOMODOS <- list(
  afc           = c("fila", "dos_filas", "arbol"),
  segundo_orden = c("fila", "dos_filas", "arbol"),
  bifactor      = c("bifactor", "fila", "dos_filas"),
  path          = c("izq_der", "arriba_abajo"),
  sem           = c("izq_der", "arriba_abajo")
)

#' @noRd
elegir_acomodo <- function(G, tipo) {
  n <- length(G$lv1)
  switch(tipo,
    afc = , segundo_orden = if (n <= 3 && !any(G$edges$tipo == "cruzada")) "fila" else "dos_filas",
    bifactor = "bifactor",
    path = {
      lev <- niveles(G$core_ov, G$edges)
      if (sum(lev == 0) >= 4) "arriba_abajo" else "izq_der"
    },
    sem = if (length(G$lv) == 1) "arriba_abajo" else "izq_der")
}

#' @noRd
ancho_bloque <- function(G, lat) {
  its <- names(G$principal)[G$principal == lat]
  w_lat <- 2 * G$nodes$a[G$nodes$id == lat]
  if (!length(its)) return(w_lat)
  a <- G$nodes$a[G$nodes$id %in% its]
  max(sum(2 * a + 0.22), w_lat)
}

# Coloca los ítems (y sus errores) de un latente en la dirección d
#' @noRd
colocar_items <- function(G, lat, d, gap_i = 0.75, gap_e = 0.5) {
  if (!is.null(G$lados[[lat]])) d <- LADOS[[G$lados[[lat]]]]
  N <- G$nodes; i <- which(N$id == lat)
  its <- names(G$principal)[G$principal == lat]
  if (!length(its)) return(G)
  dx <- d[1]; dy <- d[2]; px <- -dy; py <- dx
  if (px < -1e-9 || (abs(px) < 1e-9 && py > 0)) { px <- -px; py <- -py }
  li <- as.list(N[i, ])
  refs <- lapply(its, function(z) as.list(N[N$id == z, ]))
  anchos <- vapply(refs, function(r) 2 * soporte(r, px, py), numeric(1))
  centros <- cumsum(anchos + 0.22) - (anchos + 0.22) / 2
  off <- centros - sum(anchos + 0.22) / 2
  for (k in seq_along(its)) {
    r <- refs[[k]]; j <- which(N$id == its[k])
    dist_i <- extension(li, dx, dy) + gap_i + extension(r, dx, dy)
    N$x[j] <- li$x + dx * dist_i + px * off[k]; N$y[j] <- li$y + dy * dist_i + py * off[k]
    e <- G$de_err[its[k]]
    if (!is.na(e)) {
      je <- which(N$id == e)
      de <- dist_i + extension(r, dx, dy) + gap_e + N$a[je]
      N$x[je] <- li$x + dx * de + px * off[k]; N$y[je] <- li$y + dy * de + py * off[k]
    }
  }
  G$nodes <- N; G$dir[[lat]] <- d; G
}

# Coloca un error o perturbación en la dirección más despejada
#' @noRd
colocar_libre <- function(G, quien) {
  N <- G$nodes; e <- G$de_err[quien]; if (is.na(e)) return(G)
  n <- as.list(N[N$id == quien, ]); je <- which(N$id == e)
  puestos <- N[!is.na(N$x) & N$id != e & N$id != quien, ]
  E <- G$edges[G$edges$tipo %in% c("carga", "cruzada", "carga2", "reg", "cov"), ]
  segs <- do.call(rbind, lapply(seq_len(nrow(E)), function(j) {
    a <- N[N$id == E$from[j], ]; b <- N[N$id == E$to[j], ]
    if (is.na(a$x) || is.na(b$x)) NULL else c(a$x, a$y, b$x, b$y) }))
  mejor <- -Inf; sel <- NULL
  for (ang in seq(0, 315, 45) * pi / 180) {
    dx <- cos(ang); dy <- sin(ang)
    r <- extension(n, dx, dy) + 0.75
    cx <- n$x + dx * r; cy <- n$y + dy * r
    dn <- if (nrow(puestos)) min(sqrt((puestos$x - cx)^2 + (puestos$y - cy)^2) - pmax(puestos$a, puestos$b)) else 9
    ds <- if (!is.null(segs)) min(apply(segs, 1, function(s) dist_seg(cx, cy, s[1], s[2], s[3], s[4]))) else 9
    sc <- min(dn, ds * 1.2) + 0.05 * (abs(dx) > 0.9)
    if (sc > mejor) { mejor <- sc; sel <- c(cx, cy) }
  }
  N$x[je] <- sel[1]; N$y[je] <- sel[2]; G$nodes <- N; G
}

#' @noRd
finalizar <- function(G) {
  pend <- G$nodes$id[!G$nodes$tipo %in% c("err", "dist") & is.na(G$nodes$x)]
  if (length(pend)) stop("No se pudo ubicar: ", paste(pend, collapse = ", "), call. = FALSE)
  for (q in names(G$de_err)) {
    e <- G$de_err[[q]]; if (is.na(G$nodes$x[G$nodes$id == e])) G <- colocar_libre(G, q)
  }
  G
}

#' @noRd
fijar <- function(G, id, x, y) { G$nodes[G$nodes$id == id, c("x", "y")] <- c(x, y); G }

# --- AFC y segundo orden -------------------------------------------------
#' @noRd
acomodo_fila <- function(G, items_arriba = TRUE) {
  d <- if (items_arriba) c(0, 1) else c(0, -1)
  x <- 0; f <- G$lv1
  for (k in seq_along(f)) {
    w <- ancho_bloque(G, f[k]); if (k > 1) x <- x + w / 2
    G <- fijar(G, f[k], x, 0); x <- x + w / 2 + 0.6
    G <- colocar_items(G, f[k], d)
  }
  for (h in G$lvH) {
    hijos <- G$edges$to[G$edges$from == h & G$edges$tipo == "carga2"]
    G <- fijar(G, h, mean(G$nodes$x[G$nodes$id %in% hijos]), if (items_arriba) -2.6 else 2.8)
  }
  G$curva_ref <- "items"; finalizar(G)
}

#' @noRd
acomodo_dos_filas <- function(G) {
  f <- G$lv1; n <- length(f)
  arriba <- f[seq_len(ceiling(n / 2))]; abajo <- setdiff(f, arriba)
  dy <- if (length(G$lvH)) 3.4 else 3.0
  for (fila in list(list(v = arriba, y = dy / 2, d = c(0, 1)), list(v = abajo, y = -dy / 2, d = c(0, -1)))) {
    if (!length(fila$v)) next
    w <- vapply(fila$v, function(l) ancho_bloque(G, l), numeric(1)); paso <- max(w) + 1.3
    xs <- (seq_along(fila$v) - (length(fila$v) + 1) / 2) * paso
    for (k in seq_along(fila$v)) {
      G <- fijar(G, fila$v[k], xs[k], fila$y)
      G <- colocar_items(G, fila$v[k], fila$d)
    }
  }
  for (h in G$lvH) G <- fijar(G, h, 0, 0)
  G$curva_ref <- "items"; finalizar(G)
}

#' @noRd
acomodo_bifactor <- function(G) {
  L <- G$edges[G$edges$tipo %in% c("carga", "cruzada"), ]
  cnt <- table(L$from); gl <- names(cnt)[which.max(cnt)]
  esp <- setdiff(G$lv1, gl)
  orden <- unique(unlist(lapply(esp, function(s) L$to[L$from == s])))
  orden <- c(orden, setdiff(G$ind_ov, orden))
  x <- 0
  for (it in orden) {
    a <- G$nodes$a[G$nodes$id == it]
    G <- fijar(G, it, x, 0)
    e <- G$de_err[it]; if (!is.na(e)) G <- fijar(G, e, x + a + 0.55, 0)
    x <- x + 2 * a + 0.95
  }
  for (s in esp) G <- fijar(G, s, mean(G$nodes$x[G$nodes$id %in% L$to[L$from == s]]), -2.4)
  G <- fijar(G, gl, mean(G$nodes$x[G$nodes$id %in% orden]), 2.6)
  G$curva_ref <- "centro"; finalizar(G)
}

# --- Path y SEM (por niveles causales) -----------------------------------
#' @noRd
acomodo_capas <- function(G, sentido = "LR", items = "afuera") {
  core <- c(G$core_ov, G$lv1, G$lvH)
  lev <- niveles(core, G$edges); pos <- ordenar_niveles(lev, G$edges)
  nl <- max(lev)
  dir_de <- function(v) {
    if (!v %in% G$lv1) return(NULL)
    l <- lev[[v]]; enniv <- sum(lev == l); arriba <- pos[[v]] <= (enniv + 1) / 2
    if (items == "afuera") {
      if (l == 0 && nl > 0) return(if (sentido == "LR") c(-1, 0) else c(0, 1))
      if (l == nl && nl > 0) return(if (sentido == "LR") c(1, 0) else c(0, -1))
    }
    if (sentido == "LR") (if (arriba) c(0, 1) else c(0, -1)) else (if (arriba) c(-1, 0) else c(1, 0))
  }
  prof_item <- 0.75 + 0.34 + 0.5 + 0.4
  ext <- lapply(core, function(v) {
    d <- dir_de(v); n <- G$nodes[G$nodes$id == v, ]
    if (is.null(d)) return(c(2 * n$a, 2 * n$b))
    wb <- ancho_bloque(G, v)
    if (d[1] == 0) c(max(wb, 2 * n$a), 2 * n$b + 2 * prof_item)
    else c(2 * n$a + 2 * (prof_item + 0.5), max(wb * 0.5, 2 * n$b))
  }); names(ext) <- core
  if (sentido == "LR") {
    anch <- vapply(0:nl, function(l) max(vapply(core[lev == l], function(v) ext[[v]][1], numeric(1))), numeric(1))
    xs <- cumsum(c(0, utils::head(anch, -1) / 2 + anch[-1] / 2 + 1.6))
    for (l in 0:nl) {
      v <- core[lev == l]; v <- v[order(pos[v])]
      alt <- vapply(v, function(z) ext[[z]][2], numeric(1)); tot <- sum(alt) + 0.9 * (length(v) - 1)
      ys <- tot / 2 - cumsum(alt) + alt / 2 - 0.9 * (seq_along(v) - 1)
      for (k in seq_along(v)) G <- fijar(G, v[k], xs[l + 1], ys[k])
    }
  } else {
    alts <- vapply(0:nl, function(l) max(vapply(core[lev == l], function(v) ext[[v]][2], numeric(1))), numeric(1))
    ys <- -cumsum(c(0, utils::head(alts, -1) / 2 + alts[-1] / 2 + 1.2))
    for (l in 0:nl) {
      v <- core[lev == l]; v <- v[order(pos[v])]
      anc <- vapply(v, function(z) ext[[z]][1], numeric(1)); tot <- sum(anc) + 0.9 * (length(v) - 1)
      xs <- -tot / 2 + cumsum(anc) - anc / 2 + 0.9 * (seq_along(v) - 1)
      for (k in seq_along(v)) G <- fijar(G, v[k], xs[k], ys[l + 1])
    }
  }
  for (v in G$lv1) G <- colocar_items(G, v, dir_de(v))
  G$curva_ref <- "centro"; finalizar(G)
}

#' @noRd
bloque_caja <- function(G, lat, d) {
  H <- colocar_items(G, lat, d)
  ids <- names(G$principal)[G$principal == lat]
  ids <- c(ids, stats::na.omit(G$de_err[ids]))
  B <- H$nodes[H$nodes$id %in% ids, ]
  c(min(B$x - B$a), max(B$x + B$a), min(B$y - B$b), max(B$y + B$b))
}

#' @noRd
seg_en_caja <- function(x1, y1, x2, y2, cj) {
  t <- seq(0, 1, length.out = 30); xs <- x1 + t * (x2 - x1); ys <- y1 + t * (y2 - y1)
  any(xs > cj[1] & xs < cj[2] & ys > cj[3] & ys < cj[4])
}

# SEM: cada constructo pone sus ítems hacia el lado donde estorben menos
#' @noRd
acomodo_capas_libre <- function(G) {
  G <- acomodo_capas(G, "LR", "vertical")
  core <- c(G$core_ov, G$lv1, G$lvH)
  E <- G$edges[G$edges$tipo %in% c("reg", "cov", "carga2") & G$edges$from %in% core & G$edges$to %in% core, ]
  N <- G$nodes
  limpiar <- c(G$ind_ov, stats::na.omit(G$de_err[c(G$ind_ov, core)]))
  G$nodes$x[G$nodes$id %in% limpiar] <- NA; G$nodes$y[G$nodes$id %in% limpiar] <- NA
  cajas <- list()
  pref <- c(arriba = 0, abajo = 0.1, izquierda = 0.3, derecha = 0.3)
  for (v in G$lv1) {
    if (!is.null(G$lados[[v]])) { sel <- G$lados[[v]] } else {
      mejor <- Inf; sel <- NULL
      for (dn in names(LADOS)) {
        cj <- bloque_caja(G, v, LADOS[[dn]]); sc <- pref[[dn]]
        for (j in seq_len(nrow(E))) {
          a <- N[N$id == E$from[j], ]; b <- N[N$id == E$to[j], ]
          if (seg_en_caja(a$x, a$y, b$x, b$y, cj)) sc <- sc + 3
        }
        for (q in core[core != v]) {
          n <- N[N$id == q, ]
          if (n$x + n$a > cj[1] && n$x - n$a < cj[2] && n$y + n$b > cj[3] && n$y - n$b < cj[4]) sc <- sc + 10
        }
        for (c2 in cajas) if (cj[1] < c2[2] && c2[1] < cj[2] && cj[3] < c2[4] && c2[3] < cj[4]) sc <- sc + 10
        if (sc < mejor) { mejor <- sc; sel <- dn }
      }
    }
    cajas[[v]] <- bloque_caja(G, v, LADOS[[sel]])
    G <- colocar_items(G, v, LADOS[[sel]])
  }
  G$curva_ref <- "centro"; finalizar(G)
}

#' @noRd
aplicar_acomodo <- function(G, tipo, acomodo) {
  switch(acomodo,
    fila      = acomodo_fila(G, TRUE),
    arbol     = acomodo_fila(G, FALSE),
    dos_filas = acomodo_dos_filas(G),
    bifactor  = acomodo_bifactor(G),
    izq_der   = if (tipo == "sem") acomodo_capas_libre(G) else acomodo_capas(G, "LR", "afuera"),
    arriba_abajo = acomodo_capas(G, "TB", "afuera"))
}

# Desplaza nodos a mano; un constructo se lleva sus ítems y errores
#' @noRd
aplicar_mover <- function(G, mover) {
  if (is.null(mover)) return(G)
  if (!is.list(mover) || is.null(names(mover)))
    stop("`mover` debe ser una lista con nombres, p. ej. list(Care = c(1, 0)).", call. = FALSE)
  for (id in names(mover)) {
    real <- G$nodes$id[G$nodes$id == id | G$nodes$etiqueta == id][1]
    if (is.na(real)) stop("`mover`: no existe el elemento '", id, "'.", call. = FALSE)
    d <- mover[[id]]
    if (!is.numeric(d) || length(d) != 2) stop("`mover`: cada desplazamiento debe ser c(dx, dy).", call. = FALSE)
    grupo <- real
    if (real %in% G$lv) {
      its <- names(G$principal)[G$principal == real]
      grupo <- c(grupo, its, stats::na.omit(G$de_err[c(real, its)]))
    } else grupo <- c(grupo, stats::na.omit(G$de_err[real]))
    sel <- G$nodes$id %in% grupo
    G$nodes$x[sel] <- G$nodes$x[sel] + d[1]; G$nodes$y[sel] <- G$nodes$y[sel] + d[2]
  }
  G
}
