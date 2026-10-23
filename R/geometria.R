# Utilidades geométricas internas

#' @noRd
extension <- function(n, dx, dy) {
  if (n$forma == "rect") min(n$a / max(abs(dx), 1e-9), n$b / max(abs(dy), 1e-9))
  else 1 / sqrt((dx / n$a)^2 + (dy / n$b)^2)
}

#' @noRd
soporte <- function(n, dx, dy) {
  if (n$forma == "rect") abs(dx) * n$a + abs(dy) * n$b else sqrt((dx * n$a)^2 + (dy * n$b)^2)
}

#' @noRd
borde <- function(n, tx, ty) {
  dx <- tx - n$x; dy <- ty - n$y; d <- sqrt(dx^2 + dy^2)
  if (d < 1e-9) return(c(n$x, n$y))
  dx <- dx / d; dy <- dy / d
  s <- extension(n, dx, dy); c(n$x + s * dx, n$y + s * dy)
}

#' @noRd
nodo <- function(G, id) as.list(G$nodes[G$nodes$id == id, ])

#' @noRd
dist_seg <- function(px, py, x1, y1, x2, y2) {
  vx <- x2 - x1; vy <- y2 - y1
  t <- ((px - x1) * vx + (py - y1) * vy) / max(vx^2 + vy^2, 1e-9)
  t <- max(0, min(1, t)); sqrt((px - x1 - t * vx)^2 + (py - y1 - t * vy)^2)
}

#' @noRd
cruza <- function(p1, p2, p3, p4) {
  o <- function(a, b, c) sign((b[1] - a[1]) * (c[2] - a[2]) - (b[2] - a[2]) * (c[1] - a[1]))
  o(p1, p2, p3) * o(p1, p2, p4) < 0 && o(p3, p4, p1) * o(p3, p4, p2) < 0
}

#' @noRd
bezier <- function(p1, ctrl, p2, n = 40) {
  t <- seq(0, 1, length.out = n)
  list(x = (1 - t)^2 * p1[1] + 2 * (1 - t) * t * ctrl[1] + t^2 * p2[1],
       y = (1 - t)^2 * p1[2] + 2 * (1 - t) * t * ctrl[2] + t^2 * p2[2])
}

# Niveles causales (camino más largo desde las fuentes); las relaciones
# recíprocas quedan en el mismo nivel.
#' @noRd
niveles <- function(ids, edges) {
  E <- edges[edges$tipo %in% c("reg", "carga2") & edges$from %in% ids & edges$to %in% ids, ]
  recip <- vapply(seq_len(nrow(E)), function(j) any(E$from == E$to[j] & E$to == E$from[j]), logical(1))
  lev <- stats::setNames(rep(0, length(ids)), ids)
  for (it in seq_along(ids)) {
    cambio <- FALSE
    for (j in which(!recip)) {
      nv <- lev[[E$from[j]]] + 1
      if (nv > lev[[E$to[j]]] && nv <= length(ids)) { lev[[E$to[j]]] <- nv; cambio <- TRUE }
    }
    if (!cambio) break
  }
  for (j in which(recip)) {
    m <- max(lev[[E$from[j]]], lev[[E$to[j]]]); lev[[E$from[j]]] <- m; lev[[E$to[j]]] <- m
  }
  lev - min(lev)
}

# Orden dentro de cada nivel por baricentro (reduce cruces)
#' @noRd
ordenar_niveles <- function(lev, edges, pasadas = 4) {
  E <- edges[edges$tipo %in% c("reg", "carga2", "cov"), ]
  pos <- stats::setNames(numeric(length(lev)), names(lev))
  for (l in unique(lev)) { v <- names(lev)[lev == l]; pos[v] <- seq_along(v) }
  for (p in seq_len(pasadas)) for (l in sort(unique(lev))) {
    v <- names(lev)[lev == l]
    bc <- vapply(v, function(x) {
      vec <- c(E$from[E$to == x], E$to[E$from == x]); vec <- vec[vec %in% names(lev)]
      vec <- vec[lev[vec] != l]
      if (length(vec)) mean(pos[vec]) else pos[[x]] }, numeric(1))
    pos[v[order(bc)]] <- seq_along(v)
  }
  pos
}
