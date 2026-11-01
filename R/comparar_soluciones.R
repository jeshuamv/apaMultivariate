# AFE: comparar soluciones con distinto número de factores

#' Comparar soluciones del AFE con distinto número de factores
#'
#' Estima el análisis factorial exploratorio con varios números de factores
#' (por defecto, los que sugieren los criterios de [tabla_factorabilidad()]:
#' análisis paralelo, MAP y Kaiser) y los compara en una tabla:
#'
#' * varianza explicada acumulada;
#' * ítems sin ninguna carga destacada (menor que `umbral`);
#' * ítems con cargas cruzadas (dos o más cargas \eqn{\ge} .30 y una
#'   diferencia menor que .20 entre las dos mayores);
#' * factores con menos de tres ítems, que suelen ser inestables;
#' * correlación más alta entre factores (si pasa de .80, quizá sobran
#'   factores);
#' * índices de ajuste del método de extracción (RMSEA, TLI y BIC, con
#'   `fm = "ml"`).
#'
#' También dibuja el diagrama de cada solución. Elige la que tenga sentido
#' teórico y una estructura clara (cada ítem en un factor, al menos tres ítems
#' por factor); los números ayudan, pero no deciden por ti.
#'
#' @param datos `data.frame` con los ítems.
#' @param nfactores Números de factores que se comparan (por ejemplo,
#'   `2:4`). Por defecto, los que sugieren los criterios, más uno arriba y uno
#'   abajo si todos coinciden.
#' @param rotate,fm Rotación y método de extracción, como en [psych::fa()].
#' @param cor `"pearson"` o `"policorica"` (ítems ordinales), como en
#'   [tabla_factorabilidad()].
#' @param umbral Carga mínima para considerar que un ítem pertenece a un
#'   factor.
#' @param dibujar Si es `TRUE`, dibuja el diagrama de cada solución.
#' @param archivo Si se indica (por ejemplo, `"afe.png"`), guarda cada
#'   diagrama con el número de factores agregado (`afe_3f.png`, ...).
#' @return Un objeto `comparacion_afe` con `$tabla` (tabla APA),
#'   `$soluciones` (los resultados de `psych::fa()`, por número de factores)
#'   y `$diagramas`. Las cargas de cada solución se obtienen con, por
#'   ejemplo, `tabla_cargas(r$soluciones[["3"]])`.
#' @examples
#' \donttest{
#' if (requireNamespace("psych", quietly = TRUE) && requireNamespace("GPArotation", quietly = TRUE)) {
#'   r <- comparar_soluciones(lavaan::HolzingerSwineford1939[, paste0("x", 1:9)], 2:4, dibujar = FALSE)
#'   r
#' }
#' }
#' @export
comparar_soluciones <- function(datos, nfactores = NULL, rotate = "oblimin", fm = "ml", umbral = 0.40, cor = c("pearson", "policorica"),
                                dibujar = TRUE, archivo = NULL) {
  cor <- tipo_cor(cor)
  if (!requireNamespace("psych", quietly = TRUE))
    stop("Se necesita el paquete psych: install.packages(c(\"psych\", \"GPArotation\")).", call. = FALSE)
  nec <- setdiff(rotate, c("none", "varimax", "quartimax"))
  if (length(nec) && !requireNamespace("GPArotation", quietly = TRUE))
    stop("La rotaci\u00f3n \"", nec[1], "\" necesita el paquete GPArotation: install.packages(\"GPArotation\").", call. = FALSE)
  d <- obtener_datos(datos); d <- d[stats::complete.cases(d), , drop = FALSE]
  p <- ncol(d)
  sugeridos <- NULL
  if (is.null(nfactores)) {
    tf <- suppressMessages(tabla_factorabilidad(d, iteraciones = 50, fm = fm[1], cor = cor))
    sugeridos <- tf$datos$factores
    s <- sort(unique(sugeridos[sugeridos >= 1]))
    nfactores <- if (length(rotate) > 1 || length(fm) > 1) max(1, sugeridos[["paralelo_factores"]])
                 else if (length(s) == 1) s + (-1:1) else seq(min(s), max(s))
  }
  nfactores <- sort(unique(nfactores[nfactores >= 1 & nfactores <= floor(p / 2)]))
  if (!length(nfactores)) stop("No hay un n\u00famero de factores v\u00e1lido para comparar (como m\u00e1ximo, la mitad de los \u00edtems).", call. = FALSE)
  if (cor == "policorica") { Rpol <- matriz_cor(d, cor) } else Rpol <- NULL
  rejilla <- expand.grid(k = nfactores, fm = unique(fm), rotate = unique(rotate), stringsAsFactors = FALSE)
  rejilla <- rejilla[order(rejilla$k, match(rejilla$fm, fm), match(rejilla$rotate, rotate)), ]
  # con un factor no hay rotación: una sola fila por método
  rejilla$rotate[rejilla$k == 1] <- "none"; rejilla <- unique(rejilla)
  var_k <- length(nfactores) > 1; var_fm <- length(unique(fm)) > 1; var_rot <- length(unique(rejilla$rotate)) > 1
  nombre <- function(k, f, r) {
    x <- c(if (var_k || (!var_fm && !var_rot)) as.character(k), if (var_fm) f, if (var_rot) r)
    paste(x, collapse = "_")
  }
  sol <- list(); filas <- list(); asignaciones <- list()
  for (i in seq_len(nrow(rejilla))) {
    k <- rejilla$k[i]; f <- rejilla$fm[i]; r <- rejilla$rotate[i]
    a <- tryCatch(suppressWarnings(suppressMessages(if (is.null(Rpol)) psych::fa(d, nfactors = k, rotate = r, fm = f) else psych::fa(Rpol, nfactors = k, rotate = r, fm = f, n.obs = nrow(d)))),
                  error = function(e) NULL)
    if (is.null(a)) next
    nm <- nombre(k, f, r)
    sol[[nm]] <- a
    L <- unclass(a$loadings); L <- L[, seq_len(k), drop = FALSE]
    ab <- abs(L)
    sin_carga <- rownames(L)[apply(ab, 1, max) < umbral]
    orden <- t(apply(ab, 1, sort, decreasing = TRUE)); if (k == 1) orden <- t(orden)
    cruzadas <- if (k > 1) rownames(L)[orden[, 2] >= .30 & (orden[, 1] - orden[, 2]) < .20] else character(0)
    asign <- apply(ab, 1, which.max); asign[apply(ab, 1, max) < umbral] <- NA
    n_por <- tabulate(stats::na.omit(asign), nbins = k)
    # varianza común total = suma de comunalidades: no cambia con la rotación
    var_acum <- sum(a$communality) / p
    va <- a$Vaccounted
    por_f <- if (!is.null(va) && "Proportion Var" %in% rownames(va)) va["Proportion Var", seq_len(k)] else colSums(L^2) / p
    if (!is.null(a$Phi) && k > 1) { Phi <- a$Phi; rmax <- max(abs(Phi[upper.tri(Phi)])) } else rmax <- NA
    heywood <- any(a$communality >= .999)
    mx <- apply(ab, 1, which.max)
    asignaciones[[nm]] <- vapply(seq_len(nrow(L)), function(j) paste0(colnames(L)[mx[j]], " (", f_apa(L[j, mx[j]], 2), ")"), character(1))
    filas[[length(filas) + 1]] <- data.frame(nombre = nm, k = k, fm = f, rotate = r, var = var_acum,
      por_factor = paste(f_num(100 * por_f, 1), collapse = "/"), sin = length(sin_carga), cruz = length(cruzadas),
      pocos = sum(n_por < 3), rmax = rmax,
      rmsea = if (!is.null(a$RMSEA)) unname(a$RMSEA[1]) else NA, tli = if (!is.null(a$TLI)) a$TLI else NA,
      bic = if (!is.null(a$BIC)) a$BIC else NA, heywood = heywood,
      items_sin = paste(sin_carga, collapse = ", "), items_cruz = paste(cruzadas, collapse = ", "), stringsAsFactors = FALSE)
  }
  if (!length(filas)) stop("No se pudo estimar ninguna soluci\u00f3n.", call. = FALSE)
  D <- do.call(rbind, filas); rownames(D) <- NULL
  guion <- function(x, f) ifelse(is.na(x), "\u2014", f(x))
  rot_txt <- function(r) ifelse(r == "none", tx("sin rotar", "unrotated"), r)
  id <- cbind(if (var_k || (!var_fm && !var_rot)) formatC(D$k, format = "d"), if (var_fm) D$fm, if (var_rot) rot_txt(D$rotate))
  cab_id <- c(if (var_k || (!var_fm && !var_rot)) tx("Factores", "Factors"), if (var_fm) tx("Extracci\u00f3n", "Extraction"), if (var_rot) tx("Rotaci\u00f3n", "Rotation"))
  cel <- cbind(id, f_num(100 * D$var, 1), if (var_rot) D$por_factor, formatC(D$sin, format = "d"), formatC(D$cruz, format = "d"),
               formatC(D$pocos, format = "d"), guion(D$rmax, function(x) f_apa(x, 2)),
               guion(D$rmsea, function(x) f_apa(x, 3)), guion(D$tli, function(x) f_apa(x, 3)), guion(D$bic, function(x) f_num(x, 1)))
  o <- ncol(id) + 1 + var_rot           # columna anterior a "Sin carga"
  neg <- matrix(FALSE, nrow(cel), ncol(cel))
  neg[, o + 1] <- D$sin > 0; neg[, o + 2] <- D$cruz > 0; neg[, o + 3] <- D$pocos > 0; neg[, o + 4] <- !is.na(D$rmax) & D$rmax > .80
  limpia <- D$sin == 0 & D$cruz == 0 & D$pocos == 0 & (is.na(D$rmax) | D$rmax <= .80) & !D$heywood
  etiqueta <- function(i) paste(c(if (var_k || (!var_fm && !var_rot)) paste0(D$k[i], tx(" factores", " factors")), if (var_fm) D$fm[i], if (var_rot) rot_txt(D$rotate[i])), collapse = ", ")
  detalle <- vapply(seq_len(nrow(D)), function(i) {
    x <- character(0)
    if (nzchar(D$items_sin[i])) x <- c(x, paste0(tx("sin carga: ", "no salient loading: "), D$items_sin[i]))
    if (nzchar(D$items_cruz[i])) x <- c(x, paste0(tx("cruzadas: ", "cross-loading: "), D$items_cruz[i]))
    if (D$heywood[i]) x <- c(x, tx("comunalidad \u2265 1 (caso Heywood)", "communality \u2265 1 (Heywood case)"))
    if (length(x)) paste0(etiqueta(i), " (", paste(x, collapse = "; "), ")") else ""
  }, character(1))
  detalle <- detalle[nzchar(detalle)]
  partes <- c(if (cor == "policorica") tx("correlaciones polic\u00f3ricas", "polychoric correlations"), if (!var_fm) paste0(tx("extracci\u00f3n ", "extraction: "), fm[1]), if (!var_rot) paste0(tx("rotaci\u00f3n ", "rotation: "), rot_txt(rejilla$rotate[1])),
              if (!var_k) paste0(nfactores, tx(" factores", " factors")))
  fijo <- if (length(partes)) { x <- paste(partes, collapse = ", "); paste0(toupper(substr(x, 1, 1)), substring(x, 2), ". ") } else ""
  nota <- paste0(fijo,
                 tx(paste0("% var. = varianza com\u00fan explicada (suma de comunalidades)", if (var_rot) "; % por factor = varianza de cada factor despu\u00e9s de rotar" else "",
                           "; Sin carga = \u00edtems sin ninguna carga \u2265 ", f_apa(umbral, 2),
                           "; Cruzadas = \u00edtems con dos cargas \u2265 .30 que difieren en menos de .20; < 3 \u00edtems = factores con menos de tres \u00edtems; r m\u00e1x. = correlaci\u00f3n m\u00e1s alta entre factores. En negritas, lo que conviene revisar."),
                    paste0("% var. = explained common variance (sum of communalities)", if (var_rot) "; % per factor = variance of each factor after rotation" else "",
                           "; No loading = items with no loading \u2265 ", f_apa(umbral, 2),
                           "; Cross = items with two loadings \u2265 .30 differing by less than .20; < 3 items = factors with fewer than three items; max r = highest factor correlation. Values to review are in bold.")),
                 if (var_rot) tx(" La rotaci\u00f3n no cambia la varianza total explicada ni el ajuste (RMSEA, TLI, BIC); solo reparte la varianza entre los factores para que cada \u00edtem cargue en uno (estructura simple). Las rotaciones oblicuas (promax, oblimin) permiten que los factores se correlacionen, lo habitual en ciencias sociales.",
                                 " Rotation does not change the total explained variance or the fit (RMSEA, TLI, BIC); it only redistributes variance across factors toward simple structure. Oblique rotations (promax, oblimin) allow factors to correlate, which is usual in the social sciences.") else "",
                 if (!is.null(sugeridos)) paste0(tx(" Sugeridos por los criterios: paralelo (factores) ", " Suggested by the criteria: parallel analysis (factors) "),
                                                  sugeridos[["paralelo_factores"]], ", MAP ", sugeridos[["map"]], ", Kaiser ", sugeridos[["kaiser"]], ".") else "",
                 if (length(detalle)) paste0(" ", paste(detalle, collapse = ". "), ".") else "",
                 if (any(limpia)) paste0(tx(" Estructura m\u00e1s clara (sin \u00edtems problem\u00e1ticos): ", " Clearest structure (no problematic items): "),
                                         paste(vapply(which(limpia), etiqueta, character(1)), collapse = tx("; ", "; ")), ".") else "")
  t <- nueva_tabla(tx("Comparaci\u00f3n de soluciones factoriales", "Comparison of factor solutions"),
                   c(cab_id, tx("% var.", "% var."), if (var_rot) tx("% por factor", "% per factor"), tx("Sin carga", "No loading"), tx("Cruzadas", "Cross"),
                     tx("< 3 \u00edtems", "< 3 items"), tx("r m\u00e1x.", "max r"), "RMSEA", "TLI", "BIC"),
                   cel, neg, nota = nota, datos = D)
  if (length(sol) > 1 && length(sol) <= 8) {
    A <- do.call(cbind, asignaciones)
    t$extra <- list(nueva_tabla(tx("Factor con la carga m\u00e1s alta de cada \u00edtem, por soluci\u00f3n", "Factor with the highest loading of each item, by solution"),
                                c(tx("\u00cdtem", "Item"), vapply(match(names(asignaciones), D$nombre), etiqueta, character(1))),
                                cbind(colnames(d), A), alinear = c("l", rep("c", ncol(A))),
                                nota = tx("Entre par\u00e9ntesis, la carga (de patr\u00f3n, si la rotaci\u00f3n es oblicua). Para la tabla completa de una soluci\u00f3n: tabla_cargas(r$soluciones[[\"nombre\"]]).",
                                          "Loading in parentheses (pattern loading for oblique rotations). For the full table of one solution: tabla_cargas(r$soluciones[[\"name\"]]).")))
  }
  diagramas <- list()
  if (dibujar || !is.null(archivo)) for (k in names(sol)) {
    dg <- diagrama_sem(sol[[k]], dibujar = dibujar, umbral = .30)
    if (!is.null(archivo)) {
      ext <- tools::file_ext(archivo); base <- sub(paste0("\\.", ext, "$"), "", archivo)
      guardar_diagrama(dg, paste0(base, "_", k, if (grepl("^[0-9]+$", k)) "f" else "", ".", ext))
    }
    diagramas[[k]] <- dg
  }
  structure(list(tabla = t, soluciones = sol, diagramas = diagramas), class = "comparacion_afe")
}

#' @export
print.comparacion_afe <- function(x, ...) {
  print(x$tabla)
  nm <- names(x$soluciones)[1]
  cat(strwrap(paste0(tx("Elige la soluci\u00f3n que tenga sentido te\u00f3rico y una estructura clara: cada \u00edtem en un solo factor y al menos tres \u00edtems por factor. Para ver sus cargas: tabla_cargas(r$soluciones[[\"",
                            "Choose the solution that makes theoretical sense and has a clear structure: each item on one factor and at least three items per factor. To see its loadings: tabla_cargas(r$soluciones[[\""),
                     nm, "\"]]) (", tx("nombres disponibles: ", "available names: "), paste(names(x$soluciones), collapse = ", "), ")."), width = 80), sep = "\n")
  invisible(x)
}
