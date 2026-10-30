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
comparar_soluciones <- function(datos, nfactores = NULL, rotate = "oblimin", fm = "ml", umbral = 0.40,
                                dibujar = TRUE, archivo = NULL) {
  if (!requireNamespace("psych", quietly = TRUE))
    stop("Se necesita el paquete psych: install.packages(c(\"psych\", \"GPArotation\")).", call. = FALSE)
  if (!rotate %in% c("none", "varimax", "quartimax") && !requireNamespace("GPArotation", quietly = TRUE))
    stop("La rotaci\u00f3n \"", rotate, "\" necesita el paquete GPArotation: install.packages(\"GPArotation\").", call. = FALSE)
  d <- obtener_datos(datos); d <- d[stats::complete.cases(d), , drop = FALSE]
  p <- ncol(d)
  sugeridos <- NULL
  if (is.null(nfactores)) {
    tf <- suppressMessages(tabla_factorabilidad(d, iteraciones = 50, fm = fm))
    sugeridos <- tf$datos$factores
    s <- sort(unique(sugeridos[sugeridos >= 1]))
    nfactores <- if (length(s) == 1) s + (-1:1) else seq(min(s), max(s))
  }
  nfactores <- sort(unique(nfactores[nfactores >= 1 & nfactores <= floor(p / 2)]))
  if (!length(nfactores)) stop("No hay un n\u00famero de factores v\u00e1lido para comparar (como m\u00e1ximo, la mitad de los \u00edtems).", call. = FALSE)
  sol <- list(); filas <- list()
  for (k in nfactores) {
    a <- tryCatch(suppressWarnings(suppressMessages(psych::fa(d, nfactors = k, rotate = if (k == 1) "none" else rotate, fm = fm))),
                  error = function(e) NULL)
    if (is.null(a)) next
    sol[[as.character(k)]] <- a
    L <- unclass(a$loadings); L <- L[, seq_len(k), drop = FALSE]
    ab <- abs(L)
    sin_carga <- rownames(L)[apply(ab, 1, max) < umbral]
    orden <- t(apply(ab, 1, sort, decreasing = TRUE))
    cruzadas <- if (k > 1) rownames(L)[orden[, 2] >= .30 & (orden[, 1] - orden[, 2]) < .20] else character(0)
    asign <- apply(ab, 1, which.max); asign[apply(ab, 1, max) < umbral] <- NA
    n_por <- tabulate(stats::na.omit(asign), nbins = k)
    var_acum <- sum(colSums(L^2)) / p
    if (!is.null(a$Phi) && k > 1) { Phi <- a$Phi; rmax <- max(abs(Phi[upper.tri(Phi)])) } else rmax <- NA
    heywood <- any(a$communality >= .999)
    filas[[length(filas) + 1]] <- data.frame(k = k, var = var_acum, sin = length(sin_carga), cruz = length(cruzadas),
      pocos = sum(n_por < 3), rmax = rmax,
      rmsea = if (!is.null(a$RMSEA)) unname(a$RMSEA[1]) else NA, tli = if (!is.null(a$TLI)) a$TLI else NA,
      bic = if (!is.null(a$BIC)) a$BIC else NA, heywood = heywood,
      items_sin = paste(sin_carga, collapse = ", "), items_cruz = paste(cruzadas, collapse = ", "))
  }
  if (!length(filas)) stop("No se pudo estimar ninguna soluci\u00f3n.", call. = FALSE)
  D <- do.call(rbind, filas)
  guion <- function(x, f) ifelse(is.na(x), "\u2014", f(x))
  cel <- cbind(formatC(D$k, format = "d"), f_num(100 * D$var, 1), formatC(D$sin, format = "d"), formatC(D$cruz, format = "d"),
               formatC(D$pocos, format = "d"), guion(D$rmax, function(x) f_apa(x, 2)),
               guion(D$rmsea, function(x) f_apa(x, 3)), guion(D$tli, function(x) f_apa(x, 3)), guion(D$bic, function(x) f_num(x, 1)))
  neg <- matrix(FALSE, nrow(cel), ncol(cel))
  neg[, 3] <- D$sin > 0; neg[, 4] <- D$cruz > 0; neg[, 5] <- D$pocos > 0; neg[, 6] <- !is.na(D$rmax) & D$rmax > .80
  limpia <- D$sin == 0 & D$cruz == 0 & D$pocos == 0 & (is.na(D$rmax) | D$rmax <= .80) & !D$heywood
  detalle <- vapply(seq_len(nrow(D)), function(i) {
    x <- character(0)
    if (nzchar(D$items_sin[i])) x <- c(x, paste0(tx("sin carga: ", "no salient loading: "), D$items_sin[i]))
    if (nzchar(D$items_cruz[i])) x <- c(x, paste0(tx("cruzadas: ", "cross-loading: "), D$items_cruz[i]))
    if (D$heywood[i]) x <- c(x, tx("comunalidad \u2265 1 (caso Heywood)", "communality \u2265 1 (Heywood case)"))
    if (length(x)) paste0(D$k[i], tx(" factores (", " factors ("), paste(x, collapse = "; "), ")") else ""
  }, character(1))
  detalle <- detalle[nzchar(detalle)]
  nota <- paste0(tx("Extracci\u00f3n ", "Extraction: "), fm, tx(", rotaci\u00f3n ", ", rotation: "), rotate, ". ",
                 tx(paste0("% var. = varianza com\u00fan explicada acumulada; Sin carga = \u00edtems sin ninguna carga \u2265 ", f_apa(umbral, 2),
                           "; Cruzadas = \u00edtems con dos cargas \u2265 .30 que difieren en menos de .20; < 3 \u00edtems = factores con menos de tres \u00edtems; r m\u00e1x. = correlaci\u00f3n m\u00e1s alta entre factores. En negritas, lo que conviene revisar."),
                    paste0("% var. = cumulative explained common variance; No loading = items with no loading \u2265 ", f_apa(umbral, 2),
                           "; Cross = items with two loadings \u2265 .30 differing by less than .20; < 3 items = factors with fewer than three items; max r = highest factor correlation. Values to review are in bold.")),
                 if (!is.null(sugeridos)) paste0(tx(" Sugeridos por los criterios: paralelo (factores) ", " Suggested by the criteria: parallel analysis (factors) "),
                                                  sugeridos[["paralelo_factores"]], ", MAP ", sugeridos[["map"]], ", Kaiser ", sugeridos[["kaiser"]], ".") else "",
                 if (length(detalle)) paste0(" ", paste(detalle, collapse = ". "), ".") else "",
                 if (any(limpia)) paste0(tx(" Estructura m\u00e1s clara (sin \u00edtems problem\u00e1ticos): ", " Clearest structure (no problematic items): "),
                                         paste(D$k[limpia], collapse = tx(" y ", " and ")), tx(" factores.", " factors.")) else "")
  t <- nueva_tabla(tx("Comparaci\u00f3n de soluciones factoriales", "Comparison of factor solutions"),
                   c(tx("Factores", "Factors"), tx("% var.", "% var."), tx("Sin carga", "No loading"), tx("Cruzadas", "Cross"),
                     tx("< 3 \u00edtems", "< 3 items"), tx("r m\u00e1x.", "max r"), "RMSEA", "TLI", "BIC"),
                   cel, neg, nota = nota, datos = D)
  diagramas <- list()
  if (dibujar || !is.null(archivo)) for (k in names(sol)) {
    dg <- diagrama_sem(sol[[k]], dibujar = dibujar, umbral = .30)
    if (!is.null(archivo)) {
      ext <- tools::file_ext(archivo); base <- sub(paste0("\\.", ext, "$"), "", archivo)
      guardar_diagrama(dg, paste0(base, "_", k, "f.", ext))
    }
    diagramas[[k]] <- dg
  }
  structure(list(tabla = t, soluciones = sol, diagramas = diagramas), class = "comparacion_afe")
}

#' @export
print.comparacion_afe <- function(x, ...) {
  print(x$tabla)
  cat(strwrap(tx("Elige la soluci\u00f3n que tenga sentido te\u00f3rico y una estructura clara: cada \u00edtem en un solo factor y al menos tres \u00edtems por factor. Para ver sus cargas: tabla_cargas(r$soluciones[[\"3\"]]) (cambia el 3 por el n\u00famero de factores).",
                 "Choose the solution that makes theoretical sense and has a clear structure: each item on one factor and at least three items per factor. To see its loadings: tabla_cargas(r$soluciones[[\"3\"]]) (replace 3 with the number of factors)."), width = 80), sep = "\n")
  invisible(x)
}
