# Propuesta de un modelo alternativo (reespecificación) a partir de un modelo
# de trayectorias o SEM ya estimado: quita las trayectorias no significativas
# y, si se pide, agrega las que sugieren los índices de modificación.

# Escribe la sintaxis de lavaan a partir de una tabla de parámetros
#' @noRd
sintaxis_de_tabla <- function(pt) {
  der <- function(r) {
    pre <- if (!is.na(r$free) && r$free == 0 && r$op %in% c("=~", "~", "~~") && !is.na(r$ustart)) paste0(format(r$ustart), "*")
           else if (nzchar(r$label)) paste0(r$label, "*") else ""
    if (r$op == "~1") return(paste0(r$lhs, " ~ ", pre, "1"))
    paste0(r$lhs, " ", r$op, " ", pre, r$rhs)
  }
  ord <- c("=~", "~", "~~", "~1", ":=", "==", "<", ">")
  pt <- pt[order(match(pt$op, ord), match(pt$lhs, unique(pt$lhs)), seq_len(nrow(pt))), ]
  lin <- character(0); i <- 1
  while (i <= nrow(pt)) {
    r <- pt[i, ]
    if (r$op %in% c("=~", "~", "~~")) {
      j <- which(pt$lhs == r$lhs & pt$op == r$op); j <- j[j >= i]
      j <- j[cumsum(c(1, diff(j)) != 1) == 0]          # solo filas contiguas
      partes <- vapply(j, function(k) sub(paste0("^.*? ", r$op, " "), "", der(pt[k, ])), character(1))
      lin <- c(lin, paste0(r$lhs, " ", r$op, " ", paste(partes, collapse = " + "))); i <- max(j) + 1
    } else { lin <- c(lin, der(r)); i <- i + 1 }
  }
  paste0("\n", paste0("  ", lin, collapse = "\n"), "\n")
}

# ¿Agregar rhs -> lhs crearía un ciclo? (ya existe un camino de lhs a rhs)
#' @noRd
crea_ciclo <- function(reg, lhs, rhs) {
  visit <- lhs; frontera <- lhs
  while (length(frontera)) {
    sig <- unique(reg$lhs[reg$rhs %in% frontera]); sig <- setdiff(sig, visit)
    if (rhs %in% sig) return(TRUE)
    visit <- c(visit, sig); frontera <- sig
  }
  FALSE
}

#' Proponer un modelo alternativo
#'
#' Propone una reespecificación de un modelo de trayectorias o SEM ya
#' estimado, la estima con las mismas opciones (estimador, `likelihood`,
#' datos perdidos) y la compara con el original:
#'
#' 1. **Si se pide** (`agregar`), agrega una por una las trayectorias que
#'    sugieren los índices de modificación (solo efectos `~` entre variables
#'    del modelo que no creen ciclos).
#' 2. **Quita las trayectorias no significativas una por una**: la de mayor
#'    \eqn{p}, vuelve a estimar, y repite hasta que todas tengan
#'    \eqn{p <} `alfa` (Kline, 2016). Las variables que se quedan sin ninguna
#'    relación salen del modelo.
#' 3. **Prueba si quitarlas empeora el ajuste**: compara el modelo antes de
#'    quitarlas con el mismo modelo en que esas trayectorias se fijan en 0
#'    (\eqn{\Delta\chi^2}; con estimadores robustos, la diferencia escalada).
#'    Si no es significativa, el modelo más simple es defendible.
#'
#' Además dibuja el diagrama del modelo alternativo.
#'
#' **Advertencia.** Una reespecificación basada en los resultados es
#' exploratoria: aprovecha las particularidades de tu muestra y puede no
#' replicarse (MacCallum et al., 1992). Repórtala como tal, justifica cada
#' cambio con la teoría y, si puedes, confírmala en otra muestra. Por eso
#' `agregar` está desactivado por defecto: las trayectorias sugeridas solo se
#' listan, para que decidas si tienen sustento teórico.
#'
#' @param fit Modelo de trayectorias o SEM estimado con [lavaan::sem()].
#' @param quitar Si es `TRUE`, quita las trayectorias no significativas.
#' @param alfa Nivel de significancia para quitar trayectorias.
#' @param agregar Número de trayectorias que se agregan según los índices de
#'   modificación (0 = ninguna; solo se listan las sugeridas).
#' @param minimo_im Índice de modificación mínimo para sugerir o agregar una
#'   trayectoria (3.84, que corresponde a \eqn{p < .05}).
#' @param dibujar Si es `TRUE`, dibuja el diagrama del modelo alternativo.
#' @param archivo Si se indica, guarda el diagrama del modelo alternativo.
#' @param ... Otras opciones para [diagrama_sem()] (por ejemplo, `estilo`).
#' @return Un objeto `modelo_alternativo` con `$sintaxis` (para copiar),
#'   `$fit` (el modelo estimado), `$cambios`, `$prueba` (\eqn{\Delta\chi^2} de
#'   las trayectorias quitadas), `$comparacion` (tabla APA) y `$diagrama`.
#'   Se puede pasar a cualquier tabla con `$fit`.
#' @examples
#' library(lavaan)
#' fit <- sem("Examination ~ Education
#'             Fertility ~ Education + Examination + Catholic", data = swiss)
#' alt <- modelo_alternativo(fit, dibujar = FALSE)
#' alt
#' tabla_efectos(alt$fit, bootstrap = 0)
#' @export
modelo_alternativo <- function(fit, quitar = TRUE, alfa = .05, agregar = 0, minimo_im = 3.84,
                               dibujar = TRUE, archivo = NULL, ...) {
  if (!inherits(fit, "lavaan")) stop("`fit` debe ser un modelo estimado con sem() de lavaan.", call. = FALSE)
  entorno <- parent.frame()
  exigir_un_grupo(fit)
  if (!lavaan::lavInspect(fit, "converged")) stop("El modelo original no convergi\u00f3; rev\u00edsalo con revisar_modelo(fit) antes de proponer otro.", call. = FALSE)
  pt <- lavaan::parTable(fit)
  usr <- pt[pt$user %in% c(1, 2) | pt$op %in% c(":=", "==", "<", ">"), c("lhs", "op", "rhs", "free", "ustart", "label", "user")]
  usr <- usr[!(usr$op == "~~" & usr$user == 2), ]
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)
  if (is.null(pe$label)) pe$label <- ""
  reg <- pe[pe$op == "~", ]
  if (!nrow(reg)) stop("El modelo no tiene trayectorias (~). Para un AFC, compara modelos te\u00f3ricos alternativos con tabla_comparacion().", call. = FALSE)
  lab_usados <- unlist(regmatches(usr$rhs[usr$op %in% c(":=", "==", "<", ">")],
                                  gregexpr("[A-Za-z.][A-Za-z0-9._]*", usr$rhs[usr$op %in% c(":=", "==", "<", ">")])))
  cambios <- data.frame(cambio = character(0), trayectoria = character(0), beta = numeric(0),
                        criterio = character(0), stringsAsFactors = FALSE)
  clave <- function(d) paste(d$lhs, d$op, d$rhs)
  ajustar <- function(a) {
    cl <- stats::update(fit, evaluate = FALSE); cl$model <- sintaxis_de_tabla(a)
    r <- tryCatch(suppressWarnings(eval(cl, entorno)), error = function(e) e)
    if (inherits(r, "error") && grepl("not found|no encontrado", conditionMessage(r))) {
      cl$data <- obtener_datos(fit)            # los datos ya no est\u00e1n donde se estim\u00f3: se usan los del modelo
      r <- tryCatch(suppressWarnings(eval(cl, entorno)), error = function(e) e)
    }
    r
  }
  sugerir <- function(f) {
    mi <- tryCatch(suppressWarnings(lavaan::modindices(f, sort. = TRUE, standardized = TRUE)), error = function(e) NULL)
    if (is.null(mi)) return(mi)
    exo <- setdiff(lavaan::lavNames(fit, "ov"), unique(lavaan::parTable(fit)$lhs[lavaan::parTable(fit)$op == "~"]))
    mi <- mi[mi$op == "~" & mi$mi >= minimo_im & !mi$lhs %in% exo, ]   # no se sugieren flechas hacia las ex\u00f3genas
    pf <- lavaan::parameterEstimates(f); regf <- pf[pf$op == "~", ]
    ok <- vapply(seq_len(nrow(mi)), function(i) !crea_ciclo(regf, mi$lhs[i], mi$rhs[i]), logical(1))
    mi[ok, , drop = FALSE]
  }
  # 1. Agregar (si se pide) las trayectorias que sugieren los IM, una por una,
  #    con todas las variables todavía en el modelo
  alt <- usr; base <- fit
  for (j in seq_len(agregar)) {
    mi <- sugerir(base)
    if (is.null(mi) || !nrow(mi)) break
    r <- mi[1, ]
    alt2 <- rbind(alt, data.frame(lhs = r$lhs, op = "~", rhs = r$rhs, free = 1, ustart = NA, label = "", user = 1))
    f2 <- ajustar(alt2)
    if (inherits(f2, "error")) break
    alt <- alt2; base <- f2
    cambios <- rbind(cambios, data.frame(cambio = tx("Se agrega", "Added"), trayectoria = paste(r$rhs, "\u2192", r$lhs),
                                         beta = r$sepc.all, criterio = paste0(tx("IM = ", "MI = "), f_num(r$mi, 2)), stringsAsFactors = FALSE))
  }
  sugeridas <- if (agregar == 0) sugerir(fit) else NULL
  # 2. Quitar, una por una, la trayectoria menos significativa y volver a
  #    estimar, hasta que todas sean significativas (Kline, 2016)
  actual <- base; quitadas <- reg[0, c("lhs", "op", "rhs")]
  if (quitar) repeat {
    pa <- lavaan::parameterEstimates(actual, standardized = TRUE); if (is.null(pa$label)) pa$label <- ""
    cand <- pa[pa$op == "~" & !is.na(pa$pvalue) & pa$pvalue >= alfa & !(nzchar(pa$label) & pa$label %in% lab_usados), ]
    if (!nrow(cand)) break
    r <- cand[which.max(cand$pvalue), ]
    alt2 <- alt[clave(alt) != clave(r), ]
    f2 <- ajustar(alt2)
    if (inherits(f2, "error")) break
    alt <- alt2; actual <- f2; quitadas <- rbind(quitadas, r[, c("lhs", "op", "rhs")])
    cambios <- rbind(cambios, data.frame(cambio = tx("Se quita", "Removed"), trayectoria = paste(r$rhs, "\u2192", r$lhs),
                                         beta = r$std.all, criterio = paste("p =", f_apa(r$pvalue, 3)), stringsAsFactors = FALSE))
  }
  # 3. Prueba de las trayectorias quitadas: el modelo con todas las variables
  #    y esas trayectorias fijadas en 0, contra el modelo antes de quitarlas
  prueba <- NULL
  if (nrow(quitadas)) {
    cero <- data.frame(lhs = quitadas$lhs, op = "~", rhs = quitadas$rhs, free = 0, ustart = 0, label = "", user = 1)
    f0 <- ajustar(rbind(alt, cero))
    if (!inherits(f0, "error")) {
      d <- diferencia_chi(f0, base)
      if (!is.na(d[1])) prueba <- c(dchi = d[[1]], dgl = d[[2]], p = d[[3]])
    }
  }
  # 4. Las variables que se quedaron sin relaciones dirigidas salen del modelo
  vivas <- function(a) unique(c(a$lhs[a$op %in% c("~", "=~")], a$rhs[a$op %in% c("~", "=~")]))
  v <- vivas(alt)
  alt <- alt[!(alt$op == "~~" & !(alt$lhs %in% v & alt$rhs %in% v)) & !(alt$op == "~1" & !alt$lhs %in% v), ]
  fuera <- setdiff(lavaan::lavNames(fit, "ov"), v)
  nuevo <- ajustar(alt)
  if (inherits(nuevo, "error")) stop("No se pudo estimar el modelo alternativo: ", conditionMessage(nuevo), call. = FALSE)
  # lavaan correlaciona por defecto los errores de las variables dependientes
  # "finales" (auto.cov.y). Si al quitar trayectorias aparece una de esas
  # covarianzas que no estaba en el modelo original, se fija en 0 para que el
  # alternativo solo difiera en los cambios propuestos.
  cov_auto <- function(f) { p <- lavaan::parTable(f); p <- p[p$op == "~~" & p$lhs != p$rhs & p$user == 0 & p$free > 0, ]
                            paste(pmin(p$lhs, p$rhs), pmax(p$lhs, p$rhs)) }
  nuevas <- setdiff(cov_auto(nuevo), cov_auto(fit))
  if (length(nuevas)) {
    pr <- do.call(rbind, strsplit(nuevas, " "))
    alt <- rbind(alt, data.frame(lhs = pr[, 1], op = "~~", rhs = pr[, 2], free = 0, ustart = 0, label = "", user = 1))
    nuevo <- ajustar(alt)
    if (inherits(nuevo, "error")) stop("No se pudo estimar el modelo alternativo: ", conditionMessage(nuevo), call. = FALSE)
  }
  if (!nrow(cambios)) {
    message(tx("No hay cambios que proponer: todas las trayectorias son significativas",
               "No changes to propose: all paths are significant"),
            if (agregar == 0 && !is.null(sugeridas) && nrow(sugeridas)) tx("; revisa las trayectorias sugeridas abajo.", "; see the suggested paths below.") else ".")
  }
  if (!nrow(cambios)) {
    message(tx("No hay cambios que proponer: todas las trayectorias son significativas",
               "No changes to propose: all paths are significant"),
            if (agregar == 0 && !is.null(sugeridas) && nrow(sugeridas)) tx("; revisa las trayectorias sugeridas abajo.", "; see the suggested paths below.") else ".")
  }
  sint <- sintaxis_de_tabla(alt)
  if (length(nuevas)) sint <- sub("\n$", paste0("\n  # ", tx("covarianza(s) fijada(s) en 0: lavaan la(s) agregar\u00eda solo y el modelo original no la(s) inclu\u00eda",
                                                             "covariance(s) fixed at 0: lavaan would add them by default and the original model did not include them"), "\n"), sint)
  comp <- if (nrow(cambios)) tabla_comparacion(Original = fit, Alternativo = nuevo, anidados = TRUE,
                                               titulo = tx("Modelo original y modelo alternativo", "Original and alternative models")) else NULL
  d <- NULL
  if (dibujar || !is.null(archivo)) {
    d <- diagrama_sem(nuevo, dibujar = dibujar, ...)
    if (!is.null(archivo)) guardar_diagrama(d, archivo)
  }
  structure(list(sintaxis = sint, fit = nuevo, original = fit, cambios = cambios, fuera = fuera,
                 prueba = prueba, sugeridas = sugeridas, comparacion = comp, diagrama = d, alfa = alfa),
            class = "modelo_alternativo")
}

#' @export
print.modelo_alternativo <- function(x, ...) {
  cat("\n", tx("Modelo alternativo propuesto", "Proposed alternative model"), "\n\n", sep = "")
  if (nrow(x$cambios)) {
    cel <- cbind(x$cambios$cambio, x$cambios$trayectoria, f_apa(x$cambios$beta, 2), x$cambios$criterio)
    print(nueva_tabla(tx("Cambios respecto del modelo original", "Changes from the original model"),
                      c(tx("Cambio", "Change"), tx("Trayectoria", "Path"), "\u03b2", tx("Criterio", "Criterion")), cel,
                      nota = tx(paste0("Se quitan las trayectorias con p \u2265 ", f_apa(x$alfa, 2), "; se agregan las que sugieren los \u00edndices de modificaci\u00f3n (IM). \u03b2 = coeficiente estandarizado (en las agregadas, el cambio esperado)."),
                                paste0("Paths with p \u2265 ", f_apa(x$alfa, 2), " are removed; paths suggested by modification indices (MI) are added. \u03b2 = standardized coefficient (expected change for added paths)."))))
  } else cat(tx("Sin cambios.\n\n", "No changes.\n\n"))
  if (length(x$fuera)) cat(tx("Salen del modelo (se quedaron sin relaciones): ", "Removed from the model (no remaining paths): "),
                           paste(x$fuera, collapse = ", "), "\n\n", sep = "")
  if (!is.null(x$prueba)) {
    p <- x$prueba[["p"]]
    cat(tx("\u00bfQuitar esas trayectorias empeora el ajuste? ", "Does removing those paths worsen fit? "),
        "\u0394\u03c7\u00b2(", formatC(x$prueba[["dgl"]], format = "d"), ") = ", f_num(x$prueba[["dchi"]], 2), ", p ",
        if (p < .001) "< .001" else paste("=", f_apa(p, 3)), ". ",
        if (p >= .05) tx("No: el modelo m\u00e1s simple ajusta igual de bien.\n\n", "No: the simpler model fits equally well.\n\n")
        else tx("S\u00ed: quitarlas todas empeora el ajuste de forma significativa; considera conservar alguna (la de menor p).\n\n", "Yes: removing all of them significantly worsens fit; consider keeping one (the one with the lowest p).\n\n"), sep = "")
  }
  cat(tx("Sintaxis del modelo alternativo (c\u00f3piala a tu script):\n", "Alternative model syntax (copy it to your script):\n"),
      "modelo_alt <- \"", x$sintaxis, "\"\n\n", sep = "")
  if (!is.null(x$comparacion)) print(x$comparacion)
  if (!is.null(x$sugeridas) && nrow(x$sugeridas)) {
    s <- utils::head(x$sugeridas, 5)
    cat(tx("Trayectorias que sugieren los \u00edndices de modificaci\u00f3n (no se agregaron; solo si tienen sustento te\u00f3rico, usa agregar = 1):\n",
           "Paths suggested by modification indices (not added; only if theoretically justified, use agregar = 1):\n"),
        paste0("  ", s$rhs, " \u2192 ", s$lhs, "  (", tx("IM = ", "MI = "), f_num(s$mi, 2), ", \u03b2 ", tx("esperado", "expected"), " = ", f_apa(s$sepc.all, 2), ")", collapse = "\n"),
        "\n\n", sep = "")
  }
  cat(strwrap(tx("Atenci\u00f3n: este modelo se obtuvo a partir de los resultados, as\u00ed que es exploratorio. Justifica cada cambio con la teor\u00eda, rep\u00f3rtalo como modificado despu\u00e9s de ver los datos y, si puedes, conf\u00edrmalo en otra muestra (MacCallum et al., 1992). Para sus tablas usa $fit, por ejemplo tabla_efectos(alt$fit).",
                  "Caution: this model was derived from the results, so it is exploratory. Justify each change theoretically, report it as modified after seeing the data and, if possible, confirm it in another sample (MacCallum et al., 1992). Use $fit for its tables, e.g., tabla_efectos(alt$fit)."), width = 80), sep = "\n")
  cat("\n")
  invisible(x)
}

#' @export
plot.modelo_alternativo <- function(x, ...) {
  if (is.null(x$diagrama)) x$diagrama <- diagrama_sem(x$fit, dibujar = FALSE, ...)
  plot(x$diagrama)
  invisible(x)
}
