# Modelos rivales de un AFC: los que la teoría obliga a descartar antes de
# aceptar el modelo propuesto (un factor, factores independientes, segundo
# orden y bifactor), estimados con las mismas opciones y comparados.

#' Comparar un AFC con sus modelos rivales
#'
#' A partir de tu modelo de medición (AFC de primer orden con dos o más
#' factores), genera y estima los modelos rivales habituales, con las mismas
#' opciones (estimador, `likelihood`, datos perdidos):
#'
#' * **Un factor:** todos los ítems en un solo factor. Si ajusta igual, los
#'   factores no se distinguen (y puede haber sesgo de método común).
#' * **Factores independientes:** tus factores, sin correlacionarse.
#' * **Factores correlacionados:** tu modelo.
#' * **Segundo orden:** un factor general explica la correlación entre tus
#'   factores (requiere al menos tres factores; con tres es equivalente al de
#'   factores correlacionados).
#' * **Bifactor:** un factor general en todos los ítems más tus factores como
#'   específicos, todos independientes (requiere al menos tres factores).
#'
#' Los compara con tu modelo en una tabla (índices de ajuste, AIC, BIC y,
#' cuando están anidados, \eqn{\Delta\chi^2}), señala los que tuvieron
#' problemas de estimación y dibuja el diagrama de cada uno.
#'
#' A diferencia de [modelo_alternativo()], estos modelos no salen de los
#' datos sino de la teoría: compararlos es lo que piden los revisores para
#' sostener la estructura propuesta.
#'
#' @param fit AFC de primer orden estimado con [lavaan::cfa()].
#' @param modelos Modelos rivales que se estiman: `"un_factor"`,
#'   `"independientes"`, `"segundo_orden"` y `"bifactor"`.
#' @param dibujar Si es `TRUE`, dibuja el diagrama de cada modelo.
#' @param archivo Si se indica (por ejemplo, `"rival.png"`), guarda cada
#'   diagrama con el nombre del modelo agregado (`rival_un_factor.png`, ...).
#' @param ... Otras opciones para [diagrama_sem()] (por ejemplo, `estilo`).
#' @return Un objeto `modelos_rivales` con `$fits` (los modelos estimados,
#'   incluido el tuyo), `$sintaxis`, `$comparacion` (tabla APA),
#'   `$problemas` y `$diagramas`.
#' @examples
#' library(lavaan)
#' fit <- cfa("visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6
#'             speed =~ x7 + x8 + x9", data = HolzingerSwineford1939)
#' r <- modelos_rivales(fit, dibujar = FALSE)
#' r
#' @export
modelos_rivales <- function(fit, modelos = c("un_factor", "independientes", "segundo_orden", "bifactor"),
                            dibujar = TRUE, archivo = NULL, ...) {
  exigir_lavaan(fit, "cfa()")
  entorno <- parent.frame()
  exigir_un_grupo(fit)
  modelos <- match.arg(modelos, several.ok = TRUE)
  pt <- lavaan::parTable(fit)
  if (any(pt$op == "~")) stop("El modelo tiene trayectorias (~). modelos_rivales() es para un AFC; para trayectorias o SEM usa modelo_alternativo().", call. = FALSE)
  lv <- lavaan::lavNames(fit, "lv")
  if (any(pt$op == "=~" & pt$rhs %in% lv)) stop("Parte de tu AFC de primer orden (factores con sus \u00edtems); el de segundo orden es uno de los rivales que se generan.", call. = FALSE)
  fac <- factores_de(fit)
  if (length(fac) < 2) stop("Se necesitan al menos dos factores para generar modelos rivales.", call. = FALSE)
  items <- unlist(fac)
  if (anyDuplicated(items)) stop("Hay \u00edtems que cargan en m\u00e1s de un factor; modelos_rivales() parte de un AFC en que cada \u00edtem mide un solo factor.", call. = FALSE)
  # covarianzas entre errores que el usuario incluyó: se conservan en todos
  err <- pt[pt$op == "~~" & pt$lhs != pt$rhs & pt$lhs %in% items & pt$rhs %in% items & pt$user == 1, ]
  err_txt <- if (nrow(err)) paste0("\n  ", err$lhs, " ~~ ", err$rhs, collapse = "") else ""
  med <- paste0("  ", names(fac), " =~ ", vapply(fac, paste, character(1), collapse = " + "), collapse = "\n")
  nombre_g <- "General"; while (nombre_g %in% c(names(fac), items)) nombre_g <- paste0(nombre_g, "_")
  sint <- list(
    un_factor = paste0("\n  ", nombre_g, " =~ ", paste(items, collapse = " + "), err_txt, "\n"),
    independientes = paste0("\n", med, err_txt, "\n"),
    segundo_orden = paste0("\n", med, "\n  ", nombre_g, " =~ ", paste(names(fac), collapse = " + "), err_txt, "\n"),
    bifactor = paste0("\n", med, "\n  ", nombre_g, " =~ ", paste(items, collapse = " + "), err_txt, "\n"))
  etiquetas <- c(original = tx("Factores correlacionados (tu modelo)", "Correlated factors (your model)"),
                 un_factor = tx("Un factor", "One factor"), independientes = tx("Factores independientes", "Uncorrelated factors"),
                 segundo_orden = tx("Segundo orden", "Second order"), bifactor = "Bifactor")
  omitidos <- character(0)
  if (length(fac) < 3) {
    quitar <- intersect(modelos, c("segundo_orden", "bifactor"))
    if (length(quitar)) omitidos <- c(omitidos, paste0(etiquetas[quitar], tx(" (requiere al menos tres factores)", " (requires at least three factors)")))
    modelos <- setdiff(modelos, quitar)
  }
  fits <- list(original = fit); problemas <- character(0)
  for (m in modelos) {
    extra <- if (m %in% c("independientes", "bifactor")) list(orthogonal = TRUE, std.lv = m == "bifactor") else list()
    f <- do.call(reestimar, c(list(fit = fit, sintaxis = sint[[m]], entorno = entorno), extra))
    if (inherits(f, "error")) { omitidos <- c(omitidos, paste0(etiquetas[[m]], ": ", conditionMessage(f))); next }
    fits[[m]] <- f
  }
  for (m in names(fits)) {
    f <- fits[[m]]; pr <- character(0)
    if (!lavaan::lavInspect(f, "converged")) pr <- tx("no convergi\u00f3", "did not converge")
    else {
      pe <- lavaan::parameterEstimates(f, standardized = TRUE)
      if (any(pe$op == "~~" & pe$lhs == pe$rhs & pe$est < 0)) pr <- c(pr, tx("varianza negativa (caso Heywood)", "negative variance (Heywood case)"))
      if (any(pe$op == "=~" & abs(pe$std.all) > 1, na.rm = TRUE)) pr <- c(pr, tx("carga estandarizada > 1", "standardized loading > 1"))
    }
    if (length(pr)) problemas[[m]] <- paste(pr, collapse = "; ")
  }
  ok <- names(fits)[vapply(fits, function(f) lavaan::lavInspect(f, "converged"), logical(1))]
  fits_ok <- fits[ok]; names(fits_ok) <- etiquetas[ok]
  comp <- tabla_comparacion(fits_ok, referencia = "primero", anidados = TRUE,
                            titulo = tx("Comparaci\u00f3n del modelo propuesto con sus modelos rivales", "Comparison of the proposed model with rival models"))
  comp$nota <- paste0(comp$nota, tx(" Los modelos rivales est\u00e1n anidados en el propuesto, salvo el bifactor, en el que el propuesto est\u00e1 anidado.",
                                    " The rivals are nested within the proposed model, except the bifactor model, within which the proposed model is nested."),
                      if ("segundo_orden" %in% ok && length(fac) == 3) tx(" Con tres factores, el de segundo orden es equivalente al de factores correlacionados (mismo ajuste).",
                                                                          " With three factors, the second-order model is equivalent to the correlated-factors model (same fit).") else "",
                      if (length(problemas)) paste0(tx(" Problemas de estimaci\u00f3n: ", " Estimation problems: "),
                                                    paste0(etiquetas[names(problemas)], " (", problemas, ")", collapse = "; "), ".") else "")
  diagramas <- list()
  if (dibujar || !is.null(archivo)) for (m in ok) {
    d <- diagrama_sem(fits[[m]], dibujar = dibujar, ...)
    if (!is.null(archivo)) {
      ext <- tools::file_ext(archivo); base <- sub(paste0("\\.", ext, "$"), "", archivo)
      guardar_diagrama(d, paste0(base, "_", m, ".", ext))
    }
    diagramas[[m]] <- d
  }
  structure(list(fits = fits, sintaxis = sint[intersect(names(sint), names(fits))], comparacion = comp,
                 problemas = problemas, omitidos = omitidos, diagramas = diagramas, etiquetas = etiquetas),
            class = "modelos_rivales")
}

#' @export
print.modelos_rivales <- function(x, ...) {
  print(x$comparacion)
  if (length(x$omitidos)) cat(tx("No se estimaron: ", "Not estimated: "), paste(x$omitidos, collapse = "; "), "\n\n", sep = "")
  D <- x$comparacion$datos
  cat(strwrap(tx(paste0("C\u00f3mo leerla: tu modelo se sostiene si ajusta mejor que los rivales m\u00e1s simples (un factor, factores independientes): ",
                        "\u0394\u03c7\u00b2 significativa y un AIC y BIC m\u00e1s altos en ellos. Si el de segundo orden ajusta casi igual, la correlaci\u00f3n entre ",
                        "tus factores puede explicarse con un factor general. El bifactor suele ajustar mejor por tener m\u00e1s par\u00e1metros; para decidir si la escala ",
                        "es esencialmente unidimensional usa tabla_bifactor(r$fits$bifactor), no solo el ajuste."),
                 paste0("How to read it: your model is supported if it fits better than the simpler rivals (one factor, uncorrelated factors): ",
                        "significant \u0394\u03c7\u00b2 and higher AIC and BIC for them. If the second-order model fits almost as well, a general factor can account for ",
                        "the correlations among your factors. The bifactor model usually fits better because it has more parameters; to judge essential ",
                        "unidimensionality use tabla_bifactor(r$fits$bifactor), not fit alone.")), width = 80), sep = "\n")
  cat(tx("\nSintaxis de cada modelo en r$sintaxis; sus tablas con r$fits, por ejemplo tabla_cargas(r$fits$segundo_orden).\n",
         "\nSyntax of each model in r$sintaxis; their tables with r$fits, e.g., tabla_cargas(r$fits$segundo_orden).\n"))
  invisible(x)
}
