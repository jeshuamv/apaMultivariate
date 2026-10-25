# Revisión del modelo después de estimarlo: traduce los problemas habituales
# de lavaan (no convergencia, varianzas negativas, matrices no positivas
# definidas, falta de identificación) a mensajes claros con qué hacer.

#' Revisar el modelo después de estimarlo
#'
#' Revisa un modelo estimado con `lavaan` y explica, en español y con qué
#' hacer, los problemas que suelen aparecer como avisos en inglés
#' (*warnings*):
#'
#' * El modelo no convergió.
#' * El modelo no está identificado (gl < 0) o está exactamente identificado
#'   (gl = 0, el ajuste no puede evaluarse).
#' * No se pudieron calcular los errores estándar.
#' * Varianzas negativas (casos Heywood) o cargas estandarizadas mayores que 1.
#' * Correlaciones entre factores de 1 o más (matriz no positiva definida) o
#'   muy altas (más de .85, posible falta de validez discriminante).
#' * Factores con un solo ítem o con dos (identificación frágil).
#' * Cargas estandarizadas bajas (menores que .40) o no significativas.
#'
#' @param fit Modelo estimado con [lavaan::cfa()] o [lavaan::sem()].
#' @return Invisiblemente, un `data.frame` con cada revisión (`estado` y
#'   `texto`).
#' @examples
#' library(lavaan)
#' fit <- cfa("visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6", data = HolzingerSwineford1939)
#' revisar_modelo(fit)
#' @export
revisar_modelo <- function(fit) {
  if (!inherits(fit, "lavaan")) stop("`fit` debe ser un modelo estimado con cfa() o sem() de lavaan.", call. = FALSE)
  res <- list()
  agregar <- function(estado, texto) res[[length(res) + 1]] <<- data.frame(estado = estado, texto = texto, stringsAsFactors = FALSE)
  if (!lavaan::lavInspect(fit, "converged")) {
    agregar("problema", paste0("El modelo no convergi\u00f3: lavaan no encontr\u00f3 una soluci\u00f3n, as\u00ed que los resultados no sirven. ",
                               "Causas comunes: un \u00edtem mal escrito o en otra escala, muy pocos casos o un modelo mal especificado. ",
                               "Revisa el modelo con revisar_datos(datos, modelo) y prueba con std.lv = TRUE o con otro estimador."))
    R <- do.call(rbind, res); class(R) <- c("revision_datos", class(R)); attr(R, "titulo") <- "Revisi\u00f3n del modelo"
    print(R); return(invisible(R))
  }
  fm <- lavaan::fitMeasures(fit, c("df", "npar"))
  if (fm[["df"]] < 0) agregar("problema", paste0("El modelo no est\u00e1 identificado (gl = ", fm[["df"]], "): tiene m\u00e1s par\u00e1metros que informaci\u00f3n en los datos. ",
                                                 "Cada factor necesita al menos tres \u00edtems (o dos, si se correlaciona con otros factores). Simplifica el modelo."))
  else if (fm[["df"]] == 0) agregar("aviso", paste0("El modelo est\u00e1 exactamente identificado (gl = 0): reproduce los datos a la perfecci\u00f3n, ",
                                                    "as\u00ed que su ajuste no puede evaluarse (\u03c7\u00b2 = 0, CFI = 1). Es normal en un factor con tres \u00edtems o en una regresi\u00f3n; rep\u00f3rtalo as\u00ed."))
  else agregar("bien", paste0("El modelo est\u00e1 identificado (gl = ", fm[["df"]], ")."))
  pe <- suppressWarnings(lavaan::parameterEstimates(fit, standardized = TRUE))
  if ("group" %in% names(pe)) pe$grupo <- pe$group else pe$grupo <- 1
  if (any(is.na(pe$se[pe$op %in% c("=~", "~")])) && lavaan::lavInspect(fit, "options")$se != "none")
    agregar("problema", paste0("No se pudieron calcular los errores est\u00e1ndar (ni los valores p). Suele indicar que el modelo no est\u00e1 identificado: ",
                               "revisa que cada factor tenga al menos tres \u00edtems y que no haya par\u00e1metros repetidos o redundantes."))
  # Varianzas negativas
  vars <- pe[pe$op == "~~" & pe$lhs == pe$rhs, ]
  neg <- vars[vars$est < 0, ]
  if (nrow(neg)) {
    lv <- lavaan::lavNames(fit, "lv")
    agregar("problema", paste0("Hay varianzas negativas (caso Heywood) en: ", paste(unique(neg$lhs), collapse = ", "), ". Es imposible en la realidad, ",
                               "as\u00ed que esa parte del modelo no es confiable. ",
                               if (any(neg$lhs %in% lv)) "En un factor suele deberse a que est\u00e1 casi duplicado con otro o tiene cargas muy bajas. "
                               else "En un \u00edtem suele deberse a que su factor tiene solo dos \u00edtems, a una muestra peque\u00f1a o a que el \u00edtem pertenece a otro factor. ",
                               "Revisa ese factor; si la varianza es apenas negativa y no significativa, puede fijarse en un valor peque\u00f1o (por ejemplo, ", neg$lhs[1], " ~~ 0.01*", neg$lhs[1], ")."))
  }
  cargas <- pe[pe$op == "=~", ]
  mayor1 <- cargas[!is.na(cargas$std.all) & abs(cargas$std.all) > 1, ]
  if (nrow(mayor1)) agregar("problema", paste0("Hay cargas estandarizadas mayores que 1 en: ", paste(unique(mayor1$rhs), collapse = ", "),
                                               ". Va de la mano con las varianzas negativas: revisa ese factor."))
  # Correlaciones entre factores
  lv <- lavaan::lavNames(fit, "lv")
  if (length(lv) > 1) {
    cl <- lavaan::lavInspect(fit, "cor.lv"); if (is.list(cl)) cl <- cl[[1]]
    ut <- which(upper.tri(cl) & abs(cl) >= .85, arr.ind = TRUE)
    if (nrow(ut)) {
      pares <- paste0(rownames(cl)[ut[, 1]], " y ", colnames(cl)[ut[, 2]], " (r = ", f_apa(cl[ut], 2), ")")
      if (any(abs(cl[ut]) >= 1))
        agregar("problema", paste0("Hay factores con correlaci\u00f3n de 1 o m\u00e1s: ", paste(pares, collapse = "; "),
                                   ". La matriz de los factores no es positiva definida (el aviso 'not positive definite' de lavaan): ",
                                   "esos factores miden lo mismo. Considera unirlos en uno solo."))
      else agregar("aviso", paste0("Factores muy correlacionados: ", paste(pares, collapse = "; "),
                                   ". Puede faltar validez discriminante; rev\u00edsalo con tabla_htmt(fit) y compara con un modelo que los una."))
    }
    ni <- table(cargas$lhs[!cargas$rhs %in% lv])
    uno <- names(ni)[ni == 1]; dos <- names(ni)[ni == 2]
    if (length(uno)) agregar("aviso", paste0("Factores con un solo \u00edtem: ", paste(uno, collapse = ", "), ". Un factor necesita al menos dos \u00edtems y, de preferencia, tres."))
    if (length(dos)) agregar("aviso", paste0("Factores con solo dos \u00edtems: ", paste(dos, collapse = ", "),
                                             ". Se pueden estimar, pero son fr\u00e1giles (casos Heywood, confiabilidad baja). Lo recomendable son tres o m\u00e1s."))
  }
  # Cargas bajas o no significativas
  cv <- cargas[!cargas$rhs %in% lv & !cargas$rhs %in% neg$lhs & !(is.na(cargas$se) | cargas$se == 0) & !is.na(cargas$std.all), ]
  bajas <- cv[abs(cv$std.all) < .40, ]
  ns <- cv[!is.na(cv$pvalue) & cv$pvalue >= .05, ]
  if (nrow(ns)) agregar("aviso", paste0("Cargas no significativas (p \u2265 .05): ", paste0(ns$lhs, " \u2192 ", ns$rhs, " (", f_apa(ns$std.all, 2), ")", collapse = ", "),
                                        ". Esos \u00edtems no se relacionan con su factor; revisa si est\u00e1n redactados en sentido inverso o si pertenecen a otro factor."))
  if (nrow(bajas)) agregar("aviso", paste0("Cargas estandarizadas bajas (< .40): ", paste0(bajas$lhs, " \u2192 ", bajas$rhs, " (", f_apa(bajas$std.all, 2), ")", collapse = ", "),
                                           ". Considera si esos \u00edtems deben quedarse; si los quitas, expl\u00edcalo en el reporte."))
  else if (nrow(cv) && !nrow(ns)) agregar("bien", "Todas las cargas estandarizadas son de .40 o m\u00e1s.")
  if (!nrow(neg) && !nrow(mayor1)) agregar("bien", "No hay varianzas negativas ni cargas mayores que 1.")
  R <- do.call(rbind, res)
  class(R) <- c("revision_datos", class(R)); attr(R, "titulo") <- "Revisi\u00f3n del modelo"
  print(R)
  invisible(R)
}

# Las tablas que describen los parámetros de un grupo no aceptan modelos
# multigrupo: se explica qué hacer en lugar de mostrar en silencio el grupo 1.
#' @noRd
exigir_un_grupo <- function(fit) {
  if (!inherits(fit, "lavaan") || lavaan::lavInspect(fit, "ngroups") <= 1) return(invisible(TRUE))
  g <- lavaan::lavInspect(fit, "group"); lab <- lavaan::lavInspect(fit, "group.label")
  stop("Este modelo es multigrupo (", g, ": ", paste(lab, collapse = ", "), ") y esta tabla describe un solo grupo. ",
       "Para comparar los grupos usa tabla_invarianza(). Para la tabla de un grupo, estima el modelo solo con sus datos, por ejemplo:\n",
       "  fit_", make.names(lab[1]), " <- cfa(modelo, data = datos[datos$", g, " == \"", lab[1], "\", ])", call. = FALSE)
}
