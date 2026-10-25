# Idioma de las tablas, notas, párrafo de método y diagramas

#' Idioma de los resultados (español o inglés)
#'
#' Elige el idioma de todo lo que va al documento: títulos, encabezados,
#' celdas y notas de las tablas, el párrafo de método y sus referencias, y el
#' texto de los diagramas. Se escribe una vez al inicio del script y vale para
#' toda la sesión de R. Los mensajes de ayuda en la consola siguen en español.
#'
#' En inglés se usan los términos y abreviaturas de APA (M, SD, SE, df, CI,
#' *Note.*, *Table*).
#'
#' @param idioma `"es"` (español, por omisión) o `"en"` (inglés). Sin
#'   argumento, devuelve el idioma actual.
#' @return Invisiblemente, el idioma anterior.
#' @examples
#' library(lavaan)
#' fit <- cfa("visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6", data = HolzingerSwineford1939)
#' idioma_sem("en")
#' tabla_ajuste(fit)
#' idioma_sem("es")
#' @export
idioma_sem <- function(idioma) {
  actual <- getOption("apaMultivariate.idioma", "es")
  if (missing(idioma)) return(actual)
  idioma <- tolower(idioma)
  if (!idioma %in% c("es", "en")) stop("`idioma` debe ser \"es\" (espa\u00f1ol) o \"en\" (ingl\u00e9s).", call. = FALSE)
  options(apaMultivariate.idioma = idioma)
  invisible(actual)
}

#' @noRd
en <- function() identical(getOption("apaMultivariate.idioma", "es"), "en")

# Texto en el idioma elegido
#' @noRd
tx <- function(es, en) if (en()) en else es
