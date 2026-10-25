# Lectura y revisión de datos para principiantes

#' @noRd
sin_acentos <- function(x) chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00c1\u00c9\u00cd\u00d3\u00da\u00f1\u00d1\u00fc\u00dc\u00e0\u00e8\u00ec\u00f2\u00f9",
                                  "aeiouAEIOUnNuUaeiou", x)

# Nombres que lavaan acepta sin problemas: letras, números y guion bajo
#' @noRd
nombres_limpios <- function(n) {
  x <- sin_acentos(trimws(n))
  x <- gsub("[^A-Za-z0-9_]+", "_", x); x <- gsub("_+", "_", x); x <- gsub("^_|_$", "", x)
  x[x == ""] <- "variable"
  x <- ifelse(grepl("^[0-9]", x), paste0("x", x), x)
  make.unique(x, sep = "_")
}

#' Leer los datos de un archivo
#'
#' Lee los datos de un archivo de **Excel** (`.xlsx`, `.xls`), **SPSS**
#' (`.sav`), **CSV** (`.csv`), texto (`.txt`, `.tsv`), **Stata** (`.dta`) o
#' **SAS** (`.sas7bdat`); el formato se reconoce por la extensión. Pensada
#' para quien empieza con R:
#'
#' * Sin argumentos, `leer_datos()` abre una ventana para **elegir el
#'   archivo**, sin escribir rutas.
#' * En los CSV detecta sola si las columnas están separadas por comas o por
#'   punto y coma (como los que guarda Excel en español) y la codificación de
#'   los acentos.
#' * En los archivos de SPSS, Stata y SAS quita las etiquetas de valor y deja
#'   los números, que es lo que necesita `lavaan`.
#' * Cambia los nombres de columna que darían problemas en `lavaan`
#'   (espacios, acentos, símbolos o un número al inicio) y avisa qué cambió:
#'   `"Ítem 1"` pasa a `"Item_1"`.
#' * Convierte en datos perdidos los códigos que se indiquen en `perdidos`
#'   (por ejemplo, 99 o 999).
#'
#' Al terminar muestra un resumen: casos, variables, cuáles son numéricas y
#' cuáles no.
#'
#' @param archivo Ruta del archivo. Si se omite, se abre una ventana para
#'   elegirlo.
#' @param hoja En archivos de Excel, número o nombre de la hoja.
#' @param perdidos Códigos que deben tratarse como datos perdidos, p. ej.
#'   `c(99, 999)`.
#' @param limpiar_nombres Cambiar los nombres de columna problemáticos.
#' @return Un `data.frame`.
#' @examples
#' archivo <- file.path(tempdir(), "ejemplo.csv")
#' write.csv(data.frame(`Ítem 1` = 1:5, edad = c(20, 21, 99, 23, 22), check.names = FALSE),
#'           archivo, row.names = FALSE)
#' datos <- leer_datos(archivo, perdidos = 99)
#' @export
leer_datos <- function(archivo = NULL, hoja = 1, perdidos = NULL, limpiar_nombres = TRUE) {
  if (is.null(archivo)) {
    if (!interactive()) stop("Indica el archivo: leer_datos(\"mis_datos.xlsx\").", call. = FALSE)
    archivo <- tryCatch(file.choose(), error = function(e) NULL)
    if (is.null(archivo)) stop("No se eligi\u00f3 ning\u00fan archivo.", call. = FALSE)
  }
  if (!file.exists(archivo))
    stop("No encuentro el archivo '", archivo, "'.\n",
         "  La carpeta de trabajo es: ", getwd(), "\n",
         "  Opciones: escribe la ruta completa, mueve el archivo a esa carpeta o usa leer_datos() ",
         "sin nada entre los par\u00e9ntesis para elegirlo en una ventana.", call. = FALSE)
  ext <- tolower(tools::file_ext(archivo))
  d <- switch(ext,
    csv = {
      lin <- readLines(archivo, n = 5, warn = FALSE)
      cod <- if (all(validUTF8(lin))) "UTF-8" else "latin1"
      b <- charToRaw(lin[1]); cuenta <- function(ch) sum(b == charToRaw(ch))
      sep <- c(",", ";", "\t")[which.max(c(cuenta(","), cuenta(";"), cuenta("\t")))]
      utils::read.csv(archivo, sep = sep, dec = if (sep == ";") "," else ".", fileEncoding = cod,
                      check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A", "#N/A", "."))
    },
    txt = , tsv = , dat = {
      lin <- readLines(archivo, n = 5, warn = FALSE)
      cod <- if (all(validUTF8(lin))) "UTF-8" else "latin1"
      utils::read.delim(archivo, fileEncoding = cod, check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA", "."))
    },
    xlsx = , xls = , xlsm = as.data.frame(readxl::read_excel(archivo, sheet = hoja, na = c("", "NA", "N/A", "#N/A"))),
    sav = , zsav = as.data.frame(haven::zap_labels(haven::zap_missing(haven::read_sav(archivo)))),
    por = as.data.frame(haven::zap_labels(haven::read_por(archivo))),
    dta = as.data.frame(haven::zap_labels(haven::zap_missing(haven::read_dta(archivo)))),
    sas7bdat = as.data.frame(haven::zap_labels(haven::read_sas(archivo))),
    stop("No reconozco la extensi\u00f3n '.", ext, "'. Formatos aceptados: xlsx, xls, sav, csv, txt, tsv, dta, sas7bdat.", call. = FALSE))
  for (j in seq_along(d)) {
    if (inherits(d[[j]], "haven_labelled")) d[[j]] <- as.vector(d[[j]])
    for (a in c("format.spss", "format.stata", "format.sas", "display_width", "label", "labels")) attr(d[[j]], a) <- NULL
  }
  # Texto que en realidad es número con coma decimal
  for (j in seq_along(d)) if (is.character(d[[j]])) {
    x <- trimws(d[[j]]); ok <- !is.na(x) & nzchar(x)
    if (any(ok) && all(grepl("^-?[0-9]+([.,][0-9]+)?$", x[ok]))) d[[j]] <- as.numeric(sub(",", ".", x, fixed = TRUE))
  }
  if (!is.null(perdidos)) {
    cambios <- 0
    for (j in seq_along(d)) if (is.numeric(d[[j]])) { k <- d[[j]] %in% perdidos; cambios <- cambios + sum(k); d[[j]][k] <- NA }
    message("Se marcaron como perdidos ", cambios, " valores con los c\u00f3digos ", paste(perdidos, collapse = ", "), ".")
  }
  if (limpiar_nombres) {
    nuevos <- nombres_limpios(names(d)); cambio <- nuevos != names(d)
    if (any(cambio)) {
      message("Cambi\u00e9 ", sum(cambio), " nombre", if (sum(cambio) > 1) "s" else "", " de columna para que funcione", if (sum(cambio) > 1) "n" else "",
              " en lavaan:\n", paste0("  ", names(d)[cambio], " \u2192 ", nuevos[cambio], collapse = "\n"))
      names(d) <- nuevos
    }
  }
  num <- vapply(d, is.numeric, logical(1))
  message("Le\u00ed ", nrow(d), " casos y ", ncol(d), " variables de '", basename(archivo), "'.",
          if (any(!num)) paste0("\n  Columnas que no son num\u00e9ricas (texto): ", paste(names(d)[!num], collapse = ", "),
                                ". Si son \u00edtems, revisa que no tengan palabras o s\u00edmbolos.") else "",
          "\n  Para ver los nombres de las columnas: names(datos)")
  d
}

# --- Revisión previa ----------------------------------------------------------------------

#' @noRd
variables_del_modelo <- function(modelo) {
  pt <- tryCatch(lavaan::lavParseModelString(modelo, as.data.frame. = TRUE), error = function(e) e)
  if (inherits(pt, "error")) return(pt)
  pt <- pt[pt$op %in% c("=~", "~", "~~", "<~"), ]
  lat <- unique(pt$lhs[pt$op %in% c("=~", "<~")])
  todas <- unique(c(pt$lhs, pt$rhs))
  todas <- todas[!grepl(":", todas, fixed = TRUE) & nzchar(todas) & !grepl("^[0-9.]+$", todas)]
  list(observadas = setdiff(todas, lat), latentes = lat)
}

#' Revisar los datos antes del análisis
#'
#' Revisa los datos (y, si se indica, el modelo) antes de estimar, y explica
#' en español cualquier problema que haría fallar el análisis o que conviene
#' atender:
#'
#' * que las variables del modelo existan en los datos (y sugiere el nombre
#'   correcto si hay un error de mayúsculas o de escritura);
#' * que sean numéricas y que no sean constantes;
#' * códigos que parecen datos perdidos (99, 999, -9...);
#' * porcentaje de datos perdidos;
#' * tamaño de la muestra;
#' * si los ítems parecen ordinales (escala Likert) y qué estimador conviene.
#'
#' @param datos `data.frame` (por ejemplo, el que devuelve [leer_datos()]).
#' @param modelo Sintaxis del modelo de `lavaan` (opcional).
#' @param variables Variables que se revisan, si no se da el modelo (por
#'   omisión, todas).
#' @return Invisiblemente, una tabla con cada revisión y su resultado
#'   (`"bien"`, `"aviso"` o `"problema"`).
#' @examples
#' library(lavaan)
#' revisar_datos(HolzingerSwineford1939, "visual =~ x1 + x2 + x3
#'                                       textual =~ x4 + x5 + X6")
#' @export
revisar_datos <- function(datos, modelo = NULL, variables = NULL) {
  if (!is.data.frame(datos)) stop("`datos` debe ser un data.frame (por ejemplo, el que devuelve leer_datos()).", call. = FALSE)
  res <- list()
  agregar <- function(estado, texto) res[[length(res) + 1]] <<- data.frame(estado = estado, texto = texto, stringsAsFactors = FALSE)
  n <- nrow(datos)
  if (!is.null(modelo)) {
    vm <- variables_del_modelo(modelo)
    if (inherits(vm, "error")) {
      lugar <- gsub("\\s+", " ", sub("^<text>:[0-9]+:[0-9]+: ", "", conditionMessage(vm)))
      agregar("problema", paste0("La sintaxis del modelo tiene un error (", lugar, "). Revisa que cada l\u00ednea tenga la forma 'Factor =~ item1 + item2' o 'Y ~ X', ",
                                 "y que no falten ni sobren signos +."))
      variables <- character(0)
    } else {
      variables <- vm$observadas
      falta <- setdiff(variables, names(datos))
      if (length(falta)) {
        sug <- vapply(falta, function(v) {
          c1 <- names(datos)[tolower(names(datos)) == tolower(v)]
          if (!length(c1)) c1 <- names(datos)[agrepl(v, names(datos), max.distance = 0.2, ignore.case = TRUE)]
          if (length(c1)) paste0("'", v, "' (\u00bfquisiste decir '", c1[1], "'?)") else paste0("'", v, "'")
        }, character(1))
        agregar("problema", paste0("Estas variables del modelo no est\u00e1n en los datos: ", paste(sug, collapse = ", "),
                                   ". Los nombres deben ser id\u00e9nticos a los de las columnas, con las mismas may\u00fasculas. ",
                                   "Escribe names(datos) para verlos."))
        variables <- intersect(variables, names(datos))
      } else agregar("bien", paste0("Las ", length(variables), " variables del modelo est\u00e1n en los datos."))
      lat_en_datos <- intersect(vm$latentes, names(datos))
      if (length(lat_en_datos))
        agregar("aviso", paste0("Los factores ", paste(lat_en_datos, collapse = ", "), " tienen el mismo nombre que una columna de los datos; ",
                                "c\u00e1mbiales el nombre en el modelo (por ejemplo, 'F_", lat_en_datos[1], "') para evitar confusiones."))
    }
  } else if (is.null(variables)) variables <- names(datos)
  variables <- intersect(variables, names(datos))
  if (length(variables)) {
    d <- datos[, variables, drop = FALSE]
    no_num <- names(d)[!vapply(d, is.numeric, logical(1))]
    if (length(no_num)) {
      ej <- vapply(no_num, function(v) { x <- unique(stats::na.omit(as.character(d[[v]]))); paste0(v, " (p. ej. '", x[1], "')") }, character(1))
      agregar("problema", paste0("Estas variables no son num\u00e9ricas: ", paste(ej, collapse = ", "),
                                 ". Si son respuestas como 'De acuerdo', hay que recodificarlas a n\u00fameros antes del an\u00e1lisis."))
    }
    num <- setdiff(names(d), no_num)
    if (length(num)) {
      cte <- num[vapply(num, function(v) length(unique(stats::na.omit(d[[v]]))) <= 1, logical(1))]
      if (length(cte)) agregar("problema", paste0("Estas variables tienen el mismo valor en todos los casos: ", paste(cte, collapse = ", "),
                                                  ". No aportan informaci\u00f3n y hacen fallar el an\u00e1lisis; qu\u00edtalas del modelo."))
      cod <- num[vapply(num, function(v) {
        x <- stats::na.omit(d[[v]]); u <- sort(unique(x)); if (length(u) < 3) return(FALSE)
        alto <- max(u) %in% c(9, 99, 999, 9999, 88, 98, 888) && u[length(u) - 1] <= max(u) / 2
        bajo <- min(u) %in% c(-9, -99, -999, -1, -8, -88) && u[2] >= 0
        alto || bajo }, logical(1))]
      if (length(cod)) agregar("aviso", paste0("Estas variables tienen valores que parecen c\u00f3digos de dato perdido (99, 999, -9...): ",
                                               paste(cod, collapse = ", "), ". Si lo son, l\u00e9elas de nuevo con leer_datos(..., perdidos = c(99, 999))."))
      pp <- vapply(num, function(v) mean(is.na(d[[v]])), numeric(1))
      if (any(pp > 0)) {
        completos <- sum(stats::complete.cases(d[, num, drop = FALSE]))
        agregar(if (max(pp) > .05) "aviso" else "bien",
                paste0("Datos perdidos: hasta ", f_num(100 * max(pp), 1), " % en una variable; ", completos, " de ", n,
                       " casos est\u00e1n completos. ", if (max(pp) > .05) "Con m\u00e1s de 5 %, considera estimar con missing = \"fiml\" y rep\u00f3rtalo con tabla_perdidos()." else ""))
      } else agregar("bien", "No hay datos perdidos.")
      cats <- vapply(num, function(v) { x <- stats::na.omit(d[[v]]); if (all(x == round(x))) length(unique(x)) else Inf }, numeric(1))
      if (any(is.finite(cats) & cats <= 7)) {
        k <- max(cats[is.finite(cats) & cats <= 7])
        agregar("aviso", paste0("Las variables parecen ordinales (escala tipo Likert de hasta ", k, " puntos). ",
                                if (k >= 5) "Con 5 o m\u00e1s categor\u00edas suele aceptarse tratarlas como continuas con estimator = \"MLR\"."
                                else "Con menos de 5 categor\u00edas conviene tratarlas como ordinales: ordered = TRUE (estimador WLSMV)."))
      }
    }
  }
  agregar(if (n < 200) "aviso" else "bien",
          paste0("Tama\u00f1o de la muestra: N = ", n, ". ",
                 if (n < 100) "Es muy peque\u00f1a para AFC o SEM; los resultados ser\u00e1n poco estables."
                 else if (n < 200) "Es peque\u00f1a para AFC o SEM; se suelen recomendar al menos 200 casos (Kline, 2011)."
                 else "Adecuada para la mayor\u00eda de los modelos (al menos 200 casos)."))
  R <- do.call(rbind, res)
  class(R) <- c("revision_datos", class(R))
  print(R)
  invisible(R)
}

#' @export
print.revision_datos <- function(x, ...) {
  marca <- c(bien = "\u2714", aviso = "\u26a0", problema = "\u2716")
  cat("\nRevisi\u00f3n de los datos\n\n")
  for (i in seq_len(nrow(x))) {
    lineas <- strwrap(x$texto[i], width = 76, exdent = 4)
    cat(marca[[x$estado[i]]], " ", paste(lineas, collapse = "\n"), "\n", sep = "")
  }
  np <- sum(x$estado == "problema")
  cat("\n", if (np) paste0(np, " problema", if (np > 1) "s" else "", " que hay que corregir antes de estimar el modelo.")
      else "Todo listo para estimar el modelo.", "\n", sep = "")
  invisible(x)
}
