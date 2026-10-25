# Plantillas de análisis listas para llenar

TIPOS_PLANTILLA <- c(afe = "An\u00e1lisis factorial exploratorio",
                     afc = "An\u00e1lisis factorial confirmatorio",
                     trayectorias = "An\u00e1lisis de trayectorias (path)",
                     sem = "Modelo de ecuaciones estructurales (medici\u00f3n + estructural)",
                     moderacion = "Moderaci\u00f3n y mediaci\u00f3n moderada (PROCESS 1, 7 y 14)",
                     manova = "MANOVA de un factor")

#' Abrir una plantilla de análisis lista para llenar
#'
#' Copia a tu carpeta de trabajo un script de R con todos los pasos de un
#' análisis (leer los datos, revisarlos, estimar el modelo, hacer el
#' diagrama, las tablas y guardarlas en Word) y lo abre en RStudio. Cada paso
#' está explicado con comentarios y solo hay que cambiar lo marcado con
#' `>>> CAMBIA AQUÍ <<<`.
#'
#' Si ya existe un archivo con ese nombre no se sobrescribe: se agrega un
#' número al nombre (`analisis_afc_2.R`, ...).
#'
#' @param tipo Tipo de análisis: `"afe"`, `"afc"`, `"trayectorias"`, `"sem"`,
#'   `"moderacion"` o `"manova"`. Sin argumento, muestra la lista.
#' @param archivo Nombre del script que se crea. Por defecto
#'   `analisis_<tipo>.R` en la carpeta de trabajo (`getwd()`).
#' @param abrir Si es `TRUE` (por defecto en sesiones interactivas), abre el
#'   script en el editor.
#' @return Invisiblemente, la ruta del script creado.
#' @examples
#' plantilla()                                   # lista de plantillas
#' f <- plantilla("afc", archivo = tempfile(fileext = ".R"), abrir = FALSE)
#' head(readLines(f, encoding = "UTF-8"), 12)
#' @export
plantilla <- function(tipo = NULL, archivo = NULL, abrir = interactive()) {
  if (is.null(tipo)) {
    message("Plantillas disponibles (usa, por ejemplo, plantilla(\"afc\")):")
    for (k in names(TIPOS_PLANTILLA)) message(sprintf("  %-14s %s", paste0("\"", k, "\""), TIPOS_PLANTILLA[[k]]))
    return(invisible(names(TIPOS_PLANTILLA)))
  }
  tipo <- tolower(sin_acentos(tipo[1]))
  if (tipo %in% c("path", "trayectoria")) tipo <- "trayectorias"
  if (tipo == "cfa") tipo <- "afc"
  if (tipo == "efa") tipo <- "afe"
  if (!tipo %in% names(TIPOS_PLANTILLA))
    stop("No hay plantilla \"", tipo, "\". Las disponibles son: ",
         paste0("\"", names(TIPOS_PLANTILLA), "\"", collapse = ", "), ".", call. = FALSE)
  origen <- system.file("plantillas", paste0(tipo, ".R"), package = "apaMultivariate")
  if (!nzchar(origen)) stop("No se encontr\u00f3 la plantilla instalada; reinstala apaMultivariate.", call. = FALSE)
  if (is.null(archivo)) archivo <- paste0("analisis_", tipo, ".R")
  if (file.exists(archivo)) {
    base <- sub("\\.[Rr]$", "", archivo); i <- 2
    while (file.exists(paste0(base, "_", i, ".R"))) i <- i + 1
    archivo <- paste0(base, "_", i, ".R")
  }
  if (!file.copy(origen, archivo)) stop("No se pudo crear ", archivo, ". \u00bfTienes permiso para escribir en esa carpeta?", call. = FALSE)
  ruta <- normalizePath(archivo, winslash = "/")
  message("\u2714 Plantilla creada: ", ruta)
  if (abrir) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) rstudioapi::navigateToFile(ruta)
    else utils::file.edit(ruta)
    message("  Corre cada l\u00ednea con Ctrl + Enter (Mac: Cmd + Enter) y cambia solo lo marcado con >>> CAMBIA AQU\u00cd <<<")
  }
  invisible(ruta)
}
