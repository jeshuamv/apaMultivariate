PALETAS <- list(
  lila     = list(latente = c("#E3E6F3", "#E2CBEF"), observada = c("#E9EBF6", "#EBDDF5"), error = c("#F1F2FA", "#EEDDF6")),
  azul     = list(latente = c("#EAF2FB", "#BFD7F2"), observada = c("#F2F7FC", "#D9E7F6"), error = c("#F6F9FD", "#DDE9F7")),
  verde    = list(latente = c("#EEF7EE", "#C5E3C6"), observada = c("#F4FAF4", "#DCEEDC"), error = c("#F7FBF7", "#E1F0E1")),
  gris     = list(latente = c("#F4F4F4", "#D6D6D6"), observada = c("#F8F8F8", "#E6E6E6"), error = c("#FAFAFA", "#E9E9E9")),
  durazno  = list(latente = c("#FFF3EA", "#FAD3B5"), observada = c("#FFF8F2", "#FBE3D0"), error = c("#FFFAF6", "#FBE6D6")),
  blanco   = list(latente = c("#FFFFFF", "#FFFFFF"), observada = c("#FFFFFF", "#FFFFFF"), error = c("#FFFFFF", "#FFFFFF"))
)

#' Paleta de colores para los diagramas
#'
#' Define los colores de relleno de las figuras. Cada tipo de figura usa dos
#' colores: el del lado izquierdo y el del lado derecho del degradado. Si se da
#' un solo color, la figura se rellena de ese color sólido.
#'
#' @param nombre Paleta predefinida: `"lila"` (la clásica, por omisión),
#'   `"azul"`, `"verde"`, `"gris"`, `"durazno"` o `"blanco"` (sin relleno,
#'   útil para revistas en blanco y negro).
#' @param latente,observada,error Colores para los óvalos (constructos), los
#'   rectángulos (variables observadas) y los círculos de error. Uno o dos
#'   colores en cualquier formato que acepte R (`"#E2CBEF"`, `"lightblue"`,
#'   etc.). Si se indican, reemplazan los de la paleta elegida.
#' @param linea Color de las líneas, flechas y bordes.
#' @param texto Color del texto.
#'
#' @return Un objeto de clase `paleta_sem` para el argumento `paleta` de
#'   [diagrama_sem()].
#' @examples
#' paleta_sem("azul")
#' paleta_sem(latente = c("white", "#FAD3B5"))
#' paleta_sem("gris", linea = "gray20")
#' @export
paleta_sem <- function(nombre = "lila", latente = NULL, observada = NULL, error = NULL,
                       linea = "black", texto = "black") {
  if (!nombre %in% names(PALETAS))
    stop("Paleta desconocida. Opciones: ", paste(names(PALETAS), collapse = ", "), ".", call. = FALSE)
  p <- PALETAS[[nombre]]
  par2 <- function(x) { if (length(x) == 1) x <- c(x, x); grDevices::col2rgb(x); x[1:2] }
  if (!is.null(latente))   p$latente   <- par2(latente)
  if (!is.null(observada)) p$observada <- par2(observada)
  if (!is.null(error))     p$error     <- par2(error)
  p$linea <- linea; p$texto <- texto
  structure(p, class = "paleta_sem", nombre = nombre)
}

#' @export
print.paleta_sem <- function(x, ...) {
  cat("Paleta de diagramaSEM:", attr(x, "nombre"), "\n")
  cat("  latente:  ", paste(x$latente, collapse = " -> "), "\n")
  cat("  observada:", paste(x$observada, collapse = " -> "), "\n")
  cat("  error:    ", paste(x$error, collapse = " -> "), "\n")
  cat("  l\u00ednea:", x$linea, " | texto:", x$texto, "\n")
  invisible(x)
}
