#' diagramaSEM: diagramas de trayectorias para modelos de lavaan
#'
#' Diagramas y tablas APA para análisis factoriales exploratorios y
#' confirmatorios, análisis de trayectorias y modelos de ecuaciones
#' estructurales, con la convención gráfica clásica de los programas de SEM.
#'
#' Diagramas:
#' \itemize{
#'   \item [diagrama_sem()]: dibuja el diagrama de un AFE, AFC, análisis de
#'     trayectorias o SEM, en estilo clásico (AMOS), `"revista"` o `"color"`.
#'   \item [paleta_sem()]: elige o define los colores del estilo clásico.
#'   \item [guardar_diagrama()]: guarda en PDF, EPS, TIFF, PNG, SVG o JPEG,
#'     con anchos de revista y revisión del tamaño del texto.
#' }
#'
#' Tablas en formato APA (se guardan en Word con [guardar_tabla()] o, todas
#' juntas, con [guardar_tablas()]):
#' \itemize{
#'   \item Datos: [tabla_descriptivos()], [tabla_perdidos()], [tabla_normalidad()].
#'   \item Análisis exploratorio: [tabla_factorabilidad()], [tabla_cargas()].
#'   \item Medición: [tabla_cargas()], [tabla_confiabilidad()], [tabla_validez()],
#'     [tabla_htmt()], [tabla_metodo_comun()].
#'   \item Ajuste: [tabla_ajuste()], [tabla_comparacion()], [tabla_invarianza()].
#'   \item Modelo estructural: [tabla_efectos()].
#'   \item Redacción: [parrafo_metodo()].
#' }
#'
#' Consulte la guía de uso con `vignette("guia-de-uso", package = "diagramaSEM")`.
#' @keywords internal
"_PACKAGE"
