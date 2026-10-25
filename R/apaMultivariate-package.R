#' apaMultivariate: tablas y diagramas APA para análisis multivariado
#'
#' Tablas en formato APA y diagramas para análisis factoriales exploratorios y
#' confirmatorios, análisis de trayectorias, modelos de ecuaciones
#' estructurales, moderación y MANOVA, en español o en inglés.
#'
#' Para empezar:
#' \itemize{
#'   \item [leer_datos()]: lee Excel, SPSS, CSV, Stata, SAS o texto.
#'   \item [revisar_datos()]: revisa los datos y el modelo antes de estimar.
#'   \item [plantilla()]: crea un script comentado con todo el análisis.
#' }
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
#'   \item Datos y supuestos: [tabla_descriptivos()], [tabla_perdidos()],
#'     [tabla_normalidad()], [tabla_atipicos()], [tabla_vif()].
#'   \item Análisis exploratorio: [tabla_factorabilidad()], [tabla_cargas()].
#'   \item Medición: [tabla_cargas()], [tabla_confiabilidad()], [tabla_validez()],
#'     [tabla_htmt()], [tabla_bifactor()], [tabla_metodo_comun()].
#'   \item Ajuste: [tabla_ajuste()], [tabla_criterios_ajuste()], [tabla_comparacion()],
#'     [tabla_modificacion()], [tabla_residuales()], [tabla_invarianza()].
#'   \item Modelo estructural: [tabla_parametros()], [tabla_efectos()], [tabla_hipotesis()].
#'   \item Moderación y mediación moderada: [tabla_moderacion()],
#'     [grafica_interaccion()].
#'   \item MANOVA: [tabla_manova()].
#'   \item Redacción: [parrafo_metodo()].
#'   \item Idioma (español o inglés): [idioma_sem()].
#' }
#'
#' Consulte la guía de uso con `vignette("guia-de-uso", package = "apaMultivariate")`.
#' @keywords internal
"_PACKAGE"
