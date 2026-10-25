# =============================================================================
#  MODELO DE ECUACIONES ESTRUCTURALES (SEM)  -  plantilla de diagramaSEM
# =============================================================================
#  Se hace en dos pasos (Anderson y Gerbing, 1988): primero el modelo de
#  medición (AFC) y después el modelo estructural (las hipótesis).
#  Cómo usar: corre cada bloque con Ctrl + Enter (Mac: Cmd + Enter) y cambia
#  solo lo marcado con  >>> CAMBIA AQUÍ <<<
#  Guía de interpretación:
#  https://github.com/jeshuamv/diagramaSEM/blob/main/guia/INTERPRETACION.md
# =============================================================================

library(lavaan)
library(diagramaSEM)
# idioma_sem("en")          # quita el # si necesitas las tablas en inglés


# --- 1. Leer los datos --------------------------------------------------------
datos <- leer_datos()
# Para probar con datos de ejemplo de lavaan: pon # al inicio de la línea de arriba
# y quita el # de la línea siguiente.
# datos <- PoliticalDemocracy
names(datos)


# --- 2. Modelo de medición: cada factor con sus ítems --------------------------
# >>> CAMBIA AQUÍ <<<
medicion <- "
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8
"
revisar_datos(datos, medicion)
afc <- cfa(medicion, data = datos)

tabla_normalidad(afc)          # si el c.r. multivariado > 2, usa estimator = "MLR"
tabla_criterios_ajuste(afc)    # el modelo de medición debe ajustar bien
tabla_cargas(afc)
tabla_confiabilidad(afc)
tabla_validez(afc)
tabla_htmt(afc)
tabla_metodo_comun(afc)        # si todo viene del mismo cuestionario


# --- 3. Modelo estructural: medición + hipótesis ---------------------------------
# >>> CAMBIA AQUÍ <<<  Agrega los efectos (~) entre factores.
estructural <- paste(medicion, "
  dem60 ~ ind60
  dem65 ~ ind60 + dem60
")
fit <- sem(estructural, data = datos)


# --- 4. Diagrama -------------------------------------------------------------------
diagrama_sem(fit)
diagrama_sem(fit, estilo = "revista", archivo = "figura_sem.pdf", ancho = "pagina")


# --- 5. Ajuste, estimaciones e hipótesis -------------------------------------------
tabla_criterios_ajuste(fit)
tabla_parametros(fit)
tabla_efectos(fit)               # incluye los efectos indirectos (mediación)
# >>> CAMBIA AQUÍ <<<
tabla_hipotesis(fit, c(
  H1 = "ind60 -> dem60 (+)",
  H2 = "dem60 -> dem65 (+)",
  H3 = "ind60 -> dem65 (+)",
  H4 = "ind60 -> dem60 -> dem65"
))


# --- 6. Guardar en Word ------------------------------------------------------------
guardar_tablas(
  tabla_criterios_ajuste(afc, titulo = "Ajuste del modelo de medición"),
  tabla_cargas(afc),
  tabla_confiabilidad(afc),
  tabla_validez(afc),
  tabla_htmt(afc),
  tabla_criterios_ajuste(fit, titulo = "Ajuste del modelo estructural"),
  tabla_efectos(fit),
  archivo = "tablas_sem.docx"
)
parrafo_metodo(fit, bootstrap = 1000, archivo = "metodo_sem.docx")
getwd()
