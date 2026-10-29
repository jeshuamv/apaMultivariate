# =============================================================================
#  MODELO DE ECUACIONES ESTRUCTURALES (SEM)  -  plantilla de apaMultivariate
# =============================================================================
#  Se hace en dos pasos (Anderson y Gerbing, 1988): primero el modelo de
#  medición (AFC) y después el modelo estructural (las hipótesis).
#  Cómo usar: corre cada bloque con Ctrl + Enter (Mac: Cmd + Enter) y cambia
#  solo lo marcado con  >>> CAMBIA AQUÍ <<<
#  Guía de interpretación:
#  https://github.com/jeshuamv/apaMultivariate/blob/main/guia/INTERPRETACION.md
# =============================================================================

library(lavaan)
library(apaMultivariate)
# idioma_sem("en")          # quita el # si necesitas las tablas en inglés


# --- 1. Leer los datos --------------------------------------------------------
# >>> CAMBIA AQUÍ <<<  Así como está, se abre una ventana para elegir tu archivo.
# También puedes escribir su nombre:  datos <- leer_datos("mi_archivo.xlsx")
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
# Si tus resultados deben coincidir con AMOS o con las diapositivas de clase,
# agrega likelihood = "wishart" (AMOS divide entre N - 1; lavaan, entre N):
# afc <- cfa(medicion, data = datos, likelihood = "wishart")
revisar_modelo(afc)            # ¿hubo problemas al estimar?

tabla_normalidad(afc)          # si el c.r. multivariado > 2, usa estimator = "MLR"
tabla_atipicos(afc)            # casos atípicos multivariados
# Si decides quitar los atípicos (justifícalo y repórtalo), quita los # de
# estas dos líneas: se quitan los casos y se vuelve a estimar el modelo.
# datos <- quitar_atipicos(datos, medicion)
# afc <- update(afc, data = datos)
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
# (con likelihood = "wishart" si lo usaste en el modelo de medición)
revisar_modelo(fit)


# --- 4. Diagrama -------------------------------------------------------------------
diagrama_sem(fit)
diagrama_sem(fit, archivo = "diagrama_sem.png")      # imagen para tu documento
# Para un artículo, versión en estilo de revista:
# diagrama_sem(fit, estilo = "revista", archivo = "figura_sem.pdf", ancho = "pagina")


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

# --- Antes de entregar ------------------------------------------------------------------
# Revisa la lista de lo que debes reportar (estándares de APA):
# https://github.com/jeshuamv/apaMultivariate/blob/main/guia/INTERPRETACION.md
# (sección "Lista de verificación"; también en la página 2 de la hoja de referencia).
# Lo que el paquete no puede hacer por ti son las justificaciones: por qué ese
# estimador, por qué esas modificaciones al modelo, por qué ese modelo y no otro.
