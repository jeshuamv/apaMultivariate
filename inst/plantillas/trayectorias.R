# =============================================================================
#  ANÁLISIS DE TRAYECTORIAS (PATH)  -  plantilla de apaMultivariate
# =============================================================================
#  Cómo usar esta plantilla:
#   1. Corre cada bloque en orden: cursor en la línea y Ctrl + Enter
#      (en Mac: Cmd + Enter).
#   2. Solo cambia lo que está marcado con  >>> CAMBIA AQUÍ <<<
#  Guía de interpretación:
#  https://github.com/jeshuamv/apaMultivariate/blob/main/guia/INTERPRETACION.md
# =============================================================================

library(lavaan)
library(apaMultivariate)
# idioma_sem("en")          # quita el # si necesitas las tablas en inglés


# --- 1. Leer los datos --------------------------------------------------------
datos <- leer_datos()        # se abre una ventana para elegir tu archivo
# Para probar con datos de ejemplo incluidos en R: pon # al inicio de la línea de arriba
# y quita el # de la línea siguiente.
# datos <- swiss
# (con estos datos de ejemplo verás un aviso por la muestra chica, N = 47)
names(datos)


# --- 2. Escribir el modelo ----------------------------------------------------
# >>> CAMBIA AQUÍ <<<  Un renglón por variable dependiente:  Y ~ X1 + X2
# Ejemplo: Examination depende de Education; Fertility depende de las tres.
modelo <- "
  Examination ~ Education
  Fertility   ~ Education + Examination + Catholic
"


# --- 3. Revisar los datos y los supuestos -------------------------------------
revisar_datos(datos, modelo)
fit <- sem(modelo, data = datos)
revisar_modelo(fit)          # ¿hubo problemas al estimar?
tabla_normalidad(fit)        # c.r. multivariado < 2: normalidad multivariada
tabla_atipicos(fit)          # p1 ≤ .001: caso atípico
# Si decides quitar los atípicos (justifícalo y repórtalo), quita los # de
# estas dos líneas: se quitan los casos y se vuelve a estimar el modelo.
# datos <- quitar_atipicos(datos, modelo)
# fit <- sem(modelo, data = datos)
tabla_vif(fit)               # VIF > 5: multicolinealidad
# Si no hay normalidad, estima con MLR quitando el # de la línea siguiente:
# fit <- sem(modelo, data = datos, estimator = "MLR")


# --- 4. Diagramas -------------------------------------------------------------------
diagrama_sem(fit)                              # coeficientes estandarizados y R²
diagrama_sem(fit, estandarizado = FALSE)       # no estandarizados y varianzas (como AMOS)
diagrama_sem(fit, estilo = "revista", archivo = "figura_trayectorias.pdf", ancho = "pagina")


# --- 5. Ajuste y estimaciones -------------------------------------------------------
tabla_criterios_ajuste(fit)  # ajuste e identificación del modelo
tabla_parametros(fit)        # pesos de regresión con B, EE, C.R., p y β
tabla_efectos(fit)           # efectos directos, indirectos y totales (bootstrap)


# --- 6. Hipótesis ---------------------------------------------------------------------
# >>> CAMBIA AQUÍ <<<  Una por hipótesis: "X -> Y" (directa) o "X -> M -> Y"
# (mediación). Agrega (+) o (-) si la hipótesis predice el signo.
tabla_hipotesis(fit, c(
  H1 = "Education -> Examination (+)",
  H2 = "Education -> Fertility (-)",
  H3 = "Catholic -> Fertility (+)",
  H4 = "Education -> Examination -> Fertility"
))


# --- 7. (Opcional) Modelo alternativo ----------------------------------------------------
# Por ejemplo, sin una trayectoria no significativa; compáralo con el original.
# alterno <- sem("Examination ~ Education
#                 Fertility ~ Education + Catholic", data = datos)
# tabla_comparacion("Original" = fit, "Alternativo" = alterno, anidados = FALSE)


# --- 8. Guardar en Word ------------------------------------------------------------------
guardar_tablas(
  tabla_criterios_ajuste(fit),
  tabla_parametros(fit),
  tabla_efectos(fit),
  archivo = "tablas_trayectorias.docx"
)
parrafo_metodo(fit, bootstrap = 1000, archivo = "metodo_trayectorias.docx")
getwd()

# --- Antes de entregar ------------------------------------------------------------------
# Revisa la lista de lo que debes reportar (estándares de APA):
# https://github.com/jeshuamv/apaMultivariate/blob/main/guia/INTERPRETACION.md
# (sección "Lista de verificación"; también en la página 2 de la hoja de referencia).
# Lo que el paquete no puede hacer por ti son las justificaciones: por qué ese
# estimador, por qué esas modificaciones al modelo, por qué ese modelo y no otro.
