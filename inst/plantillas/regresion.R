# =============================================================================
#  REGRESIÓN LINEAL MÚLTIPLE  -  plantilla de apaMultivariate
# =============================================================================
#  Cómo usar esta plantilla:
#   1. Corre cada bloque en orden: pon el cursor en una línea y presiona
#      Ctrl + Enter (en Mac: Cmd + Enter).
#   2. Solo cambia lo que está marcado con  >>> CAMBIA AQUÍ <<<
#   3. Las líneas que empiezan con # son comentarios: R no las ejecuta.
#  Guía de interpretación:
#  https://github.com/jeshuamv/apaMultivariate/blob/main/guia/INTERPRETACION.md
# =============================================================================

library(apaMultivariate)
# idioma_sem("en")          # quita el # si necesitas las tablas en inglés


# --- 1. Leer los datos --------------------------------------------------------
datos <- leer_datos()        # se abre una ventana para elegir tu archivo
# Para probar con datos de ejemplo (cemento de Hald): pon # al inicio de la
# línea de arriba y quita el # de la línea siguiente.
# datos <- MASS::cement

names(datos)


# --- 2. Escribir el modelo ------------------------------------------------------
# >>> CAMBIA AQUÍ <<<  variable dependiente ~ predictores separados por +
modelo <- "y ~ x1 + x2 + x3 + x4"

revisar_datos(datos, modelo)
variables <- all.vars(as.formula(modelo))     # las variables del modelo (no cambies nada)
tabla_descriptivos(datos, variables = variables)


# --- 3. Estimar la regresión -------------------------------------------------------
fit <- lm(modelo, data = datos)


# --- 4. Supuestos ------------------------------------------------------------------
# Normalidad, homocedasticidad, linealidad, independencia, multicolinealidad
# y casos influyentes, con su interpretación.
supuestos <- tabla_supuestos_regresion(fit)
supuestos

# Gráficas de residuos: los puntos deben verse al azar alrededor de 0
# (gráfica 1) y cerca de la línea (gráfica 2, Q-Q).
plot(fit, which = 1:2)

# Casos influyentes (y cómo cambian los coeficientes sin el más influyente):
influencia <- tabla_influencia(fit)
influencia


# --- 5. Resultados ------------------------------------------------------------------
regresion <- tabla_regresion(fit)
regresion

# Si la homocedasticidad falla (Breusch-Pagan significativo), usa errores
# estándar robustos; si los residuos no son normales o la muestra es pequeña,
# intervalos por bootstrap. Quita el # de la que necesites:
# regresion <- tabla_regresion(fit, robusto = TRUE)
# regresion <- tabla_regresion(fit, bootstrap = 2000)


# --- 6. (Opcional) Comparar todos los subconjuntos de predictores ---------------------
# Útil para explorar; elegir predictores con los mismos datos aprovecha el
# azar, así que la teoría debe tener la última palabra.
subconjuntos <- tabla_subconjuntos(fit)
subconjuntos


# --- 7. Guardar en Word ---------------------------------------------------------------
guardar_tablas(
  tabla_descriptivos(datos, variables = variables),
  supuestos,
  regresion,
  influencia,
  subconjuntos,
  archivo = "tablas_regresion.docx"
)
getwd()    # carpeta donde quedaron los archivos
