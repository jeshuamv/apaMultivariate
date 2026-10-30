# =============================================================================
#  ANÁLISIS FACTORIAL CONFIRMATORIO (AFC)  -  plantilla de apaMultivariate
# =============================================================================
#  Cómo usar esta plantilla:
#   1. Corre cada bloque en orden: pon el cursor en una línea y presiona
#      Ctrl + Enter (en Mac: Cmd + Enter). Para correr todo: Ctrl + Shift + Enter.
#   2. Solo cambia lo que está marcado con  >>> CAMBIA AQUÍ <<<
#   3. Las líneas que empiezan con # son comentarios: R no las ejecuta.
#  La guía de interpretación explica cómo leer y redactar cada resultado:
#  https://github.com/jeshuamv/apaMultivariate/blob/main/guia/INTERPRETACION.md
# =============================================================================

library(lavaan)
library(apaMultivariate)
# idioma_sem("en")          # quita el # si necesitas las tablas en inglés


# --- 1. Leer los datos --------------------------------------------------------
# Si tus datos tienen códigos de dato perdido (por ejemplo 99), escribe:
# datos <- leer_datos(perdidos = c(99))
# >>> CAMBIA AQUÍ <<<  Así como está, se abre una ventana para elegir tu archivo.
# También puedes escribir su nombre:  datos <- leer_datos("mi_archivo.xlsx")
datos <- leer_datos()

# Para probar con datos de ejemplo: pon # al inicio de la línea de arriba y
# quita el # de la línea siguiente.
# datos <- HolzingerSwineford1939

names(datos)                 # muestra los nombres de tus columnas


# --- 2. Escribir el modelo ----------------------------------------------------
# >>> CAMBIA AQUÍ <<<  Un renglón por factor:  Factor =~ item1 + item2 + item3
# Los nombres de los ítems deben ser IGUALES a los de names(datos).
# Las correlaciones entre factores y los errores NO se escriben: cfa() los incluye.
modelo <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"


# --- 3. Revisar los datos antes de estimar ---------------------------------
# Si aparece un ✖, corrígelo antes de seguir.
revisar_datos(datos, modelo)
tabla_descriptivos(datos[, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")])  # >>> CAMBIA AQUÍ <<< tus ítems
tabla_perdidos(datos)


# --- 4. Estimar el modelo -------------------------------------------------------
fit <- cfa(modelo, data = datos)
# Si tus resultados deben coincidir con AMOS o con las diapositivas de clase,
# agrega likelihood = "wishart" (AMOS divide entre N - 1; lavaan, entre N):
# fit <- cfa(modelo, data = datos, likelihood = "wishart")
revisar_modelo(fit)          # ¿hubo problemas al estimar? (avisos de lavaan en español)

# Supuesto de normalidad: si el c.r. multivariado es mayor que 2, vuelve a
# estimar con estimador robusto quitando el # de la línea siguiente:
tabla_normalidad(fit)
grafica_normalidad(fit)      # gráfica Q-Q chi-cuadrado: los puntos deben seguir la línea
# fit <- update(fit, estimator = "MLR")
tabla_atipicos(fit)          # casos atípicos multivariados
# Si decides quitar los atípicos (justifícalo y repórtalo), quita los # de
# estas dos líneas: se quitan los casos y se vuelve a estimar el modelo.
# datos <- quitar_atipicos(datos, modelo)
# fit <- update(fit, data = datos)


# --- 5. Diagrama ------------------------------------------------------------------
diagrama_sem(fit)                                    # para ver en pantalla
diagrama_sem(fit, archivo = "diagrama_afc.png")      # imagen para tu documento
# Para un artículo, versión en estilo de revista:
# diagrama_sem(fit, estilo = "revista", archivo = "figura_afc.pdf", ancho = "pagina")


# --- 6. Tablas de resultados ------------------------------------------------------
tabla_criterios_ajuste(fit)  # ¿el modelo ajusta? (criterios excelente / aceptable)
tabla_cargas(fit)            # cargas factoriales: idealmente ≥ .50
tabla_confiabilidad(fit)     # alfa y omega: idealmente ≥ .70
tabla_validez(fit)           # CR, AVE y Fornell-Larcker
tabla_htmt(fit)              # validez discriminante: menor que .85
tabla_residuales(fit)        # pares de ítems que el modelo explica mal


# --- 7. (Opcional) Si el ajuste no es bueno ----------------------------------------
# Índices de modificación: solo libera un parámetro si tiene sentido teórico.
tabla_modificacion(fit, n = 10)
# Compara con un modelo alternativo (por ejemplo, de un solo factor):
# alterno <- cfa("G =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9", data = datos)
# tabla_comparacion("Un factor" = alterno, "Tres factores" = fit)


# --- 8. Guardar todo en Word ----------------------------------------------------------
guardar_tablas(
  tabla_descriptivos(datos[, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")]),  # >>> CAMBIA AQUÍ <<<
  tabla_criterios_ajuste(fit),
  tabla_cargas(fit),
  tabla_confiabilidad(fit),
  tabla_validez(fit),
  tabla_htmt(fit),
  archivo = "tablas_afc.docx"
)
parrafo_metodo(fit, archivo = "metodo_afc.docx")   # párrafo de método con referencias

getwd()    # carpeta donde quedaron los archivos

# --- Antes de entregar ------------------------------------------------------------------
# Revisa la lista de lo que debes reportar (estándares de APA):
# https://github.com/jeshuamv/apaMultivariate/blob/main/guia/INTERPRETACION.md
# (sección "Lista de verificación"; también en la página 2 de la hoja de referencia).
# Lo que el paquete no puede hacer por ti son las justificaciones: por qué ese
# estimador, por qué esas modificaciones al modelo, por qué ese modelo y no otro.
