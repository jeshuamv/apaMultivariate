# =============================================================================
#  MODERACIÓN Y MEDIACIÓN MODERADA  -  plantilla de diagramaSEM
# =============================================================================
#  Como el macro PROCESS de Hayes. Se usa con el puntaje de cada escala
#  (promedio o suma de sus ítems).
#  Cómo usar: corre cada bloque con Ctrl + Enter (Mac: Cmd + Enter) y cambia
#  solo lo marcado con  >>> CAMBIA AQUÍ <<<
#  Guía de interpretación:
#  https://github.com/jeshuamv/diagramaSEM/blob/main/guia/INTERPRETACION.md
# =============================================================================

library(diagramaSEM)
# idioma_sem("en")          # quita el # si necesitas las tablas en inglés


# --- 1. Leer los datos --------------------------------------------------------
datos <- leer_datos()

# Para probar con datos de ejemplo: pon # al inicio de leer_datos() y quita
# el # de las 4 líneas siguientes.
# set.seed(1); datos <- data.frame(apoyo = rnorm(300, 3, 1), autonomia = rnorm(300, 4, 1.5))
# datos$satisfaccion <- .4 * datos$apoyo + .3 * (datos$apoyo - 3) * (datos$autonomia - 4) + rnorm(300)
# datos$compromiso <- 1 + .3 * datos$apoyo + .25 * (datos$apoyo - 3) * (datos$autonomia - 4) + rnorm(300)
# datos$lealtad <- .5 * datos$satisfaccion + .1 * datos$apoyo + rnorm(300)

names(datos)


# --- 2. (Si hace falta) Calcular el puntaje de cada escala ---------------------
# >>> CAMBIA AQUÍ <<<  promedio de los ítems de cada escala, por ejemplo:
# datos$apoyo <- rowMeans(datos[, c("ap1", "ap2", "ap3", "ap4")], na.rm = TRUE)


# --- 3. Moderación simple (modelo 1) --------------------------------------------
# X = predictor, W = moderador, Y = resultado.   >>> CAMBIA AQUÍ <<<
mod <- tabla_moderacion(datos, x = "apoyo", w = "autonomia", y = "compromiso")
mod
grafica_interaccion(mod)                                  # en pantalla
grafica_interaccion(mod, archivo = "figura_interaccion.pdf")


# --- 4. Mediación moderada (modelos 7 o 14) -------------------------------------
# Modelo 7: W modera el efecto de X sobre el mediador M.
# Modelo 14: W modera el efecto del mediador M sobre Y.
# >>> CAMBIA AQUÍ <<<
medmod <- tabla_moderacion(datos, x = "apoyo", w = "autonomia", m = "satisfaccion",
                           y = "lealtad", modelo = 7)
medmod


# --- 5. Guardar en Word ---------------------------------------------------------------
guardar_tablas(mod, medmod, archivo = "tablas_moderacion.docx")
getwd()
