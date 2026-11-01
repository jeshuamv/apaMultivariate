# =============================================================================
#  ANÁLISIS FACTORIAL EXPLORATORIO (AFE)  -  plantilla de apaMultivariate
# =============================================================================
#  Cómo usar esta plantilla:
#   1. Corre cada bloque en orden: pon el cursor en una línea y presiona
#      Ctrl + Enter (en Mac: Cmd + Enter).
#   2. Solo cambia lo que está marcado con  >>> CAMBIA AQUÍ <<<
#   3. Las líneas que empiezan con # son comentarios: R no las ejecuta.
#  Guía de interpretación:
#  https://github.com/jeshuamv/apaMultivariate/blob/main/guia/INTERPRETACION.md
# =============================================================================

# La primera vez instala estos paquetes (quita el # y corre la línea una sola vez):
# install.packages(c("psych", "GPArotation"))

library(psych)
library(apaMultivariate)
# idioma_sem("en")          # quita el # si necesitas las tablas en inglés


# --- 1. Leer los datos --------------------------------------------------------
datos <- leer_datos()        # se abre una ventana para elegir tu archivo
# Para probar con datos de ejemplo: pon # al inicio de la línea de arriba
# y quita el # de la línea siguiente.
# datos <- lavaan::HolzingerSwineford1939

names(datos)


# --- 2. Elegir los ítems ------------------------------------------------------
# >>> CAMBIA AQUÍ <<<  Escribe los nombres de los ítems que vas a analizar.
items <- datos[, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")]

revisar_datos(items)
tabla_descriptivos(items)


# --- 3. ¿Se puede hacer un AFE? ¿Cuántos factores? -----------------------------
# KMO ≥ .60 y Bartlett significativo: los datos son factorizables.
# Para el número de factores, da más peso al análisis paralelo y a la MAP.
tabla_factorabilidad(items)
# Si tus ítems son tipo Likert con 5 opciones o menos, usa correlaciones
# policóricas (y ponlo igual en comparar_soluciones y en fa: cor = "poly"):
# tabla_factorabilidad(items, cor = "policorica")

# Gráfica de sedimentación con el análisis paralelo: se retienen los factores
# cuyo punto queda por encima de la línea punteada (datos aleatorios).
grafica_sedimentacion(items)
grafica_sedimentacion(items, archivo = "figura_sedimentacion.png")

# Compara las soluciones con el número de factores que sugieren los criterios:
# ítems sin carga, cargas cruzadas, factores con menos de tres ítems, ajuste.
# Elige la que tenga sentido teórico y una estructura clara.
soluciones <- comparar_soluciones(items)
soluciones

# Compara rotaciones con el número de factores elegido (sin rotar, varimax,
# promax). La rotación no cambia la varianza total ni el ajuste: solo reparte
# las cargas para que cada ítem quede en un factor.
# >>> CAMBIA AQUÍ <<<  el 3 por tu número de factores; fm = "pa" (ejes
# principales) o "ml" (máxima verosimilitud), el que use tu clase.
rotaciones <- comparar_soluciones(items, 3, rotate = c("none", "varimax", "promax"), fm = "pa", dibujar = FALSE)
rotaciones
# Tabla completa de cargas de una de ellas:
# tabla_cargas(rotaciones$soluciones[["promax"]])


# --- 4. Estimar el AFE ------------------------------------------------------------
# >>> CAMBIA AQUÍ <<<  nfactors = número de factores elegido en el paso 3.
# rotate = "oblimin" si esperas que los factores se correlacionen (lo usual);
# rotate = "varimax" si los supones independientes.
afe <- fa(items, nfactors = 3, rotate = "oblimin", fm = "ml")


# --- 5. Resultados ------------------------------------------------------------------
tabla_cargas(afe)            # cargas: cada ítem debería cargar ≥ .40 en un solo factor
diagrama_sem(afe)
diagrama_sem(afe, estilo = "revista", archivo = "figura_afe.pdf", ancho = "pagina")

# Confiabilidad de cada factor encontrado:
# >>> CAMBIA AQUÍ <<<  agrupa los ítems según la tabla de cargas.
tabla_confiabilidad(items, factores = list(
  Factor1 = c("x4", "x5", "x6"),
  Factor2 = c("x1", "x2", "x3"),
  Factor3 = c("x7", "x8", "x9")
))


# --- 6. Guardar en Word ---------------------------------------------------------------
guardar_tablas(
  tabla_descriptivos(items),
  tabla_factorabilidad(items),
  rotaciones$tabla,
  tabla_cargas(afe),
  archivo = "tablas_afe.docx"
)
getwd()    # carpeta donde quedaron los archivos
