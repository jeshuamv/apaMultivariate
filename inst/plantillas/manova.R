# =============================================================================
#  MANOVA DE UN FACTOR  -  plantilla de apaMultivariate
# =============================================================================
#  ¿Difieren los grupos en un conjunto de variables dependientes?
#  Cómo usar: corre cada bloque con Ctrl + Enter (Mac: Cmd + Enter) y cambia
#  solo lo marcado con  >>> CAMBIA AQUÍ <<<
#  Guía de interpretación:
#  https://github.com/jeshuamv/apaMultivariate/blob/main/guia/INTERPRETACION.md
# =============================================================================

library(apaMultivariate)
# idioma_sem("en")          # quita el # si necesitas las tablas en inglés


# --- 1. Leer los datos --------------------------------------------------------
# >>> CAMBIA AQUÍ <<<  Así como está, se abre una ventana para elegir tu archivo.
# También puedes escribir su nombre:  datos <- leer_datos("mi_archivo.xlsx")
datos <- leer_datos()
# Para probar con datos de ejemplo incluidos en R: pon # al inicio de la línea de arriba
# y quita el # de la línea siguiente.
# datos <- iris
names(datos)


# --- 2. Variables ---------------------------------------------------------------
# >>> CAMBIA AQUÍ <<<
dependientes <- c("Petal.Length", "Sepal.Length")   # dos o más variables numéricas
grupo <- "Species"                                  # la variable que define los grupos

revisar_datos(datos, variables = dependientes)
tabla_descriptivos(datos[, dependientes])


# --- 3. MANOVA ------------------------------------------------------------------------
# La tabla incluye: pruebas multivariadas (Pillai, Wilks...), supuestos (M de Box,
# normalidad, atípicos), ANOVA de seguimiento, comparaciones post hoc y medias.
resultado <- tabla_manova(datos, dependientes, grupo)
resultado


# --- 4. Guardar en Word ---------------------------------------------------------------
guardar_tablas(resultado, archivo = "tablas_manova.docx")
getwd()
