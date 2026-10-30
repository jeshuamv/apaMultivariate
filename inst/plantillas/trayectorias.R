# =============================================================================
#  ANÁLISIS DE TRAYECTORIAS (PATH)  -  plantilla de apaMultivariate
# =============================================================================
#  Cómo usar esta plantilla:
#   1. Corre cada línea en orden: cursor en la línea y Ctrl + Enter
#      (en Mac: Cmd + Enter).
#   2. Cambia solo lo marcado con  >>> CAMBIA AQUÍ <<<  (son tres cosas: el
#      archivo de datos, el modelo y las hipótesis).
#   3. Si nunca has cambiado nada, puedes correrla tal cual con datos de ejemplo
#      (ver el paso 1).
#  Guía de interpretación:
#  https://github.com/jeshuamv/apaMultivariate/blob/main/guia/INTERPRETACION.md
# =============================================================================

library(lavaan)
library(apaMultivariate)
# idioma_sem("en")          # quita el # si necesitas las tablas en inglés


# --- 1. Leer los datos --------------------------------------------------------
# >>> CAMBIA AQUÍ <<<  Así como está, se abre una ventana para elegir tu archivo
# (Excel, SPSS, CSV...). También puedes escribir su nombre, si está en tu
# carpeta de trabajo:  datos <- leer_datos("mi_archivo.xlsx")
datos <- leer_datos()
# Para probar con datos de ejemplo incluidos en R: pon # al inicio de la línea
# de arriba y quita el # de la línea siguiente (verás un aviso por N = 47).
# datos <- swiss

names(datos)                 # los nombres de tus columnas
revisar_datos(datos)         # ✔ bien  ⚠ aviso  ✖ problema que hay que corregir


# --- 2. Escribir el modelo ----------------------------------------------------
# >>> CAMBIA AQUÍ <<<  Cómo pasar tu diagrama a texto:
#   - Un renglón por cada variable que RECIBE flechas:  Y ~ X1 + X2
#     (a la izquierda la que recibe; a la derecha todas las que le apuntan).
#   - Las variables que no reciben flechas (exógenas) no llevan renglón propio.
#   - La correlación entre exógenas (flecha curva) y los errores NO se escriben.
#   - Usa los nombres exactos de tus columnas: names(datos).
# Ejemplo: Examination recibe una flecha (de Education); Fertility recibe tres.
modelo <- "
  Examination ~ Education
  Fertility   ~ Education + Examination + Catholic
"
revisar_datos(datos, modelo) # ¿los nombres del modelo coinciden con tus columnas?


# --- 3. Estimar el modelo -----------------------------------------------------
# likelihood = "wishart" da el mismo chi-cuadrado que AMOS y que las
# diapositivas de clase (AMOS divide entre N - 1; lavaan, entre N). Si no lo
# necesitas, puedes quitarlo.
fit <- sem(modelo, data = datos, likelihood = "wishart")
revisar_modelo(fit)          # ¿hubo problemas al estimar?


# --- 4. Supuestos ---------------------------------------------------------------
normalidad <- tabla_normalidad(fit)
normalidad                   # c.r. multivariado < 2: normalidad multivariada
grafica_normalidad(fit)      # gráfica Q-Q chi-cuadrado: los puntos deben seguir la línea
atipicos <- tabla_atipicos(fit)
atipicos                     # p1 ≤ .001: caso atípico multivariado
tabla_vif(fit)               # VIF > 5: multicolinealidad

# Si decides quitar los atípicos (justifícalo y repórtalo), quita los # de las
# dos líneas siguientes y vuelve a correr desde el paso 4.
# datos <- quitar_atipicos(datos, modelo)
# fit <- update(fit, data = datos)

# Si no hay normalidad multivariada, puedes estimar con MLR quitando el # de la
# línea siguiente (y volver a correr desde el paso 4).
# fit <- update(fit, estimator = "MLR")


# --- 5. Diagramas ---------------------------------------------------------------
diagrama_sem(fit)                              # coeficientes estandarizados y R²
diagrama_sem(fit, estandarizado = FALSE)       # no estandarizados y varianzas (como AMOS)
# Para tu documento (quedan en tu carpeta de trabajo):
diagrama_sem(fit, archivo = "diagrama_estandarizado.png")
diagrama_sem(fit, estandarizado = FALSE, archivo = "diagrama_no_estandarizado.png")
# Para un artículo, versión en estilo de revista:
# diagrama_sem(fit, estilo = "revista", archivo = "figura_trayectorias.pdf", ancho = "pagina")


# --- 6. Ajuste ------------------------------------------------------------------
ajuste <- tabla_criterios_ajuste(fit)
ajuste                       # índices de ajuste, identificación, exógenas y endógenas
# Si te piden el IC del RMSEA al 95 %, usa en su lugar:
# ajuste <- tabla_criterios_ajuste(fit, ic_rmsea = .95)


# --- 7. Estimaciones y efectos --------------------------------------------------
parametros <- tabla_parametros(fit)
parametros                   # pesos de regresión con B, EE, C.R., p y β
efectos <- tabla_efectos(fit)
efectos                      # efectos directos, indirectos y totales (β), y R²
# (los intervalos usan 1000 remuestreos bootstrap: puede tardar un poco)


# --- 8. Hipótesis ---------------------------------------------------------------
# >>> CAMBIA AQUÍ <<<  Una por hipótesis:
#   "X -> Y"       efecto directo (una flecha)
#   "X -> M -> Y"  efecto indirecto, a través de M (mediación)
# Agrega (+) o (-) si la hipótesis predice el signo: "X -> Y (+)".
hipotesis <- tabla_hipotesis(fit, c(
  H1 = "Education -> Examination (+)",
  H2 = "Education -> Fertility (-)",
  H3 = "Catholic -> Fertility (+)",
  H4 = "Education -> Examination -> Fertility"
))
hipotesis


# --- 9. (Opcional) Modelo alternativo ---------------------------------------------
# Si el ajuste no es bueno o hay trayectorias no significativas, el paquete
# propone un modelo alternativo: quita una por una las trayectorias no
# significativas, prueba si eso empeora el ajuste, lo compara con el original
# y dibuja su diagrama. Es exploratorio: justifica cada cambio con la teoría.
# alt <- modelo_alternativo(fit, archivo = "diagrama_alternativo.png")
# alt                                   # cambios, prueba, sintaxis y comparación
# Si además quieres agregar la trayectoria que más sugieren los índices de
# modificación (solo si tiene sustento teórico):
# alt <- modelo_alternativo(fit, agregar = 1, archivo = "diagrama_alternativo.png")
# Sus tablas, igual que las del modelo original, pero con alt$fit:
# tabla_criterios_ajuste(alt$fit)
# tabla_efectos(alt$fit)


# --- 10. Guardar en Word ------------------------------------------------------------
guardar_tablas(normalidad, atipicos, ajuste, parametros, efectos, hipotesis,
               archivo = "tablas_trayectorias.docx")
parrafo_metodo(fit, bootstrap = 1000, archivo = "metodo_trayectorias.docx")
# Si quitaste atípicos, el párrafo lo menciona si agregas:  atipicos = datos
getwd()                      # carpeta donde quedaron el Word y los diagramas


# --- Antes de entregar --------------------------------------------------------------
# Para interpretar y redactar cada resultado, usa la guía de interpretación.
# Revisa también la lista de lo que debes reportar (estándares de APA):
# https://github.com/jeshuamv/apaMultivariate/blob/main/guia/INTERPRETACION.md
# (sección "Lista de verificación"; también en la página 2 de la hoja de referencia).
# Lo que el paquete no puede hacer por ti son las justificaciones: por qué ese
# estimador, por qué esas modificaciones al modelo, por qué ese modelo y no otro.
