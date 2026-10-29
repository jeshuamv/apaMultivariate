# apaMultivariate 0.9.4

* README reorganizado para principiantes: un solo camino ("Empieza aquí": instalar,
  crear la plantilla, cambiar tres cosas y correr), una sección nueva sobre cómo
  pasar el diagrama a la sintaxis del modelo, un ejemplo completo de análisis de
  trayectorias que funciona tal cual, y qué hacer si los resultados no coinciden
  con AMOS (`likelihood = "wishart"`). Lo de revistas y estilos queda al final.
* `revisar_datos()`: las variables dicotómicas (0/1) usadas como predictoras ya
  no generan el aviso de escala Likert; solo se avisa si una dicotómica es
  variable dependiente.
* `tabla_hipotesis()`: la nota aclara que el intervalo de los efectos
  indirectos se calcula con el efecto no estandarizado.
* Plantillas: cómo escribir el modelo a partir del diagrama y la opción
  `likelihood = "wishart"` para coincidir con AMOS.

# apaMultivariate 0.9.3

* `revisar_datos()` revisa el modelo antes de pasarlo a lavaan: avisa si
  `modelo` no es texto (por ejemplo, un número), señala la línea que no tiene
  operador (`=~`, `~` o `~~`) y acepta también un modelo ya estimado. Los
  mensajes ya no dependen de la versión de lavaan.

# apaMultivariate 0.9.2

* El README y la hoja de referencia pedían `revisar_datos(datos, modelo)`
  antes de escribir el modelo, lo que daba el error `object 'modelo' not
  found`. Ahora primero se revisan los datos y después, ya con el modelo, los
  datos y el modelo juntos.
* `revisar_datos()` y `quitar_atipicos()`: si les pasas un objeto que todavía
  no existe, explican en español qué falta crear en lugar del error de R.

# apaMultivariate 0.9.1

Ajustes tras probar el paquete con la tarea de análisis de trayectorias del
curso:

* `tabla_criterios_ajuste()` y `tabla_ajuste()`: argumento `ic_rmsea` para
  reportar el intervalo del RMSEA al 95 % (por defecto, 90 %).
* `tabla_criterios_ajuste()` indica qué variables son exógenas y cuáles
  endógenas, y cuenta p y q como AMOS y los libros (incluye las varianzas y
  covarianzas de las exógenas observadas, que lavaan fija); los grados de
  libertad no cambian.
* `tabla_normalidad()`: las dos partes de la tabla ya no se contradicen
  cuando la curtosis multivariada es normal pero la asimetría no.

# apaMultivariate 0.9.0

* **El paquete cambia de nombre: antes se llamaba `diagramaSEM`.** El nombre
  nuevo describe mejor lo que hace hoy: tablas APA y diagramas para análisis
  factoriales, trayectorias, SEM, moderación y MANOVA, en español o en inglés.
  Las funciones no cambian. Para actualizar:
  `remotes::install_github("jeshuamv/apaMultivariate")` y
  `library(apaMultivariate)`.

Revisión completa antes de compartirlo:

* Se reprodujeron los scripts y la tarea del curso (AFC de Holzinger con MLM,
  AFE con `bfi`, AFC con `ChurchSurvey`) y todas las funciones se probaron
  con 14 tipos de modelo. El detalle está en `guia/VALIDACION.md`.
* `revisar_modelo()`: revisa el modelo ya estimado y explica en español los
  avisos de `lavaan` (no convergencia, falta de identificación, casos
  Heywood, factores con correlación de 1, cargas bajas) y qué hacer.
* `quitar_atipicos()`: devuelve los datos sin los atípicos multivariados
  (Mahalanobis, *p* < .001), como en los scripts de clase.
* `tabla_confiabilidad()`: ω ahora usa la varianza observada de la suma, igual
  que `semTools::compRelSEM()` por defecto (antes, la del modelo; sigue
  disponible con `varianza = "modelo"`). Con ítems ordinales indica que es el
  omega ordinal.
* `tabla_factorabilidad()`: el análisis paralelo de factores se calcula igual
  que `psych::fa.parallel()` (argumento `fm`, por defecto `"ml"`); antes podía
  sugerir más factores que esa función.
* `tabla_criterios_ajuste()`: RMSEA con los criterios habituales (≤ .06
  excelente, ≤ .08 aceptable); nuevos argumentos `cortes` (puntos de corte
  propios) y `evaluacion = FALSE` (sin la columna de calificación).
* Las tablas que describen un grupo dan un mensaje claro con modelos
  multigrupo, en lugar de mostrar en silencio el primer grupo.
* `tabla_metodo_comun()` respeta los ítems ordinales y explica por qué no
  aplica a modelos de un factor; `tabla_modificacion()` avisa cuando el modelo
  tiene restricciones de igualdad; `parrafo_metodo()` funciona con modelos
  multigrupo.
* Los diagramas y la gráfica de la interacción se guardan en PDF, EPS y TIFF
  también en Mac sin XQuartz (si cairo no está disponible se usa el
  dispositivo estándar de R).
* Word: columnas más anchas para textos con flechas y símbolos.
* Las tablas avisan cuando son más anchas que la consola.
* Lista de verificación para reportar un AFC o un SEM según los estándares de
  APA (JARS-Quant), con la función que da cada dato y lo que debe justificar
  el autor: en la guía de interpretación, en la página 2 de la hoja de
  referencia y al final de las plantillas.
* Hoja de referencia (`guia/hoja_referencia.pdf`), sección "Lo
  que no hace" en el README, `citation("apaMultivariate")`, revisión
  automática en Windows, macOS y Linux, y formularios para reportar problemas.

# diagramaSEM 0.8.0

Pensada para quien empieza con R.

* `leer_datos()`: lee Excel, SPSS, CSV (con coma o punto y coma, y coma
  decimal), Stata, SAS y texto; sin argumentos abre una ventana para elegir el
  archivo. Quita las etiquetas de SPSS, marca los códigos de dato perdido
  (`perdidos = c(99)`) y corrige los nombres de columna con espacios o acentos.
* `revisar_datos()`: revisión previa con mensajes claros: nombres del modelo
  que no están en los datos (con sugerencias), errores de sintaxis, columnas de
  texto o constantes, posibles códigos de perdido, porcentaje de perdidos,
  ítems tipo Likert y tamaño de la muestra.
* `plantilla()`: scripts comentados, paso a paso, para AFE, AFC, trayectorias,
  SEM, moderación y MANOVA, con datos de ejemplo.
* Nueva guía *Cómo interpretar y redactar los resultados*
  (`guia/INTERPRETACION.md`): criterios, ejemplos de redacción en APA y errores
  comunes para cada tabla.
* README con un "Paso 0" para instalar R y RStudio.

# diagramaSEM 0.7.0

* `tabla_moderacion()`: moderación y mediación moderada al estilo de PROCESS
  (modelos 1, 7 y 14) con regresiones OLS y bootstrap de percentiles:
  coeficientes, ΔR² de la interacción, pendientes simples, región de
  Johnson-Neyman, efectos indirectos condicionales e índice de mediación
  moderada.
* `grafica_interaccion()`: gráfica de pendientes simples en estilo de revista.
* `tabla_bifactor()`: ωH, ωHS, ωT, ωS, ECV, PUC, H y determinación factorial
  para modelos bifactor y de segundo orden (Schmid-Leiman); avisa de casos
  Heywood.

# diagramaSEM 0.6.0

* `idioma_sem("en")`: tablas, notas, párrafo de método y referencias en
  inglés, con los términos de APA (M, SD, SE, df, CI, Note., Table), para
  revistas en inglés. `idioma_sem("es")` vuelve al español.
* `tabla_manova()` agrega las comparaciones post hoc entre grupos (Tukey HSD
  o Games-Howell, elegido según la M de Box) con IC 95 %, *p* ajustado y *d*
  de Cohen.
* `tabla_residuales()`: residuales de correlación (observada menos
  reproducida), con los mayores que .10 en negritas.
* En inglés, el párrafo de método no empieza oraciones con un número.

# diagramaSEM 0.5.0

Tablas y opciones para cubrir el análisis de trayectorias, SEM y MANOVA tal
como se enseñan en clase (salidas de AMOS):

* `tabla_criterios_ajuste()`: χ², χ²/gl, RMSEA, SRMR, CFI, TLI, GFI, PNFI y
  PRATIO contra criterios excelente/aceptable con su fuente, e identificación
  del modelo (observaciones, parámetros, gl, razón N:q).
* `tabla_parametros()`: estimaciones agrupadas como en AMOS (pesos de
  regresión, cargas, covarianzas, varianzas y varianzas de error) con B, EE,
  C.R., p y β.
* `tabla_hipotesis()`: decisión sobre cada hipótesis directa o de mediación,
  con signo esperado opcional.
* `tabla_modificacion()`: índices de modificación con su sintaxis de lavaan.
* `tabla_normalidad()` ahora reporta, como AMOS, asimetría y curtosis con su
  razón crítica por variable y la curtosis multivariada de Mardia con su c.r.
  (las pruebas de Mardia pasan a la segunda parte; sus valores están en
  `$datos$mardia`).
* `tabla_atipicos()`: distancia de Mahalanobis con p1 y p2, y las filas de los
  atípicos en `$atipicos`.
* `tabla_vif()`: tolerancia y VIF de los predictores.
* `tabla_manova()`: MANOVA de un factor con Pillai, Wilks, Hotelling-Lawley y
  Roy (η²p), M de Box y demás supuestos, ANOVA de seguimiento con Bonferroni y
  medias por grupo.
* `diagrama_sem(estandarizado = FALSE)` muestra, como AMOS, la varianza de cada
  variable exógena y de cada error.

# diagramaSEM 0.4.0

* Las tablas se guardan en Word (`.docx`) y `guardar_tablas()` reúne varias
  en un solo documento, numeradas. Las tablas anchas pasan a 10 pt o a una
  página horizontal propia; los símbolos estadísticos van en cursiva (APA).
* Nuevas tablas, verificadas contra `psych`, `semTools` y `lavaan`:
  * `tabla_descriptivos()`, `tabla_perdidos()` y `tabla_normalidad()` (Mardia).
  * `tabla_confiabilidad()`: alfa con IC de Feldt, omega de McDonald y
    correlaciones ítem-total.
  * `tabla_htmt()`: validez discriminante HTMT (y HTMT2).
  * `tabla_comparacion()`: modelos alternativos con Δχ², ΔCFI, ΔRMSEA, AIC y
    BIC.
  * `tabla_metodo_comun()`: factor único de Harman y AFC de un factor.
  * `tabla_factorabilidad()`: KMO, Bartlett, Kaiser, análisis paralelo y MAP.
  * `tabla_efectos()`: efectos directos, indirectos (encontrados
    automáticamente, con bootstrap) y totales.
  * `tabla_invarianza()`: configural, métrica, escalar y estricta, con
    criterios de Chen (2007), Cheung y Rensvold (2002) o Δχ², sugerencias
    para invarianza parcial y el argumento `liberar`.
* `parrafo_metodo()` redacta el párrafo de análisis de datos con sus
  referencias APA.

# diagramaSEM 0.3.0

* Nuevo argumento `estilo` en `diagrama_sem()`: `"clasico"` (el de AMOS, por
  omisión), `"revista"` (en grises, listo para publicar) y `"color"` (un color
  por factor, paleta Okabe-Ito).
* `guardar_diagrama()` y `diagrama_sem()` aceptan `ancho = "pagina"` (174 mm) o
  `ancho = "columna"` (85 mm), informan el tamaño real del texto y avisan si
  queda por debajo de 8 pt.
* Nuevos formatos: `.eps`; `.pdf` y `.eps` con las fuentes incrustadas;
  `.tiff` a 600 ppp por omisión.
* Las trayectorias recíprocas se separan más para que sus valores no se
  encimen.
* Ya no se escribe "-.00".

# diagramaSEM 0.2.0

* Análisis factorial exploratorio (`psych::fa()` y `lavaan::efa()`).
* Tablas APA: `tabla_cargas()`, `tabla_ajuste()`, `tabla_validez()` y
  `guardar_tabla()`.
* Las cargas cruzadas ya no corren encima de otras flechas.

# diagramaSEM 0.1.0

* Primera versión: diagramas de AFC, segundo orden, bifactor, trayectorias y
  SEM.
