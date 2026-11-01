# apaMultivariate 0.12.1

* `tabla_hipotesis()` acepta tres tipos nuevos de hipótesis:
  * Efecto indirecto total (todas las trayectorias de X a Y): `"X -> * -> Y"`.
  * Efecto total (directo + indirectos): `"X -> Y [total]"`; sin mediación es
    el efecto directo.
  * Covarianza o correlación: `"X <-> W"` (o `"X ~~ W"`), en cualquier orden;
    sirve para la correlación entre factores de un AFC y para covarianzas
    residuales. Si las dos variables son exógenas observadas, explica que hay
    que estimar el modelo con `fixed.x = FALSE`.
* `tabla_hipotesis()` da mensajes claros cuando una trayectoria está fijada
  (`M ~ 0.5*X`), cuando el signo está mal escrito (`(++)`) o cuando se pide un
  efecto directo que no existe pero sí hay indirectos (sugiere `-> * ->` o
  `[total]`). Antes, la trayectoria fijada daba un error incomprensible.
* `tabla_efectos()` y `tabla_hipotesis()`: si el modelo no admite bootstrap
  (variables ordinales con WLSMV, `conditional.x = TRUE`) o el bootstrap falla,
  usan el método delta y lo avisan; antes se detenían con un error de lavaan.
* Correcciones de una revisión completa del código (validado contra psych,
  semTools, lmtest, car, rstatix y lavaan):
  * `tabla_influencia()` fallaba (o tomaba variables equivocadas del entorno)
    cuando la fórmula tenía transformaciones (`log()`, `poly()`...) y algún
    caso superaba el corte de Cook; la tabla de sensibilidad ahora se calcula
    con la matriz del modelo.
  * `tabla_subconjuntos()` fallaba con transformaciones en la fórmula (también
    en la variable dependiente, como `log(y)`).
  * `parrafo_metodo()` describía MLMV, MLMVS, WLSM, WLSMVS y ULSMV como ML,
    WLSMV o ULS; ahora nombra el estimador correcto.
  * `tabla_confiabilidad()` con un factor de un solo indicador daba alfa e IC
    vacíos, omega = 1.00 y un aviso con "NA"; ahora muestra "—" y lo explica.
  * `tabla_manova()` no reportaba los atípicos multivariados cuando los nombres
    de fila de los datos eran texto; ahora da el número de fila en `datos`.
  * `modelo_alternativo()` mostraba dos veces el mensaje "No hay cambios".
  * `tabla_efectos()`, `tabla_moderacion()`, `tabla_regresion()` y
    `tabla_factorabilidad()` ya no alteran el generador de números aleatorios
    del usuario al usar `semilla` (las dos primeras lo dejaban cambiado).
  * `guardar_tablas(t1, t2, "archivo.docx")` acepta el nombre del archivo sin
    escribir `archivo =`.
* `tabla_efectos()`: con el método delta, el error estándar de un indirecto
  con un tramo fijado usa el valor fijo de ese tramo (antes lo tomaba como 0).
  `$datos` incluye las columnas `de` y `a` de cada efecto.

# apaMultivariate 0.12.0

* Regresión lineal múltiple (con `lm()`), con su plantilla `plantilla("regresion")`:
  * `tabla_regresion()`: B, EE, IC, β, *t*, *p*, sr² y VIF, con R², R²
    ajustada y *F* en la nota; errores estándar robustos HC3 (`robusto = TRUE`)
    e intervalos por bootstrap (`bootstrap = 2000`). Avisa si hay
    multicolinealidad grave o pocos casos por predictor.
  * `tabla_supuestos_regresion()`: Shapiro-Wilk, Breusch-Pagan (Koenker),
    RESET, Durbin-Watson, VIF máximo y distancia de Cook, con una columna de
    interpretación. Coincide con `lmtest::bptest()` y `lmtest::resettest()`.
  * `tabla_influencia()`: residuo estudentizado, apalancamiento, Cook, DFFITS,
    DFBETAS y COVRATIO con sus puntos de corte, y una tabla de sensibilidad con
    los coeficientes con y sin el caso más influyente.
  * `tabla_subconjuntos()`: todas las combinaciones de predictores con R²
    ajustada, Cp de Mallows, AICc, BIC y PRESS.
* `grafica_sedimentacion()`: gráfica de sedimentación con la línea del análisis
  paralelo (factores, componentes o ambos); coincide con `tabla_factorabilidad()`.
* `comparar_soluciones()` compara también rotaciones (`rotate = c("none",
  "varimax", "promax")`) y métodos de extracción (`fm = c("pa", "ml")`), con la
  varianza de cada factor y una tabla del factor en que carga cada ítem en cada
  solución. La varianza explicada ahora es la suma de las comunalidades (antes,
  con rotaciones oblicuas, se sumaban las cargas de patrón al cuadrado).
* `tabla_factorabilidad()`, `comparar_soluciones()` y `grafica_sedimentacion()`
  aceptan `cor = "policorica"` para ítems ordinales; con Pearson, la nota sugiere
  policóricas si los ítems tienen 5 categorías o menos.
* `tabla_hipotesis()`: una hipótesis no respaldada dice "No se sostiene" (antes
  "Se rechaza"; un efecto no significativo no demuestra que sea falsa).
* `tabla_supuestos_regresion()` dice "no se rechaza" en lugar de afirmar que el
  supuesto se cumple, y la tabla de sensibilidad de `tabla_influencia()` mide el
  cambio en errores estándar (el porcentaje se disparaba con coeficientes cercanos
  a cero).
* Usabilidad:
  * Mensajes que dicen qué hacer cuando se pasa el texto del modelo, los datos o una
    regresión de `lm()` en lugar del modelo estimado, cuando un objeto tiene el nombre
    mal escrito (sugiere el parecido), cuando los argumentos de `revisar_datos()` van
    al revés y cuando falta la extensión del archivo o no se puede leer.
  * `guardar_tablas()` guarda en `tablas_apa.docx` si no se indica el archivo; las
    tablas y figuras guardadas muestran la ruta completa; aviso claro si la carpeta no
    existe o el Word está abierto.
  * `plantilla()` e `idioma_sem()` aceptan mayúsculas, acentos y espacios
    (`plantilla("Regresión")`, `idioma_sem("inglés")`).
  * Al cargar el paquete en RStudio se indica cómo empezar.
* `leer_datos()` quita la columna de números de fila que agrega
  `write.csv()` sin `row.names = FALSE`, y `revisar_datos()` avisa si una
  columna parece números de fila o un identificador.
* Plantilla de AFE con la gráfica de sedimentación y la comparación de
  rotaciones.

# apaMultivariate 0.11.0

* `modelos_rivales()`: compara un AFC con sus rivales teóricos (un factor,
  factores independientes, segundo orden y bifactor), estimados con las
  mismas opciones; señala problemas de estimación y dibuja cada modelo.
* `comparar_soluciones()`: compara soluciones del AFE con distinto número de
  factores (varianza explicada, ítems sin carga, cargas cruzadas, factores con
  menos de tres ítems, correlación entre factores, RMSEA, TLI y BIC) y dibuja
  cada una.
* `modelo_alternativo()` funciona también con un AFC: quita uno por uno los
  ítems o las cargas cruzadas con carga menor que `carga_minima` (sin dejar
  factores con menos de tres ítems) y sugiere covarianzas entre errores y
  cargas cruzadas según los índices de modificación.
* Plantillas de AFC y AFE con estos pasos.
* Guía: las imágenes tenían rutas absolutas y no se veían en GitHub.

# apaMultivariate 0.10.0

* `modelo_alternativo()`: propone un modelo alternativo de trayectorias o SEM.
  Quita una por una las trayectorias no significativas, prueba con Δχ² si eso
  empeora el ajuste, lo compara con el original y dibuja su diagrama;
  conserva el estimador y las demás opciones. Con `agregar = 1` agrega también
  las trayectorias que sugieren los índices de modificación (sin crear
  ciclos); si no, solo las lista. Advierte que la reespecificación es
  exploratoria.
* `grafica_normalidad()`: gráfica Q-Q chi-cuadrado de las distancias de
  Mahalanobis, con los atípicos marcados.
* `tabla_comparacion()`: ya no compara χ², AIC ni BIC entre modelos con
  distintas variables o distintos casos (no son comparables); lo explica en la
  nota.
* `parrafo_metodo()`: menciona la verosimilitud de Wishart y, con
  `atipicos =`, los casos atípicos quitados y su criterio.

# apaMultivariate 0.9.5

Plantillas revisadas resolviendo con ellas un ejercicio real de clase
(análisis de trayectorias de Crowson), cambiando solo lo marcado:

* La plantilla de trayectorias guarda los dos diagramas (estandarizado y no
  estandarizado) como imagen, estima con `likelihood = "wishart"` para
  coincidir con AMOS, y guarda en el Word también la normalidad, los atípicos
  y las hipótesis.
* Cada tabla se calcula una sola vez y se guarda en un objeto: el bootstrap ya
  no se repite al guardar en Word.
* Al quitar atípicos o cambiar de estimador, el modelo se vuelve a estimar con
  `update()`, que conserva las demás opciones.
* La línea de los datos está marcada como `>>> CAMBIA AQUÍ <<<` en todas las
  plantillas, con el ejemplo para escribir el nombre del archivo.

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
