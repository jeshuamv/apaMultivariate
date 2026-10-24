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
