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
