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
