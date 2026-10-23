# diagramaSEM

**Diagramas de trayectorias al estilo clásico para modelos de `lavaan`.**

`diagramaSEM` dibuja con una sola línea el diagrama de cualquier modelo estimado con
`lavaan`, con la convención gráfica de los programas clásicos de ecuaciones
estructurales:

- rectángulos para las variables observadas y óvalos para los constructos;
- círculos de error numerados (`e1`, `e2`, …) con su peso fijo en 1;
- cargas y coeficientes sobre las flechas, y R² junto a cada variable explicada;
- flechas curvas de doble punta para las correlaciones;
- relleno con degradado y colores a elección.

El paquete **detecta solo el tipo de modelo** y **acomoda los elementos
automáticamente** con reglas obtenidas al probar 22 modelos distintos. Si el resultado
no convence, se puede elegir otro acomodo, cambiar el lado de los ítems o mover
elementos a mano.

<p align="center"><img src="man/figures/afc_opciones.png" width="420"> <img src="man/figures/segundo_orden.png" width="420"></p>
<p align="center"><img src="man/figures/sem.png" width="850"></p>

## ¿Qué modelos dibuja?

| Tipo de modelo | Ejemplo | Acomodo automático |
|---|---|---|
| AFC de 1 a 3 factores | `cfa()` con 1–3 factores | En fila, correlaciones abajo |
| AFC de 4 o más factores | `cfa()` con 4+ factores | Dos filas (la "cruz" clásica) |
| AFC con 5 o más factores | | Igual, sin dibujar las correlaciones* |
| AFC con cargas cruzadas | un ítem en dos factores | Dos filas |
| AFC con errores correlacionados | `x1 ~~ x3` | Arcos entre los errores |
| Bifactor | factor general + específicos | General arriba, específicos abajo |
| Segundo orden | `G =~ F1 + F2 + F3` | En fila (≤ 3) o dos filas con G al centro (4+) |
| Análisis de trayectorias | `sem()` sin latentes | Por niveles causales, izquierda → derecha |
| Trayectorias con 4+ variables exógenas | regresión múltiple | Por niveles, arriba → abajo |
| SEM completo | latentes con efectos entre sí | Izquierda → derecha; cada constructo pone sus ítems donde menos estorban |
| MIMIC | covariables → un latente | Arriba → abajo |

\* Las correlaciones ocultas no se pierden: quedan en `$correlaciones_ocultas` y se
pueden mostrar con `correlaciones = TRUE`.

## Instalación

El paquete necesita R 4.0 o posterior y el paquete `lavaan`.

**Desde el archivo `.tar.gz`** (por ejemplo, `diagramaSEM_0.1.0.tar.gz`):

```r
install.packages("lavaan")                       # si aún no lo tienes
install.packages("ruta/a/diagramaSEM_0.1.0.tar.gz", repos = NULL, type = "source")
```

En RStudio también puedes usar *Tools → Install Packages… → Install from: Package
Archive File* y elegir el archivo.

Solo se instala una vez. Después, en cada sesión:

```r
library(diagramaSEM)
```

## Uso básico

```r
library(lavaan)
library(diagramaSEM)

modelo <- "visual  =~ x1 + x2 + x3
           textual =~ x4 + x5 + x6
           speed   =~ x7 + x8 + x9"
fit <- cfa(modelo, data = HolzingerSwineford1939)

diagrama_sem(fit)                                  # lo dibuja en la ventana de gráficos
diagrama_sem(fit, archivo = "mi_modelo.png")       # y además lo guarda
```

## Opciones principales

| Argumento | Para qué sirve | Ejemplo |
|---|---|---|
| `acomodo` | Elegir otro acomodo | `acomodo = "dos_filas"` |
| `correlaciones` | Mostrar u ocultar correlaciones entre constructos | `TRUE`, `FALSE` o `0.30` |
| `estandarizado` | Solución estandarizada o no | `estandarizado = FALSE` |
| `estrellas` | Asteriscos de significancia | `estrellas = TRUE` |
| `etiquetas` | Cambiar los nombres que se ven | `etiquetas = c(speed = "Velocidad")` |
| `lados` | Hacia dónde van los ítems de un constructo | `lados = c(F3 = "abajo")` |
| `mover` | Desplazar un elemento a mano | `mover = list(Care = c(1, -0.5))` |
| `paleta` | Colores | `paleta = paleta_sem("azul")` |
| `degradado` | Degradado o color sólido | `degradado = FALSE` |
| `mostrar_r2`, `mostrar_errores`, `mostrar_valores` | Mostrar u ocultar elementos | `mostrar_r2 = FALSE` |
| `decimales` | Número de decimales | `decimales = 3` |
| `titulo` | Título del diagrama | `titulo = "Modelo 1"` |
| `grupo` | Grupo a dibujar en modelos multigrupo | `grupo = 2` |
| `archivo` | Guardar en PNG, PDF, SVG, JPG o TIFF | `archivo = "fig1.pdf"` |

Paletas incluidas: `"lila"` (por omisión), `"azul"`, `"verde"`, `"gris"`, `"durazno"`
y `"blanco"` (sin relleno, para revistas en blanco y negro). También se pueden dar
colores propios: `paleta_sem(latente = c("white", "#FAD3B5"))`.

## Documentación

- Ayuda de cada función: `?diagrama_sem`, `?paleta_sem`, `?guardar_diagrama`.
- Guía de uso completa, con ejemplos de cada tipo de modelo:
  `vignette("guia-de-uso", package = "diagramaSEM")`.

## Limitaciones conocidas

- Con muchas covarianzas entre errores, sus arcos pueden cruzarse entre sí.
- En modelos muy grandes (más de ~18 ítems) el texto se reduce; conviene guardar en
  hoja horizontal o indicar `ancho` y `alto` en `guardar_diagrama()`.
- No dibuja interceptos, umbrales ni parámetros definidos con `:=`.
- Los modelos multigrupo se dibujan un grupo a la vez.

## Licencia

MIT © 2026 Jeshua Moreno Valladares.
