# diagramaSEM

[![Licencia: MIT](https://img.shields.io/badge/licencia-MIT-blue.svg)](LICENSE)

**Dibuja el diagrama de tu modelo de `lavaan` con una sola línea de código.**

Si ya estimaste un análisis factorial confirmatorio (AFC), un análisis de
trayectorias o un modelo de ecuaciones estructurales (SEM) con `lavaan`,
`diagramaSEM` lo dibuja con el estilo clásico de los programas de SEM:
rectángulos para los ítems, óvalos para los constructos, círculos de error
(`e1`, `e2`, …), cargas sobre las flechas y R² junto a cada variable.

No tienes que acomodar nada: el paquete reconoce el tipo de modelo y decide
dónde va cada elemento.

<p align="center"><img src="man/figures/afc_opciones.png" width="400"> <img src="man/figures/segundo_orden.png" width="400"></p>

---

## Paso 1. Instalar (solo la primera vez)

Abre R o RStudio, copia estas dos líneas, pégalas en la consola y presiona Enter:

```r
install.packages("remotes")
remotes::install_github("jeshuamv/diagramaSEM")
```

- Necesitas conexión a internet.
- Si R pregunta si quieres actualizar otros paquetes, **presiona Enter** sin
  escribir nada para continuar.
- Al terminar no aparece ningún mensaje especial. Si no hubo errores en rojo,
  quedó instalado.

## Paso 2. Dibujar tu modelo

Cada vez que abras R, carga los paquetes, estima tu modelo como siempre y
pásaselo a `diagrama_sem()`:

```r
library(lavaan)
library(diagramaSEM)

datos <- read.csv("mis_datos.csv")          # tus datos

modelo <- "
  Factor1 =~ item1 + item2 + item3
  Factor2 =~ item4 + item5 + item6
"
fit <- cfa(modelo, data = datos)            # tu modelo, como siempre

diagrama_sem(fit)                           # ¡listo! aparece el diagrama
```

Cambia `mis_datos.csv`, los nombres de los factores y los de los ítems por los
tuyos. Para un análisis de trayectorias o un SEM usa `sem()` en lugar de `cfa()`;
`diagrama_sem()` se usa igual.

### ¿Quieres probarlo antes con datos de ejemplo?

Este código funciona tal cual, porque los datos vienen incluidos en `lavaan`:

```r
library(lavaan)
library(diagramaSEM)

modelo <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
fit <- cfa(modelo, data = HolzingerSwineford1939)

diagrama_sem(fit)
```

## Paso 3. Guardar el diagrama

Agrega `archivo =` con el nombre que quieras:

```r
diagrama_sem(fit, archivo = "mi_diagrama.png")   # imagen para Word o PowerPoint
diagrama_sem(fit, archivo = "mi_diagrama.pdf")   # PDF de alta calidad
```

El archivo se guarda en tu carpeta de trabajo. Para saber cuál es, escribe
`getwd()`.

---

## Cambios más comunes

Agrega la opción dentro de `diagrama_sem( )`, separada por una coma. Puedes
combinar varias.

| Quiero… | Escribe |
|---|---|
| Cambiar los nombres que se ven | `diagrama_sem(fit, etiquetas = c(Factor1 = "Autoestima", item1 = "Ítem 1"))` |
| Otros colores | `diagrama_sem(fit, paleta = paleta_sem("azul"))` |
| Sin color (blanco y negro) | `diagrama_sem(fit, paleta = paleta_sem("blanco"))` |
| Asteriscos de significancia | `diagrama_sem(fit, estrellas = TRUE)` |
| Ocultar las correlaciones entre factores | `diagrama_sem(fit, correlaciones = FALSE)` |
| Ponerle título | `diagrama_sem(fit, titulo = "Modelo de medición")` |
| Otro acomodo | `diagrama_sem(fit, acomodo = "dos_filas")` |
| Valores sin estandarizar | `diagrama_sem(fit, estandarizado = FALSE)` |

Ejemplo combinando varias:

```r
diagrama_sem(fit,
             etiquetas = c(visual = "Visual", textual = "Verbal", speed = "Velocidad"),
             paleta    = paleta_sem("azul"),
             estrellas = TRUE,
             archivo   = "figura1.png")
```

Colores disponibles: `"lila"` (el de siempre), `"azul"`, `"verde"`, `"gris"`,
`"durazno"` y `"blanco"`.

---

## ¿Qué modelos puede dibujar?

| Modelo | ¿Cómo lo reconoce? |
|---|---|
| AFC (uno o varios factores) | Solo tiene factores con sus ítems |
| AFC de segundo orden | Un factor está formado por otros factores |
| Bifactor | Un factor general carga en todos los ítems |
| Análisis de trayectorias | Solo variables observadas con efectos (`~`) |
| SEM completo | Factores con efectos (`~`) entre ellos |

El tipo se detecta cada vez que usas `diagrama_sem()`. Para ver qué reconoció,
guarda el resultado y escríbelo en la consola:

```r
d <- diagrama_sem(fit)
d
```

<p align="center"><img src="man/figures/sem.png" width="820"></p>

---

## Más información

- **[Guía de uso completa](guia/GUIA.md)**: todos los tipos de modelo y
  todas las opciones, con ejemplos e imágenes.
- **Ayuda dentro de R**: escribe `?diagrama_sem`.

## Problemas frecuentes

**`could not find function "diagrama_sem"`** (no se encuentra la función):
falta cargar el paquete. Escribe `library(diagramaSEM)`.

**`there is no package called 'diagramaSEM'`**: el paquete no está instalado.
Repite el Paso 1.

**`there is no package called 'remotes'`**: corre primero
`install.packages("remotes")`.

**El diagrama se ve pequeño o amontonado en RStudio**: agranda el panel de
gráficos o, mejor, guárdalo con `archivo = "..."`. En el archivo las medidas se
ajustan solas.

**Una flecha o un grupo de ítems quedó en mal lugar**: prueba otro `acomodo`
(`"fila"`, `"dos_filas"`, `"arbol"`, `"izq_der"`, `"arriba_abajo"`) o consulta
la sección *Ajustes a mano* de la [guía](guia/GUIA.md).

¿Encontraste un error? Repórtalo en la pestaña
[Issues](https://github.com/jeshuamv/diagramaSEM/issues) de esta página.

## Licencia

MIT © 2026 Jeshua Moreno Valladares. Puedes usarlo, compartirlo y modificarlo
libremente.
