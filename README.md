# diagramaSEM

[![Licencia: MIT](https://img.shields.io/badge/licencia-MIT-blue.svg)](LICENSE)

**Diagramas y tablas APA para tus análisis factoriales, de trayectorias y SEM, con una sola línea de código.**

- **Diagramas** con el estilo clásico de los programas de SEM: rectángulos para los
  ítems, óvalos para los factores, círculos de error (`e1`, `e2`, …), cargas sobre las
  flechas y R² junto a cada variable. El paquete reconoce el tipo de modelo y acomoda
  todo solo.
- **Tablas en formato APA**, listas para Word: cargas factoriales, índices de ajuste y
  validez (CR, AVE y Fornell-Larcker).

Funciona con análisis **exploratorios** (`psych::fa()`, `lavaan::efa()`),
**confirmatorios** (`cfa()`), de **trayectorias** y **SEM** (`sem()`).

<p align="center"><img src="man/figures/afc_opciones.png" width="400"> <img src="man/figures/segundo_orden.png" width="400"></p>

---

## Paso 1. Instalar (solo la primera vez)

Abre R o RStudio, copia estas líneas, pégalas en la consola y presiona Enter:

```r
install.packages("remotes")
remotes::install_github("jeshuamv/diagramaSEM")
```

- Necesitas conexión a internet.
- Si R pregunta si quieres actualizar otros paquetes, **presiona Enter** sin escribir
  nada para continuar.
- Si no aparecen errores en rojo, quedó instalado.

**¿Vas a hacer análisis exploratorios con `psych`?** Instala también:

```r
install.packages(c("psych", "GPArotation"))
```

## Paso 2. Dibujar tu modelo

Cada vez que abras R, carga los paquetes, estima tu modelo como siempre y pásaselo a
`diagrama_sem()`. Elige la plantilla que corresponde a tu análisis y cambia los nombres
por los tuyos (deben ser iguales a los nombres de las columnas de tus datos).

```r
library(lavaan)
library(diagramaSEM)

datos <- read.csv("mis_datos.csv")      # tus datos
```

### Análisis factorial confirmatorio (AFC)

```r
modelo <- "
  Factor1 =~ item1 + item2 + item3       # =~  se mide con
  Factor2 =~ item4 + item5 + item6
"
fit <- cfa(modelo, data = datos)
diagrama_sem(fit)
```

### Análisis de trayectorias (path)

```r
modelo <- "
  M ~ X                                  # ~   X influye en M
  Y ~ M + X                              #     M y X influyen en Y
"
fit <- sem(modelo, data = datos)
diagrama_sem(fit)
```

<p align="center"><img src="man/figures/path_swiss.png" width="620"></p>

### Modelo de ecuaciones estructurales (SEM)

Combina las dos cosas: factores medidos con sus ítems (`=~`) y efectos entre ellos (`~`).

```r
modelo <- "
  Factor1 =~ item1 + item2 + item3
  Factor2 =~ item4 + item5 + item6
  Factor2 ~ Factor1                      # Factor1 influye en Factor2
"
fit <- sem(modelo, data = datos)
diagrama_sem(fit)
```

### Análisis factorial exploratorio (AFE)

```r
library(psych)
afe <- fa(datos, nfactors = 3, rotate = "oblimin")   # solo las columnas de los ítems
diagrama_sem(afe)
```

En el exploratorio cada ítem carga en todos los factores; el diagrama muestra solo las
cargas de .30 o más (puedes cambiarlo con `umbral = 0.40`).

### ¿Quieres probar antes con datos de ejemplo?

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

```r
diagrama_sem(fit, archivo = "mi_diagrama.png")   # imagen para Word o PowerPoint
diagrama_sem(fit, archivo = "mi_diagrama.pdf")   # PDF de alta calidad
```

El archivo se guarda en tu carpeta de trabajo. Para saber cuál es, escribe `getwd()`.

---

## Tablas para tu reporte

Tres funciones crean las tablas que suelen pedirse en artículos y tesis. Se ven en la
consola y, con `archivo = "....rtf"`, se guardan en un documento que **se abre en Word**
con formato APA (desde Word puedes guardarlo como `.docx`).

| Tabla | Código | Sirve para |
|---|---|---|
| Cargas factoriales | `tabla_cargas(fit)` | AFE (con comunalidades, varianza explicada y correlaciones entre factores) y AFC/SEM (cargas estandarizadas, EE, *p* y R²) |
| Índices de ajuste | `tabla_ajuste(fit)` | AFC, SEM y trayectorias: χ², gl, *p*, CFI, TLI, RMSEA, SRMR. Compara varios modelos: `tabla_ajuste("Modelo A" = fit1, "Modelo B" = fit2)` |
| Confiabilidad y validez | `tabla_validez(fit)` | AFC: CR, AVE y criterio de Fornell-Larcker |

```r
tabla_cargas(fit)                                   # se ve en la consola
tabla_cargas(fit, archivo = "tabla_cargas.rtf")     # y se guarda para Word
tabla_ajuste(fit, archivo = "tabla_ajuste.rtf")
tabla_validez(fit, archivo = "tabla_validez.rtf")
```

También puedes guardar en `.html` (para copiar y pegar en Word o Google Docs) o en
`.csv` (para Excel).

---

## Cambios más comunes

Agrega la opción dentro de `diagrama_sem( )`, separada por una coma. Puedes combinar
varias.

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
| En el AFE, solo cargas de .40 o más | `diagrama_sem(afe, umbral = 0.40)` |

`etiquetas` también funciona en las tablas: `tabla_cargas(afe, etiquetas = c(MR1 = "Ansiedad"))`.

Colores disponibles: `"lila"` (el de siempre), `"azul"`, `"verde"`, `"gris"`,
`"durazno"` y `"blanco"`.

---

## ¿Qué modelos puede dibujar?

| Modelo | ¿Cómo lo reconoce? |
|---|---|
| AFE | Viene de `psych::fa()` o `lavaan::efa()` |
| AFC (uno o varios factores) | Solo tiene factores con sus ítems |
| AFC de segundo orden | Un factor está formado por otros factores |
| Bifactor | Un factor general carga en todos los ítems |
| Análisis de trayectorias | Solo variables observadas con efectos (`~`) |
| SEM completo | Factores con efectos (`~`) entre ellos |

Para ver qué reconoció, guarda el resultado y escríbelo en la consola:

```r
d <- diagrama_sem(fit)
d
```

<p align="center"><img src="man/figures/sem.png" width="820"></p>

---

## Más información

- **[Guía de uso completa](guia/GUIA.md)**: cada tipo de análisis, todas las opciones
  y todas las tablas, con ejemplos e imágenes.
- **Ayuda dentro de R**: `?diagrama_sem`, `?tabla_cargas`, `?tabla_ajuste`,
  `?tabla_validez`.

## Problemas frecuentes

**`could not find function "diagrama_sem"`** (no se encuentra la función): falta
cargar el paquete. Escribe `library(diagramaSEM)`.

**`there is no package called 'diagramaSEM'`**: el paquete no está instalado. Repite
el Paso 1.

**`there is no package called 'remotes'`**: corre primero `install.packages("remotes")`.

**Error con `fa()` que menciona `GPArotation`**: corre `install.packages("GPArotation")`.

**El diagrama se ve pequeño o amontonado en RStudio**: agranda el panel de gráficos o,
mejor, guárdalo con `archivo = "..."`. En el archivo las medidas se ajustan solas.

**Una flecha o un grupo de ítems quedó en mal lugar**: prueba otro `acomodo`
(`"fila"`, `"dos_filas"`, `"arbol"`, `"izq_der"`, `"arriba_abajo"`) o consulta la
sección *Ajustes a mano* de la [guía](guia/GUIA.md).

**El archivo `.rtf` se abre en otro programa (por ejemplo, TextEdit en Mac)**: haz
clic derecho → *Abrir con* → *Microsoft Word*.

¿Encontraste un error? Repórtalo en la pestaña
[Issues](https://github.com/jeshuamv/diagramaSEM/issues) de esta página.

## Licencia

MIT © 2026 Jeshua Moreno Valladares. Puedes usarlo, compartirlo y modificarlo
libremente.
