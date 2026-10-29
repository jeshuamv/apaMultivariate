# apaMultivariate

[![Licencia: MIT](https://img.shields.io/badge/licencia-MIT-blue.svg)](LICENSE) [![R-CMD-check](https://github.com/jeshuamv/apaMultivariate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jeshuamv/apaMultivariate/actions/workflows/R-CMD-check.yaml)

**Tablas APA y diagramas para tus análisis factoriales, de trayectorias, SEM, moderación y MANOVA.**

*Multivariate analysis, APA-ready.* Lee tus datos de Excel o SPSS, revisa que todo esté
bien, dibuja el diagrama al estilo AMOS y te entrega las tablas en formato APA en Word,
en español o en inglés. Antes se llamaba `diagramaSEM` ([cómo actualizar](#si-tenías-diagramasem)).

<p align="center"><img src="man/figures/path_swiss.png" width="620"></p>

## Contenido

- **[Empieza aquí](#empieza-aquí)**: de cero a tus tablas en Word, en cinco pasos
- **[Cómo escribir tu modelo a partir del diagrama](#cómo-escribir-tu-modelo-a-partir-del-diagrama)**
- **[Ejemplo completo: análisis de trayectorias](#ejemplo-completo-análisis-de-trayectorias)**
- **[Si tus resultados no coinciden con AMOS o con tu clase](#si-tus-resultados-no-coinciden-con-amos-o-con-tu-clase)**
- [Todas las funciones](#todas-las-funciones) · [Para publicar en una revista](#para-publicar-en-una-revista) · [Problemas frecuentes](#problemas-frecuentes) · [Más información](#más-información)

---

## Empieza aquí

Sigue estos pasos en orden. No necesitas leer nada más del README para hacer tu primer
análisis.

### 1. Instala R y RStudio (solo la primera vez)

1. **Instala R** desde [cran.r-project.org](https://cran.r-project.org): elige tu
   sistema (Windows, Mac o Linux) y descarga la versión más reciente.
2. **Instala RStudio Desktop** (gratis) desde
   [posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop/).
   RStudio es el programa en el que vas a trabajar; R funciona por debajo.
3. **Abre RStudio.** Verás la *consola* (abajo a la izquierda), donde se escriben las
   instrucciones, y el panel de gráficos y archivos (abajo a la derecha).
4. Un **script** es un archivo de texto con tus instrucciones (el paso 3 te crea uno).
   Para ejecutar una línea, pon el cursor en ella y presiona **Ctrl + Enter** (en Mac,
   **Cmd + Enter**).

### 2. Instala el paquete (solo la primera vez)

Copia estas dos líneas en la consola de RStudio y presiona Enter:

```r
install.packages("remotes")
remotes::install_github("jeshuamv/apaMultivariate")
```

- Necesitas conexión a internet.
- Si R pregunta si quieres actualizar otros paquetes, **presiona Enter** sin escribir
  nada para continuar.
- Si no aparecen errores en rojo, quedó instalado.
- **Solo para análisis exploratorio (AFE)**: instala también
  `install.packages(c("psych", "GPArotation"))`.

### 3. Crea la plantilla de tu análisis

Primero elige tu carpeta de trabajo, la que tiene tus datos (*Session → Set Working
Directory → Choose Directory…*). Después corre **una** de estas líneas en la consola:

```r
library(apaMultivariate)

plantilla("trayectorias")   # análisis de trayectorias (path)
plantilla("afc")            # análisis factorial confirmatorio
plantilla("afe")            # análisis factorial exploratorio
plantilla("sem")            # ecuaciones estructurales (medición + estructural)
plantilla("moderacion")     # moderación y mediación moderada
plantilla("manova")         # MANOVA
```

Se crea un script (`analisis_trayectorias.R`, …) en tu carpeta y se abre en RStudio.
Trae **todo** el análisis en orden y explicado: leer los datos, revisarlos, estimar,
supuestos, diagrama, ajuste, efectos, hipótesis y guardar en Word.

### 4. Cambia lo marcado con `>>> CAMBIA AQUÍ <<<`

Casi siempre son tres cosas:

1. **El archivo de tus datos**: `datos <- leer_datos("tu_archivo.xlsx")` (Excel, SPSS,
   CSV…). O deja `leer_datos()` sin nada y se abre una ventana para elegirlo.
2. **El modelo**: tus variables y sus relaciones. Si no sabes cómo escribirlo, lee
   [Cómo escribir tu modelo a partir del diagrama](#cómo-escribir-tu-modelo-a-partir-del-diagrama).
3. **Las hipótesis** (en trayectorias y SEM): una por renglón, como `"X -> Y (+)"`.

Antes de cambiar nada, puedes correr la plantilla con los datos de ejemplo que trae para
ver cómo funciona.

### 5. Corre el script línea por línea

Pon el cursor en la primera línea y presiona **Ctrl + Enter** (Mac: **Cmd + Enter**);
RStudio corre esa instrucción y baja a la siguiente. Avanza así, en orden, y lee lo que
aparece en la consola:

- `revisar_datos()` y `revisar_modelo()` te avisan en español si algo está mal y qué
  hacer (✔ bien, ⚠ aviso, ✖ problema que hay que corregir).
- Las tablas se ven en la consola; al final, `guardar_tablas()` las pone en un Word.
- Los diagramas aparecen en el panel de gráficos y se guardan como imagen.

Al terminar, `getwd()` te dice en qué carpeta quedaron los archivos. Para interpretar y
redactar cada resultado, usa la guía
**[Cómo interpretar y redactar los resultados](guia/INTERPRETACION.md)**.

Cuatro reglas que evitan la mayoría de los errores: corre las líneas **en orden**; R
distingue **mayúsculas de minúsculas** (`Item1` no es `item1`); los textos van **entre
comillas** (`"mis_datos.xlsx"`), y cada paréntesis que abres se tiene que cerrar.

---

## Cómo escribir tu modelo a partir del diagrama

El modelo es un texto entre comillas con una relación por renglón. Hay tres símbolos:

| Símbolo | Significa | En el diagrama |
|---|---|---|
| `~` | "es influido por" (regresión) | Flecha de una sola punta **entre variables** |
| `=~` | "se mide con" (factor y sus ítems) | Flechas de un **óvalo** a sus **rectángulos** |
| `~~` | "se correlaciona con" | Flecha **curva de dos puntas** |

### Análisis de trayectorias: las flechas que llegan

Para cada variable **que recibe flechas**, escribe un renglón: la variable a la izquierda
de `~`, y a la derecha **todas las que le apuntan**, separadas por `+`.

En el diagrama de arriba, Examen recibe una flecha (de Educación) y Fecundidad recibe tres
(de Educación, Examen y Católicos):

```r
modelo <- "
  Examination ~ Education
  Fertility   ~ Education + Examination + Catholic
"
```

- Las variables que **no reciben ninguna flecha** (Educación y Católicos) son
  **exógenas**: no tienen renglón propio.
- La **correlación entre exógenas** (la flecha curva entre Educación y Católicos) **no se
  escribe**: `sem()` la incluye sola.
- Los **errores** (`e1`, `e2`) **no se escriben**: los pone el programa.
- Usa los nombres **exactos** de las columnas de tus datos (`names(datos)` te los muestra),
  no los que aparecen en el diagrama.

### Análisis factorial confirmatorio: los factores y sus ítems

Un renglón por factor: el factor (el óvalo) a la izquierda de `=~` y sus ítems (los
rectángulos a los que apunta) a la derecha.

```r
modelo <- "
  Visual   =~ x1 + x2 + x3
  Verbal   =~ x4 + x5 + x6
  Rapidez  =~ x7 + x8 + x9
"
```

- El nombre del factor lo inventas tú (sin espacios ni acentos); los ítems deben ser
  columnas de tus datos.
- Las correlaciones entre factores **no se escriben**: `cfa()` las incluye solas.
- Una correlación entre **errores de dos ítems** sí se escribe, y debe justificarse:
  `x7 ~~ x8`.
- **Segundo orden**: agrega un renglón con el factor general y los factores de primer
  orden: `General =~ Visual + Verbal + Rapidez`.

### SEM: las dos cosas juntas

Primero los factores (`=~`) y después las flechas entre ellos (`~`):

```r
modelo <- "
  Apoyo        =~ a1 + a2 + a3
  Satisfaccion =~ s1 + s2 + s3
  Lealtad      =~ l1 + l2 + l3
  Satisfaccion ~ Apoyo
  Lealtad      ~ Satisfaccion + Apoyo
"
```

Después de escribirlo, corre `revisar_datos(datos, modelo)`: te dice si un nombre no
coincide con tus columnas (y te sugiere el correcto) o si a un renglón le falta el
símbolo.

---

## Ejemplo completo: análisis de trayectorias

Este ejemplo funciona tal cual, porque usa los datos `swiss`, que vienen con R. Es el
modelo del diagrama de arriba. Cópialo en un script y córrelo línea por línea:

```r
library(lavaan)
library(apaMultivariate)

datos <- swiss                         # con tus datos: datos <- leer_datos("archivo.xlsx")

# 1. Modelo
modelo <- "
  Examination ~ Education
  Fertility   ~ Education + Examination + Catholic
"
revisar_datos(datos, modelo)           # ¿nombres correctos? ¿perdidos? ¿tamaño de muestra?

# 2. Estimar
fit <- sem(modelo, data = datos, likelihood = "wishart")   # "wishart": como AMOS
revisar_modelo(fit)                    # ¿hubo problemas al estimar?

# 3. Supuestos
tabla_normalidad(fit)                  # normalidad (Mardia)
tabla_atipicos(fit)                    # atípicos (Mahalanobis)

# 4. Diagramas
diagrama_sem(fit)                      # estandarizado, con R²
diagrama_sem(fit, estandarizado = FALSE)   # no estandarizado, con varianzas

# 5. Ajuste: χ², RMSEA (con su IC), CFI, TLI, SRMR, PNFI…, exógenas y endógenas
tabla_criterios_ajuste(fit)

# 6. Estimaciones, efectos directos, indirectos y totales, y R²
tabla_parametros(fit)
tabla_efectos(fit)

# 7. Hipótesis: (+) o (-) si predice el signo; con dos flechas, efecto indirecto
tabla_hipotesis(fit, c(
  H1 = "Education -> Examination (+)",
  H2 = "Education -> Fertility (-)",
  H3 = "Catholic -> Fertility (+)",
  H4 = "Examination -> Fertility (-)",
  H5 = "Education -> Examination -> Fertility"
))

# 8. Todo a Word
guardar_tablas(tabla_criterios_ajuste(fit), tabla_parametros(fit), tabla_efectos(fit),
               archivo = "tablas_trayectorias.docx")
parrafo_metodo(fit, bootstrap = 1000, archivo = "metodo.docx")
```

Es lo mismo que hace `plantilla("trayectorias")`. Para AFC y SEM, las plantillas siguen
la misma lógica y agregan las tablas de medición (cargas, confiabilidad, validez, HTMT).

---

## Si tus resultados no coinciden con AMOS o con tu clase

- **El χ² es un poco distinto.** AMOS divide entre *N* − 1 y `lavaan`, entre *N*. Para
  que coincida, estima con `likelihood = "wishart"`:
  `sem(modelo, data = datos, likelihood = "wishart")` (o en `cfa()`). Cambian un poco
  el χ² y los índices que dependen de él; las estimaciones no.
- **El IC del RMSEA**: por defecto es al 90 %; si te lo piden al 95 %, usa
  `tabla_criterios_ajuste(fit, ic_rmsea = .95)`.
- **Omega, CR o AVE**: hay varias fórmulas. `tabla_confiabilidad()` calcula ω igual que
  `semTools::compRelSEM()`; `tabla_validez()` usa las de Fornell y Larcker, y con
  `metodo = "semtools"` da las de `semTools`.
- **Mardia**: el paquete usa la fórmula original (como el paquete `MVN`); `psych::mardia()`
  da valores un poco distintos.

---

## Todas las funciones

### Leer y revisar

| Código | Qué hace |
|---|---|
| `leer_datos("archivo")` | Lee Excel, SPSS, CSV (coma o punto y coma), Stata, SAS o texto; sin nada, abre una ventana. Quita las etiquetas de SPSS y arregla los nombres con espacios o acentos. `perdidos = c(99)` marca códigos de dato perdido |
| `revisar_datos(datos)` | Revisa los datos: columnas de texto o constantes, códigos de perdido, % de perdidos, ítems tipo Likert, tamaño de muestra |
| `revisar_datos(datos, modelo)` | Lo mismo, y además que los nombres del modelo existan en los datos |
| `revisar_modelo(fit)` | Después de estimar: explica en español los avisos de `lavaan` (no convergió, no identificado, casos Heywood…) y qué hacer |
| `plantilla("tipo")` | Crea el script completo de un análisis |

### Tablas en formato APA

Listas para Word, las que suelen pedirse en una tarea, una tesis o un artículo:

| Etapa | Código | Qué reporta |
|---|---|---|
| Datos | `tabla_descriptivos(datos)` | n, M, DE, asimetría, curtosis y correlaciones (la "Tabla 1") |
| | `tabla_perdidos(datos)` | Datos perdidos por variable |
| | `tabla_normalidad(datos)` | Normalidad: asimetría y curtosis con c.r. (como AMOS) y Mardia |
| | `tabla_atipicos(datos)` | Atípicos multivariados (Mahalanobis, p1 y p2) |
| | `quitar_atipicos(datos, modelo)` | Devuelve los datos sin los atípicos (*p* < .001) y te dice cuántos quitó |
| AFE | `tabla_factorabilidad(datos)` | KMO, Bartlett y cuántos factores retener (Kaiser, análisis paralelo, MAP) |
| | `tabla_cargas(afe)` | Cargas, comunalidades, varianza explicada |
| Medición | `tabla_cargas(fit)` | Cargas estandarizadas, EE, *p* y R² |
| | `tabla_confiabilidad(fit)` | Alfa de Cronbach (con IC), omega de McDonald, correlaciones ítem-total |
| | `tabla_validez(fit)` | CR, AVE y Fornell-Larcker |
| | `tabla_bifactor(fit)` | Modelos bifactor o de segundo orden: ωH, ωHS, ECV, PUC, H y FD |
| | `tabla_htmt(fit)` | Validez discriminante HTMT (la que hoy piden los revisores) |
| | `tabla_metodo_comun(fit)` | Sesgo de método común (Harman y AFC de un factor) |
| Ajuste | `tabla_ajuste(fit)` | χ², CFI, TLI, RMSEA, SRMR |
| | `tabla_criterios_ajuste(fit)` | Cada índice (χ², χ²/gl, RMSEA, SRMR, CFI, TLI, GFI, PNFI, PRATIO) contra criterios excelente/aceptable (cámbialos con `cortes`; IC del RMSEA al 95 % con `ic_rmsea = .95`), identificación del modelo (p, q, gl) y variables exógenas y endógenas |
| | `tabla_modificacion(fit)` | Índices de modificación |
| | `tabla_residuales(fit)` | Residuales de correlación: dónde ajusta mal el modelo |
| | `tabla_comparacion("A" = fit1, "B" = fit2)` | Modelos alternativos: Δχ², ΔCFI, AIC, BIC |
| | `tabla_invarianza(modelo, datos, grupo = "sexo")` | Invarianza configural, métrica, escalar y estricta |
| Estructural | `tabla_parametros(fit)` | Estimaciones como en AMOS: B, EE, C.R., p, β, covarianzas y varianzas |
| | `tabla_efectos(fit)` | Efectos directos, indirectos (bootstrap) y totales |
| | `tabla_hipotesis(fit, c(H1 = "X -> Y (+)"))` | Cada hipótesis: se sostiene o se rechaza |
| | `tabla_vif(fit)` | Multicolinealidad entre predictores |
| Moderación | `tabla_moderacion(datos, x = "X", w = "W", y = "Y")` | Moderación (modelo 1 de PROCESS) con pendientes simples y Johnson-Neyman; mediación moderada (modelos 7 y 14) con el índice de mediación moderada. `grafica_interaccion()` dibuja la interacción |
| MANOVA | `tabla_manova(datos, c("Y1", "Y2"), "grupo")` | Pillai, Wilks, Hotelling, Roy con η²p; M de Box y demás supuestos; ANOVA de seguimiento; post hoc (Tukey o Games-Howell) con *d* de Cohen |
| Método | `parrafo_metodo(fit)` | Párrafo de software y estimación, con referencias APA |

Todas se ven en la consola. Para Word:

```r
tabla_validez(fit, archivo = "tabla_validez.docx")       # una tabla

guardar_tablas(tabla_descriptivos(datos),                # todas en un documento,
               tabla_confiabilidad(fit),                 # numeradas
               tabla_ajuste(fit),
               tabla_htmt(fit),
               archivo = "tablas_articulo.docx")
```

Las tablas anchas se pasan a letra de 10 pt o a una página horizontal. Los valores de
cada tabla se cotejaron con `psych`, `semTools`, `lavaan`, `car`, `rstatix` y
`effectsize`. La guía incluye el análisis de trayectorias y SEM **paso a paso** (supuestos,
diagrama, ajuste, estimaciones, hipótesis, efectos y modelo alternativo) y un ejemplo de
MANOVA. Los detalles y ejemplos de
cada una están en la sección *Tablas en formato APA* de la [guía](guia/GUIA.md).

### Resultados en inglés

Para una revista en inglés, escribe esto una vez al inicio del script:

```r
idioma_sem("en")
```

Desde ahí, títulos, encabezados, notas, el párrafo de método y sus referencias salen en
inglés, con los términos de APA (*M*, *SD*, *SE*, *df*, CI, *Note.*, *Table*). Para volver
al español: `idioma_sem("es")`.

---

## Diagramas

```r
diagrama_sem(fit)                              # para verlo
diagrama_sem(fit, archivo = "mi_diagrama.png") # para Word o PowerPoint
diagrama_sem(fit, archivo = "mi_diagrama.pdf") # PDF de alta calidad
```

El archivo se guarda en tu carpeta de trabajo (`getwd()` te dice cuál es).

### Estilos: clásico, revista y color

```r
diagrama_sem(fit)                       # clásico (el de AMOS)
diagrama_sem(fit, estilo = "revista")   # en grises, para artículos
diagrama_sem(fit, estilo = "color")     # un color por factor
```

<p align="center"><img src="man/figures/estilo_revista.png" width="760"></p>
<p align="center"><img src="man/figures/estilo_color.png" width="760"></p>

Los estilos `"revista"` y `"color"` usan letra más grande y sin serifa, relleno plano,
errores y correlaciones en gris, y cada valor sobre un fondo blanco para que ninguna
línea lo cruce. No escriben los "1" de las flechas de error (eso se aclara en la nota
de la figura). Los colores se distinguen aun con daltonismo y se leen bien si la revista
imprime en blanco y negro.

### Cambios más comunes

Agrega la opción dentro de `diagrama_sem( )`, separada por una coma. Puedes combinar
varias.

| Quiero… | Escribe |
|---|---|
| Cambiar los nombres que se ven | `diagrama_sem(fit, etiquetas = c(Factor1 = "Autoestima", item1 = "Ítem 1"))` |
| Estilo para un artículo | `diagrama_sem(fit, estilo = "revista")` |
| Un color por factor | `diagrama_sem(fit, estilo = "color")` |
| Otros colores (estilo clásico) | `diagrama_sem(fit, paleta = paleta_sem("azul"))` |
| Sin color (blanco y negro) | `diagrama_sem(fit, paleta = paleta_sem("blanco"))` |
| Asteriscos de significancia | `diagrama_sem(fit, estrellas = TRUE)` |
| Ocultar las correlaciones entre factores | `diagrama_sem(fit, correlaciones = FALSE)` |
| Ponerle título | `diagrama_sem(fit, titulo = "Modelo de medición")` |
| Otro acomodo | `diagrama_sem(fit, acomodo = "dos_filas")` |
| Valores sin estandarizar (con varianzas, como AMOS) | `diagrama_sem(fit, estandarizado = FALSE)` |
| En el AFE, solo cargas de .40 o más | `diagrama_sem(afe, umbral = 0.40)` |

`etiquetas` también funciona en las tablas: `tabla_cargas(afe, etiquetas = c(MR1 = "Ansiedad"))`.

Paletas del estilo clásico: `"lila"` (el de siempre), `"azul"`, `"verde"`, `"gris"`,
`"durazno"` y `"blanco"`.

### ¿Qué modelos puede dibujar?

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

## Para publicar en una revista

Las revistas piden que el texto mida **al menos 8 pt ya impreso** y que la figura se
entregue al ancho con que se publicará. Usa:

```r
diagrama_sem(fit, estilo = "revista", archivo = "figura1.pdf", ancho = "pagina")
```

- `ancho = "pagina"` (174 mm) sirve para casi cualquier modelo; `ancho = "columna"`
  (85 mm) solo para modelos muy pequeños.
- El paquete te dice el tamaño real del texto y **te avisa si queda por debajo de 8 pt**.
- Formato: `.pdf` (el mejor en general), `.eps` si la revista lo pide, o `.tiff`
  (se guarda a 600 ppp) si la revista pide imagen.
- En formato APA el número y el título de la figura van **fuera de la imagen**
  (escríbelos en Word), no con `titulo =`.

Revisa siempre las instrucciones para autores de tu revista. Más detalles en la sección
*Figuras para una revista* de la [guía](guia/GUIA.md).

---

## Más información

- **[Guía de uso completa](guia/GUIA.md)**: cada tipo de análisis, todas las opciones
  y todas las tablas, con ejemplos e imágenes.
- **[Cómo interpretar y redactar los resultados](guia/INTERPRETACION.md)**: qué mirar en
  cada tabla, los criterios, ejemplos de redacción en APA, errores comunes y una
  [lista de verificación](guia/INTERPRETACION.md#lista-de-verificación-para-reportar-un-afc-o-un-sem)
  de lo que debes reportar.
- **[Hoja de referencia](guia/hoja_referencia.pdf)** (dos páginas, para imprimir): de tus
  datos al Word en pocas líneas, y la lista de lo que debes reportar en un AFC o un SEM
  según los estándares de APA.
- **[Cómo se verificó](guia/VALIDACION.md)**: contra qué paquetes y scripts de clase se
  compararon los resultados.
- **Para citarlo**: `citation("apaMultivariate")`.
- **Ayuda dentro de R**: `?diagrama_sem` o el nombre de cualquier tabla, por ejemplo
  `?tabla_invarianza`.

## Lo que no hace (todavía)

- **No estima los modelos**: eso lo hacen `lavaan` y `psych`. El paquete revisa,
  dibuja y presenta sus resultados.
- **Modelos multigrupo**: se comparan con `tabla_invarianza()`. Las demás tablas
  describen un solo grupo; estima el modelo con los datos de cada grupo.
- **Moderación con variables observadas** (puntajes de las escalas), como PROCESS; no
  hace interacciones entre variables latentes.
- **MANOVA de un factor**; no hace diseños factoriales ni de medidas repetidas.
- **No cubre** modelos multinivel, de crecimiento, teoría de respuesta al ítem ni análisis
  de redes.
- **Diagramas complejos**: en modelos con muchas relaciones cruzadas, el acomodo
  automático puede necesitar ajustes a mano (`lados`, `mover`, `acomodo`).
- **Es una versión de prueba (0.x).** Los resultados se verificaron contra otros
  paquetes, pero si encuentras algo raro, repórtalo en
  [Issues](https://github.com/jeshuamv/apaMultivariate/issues).

## Problemas frecuentes

**`could not find function "diagrama_sem"`** (no se encuentra la función): falta
cargar el paquete. Escribe `library(apaMultivariate)`.

**`could not find function "leer_datos"` o `"plantilla"`**: tienes una versión vieja del
paquete. Reinstálalo con `remotes::install_github("jeshuamv/apaMultivariate")`.

**`object 'datos' not found`** (no se encuentra el objeto): falta correr la línea que
lee los datos, o la escribiste con otro nombre. Corre los pasos en orden.

**`No encuentro el archivo`**: el archivo no está en tu carpeta de trabajo. Escribe
`getwd()` para ver cuál es, cámbiala en *Session → Set Working Directory* o usa
`leer_datos()` sin nada para elegirlo en una ventana.

**`... not found in the data` o `object '...' not found` al estimar**: un nombre del
modelo no coincide con el de la columna. Corre `revisar_datos(datos, modelo)`: te dice
cuál y te sugiere el nombre correcto.

**Mi omega o mi AVE no coinciden con los de `semTools`**: hay varias fórmulas.
`tabla_confiabilidad()` calcula ω igual que `semTools::compRelSEM()`; `tabla_validez()`
usa las fórmulas clásicas de Fornell y Larcker, y con `metodo = "semtools"` da los de
`semTools`. Detalles en las preguntas frecuentes de la [guía](guia/GUIA.md).

**`there is no package called 'apaMultivariate'`**: el paquete no está instalado. Repite el [paso 2 de *Empieza aquí*](#2-instala-el-paquete-solo-la-primera-vez).

**`there is no package called 'remotes'`**: corre primero `install.packages("remotes")`.

**Error con `fa()` que menciona `GPArotation`**: corre `install.packages("GPArotation")`.

**El diagrama se ve pequeño o amontonado en RStudio**: agranda el panel de gráficos o,
mejor, guárdalo con `archivo = "..."`. En el archivo las medidas se ajustan solas.

**Aviso "El texto queda en ... pt"**: la figura, al ancho indicado, tendría letra más
chica de lo que aceptan las revistas. Usa `estilo = "revista"` y `ancho = "pagina"`.

**Una flecha o un grupo de ítems quedó en mal lugar**: prueba otro `acomodo`
(`"fila"`, `"dos_filas"`, `"arbol"`, `"izq_der"`, `"arriba_abajo"`) o consulta la
sección *Ajustes a mano* de la [guía](guia/GUIA.md).

**`tabla_efectos()` o `tabla_moderacion()` tardan**: calculan intervalos con miles de
remuestreos bootstrap. Es normal que tarden de segundos a un par de minutos; para probar
usa `bootstrap = 500` y, para el artículo, el valor por omisión.

**Las tablas salieron en inglés**: el idioma dura toda la sesión de R. Vuelve al
español con `idioma_sem("es")` (al reiniciar R también vuelve al español).

**Aviso "Caso Heywood" en `tabla_bifactor()`**: el modelo tiene una varianza de error
negativa y sus índices no son confiables; revisa el factor específico con cargas más
bajas (es común en modelos bifactor).

**El archivo `.docx` se abre en otro programa (por ejemplo, Pages en Mac)**: haz clic
derecho → *Abrir con* → *Microsoft Word*.

¿Encontraste un error? Repórtalo en la pestaña
[Issues](https://github.com/jeshuamv/apaMultivariate/issues) de esta página.

## Si tenías diagramaSEM

El paquete cambió de nombre; las funciones son las mismas. Instala el nuevo y
cambia la línea `library()` de tus scripts:

```r
remotes::install_github("jeshuamv/apaMultivariate")
library(apaMultivariate)          # antes: library(diagramaSEM)
```

Puedes desinstalar el anterior con `remove.packages("diagramaSEM")`.

## Licencia

MIT © 2026 Jeshua Moreno Valladares. Puedes usarlo, compartirlo y modificarlo
libremente.
