Guía de uso de diagramaSEM
================

`diagramaSEM` dibuja el diagrama de trayectorias de tus análisis
factoriales (exploratorios y confirmatorios), de trayectorias y de
ecuaciones estructurales, con la convención gráfica clásica de los
programas de SEM, y crea las tablas en formato APA que suelen pedirse en
artículos y tesis. Esta guía muestra cómo usarlo, paso a paso.

## Contenido

1.  [El primer diagrama](#el-primer-diagrama)
2.  [Con tus propios datos](#con-tus-propios-datos)
3.  [Acomodo automático](#acomodo-automático)
4.  [Segundo orden](#segundo-orden)
5.  [Análisis de trayectorias](#análisis-de-trayectorias)
6.  [Modelo de ecuaciones
    estructurales](#modelo-de-ecuaciones-estructurales)
7.  [Análisis factorial exploratorio](#análisis-factorial-exploratorio)
8.  [Estilos: clásico, revista y
    color](#estilos-clásico-revista-y-color)
9.  [Tablas en formato APA](#tablas-en-formato-apa): descriptivos,
    factorabilidad, cargas, confiabilidad, validez, HTMT, método común,
    ajuste, comparación de modelos, invarianza, efectos y párrafo de
    método
10. [Personalizar el diagrama](#personalizar-el-diagrama)
11. [Guardar el diagrama](#guardar-el-diagrama)
12. [Figuras para una revista](#figuras-para-una-revista)
13. [Preguntas frecuentes](#preguntas-frecuentes)

## Instalación

Solo la primera vez, con conexión a internet:

``` r
install.packages("remotes")
remotes::install_github("jeshuamv/diagramaSEM")
```

Para análisis exploratorios con `psych` instala también:

``` r
install.packages(c("psych", "GPArotation"))
```

En cada sesión de R se cargan los dos paquetes:

``` r
library(lavaan)
library(diagramaSEM)
```

## El primer diagrama

Basta con estimar el modelo en `lavaan` y pasárselo a `diagrama_sem()`.

``` r
modelo <- "visual  =~ x1 + x2 + x3
           textual =~ x4 + x5 + x6
           speed   =~ x7 + x8 + x9"
fit <- cfa(modelo, data = HolzingerSwineford1939)
diagrama_sem(fit)
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-4-1.png" width="100%" />

Qué se ve en el diagrama:

- **Rectángulos**: las variables observadas (ítems).
- **Óvalos**: los constructos (variables latentes).
- **Círculos `e1`, `e2`, …**: los errores, con su peso fijo en 1.
- **Números sobre las flechas**: cargas factoriales estandarizadas.
- **Número junto a cada ítem**: su R², la proporción de varianza que
  explica el constructo.
- **Flechas curvas de doble punta**: correlaciones entre constructos.

## Con tus propios datos

El procedimiento es siempre el mismo: leer los datos, escribir el
modelo, estimarlo con `lavaan` y pasárselo a `diagrama_sem()`.

``` r
datos <- read.csv("mis_datos.csv")      # o read_excel(), read_sav(), etc.

modelo <- "
  Factor1 =~ item1 + item2 + item3
  Factor2 =~ item4 + item5 + item6
"
fit <- cfa(modelo, data = datos)        # sem() para trayectorias o SEM

diagrama_sem(fit)
```

Los nombres de los ítems en el modelo deben ser exactamente los nombres
de las columnas de tus datos. Si `cfa()` o `sem()` marcan un error, el
problema está en el modelo o en los datos, no en `diagramaSEM`;
corrígelo primero en `lavaan`.

## Acomodo automático

`diagrama_sem()` detecta el tipo de modelo y elige el acomodo. Al
imprimir el resultado se ve qué decidió:

``` r
d <- diagrama_sem(fit, dibujar = FALSE)
d
#> Diagrama de diagramaSEM
#>   Tipo de modelo: análisis factorial confirmatorio 
#>   Acomodo:        fila 
#>   Constructos:    3  | Variables observadas: 9  | Errores: 9 
#>   Solución:       estandarizada 
#> Usa plot() para dibujarlo o guardar_diagrama() para guardarlo.
```

Los acomodos disponibles son:

| Acomodo          | Descripción                             | Se usa automáticamente en                                       |
|------------------|-----------------------------------------|-----------------------------------------------------------------|
| `"fila"`         | Constructos en fila, ítems arriba       | AFC, AFE y segundo orden con hasta 3 factores                   |
| `"dos_filas"`    | Dos filas, ítems hacia afuera           | AFC, AFE y segundo orden con 4+ factores, o con cargas cruzadas |
| `"arbol"`        | Factor general arriba, ítems abajo      | (a elección)                                                    |
| `"bifactor"`     | General arriba, específicos abajo       | Modelos bifactor                                                |
| `"izq_der"`      | Niveles causales de izquierda a derecha | Trayectorias y SEM                                              |
| `"arriba_abajo"` | Niveles causales de arriba hacia abajo  | MIMIC y trayectorias con 4+ exógenas                            |

Para elegir otro:

``` r
diagrama_sem(fit, acomodo = "dos_filas")
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-7-1.png" width="100%" />

## Segundo orden

``` r
fit2 <- cfa(paste(modelo, "\n Mental =~ visual + textual + speed"),
            data = HolzingerSwineford1939)
diagrama_sem(fit2)
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-8-1.png" width="100%" />

Los constructos de primer orden llevan su propio error (perturbación) y
su R², que indica cuánto de su varianza explica el factor general.

## Análisis de trayectorias

En un análisis de trayectorias todas las variables son observadas (no
hay factores) y el modelo se escribe con `~`, que se lee “es explicada
por”:

| Símbolo | Significado                              | Ejemplo                 |
|---------|------------------------------------------|-------------------------|
| `~`     | efecto (flecha de una punta)             | `Y ~ X`: X influye en Y |
| `~~`    | correlación (flecha curva de dos puntas) | `Y1 ~~ Y2`              |

Este ejemplo usa los datos `swiss`, incluidos en R: indicadores de 47
provincias de Suiza en 1888. Se plantea que la educación influye en los
resultados de un examen y que la educación, el examen y el porcentaje de
católicos influyen en la fecundidad.

``` r
modelo_path <- "
  Examination ~ Education
  Fertility   ~ Education + Examination + Catholic
"
fit3 <- sem(modelo_path, data = swiss)
diagrama_sem(fit3, etiquetas = c(Education = "Educación", Examination = "Examen",
                                 Fertility = "Fecundidad", Catholic = "Católicos"))
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-9-1.png" width="100%" />

Cómo se lee:

- Las variables se ordenan por **nivel causal**: a la izquierda las que
  solo explican (exógenas), al centro las mediadoras y a la derecha los
  resultados.
- Las **variables exógenas** se correlacionan entre sí (flecha curva).
- Cada variable explicada lleva su **error** (`e1`, `e2`) y su **R²**:
  la proporción de su varianza que explican sus predictores.
- Un efecto que “salta” un nivel (aquí Educación → Fecundidad) se curva
  para no pasar por encima de otras cajas.

Con tus datos, una mediación simple se escribe así:

``` r
modelo <- "
  M ~ X          # X influye en el mediador
  Y ~ M + X      # el mediador y X influyen en el resultado
"
fit <- sem(modelo, data = datos)
diagrama_sem(fit)
```

Los efectos indirectos o totales que definas con `:=` (por ejemplo,
`ind := a*b`) no se dibujan, porque no son flechas del modelo;
consúltalos con `summary(fit)`.

## Modelo de ecuaciones estructurales

``` r
fit4 <- sem("ind60 =~ x1 + x2 + x3
             dem60 =~ y1 + y2 + y3 + y4
             dem65 =~ y5 + y6 + y7 + y8
             dem60 ~ ind60
             dem65 ~ ind60 + dem60
             y1 ~~ y5
             y2 ~~ y4 + y6
             y3 ~~ y7
             y4 ~~ y8
             y6 ~~ y8", data = PoliticalDemocracy)
diagrama_sem(fit4)
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-11-1.png" width="100%" />

Cada constructo coloca sus ítems donde menos estorban a las flechas
entre constructos. Las covarianzas entre errores se dibujan como arcos
entre los círculos de error.

## Análisis factorial exploratorio

`diagrama_sem()` también dibuja análisis exploratorios hechos con
`psych::fa()` o con `lavaan::efa()`.

``` r
library(psych)
items <- HolzingerSwineford1939[, paste0("x", 1:9)]   # solo las columnas de los ítems
afe <- fa(items, nfactors = 3, rotate = "oblimin", fm = "ml")
diagrama_sem(afe)
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-12-1.png" width="100%" />

En un exploratorio cada ítem carga en todos los factores, así que el
diagrama muestra solo lo importante:

- Cada ítem va con el factor en que **más carga**, ordenado de mayor a
  menor.
- Se agregan las **cargas cruzadas** de .30 o más (aquí, x9 también
  carga en otro factor). El umbral se cambia con `umbral = 0.40`.
- Si un ítem no llega al umbral en ningún factor, su flecha se dibuja
  **punteada** y aparece un aviso.
- Con rotación oblicua (`oblimin`, `promax`) se muestran las
  **correlaciones entre factores**; con rotación ortogonal (`varimax`)
  no hay.
- El número junto a cada ítem es su **comunalidad** (h²).

Los nombres `ML1`, `ML2`… los pone `psych`; cámbialos con `etiquetas`:

``` r
diagrama_sem(afe, etiquetas = c(ML1 = "Verbal", ML2 = "Velocidad", ML3 = "Visual"))
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-13-1.png" width="100%" />

Con `lavaan::efa()` es igual. Si pediste varias soluciones (por ejemplo,
`nfactors = 2:4`), indica cuál dibujar:

``` r
afe2 <- efa(data = items, nfactors = 2:4)
diagrama_sem(afe2, nfactores = 3)
```

## Estilos: clásico, revista y color

El argumento `estilo` cambia el aspecto del diagrama sin cambiar su
contenido.

| Estilo                    | Aspecto                                                                  | Para qué                                                  |
|---------------------------|--------------------------------------------------------------------------|-----------------------------------------------------------|
| `"clasico"` (por omisión) | El de AMOS: degradado, bordes negros, los “1” de los errores             | Clases, tareas, tesis                                     |
| `"revista"`               | Grises, letra más grande, relleno plano, errores y correlaciones en gris | Artículos (sirve en cualquier revista)                    |
| `"color"`                 | Como `"revista"`, pero cada factor y sus ítems con su propio color       | Presentaciones, carteles y revistas que publican en color |

``` r
diagrama_sem(fit, estilo = "revista")
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-15-1.png" width="100%" />

``` r
diagrama_sem(fit, estilo = "color")
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-16-1.png" width="100%" />

Qué cambia en los estilos `"revista"` y `"color"`:

- La letra es más grande y sin serifa (Arial o Helvetica, según el
  equipo), y las cajas crecen para que quepa.
- Las cargas y los efectos van en negro; los errores, las correlaciones
  y las R² en gris, para que destaque lo importante.
- Cada valor va sobre un fondo blanco, así que ninguna línea lo cruza.
- Relleno plano, sin degradados ni sombras.
- No se escriben los “1” de las flechas de error. Si su revista o su
  profesor los pide, use el estilo `"clasico"` o explíquelo en la nota
  de la figura (por ejemplo: *Las cargas de los errores se fijaron en
  1*).

Los colores del estilo `"color"` son de la paleta Okabe-Ito: se
distinguen aun con daltonismo y siguen leyéndose si la revista imprime
en blanco y negro. Con más de seis factores los colores se repiten. Las
opciones `paleta` y `degradado` solo aplican al estilo `"clasico"`.

## Tablas en formato APA

El paquete crea las tablas que suelen pedirse en un artículo o una tesis
con análisis factoriales, de trayectorias o SEM. Todas se ven en la
consola, se guardan en Word con formato APA (número, título en cursiva,
solo líneas horizontales y nota) y guardan sus valores numéricos en
`$datos`.

| Etapa       | Función                  | Qué reporta                                                         |
|-------------|--------------------------|---------------------------------------------------------------------|
| Datos       | `tabla_descriptivos()`   | n, M, DE, asimetría, curtosis y correlaciones                       |
|             | `tabla_perdidos()`       | Datos perdidos por variable y casos completos                       |
|             | `tabla_normalidad()`     | Normalidad multivariada de Mardia                                   |
| AFE         | `tabla_factorabilidad()` | KMO, Bartlett y número de factores (Kaiser, análisis paralelo, MAP) |
|             | `tabla_cargas()`         | Cargas, comunalidades y varianza explicada                          |
| Medición    | `tabla_cargas()`         | Cargas estandarizadas, EE, *p* y R²                                 |
|             | `tabla_confiabilidad()`  | Alfa (con IC), omega y correlaciones ítem-total                     |
|             | `tabla_validez()`        | CR, AVE y criterio de Fornell-Larcker                               |
|             | `tabla_htmt()`           | Validez discriminante HTMT                                          |
|             | `tabla_metodo_comun()`   | Sesgo de método común (Harman y AFC de un factor)                   |
| Ajuste      | `tabla_ajuste()`         | χ², CFI, TLI, RMSEA, SRMR de uno o varios modelos                   |
|             | `tabla_comparacion()`    | Modelos alternativos con Δχ², ΔCFI, AIC y BIC                       |
|             | `tabla_invarianza()`     | Invarianza configural, métrica, escalar y estricta                  |
| Estructural | `tabla_efectos()`        | Efectos directos, indirectos (bootstrap) y totales                  |
| Método      | `parrafo_metodo()`       | Párrafo de software y estimación con sus referencias                |

### Descriptivos, datos perdidos y normalidad

La “Tabla 1” de casi cualquier artículo: descriptivos y correlaciones
con asteriscos de significancia. La asimetría y la curtosis son las
mismas que da SPSS.

``` r
items <- HolzingerSwineford1939[, paste0("x", 1:9)]
tabla_descriptivos(items[, 1:5])
#> 
#> Estadísticos descriptivos y correlaciones
#> ─────────────────────────────────────────────────────────────────────────────
#> Variable   n    M     DE   Asimetría  Curtosis    1       2       3      4   
#> ─────────────────────────────────────────────────────────────────────────────
#> 1. x1     301  4.94  1.17    -0.26      0.36      —                          
#> 2. x2     301  6.09  1.18    0.47       0.38    .30***    —                  
#> 3. x3     301  2.25  1.13    0.39      -0.89    .44***  .34***    —          
#> 4. x4     301  3.06  1.16    0.27       0.12    .37***  .15**   .16**    —   
#> 5. x5     301  4.34  1.29    -0.35     -0.53    .29***   .14*    .08   .73***
#> ─────────────────────────────────────────────────────────────────────────────
#> Nota. M = media; DE = desviación estándar. Asimetría y curtosis corregidas
#> por sesgo; valores de asimetría entre −2 y 2 y de curtosis entre −7 y 7
#> suelen considerarse aceptables (West et al., 1995). Correlaciones de Pearson
#> con eliminación por pares. * p < .05. ** p < .01. *** p < .001.
```

``` r
tabla_perdidos(airquality)      # datos de ejemplo de R con datos perdidos
#> 
#> Datos perdidos por variable
#> ──────────────────────────────────────────────
#> Variable         Válidos  Perdidos  % perdidos
#> ──────────────────────────────────────────────
#> Ozone              116       37        24.2   
#> Solar.R            146       7         4.6    
#> Wind               153       0         0.0    
#> Temp               153       0         0.0    
#> Month              153       0         0.0    
#> Day                153       0         0.0    
#> ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈
#> Casos completos    111       42        27.5   
#> ──────────────────────────────────────────────
#> Nota. N = 153. En la última fila, casos sin ningún dato
#> perdido y casos con al menos uno. Datos perdidos en total:
#> 4.8 % de las celdas. Para estimar el modelo con todos los
#> casos puede usarse máxima verosimilitud con información
#> completa (missing = "fiml" en lavaan).
```

``` r
tabla_normalidad(items)
#> 
#> Pruebas de normalidad multivariada
#> ─────────────────────────────────────────────────────────────
#> Prueba                  Coeficiente  Estadístico  gl     p   
#> ─────────────────────────────────────────────────────────────
#> Asimetría multivariada     6.88      χ² = 344.91  165  < .001
#> Curtosis multivariada     103.59      z = 2.83          .005 
#> ─────────────────────────────────────────────────────────────
#> Nota. Pruebas de Mardia (1970) con 301 casos completos y 9
#> variables. Se rechaza la normalidad multivariada; se
#> recomienda un estimador robusto (MLR o MLM). Univariadas:
#> todas las variables tienen asimetría entre ±2 y curtosis
#> entre ±7 (West et al., 1995).
```

Si la prueba de Mardia es significativa, conviene estimar el modelo con
`estimator = "MLR"`; la nota lo indica.

### Antes de un análisis exploratorio

Factorabilidad (KMO, Bartlett) y cuántos factores retener según cuatro
criterios:

``` r
tabla_factorabilidad(items)
#> 
#> Factorabilidad de la matriz de correlaciones
#> ───────────────────────────────────────────────────────────────
#> Indicador                      Valor            Criterio       
#> ───────────────────────────────────────────────────────────────
#> KMO global                      .75             ≥ .60 (mediano)
#> MSA mínimo por ítem          .59 (x7)           ≥ .50          
#> Prueba de Bartlett   χ²(36) = 904.10, p < .001  p < .05        
#> Determinante                 4.72e-02           > 0.00001      
#> ───────────────────────────────────────────────────────────────
#> Nota. KMO = medida de adecuación muestral de
#> Kaiser-Meyer-Olkin (calificación de Kaiser, 1974); MSA =
#> adecuación de cada ítem. N = 301 casos completos. Todos los
#> ítems tienen MSA ≥ .50.
#> 
#> Número de factores sugerido por cada criterio
#> ─────────────────────────────────────────────────────
#> Criterio                                     Factores
#> ─────────────────────────────────────────────────────
#> Autovalores mayores que 1 (Kaiser)              3    
#> Análisis paralelo, componentes (Horn, 1965)     3    
#> Análisis paralelo, factores                     3    
#> MAP de Velicer (1976)                           2    
#> ─────────────────────────────────────────────────────
#> Nota. Análisis paralelo con 100 conjuntos de datos
#> aleatorios y el percentil 95. El criterio de Kaiser tiende
#> a sobrestimar el número de factores; se recomienda dar más
#> peso al análisis paralelo y a la MAP, y a la
#> interpretabilidad de la solución.
#> 
#> Autovalores observados y aleatorios (percentil 95)
#> ──────────────────────────────────────────────────
#> N.º  Componentes  Aleatorios  Factores  Aleatorios
#> ──────────────────────────────────────────────────
#> 1       3.22         1.35       2.71       0.41   
#> 2       1.64         1.26       1.07       0.29   
#> 3       1.37         1.16       0.66       0.19   
#> 4       0.70         1.08      -0.03       0.11   
#> 5       0.58         1.04      -0.06       0.06   
#> 6       0.50         0.98      -0.10       0.00   
#> 7       0.47         0.92      -0.16      -0.05   
#> 8       0.29         0.87      -0.18      -0.11   
#> 9       0.24         0.81      -0.24      -0.17   
#> ──────────────────────────────────────────────────
#> Nota. En negritas, autovalores observados mayores que los
#> aleatorios. Factores: autovalores de la matriz reducida
#> (correlaciones múltiples al cuadrado en la diagonal).
```

### Cargas factoriales

En un **exploratorio**, la tabla incluye la matriz de cargas (de patrón
si la rotación es oblicua), las comunalidades, la varianza explicada y
las correlaciones entre factores:

``` r
tabla_cargas(afe, etiquetas = c(ML1 = "Verbal", ML2 = "Velocidad", ML3 = "Visual"))
#> 
#> Cargas factoriales del análisis factorial exploratorio
#> ──────────────────────────────────────────────────
#> Ítem                Verbal  Visual  Velocidad  h² 
#> ──────────────────────────────────────────────────
#> x5                   .89                       .76
#> x4                   .84                       .72
#> x6                   .81                       .69
#> x3                           .69               .46
#> x1                           .60               .49
#> x2                           .51               .25
#> x7                                     .72     .50
#> x8                                     .70     .53
#> x9                           .37       .46     .46
#> ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈
#> Varianza explicada   2.24    1.34     1.28        
#> % de varianza        24.9    14.9     14.2        
#> % acumulado          24.9    39.7     53.9        
#> ──────────────────────────────────────────────────
#> Nota. Matriz de patrón. Método: psych::fa (ml, rotación
#> oblimin). N = 301. Se omiten cargas menores que .30. En
#> negritas, la carga más alta de cada ítem. h² = comunalidad.
#> 
#> Correlaciones entre factores
#> ────────────────────────────────────
#> Factor     Verbal  Visual  Velocidad
#> ────────────────────────────────────
#> Verbal       —                      
#> Visual      .33      —              
#> Velocidad   .22     .27        —    
#> ────────────────────────────────────
```

Por omisión se dejan en blanco las cargas menores que .30 y se marca en
negritas la carga más alta de cada ítem (las negritas se ven en Word).
Para ver todas las cargas: `ocultar_bajas = FALSE`.

En un **confirmatorio o SEM**, la tabla muestra las cargas
estandarizadas con su error estándar, valor *p* y R²:

``` r
tabla_cargas(fit)
#> 
#> Cargas factoriales estandarizadas
#> ─────────────────────────────────────
#> Factor   Ítem   λ    EE     p     R² 
#> ─────────────────────────────────────
#> visual   x1    .77  .055  < .001  .60
#>          x2    .42  .060  < .001  .18
#>          x3    .58  .055  < .001  .34
#> textual  x4    .85  .023  < .001  .73
#>          x5    .86  .022  < .001  .73
#>          x6    .84  .023  < .001  .70
#> speed    x7    .57  .053  < .001  .32
#>          x8    .72  .051  < .001  .52
#>          x9    .67  .051  < .001  .44
#> ─────────────────────────────────────
#> Nota. λ = carga factorial estandarizada; EE = error
#> estándar; R² = proporción de varianza del ítem explicada
#> por su factor. N = 301.
```

### Confiabilidad y validez

Alfa de Cronbach con su intervalo de confianza, omega de McDonald y el
rango de las correlaciones ítem-total:

``` r
tabla_confiabilidad(fit)
#> 
#> Confiabilidad de las escalas
#> ──────────────────────────────────────────────────
#> Factor   Ítems   α    IC 95 %     ω   r ítem-total
#> ──────────────────────────────────────────────────
#> visual     3    .63  [.55, .69]  .63    .37–.48   
#> textual    3    .88  [.86, .90]  .89    .77–.79   
#> speed      3    .69  [.62, .74]  .69    .46–.57   
#> ──────────────────────────────────────────────────
#> Nota. α = alfa de Cronbach; IC 95 % de Feldt; ω = omega de
#> McDonald, calculado con las cargas del modelo; r ítem-total
#> = correlación ítem-total corregida (mínima–máxima). Valores
#> de α y ω ≥ .70 suelen considerarse aceptables (Nunnally y
#> Bernstein, 1994) y una correlación ítem-total ≥ .30
#> adecuada. n = 301 casos completos.
```

Sin un modelo, indica los ítems de cada factor:
`tabla_confiabilidad(datos, factores = list(F1 = c("i1", "i2", "i3"), F2 = c(...)))`.

Confiabilidad compuesta (CR), varianza promedio extraída (AVE) y la
matriz de Fornell y Larcker:

``` r
tabla_validez(fit)
#> 
#> Confiabilidad compuesta, varianza promedio extraída y correlaciones entre factores
#> ─────────────────────────────────────────
#> Factor   CR   AVE  visual  textual  speed
#> ─────────────────────────────────────────
#> visual   .63  .37   .61                  
#> textual  .89  .72   .46      .85         
#> speed    .69  .43   .47      .28     .66 
#> ─────────────────────────────────────────
#> Nota. CR = confiabilidad compuesta; AVE = varianza promedio
#> extraída. En negritas, la raíz cuadrada del AVE; debajo de
#> la diagonal, correlaciones entre factores. Criterios: CR >
#> .70, AVE > .50 (Hair et al., 2014) y raíz del AVE mayor que
#> las correlaciones del factor (Fornell y Larcker, 1981).
#> Todos los factores cumplen el criterio de Fornell-Larcker.
```

Las fórmulas son las clásicas, con las cargas estandarizadas; con
`metodo = "semtools"` se obtienen exactamente los valores de `semTools`.

Muchos revisores piden además el **HTMT**, un criterio de validez
discriminante más exigente que el de Fornell-Larcker. Valores menores
que .85 indican validez discriminante:

``` r
tabla_htmt(fit)
#> 
#> Validez discriminante: razón heterorrasgo-monorrasgo (HTMT)
#> ────────────────────────
#> Factor   visual  textual
#> ────────────────────────
#> visual     —            
#> textual   .42       —   
#> speed     .47      .29  
#> ────────────────────────
#> Nota. HTMT = razón heterorrasgo-monorrasgo (Henseler et
#> al., 2015). Valores menores que .85 (criterio estricto) o
#> .90 (flexible) indican validez discriminante. Todos los
#> pares son menores que .85.
```

Si todos los datos vienen de un mismo cuestionario, es habitual reportar
el **sesgo de método común**:

``` r
tabla_metodo_comun(fit)
#> 
#> Pruebas de sesgo de método común
#> ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> Prueba                  Resultado                                            Criterio                                   Conclusión                             
#> ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> Factor único de Harman  Primer componente: 35.7 % de la varianza             < 50 %                                     No hay un factor general dominante     
#> AFC de un factor        CFI = .677, RMSEA = .187; Δχ²(3) = 226.96, p < .001  Peor que el modelo propuesto (CFI = .931)  El sesgo de método no explica los datos
#> ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> Nota. Pruebas según Podsakoff et al. (2003). Harman: análisis de componentes principales sin rotar de los 9 ítems. AFC de un factor: todos los ítems en un
#> factor, comparado con el modelo propuesto (Δχ² y ΔCFI). Estas pruebas son poco sensibles: un resultado favorable no descarta por completo el sesgo de método
#> común.
```

### Índices de ajuste y comparación de modelos

``` r
tabla_ajuste(fit)
#> 
#> Índices de ajuste
#> ───────────────────────────────────────────────────────────────────────
#> Modelo     χ²    gl    p     χ²/gl  CFI   TLI    RMSEA [IC 90 %]   SRMR
#> ───────────────────────────────────────────────────────────────────────
#> Modelo 1  85.31  24  < .001  3.55   .931  .896  .092 [.071, .114]  .065
#> ───────────────────────────────────────────────────────────────────────
#> Nota. Criterios de buen ajuste (Hu y Bentler, 1999): CFI y TLI ≥ .95
#> (≥ .90 aceptable), RMSEA ≤ .06 (≤ .08 aceptable), SRMR ≤ .08. Con
#> muestras grandes la prueba χ² suele ser significativa aunque el ajuste
#> sea bueno.
```

Si estimaste con un estimador robusto (`estimator = "MLR"`, `"MLM"`,
`"WLSMV"`…), las tablas usan automáticamente los índices corregidos.

Para comparar modelos alternativos, con la prueba de diferencias Δχ²,
ΔCFI, AIC y BIC:

``` r
un_factor <- cfa("g =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9",
                 data = HolzingerSwineford1939)
tabla_comparacion("Un factor" = un_factor, "Tres factores" = fit)
#> 
#> Comparación de modelos alternativos
#> ───────────────────────────────────────────────────────────────────────────────────────────────────────
#> Modelo           χ²    gl  CFI   TLI   RMSEA  SRMR    AIC      BIC    Δχ² (Δgl)     p     ΔCFI   ΔRMSEA
#> ───────────────────────────────────────────────────────────────────────────────────────────────────────
#> Un factor      312.26  27  .677  .569  .187   .143  7,738.4  7,805.2      —         —       —      —   
#> Tres factores  85.31   24  .931  .896  .092   .065  7,517.5  7,595.3  226.96 (3)  < .001  +.254  -.095 
#> ───────────────────────────────────────────────────────────────────────────────────────────────────────
#> Nota. Diferencias respecto del modelo anterior. Δχ² válida solo para modelos anidados. Un ΔCFI menor
#> que −.010 junto con un ΔRMSEA mayor que .015 indica un ajuste apreciablemente peor (Chen, 2007). AIC y
#> BIC: menor es mejor; el AIC más bajo es el de Tres factores y también el BIC.
```

La Δχ² solo es válida si un modelo es una versión restringida del otro
(modelos anidados); si no lo son, usa `anidados = FALSE` y compara AIC y
BIC.

### Invarianza de medición

¿El instrumento mide lo mismo en distintos grupos? Indica el modelo, los
datos y la variable de grupo:

``` r
tabla_invarianza(modelo, HolzingerSwineford1939, grupo = "school")
#> 
#> Invarianza de medición entre grupos
#> ──────────────────────────────────────────────────────────────────────────────────────────────────
#> Modelo        χ²    gl  CFI   RMSEA  SRMR  Δχ² (Δgl)    p     ΔCFI   ΔRMSEA  ΔSRMR     Decisión   
#> ──────────────────────────────────────────────────────────────────────────────────────────────────
#> Configural  115.85  48  .923  .097   .068      —        —       —      —       —          —       
#> Métrica     124.04  54  .921  .093   .072  8.19 (6)    .224   -.002  -.004   +.004   Se sostiene  
#> Escalar     164.10  60  .882  .107   .082  40.06 (6)  < .001  -.038  +.015   +.011  No se sostiene
#> Estricta    181.51  69  .873  .104   .088  17.41 (9)   .043   -.009  -.003   +.006   No se evalúa 
#> ──────────────────────────────────────────────────────────────────────────────────────────────────
#> Nota. Grupos: Pasteur (n = 156), Grant-White (n = 145). Cada modelo se compara con el anterior.
#> No se sostiene la invarianza si ΔCFI < −.010 junto con ΔRMSEA > .015 o ΔSRMR > .030 (métrica) o >
#> .010 (escalar y estricta) (Chen, 2007). Nivel más alto sostenido: métrica. Para probar invarianza
#> parcial, los parámetros cuya igualdad más empeora el ajuste son, en orden: x3 ~ 1; x7 ~ 1; x2 ~
#> 1. Libérelos de uno en uno, empezando por el primero (liberar = "x3 ~ 1"), y vuelva a revisar la
#> tabla.
```

Cada nivel se compara con el anterior con el criterio de Chen (2007);
también puedes usar `criterio = "cfi"` o `criterio = "chi2"`. Si un
nivel no se sostiene, la nota indica qué parámetro conviene liberar para
probar la **invarianza parcial**:

``` r
tabla_invarianza(modelo, HolzingerSwineford1939, grupo = "school", liberar = "x3 ~ 1")
#> 
#> Invarianza de medición entre grupos
#> ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> Modelo              χ²    gl  CFI   RMSEA  SRMR  Δχ² (Δgl)    p     ΔCFI   ΔRMSEA  ΔSRMR   Decisión  
#> ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> Configural        115.85  48  .923  .097   .068      —        —       —      —       —         —     
#> Métrica           124.04  54  .921  .093   .072  8.19 (6)    .224   -.002  -.004   +.004  Se sostiene
#> Escalar parcial   144.58  59  .903  .098   .077  20.54 (5)  < .001  -.018  +.005   +.006  Se sostiene
#> Estricta parcial  162.12  68  .894  .096   .083  17.55 (9)   .041   -.010  -.002   +.006  Se sostiene
#> ─────────────────────────────────────────────────────────────────────────────────────────────────────
#> Nota. Grupos: Pasteur (n = 156), Grant-White (n = 145). Cada modelo se compara con el anterior. No
#> se sostiene la invarianza si ΔCFI < −.010 junto con ΔRMSEA > .015 o ΔSRMR > .030 (métrica) o > .010
#> (escalar y estricta) (Chen, 2007). Parámetros liberados (invarianza parcial): x3 ~ 1. Se sostiene la
#> invarianza estricta.
```

Con ítems ordinales agrega `ordered = TRUE` (se igualan los umbrales);
con un estimador robusto, `estimator = "MLR"`.

### Efectos directos, indirectos y totales

Para trayectorias y SEM. Las trayectorias indirectas se encuentran solas
(no hace falta definirlas con `:=`) y sus intervalos se calculan por
bootstrap:

``` r
tabla_efectos(fit3, bootstrap = 500)
#> 
#> Efectos directos, indirectos y totales
#> ─────────────────────────────────────────────────────────────────────────────────
#> Efecto                                    B     EE      IC 95 %       β      p   
#> ─────────────────────────────────────────────────────────────────────────────────
#> Efectos directos                                                                 
#>    Education → Examination              0.58   0.12   [0.42, 0.89]   .70   < .001
#>    Education → Fertility                -0.76  0.26  [-1.08, -0.02]  -.59  < .001
#>    Examination → Fertility              -0.05  0.29  [-0.71, 0.40]   -.03   .796 
#>    Catholic → Fertility                 0.11   0.04   [0.02, 0.18]   .36   < .001
#> ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈
#> Efectos indirectos                                                               
#>    Education → Examination → Fertility  -0.03  0.19  [-0.52, 0.23]   -.02        
#> ┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈
#> Efectos totales                                                                  
#>    Education → Fertility                -0.79  0.16  [-1.05, -0.39]  -.61        
#> ─────────────────────────────────────────────────────────────────────────────────
#> Nota. B = coeficiente no estandarizado; EE = error estándar; IC 95 % por
#> bootstrap de percentiles con 500 remuestreos; β = coeficiente estandarizado. Los
#> valores p de los efectos directos son los de lavaan; para los indirectos y
#> totales la significancia se juzga por el intervalo. En negritas, efectos
#> indirectos cuyo intervalo no incluye el 0 (significativos).
#> 
#> Varianza explicada de las variables endógenas
#> ────────────────
#> Variable     R² 
#> ────────────────
#> Examination  .49
#> Fertility    .57
#> ────────────────
```

Usa 1000 o más remuestreos para un artículo (`bootstrap = 1000`, el
valor por omisión). Un efecto indirecto es significativo si su intervalo
no incluye el 0; esos van en negritas en Word.

### Párrafo de método

Redacta el párrafo de análisis de datos (software, versiones, estimador,
datos perdidos y criterios de ajuste) con sus referencias en formato
APA. Es un punto de partida para adaptar:

``` r
parrafo_metodo(fit, paquetes = c("semTools", "diagramaSEM"))
#> 
#> Los análisis se realizaron en R (versión 4.3.3; R Core Team, 2024) con los
#> paquetes lavaan (versión 0.6-17; Rosseel, 2012), semTools (versión 0.5-6;
#> Jorgensen et al., 2022) y diagramaSEM (versión 0.4.0; Moreno Valladares, 2026).
#> El análisis factorial confirmatorio se estimó con una muestra de 301
#> participantes por máxima verosimilitud (ML). El ajuste se evaluó con la prueba
#> χ², el índice de ajuste comparativo (CFI), el índice de Tucker-Lewis (TLI), el
#> error cuadrático medio de aproximación (RMSEA) con su intervalo de confianza al
#> 90 % y la raíz del residuo cuadrático medio estandarizado (SRMR). Se
#> consideraron indicadores de buen ajuste CFI y TLI ≥ .95, RMSEA ≤ .06 y SRMR ≤
#> .08 (Hu y Bentler, 1999).
#> 
#> Referencias
#> 
#> Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance
#>     structure analysis: Conventional criteria versus new alternatives.
#>     Structural Equation Modeling, 6(1), 1–55.
#>     https://doi.org/10.1080/10705519909540118
#> 
#> Jorgensen, T. D., Pornprasertmanit, S., Schoemann, A. M., & Rosseel, Y. (2022).
#>     semTools: Useful tools for structural equation modeling (Paquete de R,
#>     versión 0.5-6). https://CRAN.R-project.org/package=semTools.
#> 
#> Moreno Valladares, J. (2026). diagramaSEM: Diagramas de trayectorias y tablas
#>     APA para modelos lavaan y psych (Paquete de R, versión 0.4.0).
#>     https://github.com/jeshuamv/diagramaSEM.
#> 
#> R Core Team. (2024). R: A language and environment for statistical computing. R
#>     Foundation for Statistical Computing. https://www.R-project.org/
#> 
#> Rosseel, Y. (2012). lavaan: An R package for structural equation modeling.
#>     Journal of Statistical Software, 48(2), 1–36.
#>     https://doi.org/10.18637/jss.v048.i02
```

Con `archivo = "metodo.docx"` se guarda en Word.

### Guardar las tablas

Una tabla:

``` r
tabla_validez(fit, archivo = "tabla_validez.docx")   # Word
tabla_ajuste(fit, archivo = "tabla_ajuste.html")     # para copiar y pegar
tabla_cargas(fit, archivo = "tabla_cargas.csv")      # para Excel
```

Todas las tablas de un artículo en un solo documento de Word, numeradas:

``` r
guardar_tablas(
  tabla_descriptivos(items),
  tabla_confiabilidad(fit),
  tabla_ajuste(fit),
  tabla_validez(fit),
  tabla_htmt(fit),
  archivo = "tablas_articulo.docx"
)
```

Las tablas anchas se pasan a letra de 10 pt y, si aún no caben, se
colocan en una página horizontal. También se puede guardar en `.rtf`.

## Personalizar el diagrama

### Nombres

``` r
diagrama_sem(fit, etiquetas = c(visual = "Visual", textual = "Verbal",
                                speed = "Velocidad", x1 = "Ítem 1"))
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-35-1.png" width="100%" />

Use `"\n"` dentro de un nombre para partirlo en dos renglones.

### Significancia, decimales y solución no estandarizada

``` r
diagrama_sem(fit, estrellas = TRUE, decimales = 3)
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-36-1.png" width="100%" />

Con `estandarizado = FALSE` se muestran los valores no estandarizados (y
no se muestra R²):

``` r
diagrama_sem(fit, estandarizado = FALSE)
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-37-1.png" width="100%" />

### Correlaciones

``` r
diagrama_sem(fit, correlaciones = FALSE)   # ocultarlas
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-38-1.png" width="100%" />

``` r
diagrama_sem(fit, correlaciones = 0.40)    # solo las de |r| >= .40
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-38-2.png" width="100%" />

En AFC con 5 o más factores se ocultan automáticamente, porque se cruzan
entre sí sin importar el acomodo. Siguen disponibles en el resultado:

``` r
d <- diagrama_sem(fit, correlaciones = FALSE, dibujar = FALSE)
d$correlaciones_ocultas
#>    variable_1 variable_2     valor            p
#> 10     visual    textual 0.4585093 6.528111e-13
#> 11     visual      speed 0.4705345 1.040015e-10
#> 12    textual      speed 0.2829847 3.832000e-05
```

### Colores

``` r
diagrama_sem(fit, paleta = paleta_sem("verde"))
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-40-1.png" width="100%" />

``` r
diagrama_sem(fit, paleta = paleta_sem("blanco"), degradado = FALSE)
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-40-2.png" width="100%" />

``` r
diagrama_sem(fit, paleta = paleta_sem(latente = c("white", "#FAD3B5"),
                                      observada = "#FFF3EA"))
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-40-3.png" width="100%" />

Paletas incluidas: `"lila"` (por omisión), `"azul"`, `"verde"`,
`"gris"`, `"durazno"` y `"blanco"`.

### Ajustes a mano

Hacia dónde van los ítems de un constructo:

``` r
diagrama_sem(fit, lados = c(speed = "abajo"))
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-41-1.png" width="100%" />

Mover un elemento (un constructo se mueve con sus ítems y errores). La
unidad equivale aproximadamente al ancho de un ítem:

``` r
diagrama_sem(fit, mover = list(speed = c(0, -1)))
```

<img src="GUIA_files/figure-gfm/unnamed-chunk-42-1.png" width="100%" />

## Guardar el diagrama

``` r
diagrama_sem(fit, archivo = "figura1.png")    # imagen para Word o PowerPoint
diagrama_sem(fit, archivo = "figura1.pdf")    # PDF vectorial (nítido a cualquier tamaño)
```

Si no se indican medidas, se calculan solas según el tamaño del
diagrama. También se aceptan `.tiff`, `.eps`, `.svg` y `.jpg`. Para
guardar un diagrama que ya dibujó, use `guardar_diagrama()`:

``` r
d <- diagrama_sem(fit, estilo = "revista")
guardar_diagrama(d, "figura1.pdf", ancho = "pagina")
```

## Figuras para una revista

Las revistas piden que el texto de la figura mida **al menos 8 pt ya
impresa** (APA pide de 8 a 14 pt, sin serifa) y que la figura se
entregue al ancho con que se publicará. Por eso, para un artículo:

``` r
diagrama_sem(fit, estilo = "revista", archivo = "figura1.pdf", ancho = "pagina")
```

Al indicar `ancho`, el paquete informa el tamaño real del texto:

    Figura de 174 x 62 mm; texto de los ítems y valores: 8.4 pt.

Si queda por debajo de 8 pt, aparece un aviso con sugerencias.

**`ancho`**

- `"pagina"`: 174 mm, a lo ancho de la página. Es la opción adecuada
  para casi cualquier modelo.
- `"columna"`: 85 mm, una columna en revistas de dos columnas. Solo
  alcanza para modelos muy pequeños (un factor con pocos ítems, una
  trayectoria simple).
- Un número: el ancho en pulgadas.

**Formato**

| Formato | Cuándo usarlo                                                                                                                    |
|---------|----------------------------------------------------------------------------------------------------------------------------------|
| `.pdf`  | La mejor opción en general: vectorial, con las fuentes incluidas. La aceptan Elsevier, Springer, Taylor & Francis, Wiley y SAGE. |
| `.eps`  | Cuando la revista pide EPS (vectorial).                                                                                          |
| `.tiff` | Cuando la revista pide imagen (por ejemplo PLOS). Se guarda a 600 ppp con compresión LZW.                                        |
| `.png`  | Para Word, PowerPoint o la tesis (300 ppp).                                                                                      |

**Formato APA del resto de la figura.** En APA el número de la figura
(en negritas) y su título (en cursiva) van **arriba y fuera de la
imagen**, y las notas debajo; escríbalos en Word, no con `titulo =`. Por
ejemplo:

> **Figura 1**  
> *Modelo de medición de tres factores*  
> \[diagrama\]  
> *Nota.* Cargas estandarizadas. Los números junto a cada ítem son las
> R². Las cargas de los errores se fijaron en 1. *N* = 301.

Siempre revise las instrucciones para autores de la revista elegida:
algunas piden otro ancho, otro formato o la figura en un archivo aparte.

## Preguntas frecuentes

**El diagrama sale muy pequeño o el texto muy chico.** Guárdelo con
`guardar_diagrama()`; en la ventana de RStudio el tamaño depende del
panel. Para un artículo use `estilo = "revista"` y `ancho = "pagina"`
(véase [Figuras para una revista](#figuras-para-una-revista)).

**¿Qué estilo uso?** Para clases y tareas, el que prefiera. Para un
artículo, `"revista"`; `"color"` si la revista publica en color o para
una presentación.

**Una flecha pasa por donde no quiero.** Pruebe otro `acomodo`, cambie
el lado de los ítems con `lados` o desplace el constructo con `mover`.

**El modelo es multigrupo.** Se dibuja un grupo a la vez con
`grupo = 1`, `grupo = 2`, etc.

**Aparece el aviso “El modelo no convergió”.** Revise el modelo en
`lavaan`; el diagrama se dibuja, pero sus valores no son confiables.

**`fa()` marca un error sobre `GPArotation`.** Instala el paquete con
`install.packages("GPArotation")`; `psych` lo necesita para las
rotaciones oblicuas.

**El archivo `.rtf` se abre en otro programa.** Haz clic derecho →
*Abrir con* → *Microsoft Word*.
