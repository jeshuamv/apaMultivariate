# Cómo se verificó apaMultivariate

Cada tabla se comparó con el resultado de un paquete de referencia o con el cálculo a
mano. Las comparaciones forman parte de las pruebas automáticas del paquete
(`tests/testthat/`). Se repiten cada vez que cambia el código y el paquete no se publica
si alguna falla.

## Comparaciones con otros paquetes

| Tabla o función | Se comparó con | Resultado |
|---|---|---|
| `tabla_descriptivos()` | `psych::describe()` (asimetría y curtosis tipo 2, las de SPSS) | Iguales |
| `tabla_normalidad()` | Fórmulas de Mardia (1970) con denominador *n*, como `MVN::mvn()`; `psych::mardia()` usa *n* − 1 | Iguales (documentada la diferencia con `psych`) |
| `tabla_atipicos()`, `quitar_atipicos()` | `mahalanobis()` con el corte χ² de *p* < .001 de los scripts de clase | Los mismos casos |
| `tabla_vif()` | `car::vif()` | Iguales |
| `tabla_factorabilidad()` | `psych::KMO()`, `psych::cortest.bartlett()`, `psych::vss()` (MAP) y `psych::fa.parallel()` | Iguales (el análisis paralelo usa datos aleatorios) |
| `tabla_cargas()` (AFE) | `psych::fa()` y `lavaan::efa()` | Iguales |
| `tabla_cargas()`, `tabla_parametros()` (AFC y SEM) | `lavaan::standardizedSolution()` y `parameterEstimates()` | Iguales |
| `tabla_ajuste()`, `tabla_criterios_ajuste()` | `lavaan::fitMeasures()`; con estimadores robustos, los índices corregidos | Iguales |
| `tabla_confiabilidad()` | `psych::alpha()` (α y *r* ítem-total) y `semTools::compRelSEM()` (ω) | Iguales |
| `tabla_validez()` | Fórmulas de Fornell y Larcker (1981); con `metodo = "semtools"`, `semTools::compRelSEM()` y `AVE()` | Iguales |
| `tabla_htmt()` | `semTools::htmt()` (HTMT y HTMT2) | Iguales |
| `tabla_comparacion()` | `lavaan::lavTestLRT()` (también la diferencia escalada con estimadores robustos) | Iguales |
| `tabla_invarianza()` | Modelos multigrupo de `lavaan` y `lavTestLRT()` | Iguales |
| `tabla_efectos()`, `tabla_hipotesis()` | Parámetros definidos con `:=` en `lavaan` (método delta) y bootstrap de `lavaan` | Iguales (bootstrap: intervalos parecidos, por el azar del remuestreo) |
| `tabla_bifactor()` | Fórmulas de Rodriguez et al. (2016) calculadas aparte con las matrices estandarizadas que reproduce `lavaan` (ωH, ωHS y Schmid-Leiman) | Iguales |
| `tabla_moderacion()` | `lm()` (coeficientes, pendientes simples, Johnson-Neyman) y `lavaan` (índice de mediación moderada) | Iguales |
| `tabla_manova()` | `manova()`, `TukeyHSD()` y Games-Howell; el ejemplo de clase con `iris` | Iguales |
| `tabla_residuales()` | Correlación observada menos la reproducida por el modelo (`fitted()` de `lavaan`) | Iguales |

## Reproducción de los scripts del curso

- **AFC de Holzinger y Swineford** (script `CFA.R`, estimador MLM):
  - los mismos casos atípicos (filas 163 y 180);
  - los mismos índices de ajuste robustos;
  - el mismo ω y el mismo AVE que `semTools`.
- **AFE con `bfi`** (script `bfi_EFA 2026.R`):
  - los mismos 84 atípicos;
  - KMO = .85 y χ² de Bartlett = 18,369.66;
  - las mismas comunalidades del análisis de ejes principales con promax;
  - el mismo número de factores del análisis paralelo (6 factores, 5 componentes) con `ml`, `pa` y `minres`.
- **Tarea de AFC con `ChurchSurvey`:**
  - los mismos índices (χ², RMSEA, CFI, TLI, GFI, PNFI) antes y después de quitar los atípicos;
  - los mismos 74 atípicos.

## Prueba de esfuerzo

Todas las funciones se corrieron con 14 tipos de modelo:

- máxima verosimilitud, MLR, MLM y WLSMV con ítems ordinales;
- datos perdidos con FIML y con eliminación por lista;
- multigrupo;
- 8 factores;
- trayectorias, SEM con restricciones de igualdad, MIMIC, segundo orden, un factor y `std.lv = TRUE`.

Cada función debía dar un resultado correcto o un mensaje claro en español de por qué no
aplica. Por ejemplo, las tablas de un solo grupo con un modelo multigrupo indican qué
hacer en su lugar.
