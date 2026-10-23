#' Dibuja el diagrama de un modelo estimado con lavaan
#'
#' Genera el diagrama de trayectorias de un modelo de `lavaan` con la
#' convención gráfica clásica de los programas de ecuaciones estructurales:
#' rectángulos para las variables observadas, óvalos para los constructos,
#' círculos de error numerados (`e1`, `e2`, ...), cargas y coeficientes sobre
#' las flechas y \eqn{R^2} junto a cada variable explicada.
#'
#' El tipo de modelo se detecta solo (AFC, segundo orden, bifactor, análisis de
#' trayectorias o SEM completo) y el acomodo de los elementos se elige según
#' el tipo; véase la sección *Acomodos*.
#'
#' @param fit Modelo estimado con [lavaan::cfa()], [lavaan::sem()] o
#'   [lavaan::lavaan()], o un análisis factorial exploratorio hecho con
#'   `psych::fa()` o [lavaan::efa()].
#' @param acomodo Forma de acomodar los elementos. `"auto"` (por omisión) elige
#'   la más adecuada. Véase la sección *Acomodos*.
#' @param correlaciones Qué correlaciones entre constructos dibujar:
#'   `"auto"` (por omisión) las muestra en AFC con hasta 4 factores y en
#'   cualquier SEM o análisis de trayectorias, y las oculta en AFC con 5 o más
#'   factores; `TRUE` las muestra siempre; `FALSE` las oculta; un número entre
#'   0 y 1 muestra solo las que tengan un valor absoluto igual o mayor. Las
#'   correlaciones entre errores se dibujan siempre. Las ocultas quedan en el
#'   elemento `correlaciones_ocultas` del resultado.
#' @param estandarizado Si `TRUE` (por omisión) muestra la solución
#'   estandarizada; si `FALSE`, la no estandarizada (sin \eqn{R^2}).
#' @param mostrar_r2 Mostrar la \eqn{R^2} de cada variable explicada.
#' @param mostrar_errores Dibujar los círculos de error.
#' @param mostrar_valores Escribir los valores sobre las flechas.
#' @param estrellas Añadir asteriscos de significancia (`*` p < .05,
#'   `**` p < .01, `***` p < .001) a los parámetros libres.
#' @param decimales Número de decimales.
#' @param etiquetas Vector o lista con nombres para cambiar lo que se escribe
#'   dentro de las figuras, p. ej. `c(Biblical = "Bíblico")`. Use `"\n"` para
#'   partir un nombre en dos renglones.
#' @param lados Vector o lista con nombres para indicar hacia dónde van los
#'   ítems de un constructo: `"arriba"`, `"abajo"`, `"izquierda"` o
#'   `"derecha"`, p. ej. `c(F3 = "abajo")`.
#' @param mover Lista con nombres para desplazar elementos a mano, en unidades
#'   del diagrama (el ancho de un ítem es aproximadamente 1), p. ej.
#'   `list(Care = c(1, -0.5))`. Un constructo se mueve junto con sus ítems y
#'   errores.
#' @param estilo Aspecto del diagrama. Véase la sección *Estilos*.
#'   * `"clasico"` (por omisión): el de AMOS, con degradado y la paleta elegida.
#'   * `"revista"`: en grises, listo para publicar.
#'   * `"color"`: como `"revista"`, pero cada factor y sus ítems con un color
#'     propio.
#' @param paleta Colores, creados con [paleta_sem()]. Solo en el estilo
#'   `"clasico"`.
#' @param degradado Rellenar con degradado (`TRUE`) o con color sólido. Solo
#'   en el estilo `"clasico"`.
#' @param titulo Título opcional. Para un artículo conviene no ponerlo: en
#'   formato APA el número y el título de la figura van fuera de la imagen.
#' @param prefijo_error Prefijo de los nombres de los errores (`"e"`).
#' @param grupo En modelos multigrupo, número del grupo que se dibuja.
#' @param umbral Solo para análisis exploratorios: carga mínima (en valor
#'   absoluto) para dibujar una flecha. Por omisión .30. Cada ítem se dibuja
#'   con el factor en que más carga; si esa carga es menor que el umbral, la
#'   flecha se dibuja punteada.
#' @param nfactores Solo para `lavaan::efa()` con varias soluciones: número de
#'   factores de la solución que se dibuja.
#' @param archivo Si se indica, guarda el diagrama en ese archivo (`.pdf`,
#'   `.png`, `.tiff`, `.eps`, `.svg` o `.jpg`) con [guardar_diagrama()].
#' @param ancho Ancho del archivo: `"pagina"` (174 mm), `"columna"` (85 mm) o
#'   un número en pulgadas. Véase [guardar_diagrama()].
#' @param dibujar Si `TRUE` (por omisión) dibuja en la ventana de gráficos.
#'
#' @section Estilos:
#' Los estilos `"revista"` y `"color"` siguen los requisitos habituales de las
#' revistas (APA, Elsevier, Springer, Taylor & Francis, PLOS): letra sin
#' serifa (Arial o Helvetica) más grande, líneas de 0.6 a 0.9 pt, relleno
#' plano, sin sombras ni degradados, y los valores sobre un fondo blanco para
#' que ninguna línea los cruce. Los errores y las correlaciones van en gris
#' para que destaquen las cargas y los efectos. No se escriben los "1" de las
#' flechas de error (se pueden explicar en la nota de la figura).
#'
#' El estilo `"color"` usa la paleta Okabe-Ito, distinguible por personas con
#' daltonismo y legible si la revista imprime en blanco y negro. Muchas
#' revistas publican en color sin costo en línea, pero cobran el color
#' impreso; si no está seguro, use `"revista"`.
#'
#' @section Acomodos:
#' \describe{
#'   \item{`"fila"`}{Constructos en fila, ítems arriba y correlaciones como
#'     arcos abajo; en segundo orden, el factor general va abajo. Automático en
#'     AFC y segundo orden con hasta 3 factores.}
#'   \item{`"dos_filas"`}{Constructos en dos filas, ítems hacia afuera (la
#'     "cruz" clásica); en segundo orden, el factor general va al centro.
#'     Automático con 4 o más factores.}
#'   \item{`"arbol"`}{Factor general arriba, constructos en fila e ítems abajo.}
#'   \item{`"bifactor"`}{Factor general arriba, ítems al centro y factores
#'     específicos abajo. Automático en modelos bifactor.}
#'   \item{`"izq_der"`}{Por niveles causales de izquierda a derecha. En SEM,
#'     cada constructo pone sus ítems donde menos estorban. Automático en
#'     análisis de trayectorias y SEM.}
#'   \item{`"arriba_abajo"`}{Por niveles causales de arriba hacia abajo.
#'     Automático en modelos MIMIC y en trayectorias con 4 o más variables
#'     exógenas.}
#' }
#'
#' @return Invisiblemente, un objeto de clase `diagrama_sem` que puede volver
#'   a dibujarse con `plot()` o guardarse con [guardar_diagrama()]. Contiene,
#'   entre otros, `tipo`, `acomodo` y `correlaciones_ocultas`.
#'
#' @examples
#' library(lavaan)
#' modelo <- "visual  =~ x1 + x2 + x3
#'            textual =~ x4 + x5 + x6
#'            speed   =~ x7 + x8 + x9"
#' fit <- cfa(modelo, data = HolzingerSwineford1939)
#' diagrama_sem(fit)
#' diagrama_sem(fit, acomodo = "dos_filas", paleta = paleta_sem("azul"))
#' diagrama_sem(fit, estilo = "revista")
#' diagrama_sem(fit, etiquetas = c(visual = "Visual", textual = "Verbal",
#'                                 speed = "Velocidad"), estrellas = TRUE)
#'
#' # Guardar en un archivo
#' archivo <- file.path(tempdir(), "afc.png")
#' diagrama_sem(fit, archivo = archivo, dibujar = FALSE)
#'
#' # Figura para un artículo: PDF a lo ancho de la página
#' archivo <- file.path(tempdir(), "figura1.pdf")
#' diagrama_sem(fit, estilo = "revista", archivo = archivo, ancho = "pagina",
#'              dibujar = FALSE)
#' @export
diagrama_sem <- function(fit, acomodo = "auto", correlaciones = "auto",
                         estandarizado = TRUE, mostrar_r2 = TRUE, mostrar_errores = TRUE,
                         mostrar_valores = TRUE, estrellas = FALSE, decimales = 2,
                         etiquetas = NULL, lados = NULL, mover = NULL, estilo = "clasico",
                         paleta = paleta_sem(), degradado = TRUE, titulo = NULL,
                         prefijo_error = "e", grupo = 1, umbral = 0.30, nfactores = NULL,
                         archivo = NULL, ancho = NULL, dibujar = TRUE) {
  if (!inherits(paleta, "paleta_sem")) stop("`paleta` debe crearse con paleta_sem().", call. = FALSE)
  estilo <- validar_estilo(estilo)
  if (es_afe(fit)) {
    G <- estructura_afe(leer_afe(fit, nfactores), umbral, etiquetas, prefijo_error, mostrar_errores)
    tipo <- "afe"; estandarizado <- TRUE
    if (length(G$debiles))
      message("Sin cargas >= ", formato_valor(umbral, TRUE, 2), ": ", paste(G$debiles, collapse = ", "),
              ". Se dibujan con l\u00ednea punteada hacia el factor en que m\u00e1s cargan.")
  } else {
    G <- extraer_estructura(fit, estandarizado, grupo, etiquetas, prefijo_error, mostrar_errores)
    tipo <- detectar_tipo(G)
  }

  # Correlaciones entre constructos
  es_cor_lv <- G$edges$tipo == "cov"
  if (tipo %in% c("afc", "afe", "segundo_orden", "bifactor"))
    es_cor_lv <- es_cor_lv & G$edges$from %in% G$lv & G$edges$to %in% G$lv
  ocultar <- rep(FALSE, nrow(G$edges))
  if (identical(correlaciones, "auto")) {
    if (tipo %in% c("afc", "afe", "bifactor") && length(G$lv1) >= 5) {
      ocultar <- es_cor_lv
      if (any(ocultar)) message("Se ocultaron ", sum(ocultar), " correlaciones entre factores para ",
                                "que el diagrama sea legible (5 o m\u00e1s factores). Usa correlaciones = TRUE ",
                                "para mostrarlas; los valores est\u00e1n en $correlaciones_ocultas.")
    }
  } else if (isFALSE(correlaciones)) {
    ocultar <- es_cor_lv
  } else if (is.numeric(correlaciones)) {
    ocultar <- es_cor_lv & abs(G$edges$val) < correlaciones
  } else if (!isTRUE(correlaciones)) {
    stop("`correlaciones` debe ser \"auto\", TRUE, FALSE o un n\u00famero entre 0 y 1.", call. = FALSE)
  }
  ocultas <- G$edges[ocultar, c("from", "to", "val", "p")]
  names(ocultas) <- c("variable_1", "variable_2", "valor", "p")
  G$edges <- G$edges[!ocultar, ]

  # Lados de los ítems
  if (!is.null(lados)) {
    lados <- as.list(lados)
    malos <- setdiff(names(lados), G$lv1)
    if (length(malos)) stop("`lados`: estos no son constructos con \u00edtems: ", paste(malos, collapse = ", "), call. = FALSE)
    if (!all(unlist(lados) %in% names(LADOS)))
      stop("`lados` acepta: ", paste(names(LADOS), collapse = ", "), ".", call. = FALSE)
    G$lados <- lados
  }

  G <- ajustar_tamanos(G, estilo)

  # Acomodo
  validos <- ACOMODOS[[tipo]]
  if (identical(acomodo, "auto")) acomodo <- elegir_acomodo(G, tipo)
  else if (!acomodo %in% validos)
    stop("El acomodo '", acomodo, "' no aplica a este modelo (", nombre_tipo(tipo), "). Opciones: ",
         paste(c("auto", validos), collapse = ", "), ".", call. = FALSE)
  G <- aplicar_acomodo(G, tipo, acomodo)
  G <- aplicar_mover(G, mover)

  obj <- structure(list(
    G = G, trazos = trazar(G), tipo = tipo, acomodo = acomodo, correlaciones_ocultas = ocultas,
    opciones = list(estandarizado = estandarizado, mostrar_r2 = mostrar_r2, mostrar_valores = mostrar_valores,
                    estrellas = estrellas, decimales = decimales, paleta = paleta,
                    degradado = degradado, titulo = titulo, estilo = estilo)),
    class = "diagrama_sem")

  if (dibujar) dibujar_diagrama(obj)
  if (!is.null(archivo)) guardar_diagrama(obj, archivo, ancho = ancho)
  invisible(obj)
}

#' @noRd
nombre_tipo <- function(tipo) switch(tipo,
  afc = "an\u00e1lisis factorial confirmatorio", afe = "an\u00e1lisis factorial exploratorio", segundo_orden = "AFC de segundo orden",
  bifactor = "modelo bifactor", path = "an\u00e1lisis de trayectorias", sem = "modelo de ecuaciones estructurales")

#' Guarda un diagrama en un archivo
#'
#' El formato se deduce de la extensión del archivo. Si no se indica el ancho,
#' se calcula a partir del tamaño del diagrama.
#'
#' @section Para enviar a una revista:
#' Indique el ancho con que se imprimirá la figura: `ancho = "pagina"`
#' (174 mm, a lo ancho de la página) o `ancho = "columna"` (85 mm, una
#' columna). El texto queda del tamaño real que tendrá impreso, y la función
#' avisa si los ítems o los valores quedan por debajo de 8 pt, el mínimo que
#' piden APA y la mayoría de las revistas. Los diagramas con varios factores
#' casi nunca caben legibles en una columna.
#'
#' Formatos recomendados:
#' * `.pdf` o `.eps`: vectoriales, con las fuentes incrustadas. Se ven nítidos
#'   a cualquier tamaño; son los preferidos por Elsevier, Springer y Taylor &
#'   Francis.
#' * `.tiff`: imagen con compresión LZW, a 600 dpi por omisión (PLOS pide
#'   TIFF o EPS).
#' * `.png`: para Word, PowerPoint o tesis (300 dpi por omisión).
#' * `.svg` y `.jpg` también se aceptan.
#'
#' @param x Objeto creado por [diagrama_sem()].
#' @param archivo Ruta del archivo: `.pdf`, `.eps`, `.tiff`, `.png`, `.svg`
#'   o `.jpg`.
#' @param ancho Ancho de la figura: `"pagina"` (174 mm), `"columna"` (85 mm)
#'   o un número en pulgadas. Si se omite, se calcula según el diagrama.
#' @param alto Alto en pulgadas (opcional). Si se omite, se conserva la
#'   proporción.
#' @param res Resolución en puntos por pulgada para las imágenes. Por
#'   omisión, 600 para `.tiff` y 300 para los demás.
#' @return Invisiblemente, la ruta del archivo.
#' @examples
#' library(lavaan)
#' fit <- cfa("F =~ x1 + x2 + x3 + x4", data = HolzingerSwineford1939)
#' d <- diagrama_sem(fit, estilo = "revista", dibujar = FALSE)
#' guardar_diagrama(d, file.path(tempdir(), "figura1.pdf"), ancho = "pagina")
#' @export
guardar_diagrama <- function(x, archivo, ancho = NULL, alto = NULL, res = NULL) {
  if (!inherits(x, "diagrama_sem")) stop("`x` debe crearse con diagrama_sem().", call. = FALSE)
  ext <- tolower(tools::file_ext(archivo))
  if (!ext %in% c("png", "jpg", "jpeg", "tiff", "tif", "pdf", "eps", "svg"))
    stop("Formato no reconocido: '", ext, "'. Usa pdf, eps, tiff, png, svg o jpg.", call. = FALSE)
  if (is.null(res)) res <- if (ext %in% c("tiff", "tif")) 600 else 300
  nombre_ancho <- NULL
  if (is.character(ancho)) {
    if (!ancho %in% names(ANCHOS)) stop("`ancho` debe ser \"pagina\", \"columna\" o un n\u00famero en pulgadas.", call. = FALSE)
    nombre_ancho <- ancho; ancho <- ANCHOS[[ancho]]
  }
  cj <- caja_total(x$G, x$trazos)
  W <- diff(cj[1:2]) + 0.5; H <- diff(cj[3:4]) + 0.5 + if (is.null(x$opciones$titulo)) 0 else 0.6
  if (is.null(ancho) && is.null(alto)) {
    ancho <- W * PULG_POR_UNIDAD; alto <- H * PULG_POR_UNIDAD
    k <- min(1, 14 / ancho, 14 / alto)                    # máximo 14 pulgadas por lado
    if (max(ancho, alto) * k < 4) k <- 4 / max(ancho, alto)  # mínimo 4 pulgadas
    ancho <- ancho * k; alto <- alto * k
  } else if (is.null(alto)) alto <- ancho * H / W
  else if (is.null(ancho)) ancho <- alto * W / H

  if (!is.null(nombre_ancho)) revisar_tamano(x, ancho, alto, W, H)

  cairo <- capabilities("cairo")
  switch(ext,
    png  = grDevices::png(archivo, width = ancho, height = alto, units = "in", res = res),
    jpg  = , jpeg = grDevices::jpeg(archivo, width = ancho, height = alto, units = "in", res = res, quality = 95),
    tiff = , tif = grDevices::tiff(archivo, width = ancho, height = alto, units = "in", res = res, compression = "lzw"),
    pdf  = if (cairo) grDevices::cairo_pdf(archivo, width = ancho, height = alto)
           else grDevices::pdf(archivo, width = ancho, height = alto),
    eps  = if (cairo) grDevices::cairo_ps(archivo, width = ancho, height = alto)
           else grDevices::postscript(archivo, width = ancho, height = alto, horizontal = FALSE,
                                      onefile = FALSE, paper = "special"),
    svg  = {
      if (!cairo) stop("Para guardar en SVG se necesita soporte de cairo en R.", call. = FALSE)
      grDevices::svg(archivo, width = ancho, height = alto)
    })
  on.exit(grDevices::dev.off())
  dibujar_diagrama(x)
  invisible(archivo)
}

# Calcula el tamaño real del texto al ancho de impresión y avisa si es menor
# de 8 pt o si la figura no cabe en una página.
#' @noRd
tamano_texto <- function(x, ancho, alto, W, H) {
  # mismo cálculo que dibujar_diagrama(): márgenes de 0.3 líneas (~0.06 pulgadas)
  pin <- c(ancho, alto) - 0.12 - c(0, if (is.null(x$opciones$titulo)) 0 else 0.34)
  f <- min(pin[1] / (W - 0.0), pin[2] / (H - if (is.null(x$opciones$titulo)) 0 else 0.6)) / PULG_POR_UNIDAD
  est <- if (is.null(x$opciones$estilo)) "clasico" else x$opciones$estilo
  12 * TEXTO_ITEM[[est]] * f
}

#' @noRd
revisar_tamano <- function(x, ancho, alto, W, H) {
  pt <- tamano_texto(x, ancho, alto, W, H)
  mm <- round(c(ancho, alto) * 25.4)
  message(sprintf("Figura de %d x %d mm; texto de los \u00edtems y valores: %.1f pt.", mm[1], mm[2], pt))
  if (pt < 8) {
    sug <- c(
      if (ancho < ANCHOS[["pagina"]] - 1e-6) "usa ancho = \"pagina\"",
      if (!identical(x$opciones$estilo, "revista") && !identical(x$opciones$estilo, "color"))
        "usa estilo = \"revista\" (letra m\u00e1s grande)",
      "oculta los errores con mostrar_errores = FALSE o acorta las etiquetas")
    warning(sprintf("El texto queda en %.1f pt; APA y la mayor\u00eda de las revistas piden al menos 8 pt. Para agrandarlo: %s.",
                    pt, paste(sug, collapse = "; ")), call. = FALSE)
  }
  if (mm[2] > 230)
    warning("La figura mide ", mm[2], " mm de alto, m\u00e1s que el \u00e1rea de texto de una p\u00e1gina (~230 mm).",
            call. = FALSE)
  invisible(pt)
}

#' @export
plot.diagrama_sem <- function(x, ...) dibujar_diagrama(x)

#' @export
print.diagrama_sem <- function(x, ...) {
  N <- x$G$nodes
  cat("Diagrama de diagramaSEM\n")
  cat("  Tipo de modelo:", nombre_tipo(x$tipo), "\n")
  cat("  Acomodo:       ", x$acomodo, "\n")
  cat("  Constructos:   ", sum(N$tipo == "lv"), " | Variables observadas:", sum(N$tipo %in% c("item", "ovc")),
      " | Errores:", sum(N$tipo %in% c("err", "dist")), "\n")
  cat("  Soluci\u00f3n:      ", if (x$opciones$estandarizado) "estandarizada" else "no estandarizada", "\n")
  if (nrow(x$correlaciones_ocultas))
    cat("  Correlaciones ocultas:", nrow(x$correlaciones_ocultas), "(ver $correlaciones_ocultas)\n")
  cat("Usa plot() para dibujarlo o guardar_diagrama() para guardarlo.\n")
  invisible(x)
}
