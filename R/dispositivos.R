# Abre el dispositivo gráfico según la extensión, dibuja y lo cierra. En los
# formatos vectoriales intenta primero cairo (incrusta las fuentes) y, si no
# funciona (por ejemplo, en una Mac sin XQuartz), usa pdf() o postscript().
#' @noRd
con_dispositivo <- function(archivo, ext, ancho, alto, dibujar, res = 300, pointsize = 12) {
  abrir <- function(cairo) switch(ext,
    png  = grDevices::png(archivo, width = ancho, height = alto, units = "in", res = res, pointsize = pointsize),
    jpg  = , jpeg = grDevices::jpeg(archivo, width = ancho, height = alto, units = "in", res = res, quality = 95, pointsize = pointsize),
    tiff = , tif = grDevices::tiff(archivo, width = ancho, height = alto, units = "in", res = res, compression = "lzw", pointsize = pointsize),
    pdf  = if (cairo) grDevices::cairo_pdf(archivo, width = ancho, height = alto, pointsize = pointsize)
           else grDevices::pdf(archivo, width = ancho, height = alto, pointsize = pointsize, useDingbats = FALSE),
    eps  = if (cairo) grDevices::cairo_ps(archivo, width = ancho, height = alto, pointsize = pointsize)
           else grDevices::postscript(archivo, width = ancho, height = alto, pointsize = pointsize, horizontal = FALSE,
                                      onefile = FALSE, paper = "special"),
    svg  = {
      if (!cairo) stop("Para guardar en SVG se necesita soporte de cairo en R; guarda en PDF o PNG.", call. = FALSE)
      grDevices::svg(archivo, width = ancho, height = alto, pointsize = pointsize)
    },
    stop("Formato no reconocido: usa pdf, png, tiff, eps, svg o jpg.", call. = FALSE))
  intento <- function(cairo) {
    if (file.exists(archivo)) unlink(archivo)
    d0 <- grDevices::dev.list()
    abierto <- tryCatch({ abrir(cairo); TRUE },
                        warning = function(w) FALSE,
                        error = function(e) if (grepl("Formato|SVG", conditionMessage(e))) stop(e) else FALSE)
    nuevos <- setdiff(grDevices::dev.list(), d0)
    if (!abierto) { for (d in nuevos) try(grDevices::dev.off(d), silent = TRUE); return(FALSE) }
    r <- tryCatch({ dibujar(); NULL }, error = function(e) e)
    grDevices::dev.off()
    if (!is.null(r)) stop(r)
    file.exists(archivo) && isTRUE(file.size(archivo) > 0)
  }
  vectorial <- ext %in% c("pdf", "eps", "svg")
  viejo <- options(apaMultivariate.sin_unicode = FALSE); on.exit(options(viejo), add = TRUE)
  if (vectorial && capabilities("cairo") && intento(TRUE)) return(invisible(archivo))
  if (ext == "svg") stop("No se pudo guardar en SVG en esta computadora; guarda en PDF o PNG.", call. = FALSE)
  if (vectorial) options(apaMultivariate.sin_unicode = TRUE)
  if (!intento(FALSE)) stop("No se pudo guardar '", archivo, "'. Revisa que la carpeta exista y que el archivo no est\u00e9 abierto en otro programa.", call. = FALSE)
  invisible(archivo)
}

# Sin cairo, pdf() y postscript() no tienen el signo menos tipográfico.
#' @noRd
txt_disp <- function(x) if (isTRUE(getOption("apaMultivariate.sin_unicode"))) gsub("\u2212", "-", x) else x
