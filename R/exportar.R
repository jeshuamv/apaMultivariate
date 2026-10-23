# Impresión en consola y guardado de las tablas (Word .rtf, HTML, CSV)

#' @noRd
bloques <- function(t) c(list(t), t$extra)

#' @export
print.tabla_apa <- function(x, ...) {
  for (k in seq_along(bloques(x))) {
    b <- bloques(x)[[k]]
    cat(if (k == 1) "\n" else "", b$titulo, "\n", sep = "")
    M <- rbind(b$columnas, b$celdas)
    w <- apply(M, 2, function(v) max(nchar(v, type = "width")))
    linea <- strrep("\u2500", sum(w) + 2 * (length(w) - 1))
    fila <- function(v) paste(mapply(function(s, wi, al) {
      pad <- wi - nchar(s, type = "width")
      if (al == "l") paste0(s, strrep(" ", pad)) else paste0(strrep(" ", floor(pad / 2)), s, strrep(" ", ceiling(pad / 2)))
    }, v, w, b$alinear), collapse = "  ")
    cat(linea, "\n", fila(b$columnas), "\n", linea, "\n", sep = "")
    for (i in seq_len(nrow(b$celdas))) {
      cat(fila(b$celdas[i, ]), "\n", sep = "")
      if (i %in% b$separadores) cat(strrep("\u2508", nchar(linea)), "\n", sep = "")
    }
    cat(linea, "\n", sep = "")
    if (!is.null(b$nota)) cat(strwrap(paste("Nota.", b$nota), width = max(60, nchar(linea))), sep = "\n")
    cat("\n")
  }
  invisible(x)
}

#' Guarda una tabla en un archivo
#'
#' Guarda una tabla creada con [tabla_cargas()], [tabla_ajuste()] o
#' [tabla_validez()]. El formato se elige por la extensión:
#'
#' * `.rtf` (o `.doc`): documento que se abre en **Word** con la tabla en
#'   formato APA (líneas horizontales, título y nota). Desde Word se puede
#'   guardar como `.docx`.
#' * `.html`: página que se abre en el navegador; la tabla se puede copiar y
#'   pegar en Word o Google Docs conservando el formato.
#' * `.csv`: solo los datos, para Excel.
#'
#' @param x Tabla creada por una función `tabla_*()`.
#' @param archivo Ruta del archivo.
#' @param numero Número de la tabla para el encabezado APA (`Tabla 1`).
#' @return Invisiblemente, la ruta del archivo.
#' @examples
#' library(lavaan)
#' fit <- cfa("visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6", data = HolzingerSwineford1939)
#' guardar_tabla(tabla_ajuste(fit), file.path(tempdir(), "ajuste.rtf"))
#' @export
guardar_tabla <- function(x, archivo, numero = 1) {
  if (!inherits(x, "tabla_apa")) stop("`x` debe crearse con tabla_cargas(), tabla_ajuste() o tabla_validez().", call. = FALSE)
  ext <- tolower(tools::file_ext(archivo))
  switch(ext,
    rtf = , doc = writeLines(tabla_rtf(x, numero), archivo, useBytes = TRUE),
    html = , htm = writeLines(enc2utf8(tabla_html(x, numero)), archivo, useBytes = TRUE),
    csv = {
      b <- bloques(x); con <- file(archivo, open = "w", encoding = "UTF-8"); on.exit(close(con))
      for (k in seq_along(b)) {
        if (k > 1) writeLines("", con)
        M <- rbind(b[[k]]$columnas, b[[k]]$celdas); M[] <- utf8_real(M)
        utils::write.table(M, con, sep = ",", row.names = FALSE,
                           col.names = FALSE, qmethod = "double")
      }
    },
    docx = stop("Para Word usa la extensi\u00f3n .rtf: se abre en Word y desde ah\u00ed puedes guardarlo como .docx.",
                call. = FALSE),
    stop("Formato no reconocido: '", ext, "'. Usa .rtf (Word), .html o .csv.", call. = FALSE))
  invisible(archivo)
}

# Repara textos que R convirtió a "<U+00ED>" en sistemas sin UTF-8
#' @noRd
utf8_real <- function(s) {
  s <- enc2utf8(as.character(s))
  m <- gregexpr("<U\\+[0-9A-Fa-f]{4,6}>", s)
  regmatches(s, m) <- lapply(regmatches(s, m), function(v)
    vapply(v, function(z) intToUtf8(strtoi(substr(z, 4, nchar(z) - 1), 16L)), character(1)))
  s
}

# --- RTF (Word) -----------------------------------------------------------
#' @noRd
rtf_txt <- function(s) {
  s <- utf8_real(s)
  s <- gsub("\\\\", "\\\\\\\\", s); s <- gsub("\\{", "\\\\{", s); s <- gsub("\\}", "\\\\}", s)
  cp <- utf8ToInt(enc2utf8(s)); if (!length(cp)) return("")
  paste(vapply(cp, function(u) if (u < 128) intToUtf8(u) else paste0("\\u", if (u > 32767) u - 65536 else u, "?"),
               character(1)), collapse = "")
}

#' @noRd
tabla_rtf <- function(x, numero) {
  out <- c("{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 Times New Roman;}}\\f0\\fs24")
  for (k in seq_along(bloques(x))) {
    b <- bloques(x)[[k]]
    M <- rbind(b$columnas, b$celdas); N <- rbind(rep(FALSE, ncol(M)), b$negritas)
    w <- apply(M, 2, function(v) max(nchar(v))) + 2
    fs <- 24; tw <- w * 118                          # ~118 twips por carácter a 12 pt
    if (sum(tw) > 9360) { fs <- 20; tw <- w * 98 }   # tabla ancha: letra de 10 pt
    if (sum(tw) > 9360) tw <- tw * 9360 / sum(tw)    # y si aún no cabe, se ajusta
    anchos <- cumsum(round(tw))
    if (k == 1) out <- c(out, paste0("{\\pard\\sb0\\sa0\\b Tabla ", numero, "\\par}"))
    out <- c(out, paste0("{\\pard\\sb0\\sa120\\i ", rtf_txt(b$titulo), "\\par}"))
    for (i in seq_len(nrow(M))) {
      arriba <- i == 1; abajo <- i == 1 || i == nrow(M)
      punteada <- (i - 1) %in% b$separadores && i > 1
      defs <- paste0(vapply(anchos, function(a) paste0(
        if (arriba) "\\clbrdrt\\brdrs\\brdrw10" else "",
        if (abajo) "\\clbrdrb\\brdrs\\brdrw10" else if (punteada) "\\clbrdrb\\brdrdot\\brdrw10" else "",
        "\\cellx", a), character(1)), collapse = "")
      celdas <- paste0(vapply(seq_len(ncol(M)), function(j) paste0(
        "\\pard\\intbl\\fs", fs, "\\", if (b$alinear[j] == "l") "ql" else "qc", " ",
        if (N[i, j]) "{\\b " else "{", rtf_txt(M[i, j]), "}\\cell"), character(1)), collapse = "")
      out <- c(out, paste0("\\trowd\\trgaph80", defs, "\\fs", fs, " ", celdas, "\\row\\fs24"))
    }
    if (!is.null(b$nota)) out <- c(out, paste0("{\\pard\\sb60\\sa240\\fs20{\\i Nota.} ", rtf_txt(b$nota), "\\par}"))
    else out <- c(out, "{\\pard\\sa240\\par}")
  }
  c(out, "}")
}

# --- HTML -----------------------------------------------------------------
#' @noRd
html_txt <- function(s) { s <- utf8_real(s); s <- gsub("&", "&amp;", s); s <- gsub("<", "&lt;", s); gsub(">", "&gt;", s) }

#' @noRd
tabla_html <- function(x, numero) {
  css <- paste("body{font-family:'Times New Roman',serif;font-size:12pt;margin:2em;}",
               "table{border-collapse:collapse;margin:.3em 0 .4em;}",
               "th{font-weight:normal;border-top:1.5px solid #000;border-bottom:1px solid #000;padding:4px 12px;}",
               "td{padding:3px 12px;} tr.ultima td{border-bottom:1.5px solid #000;}",
               "tr.sep td{border-bottom:1px dotted #000;} .l{text-align:left;} .c{text-align:center;}",
               "p.titulo{font-style:italic;margin:0 0 .2em;} p.nota{font-size:10pt;margin:.2em 0 2em;max-width:42em;}")
  out <- c("<!DOCTYPE html><html lang=\"es\"><head><meta charset=\"utf-8\"><title>Tabla</title>",
           paste0("<style>", css, "</style></head><body>"))
  for (k in seq_along(bloques(x))) {
    b <- bloques(x)[[k]]
    if (k == 1) out <- c(out, paste0("<p style=\"font-weight:bold;margin:0\">Tabla ", numero, "</p>"))
    out <- c(out, paste0("<p class=\"titulo\">", html_txt(b$titulo), "</p><table>"),
             paste0("<tr>", paste0("<th class=\"", b$alinear, "\">", html_txt(b$columnas), "</th>", collapse = ""), "</tr>"))
    for (i in seq_len(nrow(b$celdas))) {
      cls <- if (i == nrow(b$celdas)) " class=\"ultima\"" else if (i %in% b$separadores) " class=\"sep\"" else ""
      tds <- vapply(seq_len(ncol(b$celdas)), function(j) {
        v <- html_txt(b$celdas[i, j]); if (b$negritas[i, j]) v <- paste0("<b>", v, "</b>")
        paste0("<td class=\"", b$alinear[j], "\">", v, "</td>") }, character(1))
      out <- c(out, paste0("<tr", cls, ">", paste(tds, collapse = ""), "</tr>"))
    }
    out <- c(out, "</table>", if (!is.null(b$nota)) paste0("<p class=\"nota\"><i>Nota.</i> ", html_txt(b$nota), "</p>"))
  }
  c(out, "</body></html>")
}
