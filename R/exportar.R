# Impresión en consola y guardado de las tablas (Word .rtf, HTML, CSV)

#' @noRd
bloques <- function(t) c(list(t), t$extra)

# Símbolos estadísticos que APA escribe en cursiva (las letras griegas no)
CURSIVAS <- c("p", "M", "DE", "EE", "n", "N", "r", "gl", "z", "t", "F", "B", "b", "SE", "SD", "df")

#' @noRd
es_cursiva <- function(s) s %in% CURSIVAS

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
    if (!is.null(b$nota)) cat(strwrap(paste(tx("Nota.", "Note."), b$nota), width = min(getOption("width", 80) - 2, max(60, nchar(linea)))), sep = "\n")
    cat("\n")
    if (nchar(linea) > getOption("width", 80)) ancha <- TRUE
  }
  if (exists("ancha", inherits = FALSE))
    message(tx("La tabla es m\u00e1s ancha que la consola y puede verse cortada. Agranda el panel de la consola o gu\u00e1rdala en Word: guardar_tabla(tabla, \"tabla.docx\").",
               "The table is wider than the console and may look broken. Widen the console pane or save it to Word: guardar_tabla(table, \"table.docx\")."))
  invisible(x)
}

#' Guarda una tabla en un archivo
#'
#' Guarda una tabla creada con [tabla_cargas()], [tabla_ajuste()] o
#' [tabla_validez()]. El formato se elige por la extensión:
#'
#' * `.docx`: documento de **Word** con la tabla en formato APA (líneas
#'   horizontales, número, título y nota). Las tablas anchas se pasan a letra
#'   de 10 pt y, si aún no caben, a página horizontal.
#' * `.rtf`: igual, en formato RTF (lo abre cualquier procesador de textos).
#' * `.html`: página que se abre en el navegador; la tabla se puede copiar y
#'   pegar en Word o Google Docs conservando el formato.
#' * `.csv`: solo los datos, para Excel.
#'
#' @param x Tabla creada por una función `tabla_*()`.
#' @param archivo Ruta del archivo.
#' @param numero Número de la tabla para el encabezado APA (`Tabla 1`).
#' @return Invisiblemente, la ruta del archivo.
#' @seealso [guardar_tablas()] para guardar varias tablas en un mismo
#'   documento de Word.
#' @examples
#' library(lavaan)
#' fit <- cfa("visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6", data = HolzingerSwineford1939)
#' guardar_tabla(tabla_ajuste(fit), file.path(tempdir(), "ajuste.docx"))
#' @export
guardar_tabla <- function(x, archivo, numero = 1) {
  if (!inherits(x, "tabla_apa")) stop("`x` debe crearse con tabla_cargas(), tabla_ajuste() o tabla_validez().", call. = FALSE)
  ext <- tolower(tools::file_ext(archivo))
  switch(ext,
    docx = escribir_docx(list(x), archivo, numero),
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
    stop("Formato no reconocido: '", ext, "'. Usa .docx (Word), .rtf, .html o .csv.", call. = FALSE))
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
    w <- apply(M, 2, function(v) max(ancho_txt(v))) + 3
    fs <- 24; tw <- w * 118                          # ~118 twips por carácter a 12 pt
    if (sum(tw) > 9360) { fs <- 20; tw <- w * 98 }   # tabla ancha: letra de 10 pt
    if (sum(tw) > 9360) tw <- tw * 9360 / sum(tw)    # y si aún no cabe, se ajusta
    anchos <- cumsum(round(tw))
    if (k == 1) out <- c(out, paste0("{\\pard\\sb0\\sa0\\b ", tx("Tabla", "Table"), " ", numero, "\\par}"))
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
        if (N[i, j]) "{\\b " else if (i == 1 && es_cursiva(M[i, j])) "{\\i " else "{", rtf_txt(M[i, j]), "}\\cell"), character(1)), collapse = "")
      out <- c(out, paste0("\\trowd\\trgaph80", defs, "\\fs", fs, " ", celdas, "\\row\\fs24"))
    }
    if (!is.null(b$nota)) out <- c(out, paste0("{\\pard\\sb60\\sa240\\fs20{\\i ", tx("Nota.", "Note."), "} ", rtf_txt(b$nota), "\\par}"))
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
  out <- c(paste0("<!DOCTYPE html><html lang=\"", tx("es", "en"), "\"><head><meta charset=\"utf-8\"><title>", tx("Tabla", "Table"), "</title>"),
           paste0("<style>", css, "</style></head><body>"))
  for (k in seq_along(bloques(x))) {
    b <- bloques(x)[[k]]
    if (k == 1) out <- c(out, paste0("<p style=\"font-weight:bold;margin:0\">", tx("Tabla", "Table"), " ", numero, "</p>"))
    out <- c(out, paste0("<p class=\"titulo\">", html_txt(b$titulo), "</p><table>"),
             paste0("<tr>", paste0("<th class=\"", b$alinear, "\">",
                                   ifelse(es_cursiva(b$columnas), paste0("<i>", html_txt(b$columnas), "</i>"), html_txt(b$columnas)),
                                   "</th>", collapse = ""), "</tr>"))
    for (i in seq_len(nrow(b$celdas))) {
      cls <- if (i == nrow(b$celdas)) " class=\"ultima\"" else if (i %in% b$separadores) " class=\"sep\"" else ""
      tds <- vapply(seq_len(ncol(b$celdas)), function(j) {
        v <- html_txt(b$celdas[i, j]); if (b$negritas[i, j]) v <- paste0("<b>", v, "</b>")
        paste0("<td class=\"", b$alinear[j], "\">", v, "</td>") }, character(1))
      out <- c(out, paste0("<tr", cls, ">", paste(tds, collapse = ""), "</tr>"))
    }
    out <- c(out, "</table>", if (!is.null(b$nota)) paste0("<p class=\"nota\"><i>", tx("Nota.", "Note."), "</i> ", html_txt(b$nota), "</p>"))
  }
  c(out, "</body></html>")
}

#' Guarda varias tablas en un mismo documento de Word
#'
#' Reúne en un solo archivo `.docx` varias tablas creadas con las funciones
#' `tabla_*()`, numeradas en orden (Tabla 1, Tabla 2, ...), cada una con su
#' título y su nota en formato APA. Útil para entregar todas las tablas de un
#' artículo o de una tesis en un archivo.
#'
#' @param ... Tablas creadas con las funciones `tabla_*()`, o una lista de
#'   ellas.
#' @param archivo Ruta del archivo `.docx`.
#' @param numero Número de la primera tabla.
#' @return Invisiblemente, la ruta del archivo.
#' @examples
#' library(lavaan)
#' fit <- cfa("visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6", data = HolzingerSwineford1939)
#' guardar_tablas(tabla_ajuste(fit), tabla_cargas(fit), tabla_validez(fit),
#'                archivo = file.path(tempdir(), "tablas.docx"))
#' @export
guardar_tablas <- function(..., archivo, numero = 1) {
  tablas <- list(...)
  if (length(tablas) == 1 && !inherits(tablas[[1]], "tabla_apa") && is.list(tablas[[1]])) tablas <- tablas[[1]]
  if (!length(tablas) || !all(vapply(tablas, inherits, logical(1), "tabla_apa")))
    stop("Pasa tablas creadas con las funciones tabla_*().", call. = FALSE)
  if (tolower(tools::file_ext(archivo)) != "docx")
    stop("guardar_tablas() guarda en Word: usa un archivo .docx.", call. = FALSE)
  escribir_docx(tablas, archivo, numero)
  invisible(archivo)
}

# --- DOCX (Word) ----------------------------------------------------------
#' @noRd
xml_txt <- function(s) {
  s <- utf8_real(s)
  s <- gsub("&", "&amp;", s, fixed = TRUE); s <- gsub("<", "&lt;", s, fixed = TRUE); gsub(">", "&gt;", s, fixed = TRUE)
}

#' @noRd
docx_run <- function(txt, b = FALSE, i = FALSE, sz = NULL) {
  paste0("<w:r><w:rPr>", if (b) "<w:b/>" else "", if (i) "<w:i/>" else "",
         if (!is.null(sz)) paste0("<w:sz w:val=\"", sz, "\"/><w:szCs w:val=\"", sz, "\"/>") else "",
         "</w:rPr><w:t xml:space=\"preserve\">", xml_txt(txt), "</w:t></w:r>")
}

#' @noRd
docx_par <- function(runs, antes = 0, despues = 0, jc = NULL, mantener = FALSE) {
  paste0("<w:p><w:pPr>", if (mantener) "<w:keepNext/>" else "",
         "<w:spacing w:before=\"", antes, "\" w:after=\"", despues, "\" w:line=\"240\" w:lineRule=\"auto\"/>",
         if (!is.null(jc)) paste0("<w:jc w:val=\"", jc, "\"/>") else "", "</w:pPr>", paste(runs, collapse = ""), "</w:p>")
}

# Ancho aproximado del texto en "caracteres promedio" (las mayúsculas ocupan más)
#' @noRd
ancho_txt <- function(v) vapply(utf8_real(v), function(z) {
  ch <- strsplit(z, "")[[1]]
  sum(ifelse(grepl("[\u2190-\u21ff\u2194\u2264\u2265\u2014]", ch), 1.7,
             ifelse(grepl("[A-Z\u00c0-\u00de\u0391-\u03a9]", ch), 1.45, ifelse(grepl("[ .,:;il|1]", ch), 0.6, 1))))
}, numeric(1), USE.NAMES = FALSE)

# Anchos de columna (twips) y tamaño de letra de un bloque de tabla
#' @noRd
docx_medidas <- function(b, disponible) {
  M <- rbind(b$columnas, b$celdas)
  w <- apply(M, 2, function(v) max(ancho_txt(v))) + 3
  sz <- 24; tw <- w * 118
  if (sum(tw) > disponible) { sz <- 20; tw <- w * 98 }
  list(sz = sz, tw = tw, ancho = sum(tw))
}

#' @noRd
docx_tabla <- function(b, med, disponible) {
  tw <- med$tw; if (sum(tw) > disponible) tw <- tw * disponible / sum(tw)
  tw <- round(tw); sz <- med$sz
  M <- rbind(b$columnas, b$celdas); N <- rbind(rep(FALSE, ncol(M)), b$negritas)
  linea <- function(lado, tipo = "single", g = 8) paste0("<w:", lado, " w:val=\"", tipo, "\" w:sz=\"", g, "\" w:space=\"0\" w:color=\"000000\"/>")
  filas <- vapply(seq_len(nrow(M)), function(i) {
    arriba <- i == 1; abajo <- i == 1 || i == nrow(M); punteada <- i > 1 && (i - 1) %in% b$separadores
    bordes <- paste0(if (arriba) linea("top") else "",
                     if (abajo) linea("bottom") else if (punteada) linea("bottom", "dotted", 4) else "")
    celdas <- vapply(seq_len(ncol(M)), function(j) paste0(
      "<w:tc><w:tcPr><w:tcW w:w=\"", tw[j], "\" w:type=\"dxa\"/>",
      if (nzchar(bordes)) paste0("<w:tcBorders>", bordes, "</w:tcBorders>") else "",
      "<w:vAlign w:val=\"bottom\"/></w:tcPr>",
      docx_par(docx_run(M[i, j], b = N[i, j], i = i == 1 && es_cursiva(M[i, j]), sz = sz),
               antes = if (i == 1) 40 else 0, despues = if (i == 1) 40 else 0,
               jc = if (b$alinear[j] == "l") "left" else "center", mantener = i < nrow(M)), "</w:tc>"), character(1))
    paste0("<w:tr><w:trPr><w:cantSplit/>", if (i == 1) "<w:tblHeader/>" else "", "</w:trPr>",
           paste(celdas, collapse = ""), "</w:tr>")
  }, character(1))
  paste0("<w:tbl><w:tblPr><w:tblW w:w=\"", sum(tw), "\" w:type=\"dxa\"/><w:tblLayout w:type=\"fixed\"/>",
         "<w:tblCellMar><w:left w:w=\"60\" w:type=\"dxa\"/><w:right w:w=\"60\" w:type=\"dxa\"/></w:tblCellMar>",
         "<w:tblLook w:val=\"0000\"/></w:tblPr><w:tblGrid>",
         paste0("<w:gridCol w:w=\"", tw, "\"/>", collapse = ""), "</w:tblGrid>", paste(filas, collapse = ""), "</w:tbl>")
}

#' @noRd
escribir_docx <- function(tablas, archivo, numero = 1) {
  if (!requireNamespace("zip", quietly = TRUE))
    stop("Para guardar en .docx instala el paquete zip: install.packages(\"zip\").", call. = FALSE)
  # Cada tabla va en página vertical (9360 twips útiles) u horizontal (12960)
  # si no cabe; las tablas horizontales quedan en su propia sección
  horiz <- vapply(tablas, function(t) any(vapply(bloques(t), function(b) docx_medidas(b, 9360)$ancho > 9360, logical(1))), logical(1))
  cuerpo <- character(0)
  for (k in seq_along(tablas)) {
    if (k > 1 && horiz[k] != horiz[k - 1])
      cuerpo <- c(cuerpo, paste0("<w:p><w:pPr>", seccion_docx(horiz[k - 1]), "</w:pPr></w:p>"))
    disponible <- if (horiz[k]) 12960 else 9360
    nueva_seccion <- k > 1 && horiz[k] != horiz[k - 1]
    bl <- bloques(tablas[[k]])
    for (m in seq_along(bl)) {
      b <- bl[[m]]; med <- docx_medidas(b, disponible)
      if (m == 1) cuerpo <- c(cuerpo, docx_par(docx_run(paste(tx("Tabla", "Table"), numero + k - 1), b = TRUE),
                                               antes = if (k > 1 && !nueva_seccion) 240 else 0, mantener = TRUE))
      cuerpo <- c(cuerpo, docx_par(docx_run(b$titulo, i = TRUE), despues = 120, mantener = TRUE),
                  docx_tabla(b, med, disponible))
      cuerpo <- c(cuerpo, if (!is.null(b$nota))
        docx_par(c(docx_run(tx("Nota.", "Note."), i = TRUE, sz = 20), docx_run(paste0(" ", b$nota), sz = 20)), antes = 60, despues = 240)
        else docx_par("", despues = 240))
    }
  }
  empaquetar_docx(paste(cuerpo, collapse = ""), archivo, horiz[length(horiz)])
}

# Propiedades de sección (tamaño carta, márgenes de 1 pulgada)
#' @noRd
seccion_docx <- function(horizontal) {
  paste0("<w:sectPr>", if (horizontal) "<w:pgSz w:w=\"15840\" w:h=\"12240\" w:orient=\"landscape\"/>" else "<w:pgSz w:w=\"12240\" w:h=\"15840\"/>",
         "<w:pgMar w:top=\"1440\" w:right=\"1440\" w:bottom=\"1440\" w:left=\"1440\" ",
         "w:header=\"720\" w:footer=\"720\" w:gutter=\"0\"/></w:sectPr>")
}

# Arma el archivo .docx a partir del XML del cuerpo del documento
#' @noRd
empaquetar_docx <- function(cuerpo, archivo, horizontal = FALSE) {
  if (!requireNamespace("zip", quietly = TRUE))
    stop("Para guardar en .docx instala el paquete zip: install.packages(\"zip\").", call. = FALSE)
  doc <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
                "<w:document xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:body>",
                cuerpo, seccion_docx(horizontal), "</w:body></w:document>")
  estilos <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
                    "<w:styles xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
                    "<w:docDefaults><w:rPrDefault><w:rPr><w:rFonts w:ascii=\"Times New Roman\" w:hAnsi=\"Times New Roman\" ",
                    "w:cs=\"Times New Roman\" w:eastAsia=\"Times New Roman\"/><w:sz w:val=\"24\"/><w:szCs w:val=\"24\"/>",
                    "<w:lang w:val=\"", tx("es-MX", "en-US"), "\"/></w:rPr></w:rPrDefault><w:pPrDefault><w:pPr><w:spacing w:after=\"0\" ",
                    "w:line=\"240\" w:lineRule=\"auto\"/></w:pPr></w:pPrDefault></w:docDefaults>",
                    "<w:style w:type=\"paragraph\" w:default=\"1\" w:styleId=\"Normal\"><w:name w:val=\"Normal\"/></w:style>",
                    "<w:style w:type=\"table\" w:default=\"1\" w:styleId=\"TableNormal\"><w:name w:val=\"Normal Table\"/>",
                    "<w:tblPr><w:tblInd w:w=\"0\" w:type=\"dxa\"/><w:tblCellMar><w:top w:w=\"0\" w:type=\"dxa\"/>",
                    "<w:left w:w=\"108\" w:type=\"dxa\"/><w:bottom w:w=\"0\" w:type=\"dxa\"/><w:right w:w=\"108\" w:type=\"dxa\"/>",
                    "</w:tblCellMar></w:tblPr></w:style></w:styles>")
  tipos <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
                  "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">",
                  "<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>",
                  "<Default Extension=\"xml\" ContentType=\"application/xml\"/>",
                  "<Override PartName=\"/word/document.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml\"/>",
                  "<Override PartName=\"/word/styles.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml\"/>",
                  "</Types>")
  rels <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
                 "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">",
                 "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"word/document.xml\"/>",
                 "</Relationships>")
  drels <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
                  "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">",
                  "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles\" Target=\"styles.xml\"/>",
                  "</Relationships>")
  tmp <- tempfile("docx"); dir.create(file.path(tmp, "_rels"), recursive = TRUE)
  dir.create(file.path(tmp, "word", "_rels"), recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  escribe <- function(txt, ruta) { con <- file(file.path(tmp, ruta), open = "wb"); writeBin(charToRaw(enc2utf8(txt)), con); close(con) }
  escribe(tipos, "[Content_Types].xml"); escribe(rels, "_rels/.rels")
  escribe(doc, "word/document.xml"); escribe(estilos, "word/styles.xml"); escribe(drels, "word/_rels/document.xml.rels")
  destino <- normalizePath(archivo, mustWork = FALSE)
  if (file.exists(destino)) file.remove(destino)
  zip::zipr(destino, file.path(tmp, c("[Content_Types].xml", "_rels", "word")), include_directories = FALSE)
  invisible(archivo)
}
