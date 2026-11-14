# Presentación de las tablas: en la consola (texto alineado) y en Quarto o
# R Markdown (tabla real + "Cómo se lee" como lista numerada)

# Una columna se alinea a la derecha si todas sus celdas empiezan como número
# ("-3.31", ".905", "< .001", "[-6.45, -0.87]", "9 (75.0 %)")
.es_numerica <- function(v) {
  v <- v[!is.na(v) & nzchar(v)]
  # una lista numerada ("1. ADA") es texto
  length(v) > 0 && !all(grepl("^\\s*[0-9]+\\.\\s+\\D", v)) && all(grepl("^\\s*(<\\s*)?[-\u2212]?\\[?[-\u2212]?[.0-9]", v))
}

# Parte cada celda en líneas de hasta `ancho` caracteres
.partir <- function(v, ancho) lapply(v, function(s) {
  if (is.na(s) || !nzchar(s)) return("")
  if (nchar(s) <= ancho) return(s)
  strwrap(s, width = ancho)
})

# Tabla como líneas de texto: encabezado, una regla por columna y las filas.
# Si no cabe en el ancho de la consola, parte el texto de las columnas largas.
.tabla_texto <- function(tabla, ancho_total = getOption("width", 80), sangria = "  ") {
  t <- as.data.frame(lapply(tabla, function(v) ifelse(is.na(v), "", as.character(v))),
                     stringsAsFactors = FALSE, check.names = FALSE)
  nombres <- names(t)
  derecha <- vapply(t, .es_numerica, logical(1))
  # cada columna de texto tiene su propio máximo; si no cabe, se angosta la más ancha
  maximo <- rep(60, ncol(t))
  repeat {
    celdas <- mapply(.partir, t, maximo, SIMPLIFY = FALSE)
    anchos <- mapply(function(cab, col) max(nchar(cab), vapply(col, function(l) max(nchar(l)), numeric(1))),
                     nombres, celdas)
    total <- sum(anchos) + 2 * (length(anchos) - 1) + nchar(sangria)
    candidatas <- which(!derecha & anchos > pmax(14, nchar(nombres)))
    if (total <= ancho_total || !length(candidatas)) break
    j <- candidatas[which.max(anchos[candidatas])]
    maximo[j] <- anchos[j] - 2
  }
  ajustar <- function(s, j) formatC(s, width = anchos[j], flag = if (derecha[j]) "" else "-")
  linea <- function(valores) paste0(sangria, sub("\\s+$", "", paste(mapply(ajustar, valores, seq_along(valores)),
                                                                     collapse = "  ")))
  out <- c(linea(nombres), linea(vapply(anchos, function(a) strrep("-", a), "")))
  # si alguna fila ocupa varias líneas, una línea en blanco separa todas las filas
  multilinea <- any(vapply(celdas, function(col) any(lengths(col) > 1), logical(1)))
  for (i in seq_len(nrow(t))) {
    partes <- lapply(celdas, `[[`, i)
    alto <- max(lengths(partes))
    for (k in seq_len(alto)) out <- c(out, linea(vapply(partes, function(p) if (k <= length(p)) p[k] else "", "")))
    if (multilinea && i < nrow(t)) out <- c(out, "")
  }
  out
}

.imprimir_tabla <- function(tabla) cat(.tabla_texto(tabla), sep = "\n")

# Una nota puede indicar su columna por nombre o por número
.col_nota <- function(cl, nombres)
  if (is.numeric(cl) && length(cl) == 1 && cl >= 1 && cl <= length(nombres)) nombres[cl] else cl

# ---- Quarto / R Markdown ---------------------------------------------------

.md_celda <- function(s) {
  s <- ifelse(is.na(s), "", as.character(s))
  s <- gsub("|", "\\|", s, fixed = TRUE)
  s <- gsub("[", "\\[", s, fixed = TRUE)
  s <- gsub("]", "\\]", s, fixed = TRUE)
  s <- gsub("*", "\\*", s, fixed = TRUE)
  s <- gsub("\n", " ", s, fixed = TRUE)
  # la sangría inicial (niveles del flujo) se conserva con espacios duros
  vapply(s, function(z) {
    k <- attr(regexpr("^ +", z), "match.length")
    if (k > 0) paste0(strrep("\u00a0", k), substring(z, k + 1)) else z
  }, "", USE.NAMES = FALSE)
}

# Tabla en markdown; cada celda que comenta una nota lleva su número (superíndice)
.md_tabla <- function(tabla, notas = list()) {
  t <- as.data.frame(lapply(tabla, .md_celda), stringsAsFactors = FALSE, check.names = FALSE)
  # varias notas en la misma celda: un solo superíndice "2,3"
  marcas <- list()
  for (k in seq_along(notas)) {
    f <- notas[[k]]$fila; cl <- .col_nota(notas[[k]]$columna, names(t))
    if (!is.null(f) && !is.null(cl) && cl %in% names(t) && f >= 1 && f <= nrow(t)) {
      clave <- paste(f, cl, sep = "\r")
      marcas[[clave]] <- c(marcas[[clave]], k)
    }
  }
  for (clave in names(marcas)) {
    p <- strsplit(clave, "\r", fixed = TRUE)[[1]]
    f <- as.integer(p[1]); cl <- p[2]
    t[f, cl] <- paste0(t[f, cl], "^", paste(marcas[[clave]], collapse = ","), "^")
  }
  derecha <- vapply(tabla, function(v) .es_numerica(as.character(v)), logical(1))
  # en las columnas numéricas un IC o un "n (%)" no se parte en dos líneas
  for (j in which(derecha)) t[[j]] <- gsub(" ", "\u00a0", t[[j]], fixed = TRUE)
  fila <- function(v) paste0("| ", paste(v, collapse = " | "), " |")
  # el largo de los guiones fija el ancho relativo de cada columna (pandoc)
  largo <- pmax(4, mapply(function(cab, v) max(nchar(cab), nchar(v)), names(tabla),
                          lapply(tabla, function(v) ifelse(is.na(v), "", as.character(v)))))
  regla <- ifelse(derecha, paste0(strrep("-", largo - 1), ":"), paste0(":", strrep("-", largo - 1)))
  c(fila(.md_celda(names(t))), fila(regla), apply(t, 1, fila))
}

.md_notas <- function(notas) {
  if (!length(notas)) return(character())
  c("", "::: {.callout-note title=\"C\u00f3mo se lee\" appearance=\"simple\"}",
    sprintf("%d. %s", seq_along(notas), vapply(notas, `[[`, "", "texto")), ":::")
}

.md_resultado <- function(x, nivel = "") {
  # en HTML la tabla lleva las ayudas al pasar el mouse; en Word o PDF, markdown
  tabla <- if (.es_html()) c("```{=html}", .html_tabla(x$tabla, x$notas), "```") else .md_tabla(x$tabla, x$notas)
  partes <- c(sprintf("%s**%s**", nivel, x$titulo), "", tabla, .md_notas(x$notas), "")
  for (k in setdiff(names(x), c("tabla", "titulo", "notas")))
    if (inherits(x[[k]], "ps_resultado")) partes <- c(partes, "", .md_resultado(x[[k]]))
  partes
}

# Con options(PredSEM.tablas = "texto") se mantiene la salida de la consola
.usar_md <- function() !identical(getOption("PredSEM.tablas"), "texto")

.es_html <- function() isTRUE(tryCatch(knitr::is_html_output(), error = function(e) FALSE))

.asis <- function(lineas) knitr::asis_output(paste(c("", lineas, ""), collapse = "\n"))

#' Tablas de PredSEM en Quarto y R Markdown
#'
#' Dentro de un documento Quarto o R Markdown, los resultados de PredSEM se
#' muestran como tablas con formato y la sección "Cómo se lee" como lista
#' numerada; cada número aparece también en la celda que comenta. En HTML,
#' además, al pasar el mouse sobre un encabezado o una fila conocida aparece
#' qué significa, y sobre una celda resaltada, su lectura. Para ver la
#' salida de texto de la consola, usa `options(PredSEM.tablas = "texto")`.
#'
#' @param x Un resultado de PredSEM.
#' @param ... No se usa.
#' @return Texto markdown que knitr inserta en el documento.
#' @name knit_print_ps
#' @keywords internal
NULL

#' @rdname knit_print_ps
#' @exportS3Method knitr::knit_print
knit_print.ps_resultado <- function(x, ...) {
  if (!.usar_md()) return(knitr::normal_print(x))
  .asis(.md_resultado(x))
}

#' @rdname knit_print_ps
#' @exportS3Method knitr::knit_print
knit_print.ps_base <- function(x, ...) {
  if (!.usar_md()) return(knitr::normal_print(x))
  r <- .flujo_resultado(x)
  .asis(c(.md_resultado(r), sprintf("La base tiene %d filas y %d columnas. Para verla: `as.data.frame(base)`",
                                    nrow(x), ncol(x))))
}

#' @rdname knit_print_ps
#' @exportS3Method knitr::knit_print
knit_print.ps_modelo <- function(x, ...) {
  if (!.usar_md()) return(knitr::normal_print(x))
  partes <- c(sprintf("**Modelo de ecuaciones estructurales (ps_modelo):** %s", .resumen_modelo(x)), "")
  for (k in c("ajuste", "discriminante", "estructural", "indirectos"))
    if (!is.null(x[[k]])) partes <- c(partes, .md_resultado(x[[k]]))
  .asis(partes)
}
