# Funciones internas comunes (no exportadas)

.f2 <- function(x) ifelse(is.na(x), "", formatC(as.numeric(x), format = "f", digits = 2))
.f3 <- function(x) ifelse(is.na(x), "", sub("^(-?)0[.]", "\\1.", formatC(as.numeric(x), format = "f", digits = 3)))
.fp <- function(p) ifelse(p < .001, "< .001", sub("^0", "", formatC(p, format = "f", digits = 3)))
.fpe <- function(p) ifelse(p < .001, "< .001", paste("=", .fp(p)))
.ic <- function(a, b) sprintf("[%s, %s]", .f2(a), .f2(b))
.ic3 <- function(a, b) sprintf("[%s, %s]", .f3(a), .f3(b))
.sin0 <- function(x) sub("^0", "", .f2(x))
.incluye0 <- function(li, ls) li * ls <= 0
.lista <- function(x) {
  if (length(x) == 1) return(x)
  conj <- if (grepl("^(i|hi)", tolower(x[length(x)]))) " e " else " y "
  paste0(paste(x[-length(x)], collapse = ", "), conj, x[length(x)])
}
.magnitud_beta <- function(b) {
  b <- abs(b)
  if (b >= .50) "grande" else if (b >= .30) "moderado" else if (b >= .10) "peque\u00f1o" else "trivial"
}

.nota <- function(fila, columna, texto) list(fila = fila, columna = columna, texto = texto)

.resultado <- function(tabla, titulo, notas = list(), clase = NULL, ...) {
  rownames(tabla) <- NULL
  structure(list(tabla = tabla, titulo = titulo, notas = notas, ...), class = c(clase, "ps_resultado"))
}

.imprimir_notas <- function(notas) {
  if (!length(notas)) return(invisible())
  cat("\nC\u00f3mo se lee:\n")
  for (k in seq_along(notas)) {
    txt <- strwrap(notas[[k]]$texto, width = 76, exdent = 5)
    txt[1] <- sprintf("  %d. %s", k, txt[1])
    if (length(txt) > 1) txt[-1] <- paste0(" ", txt[-1])
    cat(txt, sep = "\n")
  }
}

#' Imprimir un resultado de PredSEM
#'
#' Muestra el título, la tabla y la sección "Cómo se lee" con la
#' interpretación de los valores clave.
#'
#' @param x Un resultado de cualquier función `ps_`.
#' @param ... No se usa.
#' @return `x`, de forma invisible.
#' @export
print.ps_resultado <- function(x, ...) {
  cat("\n", x$titulo, "\n", strrep("-", nchar(x$titulo)), "\n", sep = "")
  print(x$tabla, row.names = FALSE, right = FALSE)
  .imprimir_notas(x$notas)
  for (k in setdiff(names(x), c("tabla", "titulo", "notas")))
    if (inherits(x[[k]], "ps_resultado")) print(x[[k]])
  invisible(x)
}

.con_semilla <- function(semilla, expr) {
  if (is.null(semilla)) return(expr)
  hay <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (hay) previa <- get(".Random.seed", envir = globalenv())
  on.exit(if (hay) assign(".Random.seed", previa, envir = globalenv())
          else rm(".Random.seed", envir = globalenv()))
  set.seed(semilla)
  expr
}

.a_logico <- function(x) {
  if (is.logical(x)) return(x)
  v <- chartr("\u00ed", "i", tolower(trimws(as.character(x))))
  ifelse(is.na(v) | v == "", NA, v %in% c("si", "s", "yes", "y", "1", "true", "verdadero"))
}
.es_si_no <- function(x) {
  if (!is.character(x)) return(FALSE)
  v <- unique(chartr("\u00ed", "i", tolower(trimws(x[!is.na(x) & x != ""]))))
  length(v) > 0 && all(v %in% c("si", "s", "no", "n"))
}

# ---------------------------------------------------------------------------
# Definición de una variable latente con texto corto:
#   "ASS"                      todos los ítems ASS1, ASS2, ...
#   "BERQ1:BERQ8"              un rango de ítems
#   "EAP1, EAP3, EAP5"         ítems sueltos
#   "BERQ1:BERQ4 | BERQ5:BERQ8"  parcelas (cada tramo es la media de sus ítems)
# Devuelve una lista: items (todos), parcelas (lista de tramos o NULL)
# ---------------------------------------------------------------------------
.expandir <- function(txt, columnas) {
  piezas <- trimws(strsplit(txt, ",")[[1]])
  unlist(lapply(piezas, function(p) {
    if (grepl(":", p)) {
      ab <- trimws(strsplit(p, ":")[[1]])
      pre <- sub("[0-9]+$", "", ab[1])
      ini <- as.numeric(sub("^.*?([0-9]+)$", "\\1", ab[1])); fin <- as.numeric(sub("^.*?([0-9]+)$", "\\1", ab[2]))
      intersect(paste0(pre, ini:fin), columnas)   # numeraci\u00f3n salteada: solo los \u00edtems que existen
    } else if (p %in% columnas) p
    else {
      v <- grep(paste0("^", p, "[0-9]+$"), columnas, value = TRUE)
      v[order(as.numeric(sub(p, "", v, fixed = TRUE)))]
    }
  }))
}
.definir <- function(def, columnas) {
  tramos <- trimws(strsplit(def, "|", fixed = TRUE)[[1]])
  par <- lapply(tramos, .expandir, columnas = columnas)
  items <- unlist(par)
  falta <- setdiff(items, columnas)
  if (!length(items)) stop("La definici\u00f3n \"", def, "\" no corresponde a ning\u00fan \u00edtem de la base.", call. = FALSE)
  if (length(falta)) stop("No est\u00e1n en la base: ", paste(falta, collapse = ", "), call. = FALSE)
  list(items = items, parcelas = if (length(par) > 1) par else NULL)
}

# Indicadores de cada latente (ítems o parcelas) y datos con las parcelas calculadas
.indicadores <- function(base, defs) {
  d <- as.data.frame(base); cols <- names(d)
  ind <- list(); ordinales <- character()
  for (f in names(defs)) {
    df <- .definir(defs[[f]], cols)
    if (is.null(df$parcelas)) {
      ind[[f]] <- df$items; ordinales <- c(ordinales, df$items)
    } else {
      nom <- paste0(f, "_p", seq_along(df$parcelas))
      for (k in seq_along(nom)) d[[nom[k]]] <- rowMeans(d[, df$parcelas[[k]], drop = FALSE], na.rm = TRUE)
      ind[[f]] <- nom
    }
  }
  list(datos = d, ind = ind, ordinales = unique(ordinales))
}

.sintaxis_medida <- function(ind) paste(sprintf("%s =~ %s", names(ind), vapply(ind, paste, "", collapse = " + ")), collapse = "\n")

.comprobar_base <- function(base) if (!inherits(base, "ps_base"))
  stop("Primero prepara la base con ps_preparar().", call. = FALSE)
.comprobar_modelo <- function(m) if (!inherits(m, "ps_modelo"))
  stop("Usa el resultado de ps_modelo().", call. = FALSE)

# Índices de ajuste (robustos si existen)
.ajuste <- function(fit) {
  ix <- lavaan::fitMeasures(fit)
  pick <- function(a, b) if (!is.na(ix[a])) unname(ix[a]) else unname(ix[b])
  c(chi2 = pick("chisq.scaled", "chisq"), gl = unname(ix["df"]), p = pick("pvalue.scaled", "pvalue"),
    cfi = pick("cfi.robust", "cfi.scaled"), tli = pick("tli.robust", "tli.scaled"),
    rmsea = pick("rmsea.robust", "rmsea.scaled"), rmsea_li = pick("rmsea.ci.lower.robust", "rmsea.ci.lower.scaled"),
    rmsea_ls = pick("rmsea.ci.upper.robust", "rmsea.ci.upper.scaled"), srmr = unname(ix["srmr"]))
}
.ajuste_ok <- function(a) !is.na(a["cfi"]) && a["cfi"] >= .95 && a["tli"] >= .95 && a["rmsea"] <= .08 && a["srmr"] <= .08
.ajuste_aceptable <- function(a) !is.na(a["cfi"]) && a["cfi"] >= .90 && a["tli"] >= .90 && a["rmsea"] <= .10 && a["srmr"] <= .10
.lee_ajuste <- function(a) if (.ajuste_ok(a)) "ajuste BUENO (CFI y TLI \u2265 .95, RMSEA y SRMR \u2264 .08)" else if (.ajuste_aceptable(a))
  "ajuste ACEPTABLE (CFI y TLI \u2265 .90); conviene revisar el diagn\u00f3stico local" else
  "ajuste INSUFICIENTE: antes de seguir, revisa cargas bajas o correlaciones residuales con sustento te\u00f3rico"

utils::globalVariables(c("x", "y", "xend", "yend", "etiqueta", "tipo", "lab", "h", "valor", "item",
                         "categoria", "pct", "B", "li", "ls", "xs", "ys", "xe", "ye", "sig"))
