#' Preparar la base: limpieza, criterios e ítems inversos (paso 2)
#'
#' Lee la base tal como llega del formulario (una fila por persona) y la deja
#' lista para analizar: pasa las respuestas a número, deja vacías las que caen
#' fuera del rango de cada escala, unifica sexo y respuestas Sí/No, invierte
#' los ítems inversos, aplica los criterios de elegibilidad en orden (con el
#' motivo de cada exclusión), excluye los envíos repetidos y marca, sin
#' eliminar, las respuestas planas (la misma alternativa en toda una escala de 6
#' o más ítems y 4 o más opciones).
#'
#' @param datos Ruta de un archivo Excel (se lee la primera hoja) o un
#'   `data.frame`, con una fila por persona.
#' @param escalas Prefijo de los ítems de cada instrumento con su rango de
#'   respuesta, por ejemplo `c(BERQ = "1-5", EAP = "1-5", ASS = "1-5")`. Con
#'   `"BERQ"` se usan las columnas `BERQ1`, `BERQ2`, ...
#' @param inversos Ítems que se puntúan al revés, por ejemplo
#'   `c("BIEPS3", "BIEPS7")`. Se recodifican como mínimo + máximo - valor.
#' @param criterios Condiciones para QUEDARSE en el estudio, escritas como en
#'   R, por ejemplo `c("Consentimiento" = "consentimiento",
#'   "Edad de 18 a 30" = "edad >= 18 & edad <= 30")`. Antes se excluye siempre a
#'   quien no respondió completa alguna escala (más del 20 % de sus ítems vacíos)
#'   y los envíos repetidos.
#'
#' @return Un `data.frame` de clase `ps_base` con una fila por participante y
#'   los ítems ya recodificados. Agrega la columna lógica `respuesta_plana`.
#'   Lleva como atributos el flujo de participantes (`flujo`), las escalas y
#'   sus rangos y los avisos de la limpieza.
#' @examples
#' base <- ps_preparar(ps_ejemplo(), escalas = c(BERQ = "1-5", EAP = "1-4", ASS = "1-3"),
#'                     criterios = c("Consentimiento" = "consentimiento",
#'                                   "Edad de 18 a 35" = "edad >= 18 & edad <= 35"))
#' base
#' @export
ps_preparar <- function(datos, escalas, inversos = NULL, criterios = NULL) {
  if (is.character(datos) && length(datos) == 1) {
    if (!file.exists(datos)) stop("No encuentro el archivo: ", datos, call. = FALSE)
    datos <- readxl::read_excel(datos, sheet = 1)
  }
  d <- as.data.frame(datos, stringsAsFactors = FALSE)
  rangos <- lapply(strsplit(escalas, "-"), as.numeric)
  items <- lapply(names(escalas), function(p) {
    v <- grep(paste0("^", p, "[0-9]+$"), names(d), value = TRUE)
    if (!length(v)) stop("No hay columnas que empiecen con \"", p, "\" seguidas de un n\u00famero.", call. = FALSE)
    v[order(as.numeric(sub(p, "", v, fixed = TRUE)))]
  })
  names(items) <- names(rangos) <- names(escalas)
  avisos <- list()
  if (!"id" %in% names(d)) d$id <- sprintf("P%03d", seq_len(nrow(d)))

  # 1. Unificar sexo y respuestas Sí/No
  if ("sexo" %in% names(d)) {
    s <- tolower(trimws(as.character(d$sexo)))
    d$sexo <- ifelse(grepl("^(fem|muj|f$)", s), "Mujer", ifelse(grepl("^(mas|hom|var|h$)", s), "Hombre", NA))
  }
  todos <- unlist(items)
  for (v in setdiff(names(d), todos)) if (.es_si_no(d[[v]])) d[[v]] <- .a_logico(d[[v]])

  # 2. Ítems a número; fuera de rango → vacío
  fuera <- 0
  for (e in names(items)) for (v in items[[e]]) {
    x <- d[[v]]
    if (!is.numeric(x)) x <- suppressWarnings(as.numeric(sub("^\\s*(-?[0-9]+([.][0-9]+)?).*$", "\\1", as.character(x))))
    r <- rangos[[e]]; mal <- !is.na(x) & (x < r[1] | x > r[2] | x != round(x))
    fuera <- fuera + sum(mal); x[mal] <- NA; d[[v]] <- x
  }
  avisos$fuera_rango <- fuera

  # 3. Ítems inversos
  if (length(inversos)) {
    falta <- setdiff(inversos, todos)
    if (length(falta)) stop("\u00cdtems inversos que no est\u00e1n en las escalas: ", paste(falta, collapse = ", "), call. = FALSE)
    for (v in inversos) {
      e <- names(items)[vapply(items, function(i) v %in% i, logical(1))]
      d[[v]] <- sum(rangos[[e]]) - d[[v]]
    }
  }

  # 4. Respuestas planas: la misma alternativa en TODOS los ítems de una escala. Solo en escalas
  #    de 6 o más ítems y 4 o más alternativas: en escalas cortas de 3 opciones es una respuesta normal
  plana <- rep(FALSE, nrow(d))
  for (e in names(items)) if (length(items[[e]]) >= 6 && diff(rangos[[e]]) >= 3) {
    m <- as.matrix(d[, items[[e]]])
    if (length(inversos)) m[, colnames(m) %in% inversos] <- sum(rangos[[e]]) - m[, colnames(m) %in% inversos]
    plana <- plana | apply(m, 1, function(r) { r <- r[!is.na(r)]; length(r) >= 5 && length(unique(r)) == 1 })
  }
  d$respuesta_plana <- plana

  # 5. Criterios en orden: escalas incompletas, envíos repetidos y los del usuario
  incompleta <- apply(sapply(items, function(i) rowMeans(is.na(d[, i, drop = FALSE])) > .20), 1, any)
  repetido <- duplicated(d[, todos])
  d$.incompleta <- incompleta; d$.repetido <- repetido
  crit <- c()
  if (length(criterios)) {
    nom <- names(criterios); if (is.null(nom)) nom <- rep("", length(criterios))
    nom[nom == ""] <- criterios[nom == ""]
    crit <- stats::setNames(criterios, paste("No cumple:", nom))
  }
  crit <- c(crit, "Escala incompleta (m\u00e1s del 20 % de \u00edtems vac\u00edos)" = "!.incompleta",
            "Env\u00edo repetido" = "!.repetido")
  d$motivo <- NA_character_
  for (j in seq_along(crit)) {
    queda <- tryCatch(eval(parse(text = crit[[j]]), d), error = function(e)
      stop("No pude evaluar el criterio \"", crit[[j]], "\": ", conditionMessage(e), call. = FALSE))
    queda <- rep_len(as.logical(queda), nrow(d)); queda[is.na(queda)] <- FALSE
    d$motivo[is.na(d$motivo) & !queda] <- names(crit)[j]
  }
  final <- d[is.na(d$motivo), setdiff(names(d), c("motivo", ".incompleta", ".repetido")), drop = FALSE]
  exc <- table(factor(d$motivo, levels = names(crit)))
  flujo <- data.frame(Etapa = c("Respuestas recibidas", names(exc), "Participantes analizados"),
                      n = c(nrow(d), as.integer(exc), nrow(final)), check.names = FALSE)
  avisos$planas <- sum(final$respuesta_plana)
  avisos$vacios <- sum(is.na(final[, todos]))
  primero <- c("id", setdiff(names(final), c("id", todos, "respuesta_plana")))
  final <- final[, c(primero, "respuesta_plana", todos)]
  rownames(final) <- NULL
  structure(final, class = c("ps_base", "data.frame"), flujo = flujo, escalas = items,
            rangos = rangos, inversos = inversos, avisos = avisos)
}

#' @export
print.ps_base <- function(x, ...) {
  print(.flujo_resultado(x))
  cat(sprintf("\nLa base tiene %d filas y %d columnas. Para verla: as.data.frame(base)\n", nrow(x), ncol(x)))
  invisible(x)
}

# El flujo de participantes con su lectura (lo usan print y la presentacion del curso)
.flujo_resultado <- function(x) {
  f <- attr(x, "flujo"); a <- attr(x, "avisos"); n <- stats::setNames(f$n, f$Etapa)
  e <- attr(x, "escalas")
  notas <- list(
    .nota(nrow(f), "n", sprintf("De %d respuestas quedaron %d participantes para el an\u00e1lisis. Cada exclusi\u00f3n tiene su motivo: as\u00ed se informa en Participantes.",
                                n["Respuestas recibidas"], n["Participantes analizados"])),
    .nota(1, "n", sprintf("Escalas: %s.%s", paste(sprintf("%s (%d \u00edtems)", names(e), lengths(e)), collapse = ", "),
                          if (length(attr(x, "inversos"))) sprintf(" Se invirtieron %d \u00edtem(s).", length(attr(x, "inversos"))) else "")))
  if (a$fuera_rango > 0 || a$vacios > 0) notas[[length(notas) + 1]] <- .nota(1, "n", sprintf(
    "Limpieza: %d respuesta(s) fuera de rango quedaron vac\u00edas; quedan %d celda(s) vac\u00eda(s) en los \u00edtems.", a$fuera_rango, a$vacios))
  if (a$planas > 0) notas[[length(notas) + 1]] <- .nota(1, "n", sprintf(
    "%d participante(s) marcaron la misma alternativa en toda una escala (columna respuesta_plana). Se MARCAN, no se eliminan: el an\u00e1lisis sin ellos va como sensibilidad.", a$planas))
  .resultado(f, "Base preparada (ps_preparar)", notas)
}

#' Distribución de respuestas por ítem
#'
#' Los ítems tipo Likert son ordinales: se describen con el porcentaje de
#' cada alternativa, nunca con media y desviación estándar.
#'
#' @param base La base que devuelve [ps_preparar()].
#' @param escala Prefijo de la escala, por ejemplo `"ASS"`.
#' @return Un resultado `ps_resultado` con el porcentaje de cada alternativa
#'   por ítem y su lectura.
#' @examples
#' base <- ps_preparar(ps_ejemplo(), escalas = c(BERQ = "1-5", EAP = "1-4", ASS = "1-3"))
#' ps_items(base, "ASS")
#' @export
ps_items <- function(base, escala) {
  .comprobar_base(base)
  it <- attr(base, "escalas")[[escala]]; r <- attr(base, "rangos")[[escala]]
  if (is.null(it)) stop("La escala \"", escala, "\" no est\u00e1 en la base preparada.", call. = FALSE)
  cats <- r[1]:r[2]
  t <- t(vapply(it, function(v) {
    x <- base[[v]]; x <- x[!is.na(x)]
    100 * as.numeric(table(factor(x, levels = cats))) / length(x)
  }, numeric(length(cats))))
  tabla <- data.frame("\u00cdtem" = it, matrix(sprintf("%.1f", t), nrow = length(it)), check.names = FALSE)
  names(tabla)[-1] <- paste0(cats, " (%)")
  extremo <- t[, ncol(t)] + t[, 1]
  i_max <- which.max(t[, ncol(t)]); i_min <- which.max(t[, 1])
  notas <- list(
    .nota(i_max, ncol(tabla), sprintf("%s es el \u00edtem con m\u00e1s respuestas en la alternativa m\u00e1s alta (%.1f %%).", it[i_max], t[i_max, ncol(t)])),
    .nota(i_min, 2, sprintf("Los \u00edtems son ordinales: se describen con porcentajes, no con medias. La alternativa m\u00e1s baja va de %.1f %% a %.1f %% y la m\u00e1s alta de %.1f %% a %.1f %%; si una alternativa extrema concentra casi todas las respuestas, el \u00edtem discrimina poco (efecto techo o suelo).",
                            min(t[, 1]), max(t[, 1]), min(t[, ncol(t)]), max(t[, ncol(t)]))))
  .resultado(tabla, sprintf("Respuestas por \u00edtem: %s (ps_items)", escala), notas)
}
