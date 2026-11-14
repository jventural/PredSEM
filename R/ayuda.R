# Ayuda al pasar el mouse: glosario sencillo de columnas y filas, y la tabla
# HTML que lo usa (solo cuando el documento sale en HTML)

# Qué significa cada columna, en lenguaje sencillo
.glosario <- c(
  "n" = "N\u00famero de personas que entran en ese c\u00e1lculo.",
  "Etapa" = "Cada paso del recorrido de los participantes: cu\u00e1ntas respuestas llegaron, cu\u00e1ntas salieron y por qu\u00e9, y cu\u00e1ntas quedaron.",
  "\u03b2 m\u00ednimo" = "El efecto m\u00e1s peque\u00f1o que se quiere poder detectar, en la escala estandarizada: .10 peque\u00f1o, .30 moderado, .50 grande.",
  "Carga" = "Qu\u00e9 tan bien se supone que cada \u00edtem representa a su variable latente (de 0 a 1). Con cargas altas hacen falta menos personas.",
  "Potencia" = "La probabilidad deseada de detectar el efecto si de verdad existe. Lo usual es .80: 8 de cada 10 estudios lo encontrar\u00edan.",
  "N necesario" = "Cu\u00e1ntas personas hacen falta para alcanzar esa potencia.",
  "n que tienes" = "Cu\u00e1ntas personas tiene la base despu\u00e9s de la limpieza.",
  "Potencia lograda" = "La potencia que se alcanza con la muestra que ya tienes. Desde .80 la muestra es suficiente.",
  "\u00cdtem" = "Cada pregunta del instrumento.",
  "Modelo" = "Qu\u00e9 modelo se estim\u00f3 en esa fila.",
  "Estimador" = "El m\u00e9todo de c\u00e1lculo. WLSMV es el adecuado para \u00edtems tipo Likert, que son ordinales (opciones ordenadas, no n\u00fameros continuos).",
  "\u03c7\u00b2 (gl)" = "Chi cuadrado y sus grados de libertad: cu\u00e1nto se aleja el modelo de los datos. Con muestras grandes casi siempre sale significativo, por eso se miran tambi\u00e9n CFI, TLI, RMSEA y SRMR.",
  "CFI" = "\u00cdndice de ajuste comparativo, de 0 a 1. Desde .95 el ajuste es bueno; desde .90, aceptable.",
  "TLI" = "\u00cdndice de Tucker-Lewis: como el CFI, pero castiga los modelos con muchos par\u00e1metros. Desde .95 es bueno; desde .90, aceptable.",
  "RMSEA [IC 90 %]" = "Error de aproximaci\u00f3n: cu\u00e1nto se equivoca el modelo por grado de libertad. Hasta .08 es aceptable; hasta .05, bueno. Entre corchetes, su intervalo.",
  "RMSEA" = "Error de aproximaci\u00f3n: cu\u00e1nto se equivoca el modelo por grado de libertad. Hasta .08 es aceptable; hasta .05, bueno.",
  "SRMR" = "Diferencia promedio entre las correlaciones observadas y las que reproduce el modelo. Hasta .08 es aceptable.",
  "Factor" = "La variable latente (el constructo) que miden esos \u00edtems.",
  "\u00cdtems" = "Cu\u00e1ntos \u00edtems miden ese factor.",
  "Cargas (m\u00edn.\u2013m\u00e1x.)" = "La carga m\u00e1s baja y la m\u00e1s alta del factor. Desde .50 el \u00edtem representa bien a su factor.",
  "\u03c9" = "Omega: fiabilidad del puntaje cuando cada \u00edtem puede medir con distinta fuerza (modelo congen\u00e9rico). Va de 0 a 1; desde .80 es adecuada para investigaci\u00f3n.",
  "\u03b1" = "Alfa: fiabilidad del puntaje suponiendo que todos los \u00edtems miden con la misma fuerza (modelo tau-equivalente). Va de 0 a 1.",
  "Variable" = "El puntaje de cada variable del modelo (promedio de sus \u00edtems). El n\u00famero permite ubicarla en las columnas de correlaciones.",
  "M" = "Media del puntaje: el promedio de sus \u00edtems, en la escala de respuesta. Se calcula para el PUNTAJE, nunca para un \u00edtem suelto.",
  "DE" = "Desviaci\u00f3n est\u00e1ndar: cu\u00e1nto se alejan las personas, en promedio, de la media.",
  "g1" = "Asimetr\u00eda. 0 = sim\u00e9trica; entre -1 y 1, cercana a la normal. Negativa: m\u00e1s personas con puntajes altos.",
  "g2" = "Curtosis: qu\u00e9 tan puntiaguda o aplanada es la distribuci\u00f3n. Entre -1 y 1, cercana a la normal.",
  "Par" = "Las dos variables latentes que se comparan.",
  "r latente" = "Correlaci\u00f3n entre las dos latentes, ya libre del error de medida. Si se acerca a 1, miden casi lo mismo.",
  "IC 95 %" = "Intervalo de confianza al 95 %: el rango de valores cre\u00edbles para el valor verdadero. Si NO incluye el 0, el resultado no se confunde con cero. Cuanto m\u00e1s ancho, menos precisi\u00f3n.",
  "HTMT" = "Raz\u00f3n heterorrasgo-monorrasgo: otra forma de ver si dos constructos se distinguen. Por debajo de .85, son distintos.",
  "Ruta" = "Cada hip\u00f3tesis del modelo. La flecha apunta al criterio: \"SAT \u2190 AE\" es el efecto de AE sobre SAT.",
  "\u03b2" = "Coeficiente estandarizado: cu\u00e1ntas desviaciones est\u00e1ndar cambia el criterio por cada desviaci\u00f3n est\u00e1ndar del predictor, con lo dem\u00e1s constante. .10 peque\u00f1o, .30 moderado, .50 grande.",
  "B" = "El mismo efecto sin estandarizar, en las unidades de las variables.",
  "EE" = "Error est\u00e1ndar: cu\u00e1nta incertidumbre tiene la estimaci\u00f3n. M\u00e1s peque\u00f1o = m\u00e1s preciso.",
  "p" = "Probabilidad de obtener un resultado as\u00ed (o m\u00e1s extremo) si en realidad no hubiera relaci\u00f3n. Menor que .05 suele leerse como poco compatible con el azar. No dice cu\u00e1n grande es el efecto.",
  "R\u00b2 del criterio" = "Proporci\u00f3n de la variaci\u00f3n del criterio que explican sus predictores, de 0 a 1. Por ejemplo, .25 = el 25 %.",
  "Efecto" = "El camino indirecto: del predictor al mediador y del mediador al criterio.",
  "\u03b2 indirecto" = "La parte del efecto que pasa A TRAV\u00c9S del mediador. Se obtiene multiplicando los dos \u03b2 del camino.",
  "\u03b2 directo" = "La parte del efecto que va directo del predictor al criterio, sin pasar por el mediador.",
  "Ecuaciones" = "Las hip\u00f3tesis de cada modelo, escritas como en R: \"Y ~ X\" se lee \"X predice Y\".",
  "\u0394\u03c7\u00b2 (gl), p" = "Diferencia de chi cuadrado entre este modelo y el anterior. Si p < .05, el modelo m\u00e1s restringido ajusta peor.",
  "\u0394CFI" = "Cu\u00e1nto cambia el CFI frente al modelo anterior. Una ca\u00edda mayor que .010 indica que la restricci\u00f3n empeora el ajuste.",
  "\u0394RMSEA" = "Cu\u00e1nto cambia el RMSEA frente al modelo anterior. Un aumento mayor que .015 indica que la restricci\u00f3n empeora el ajuste.",
  "Grupo" = "Cada grupo que se compara (por ejemplo, mujeres y hombres).",
  "Principal" = "El \u03b2 del modelo principal, como referencia para comparar.",
  "MLR (\u00edtems continuos)" = "El mismo modelo, tratando los \u00edtems como n\u00fameros continuos (estimador MLR).",
  "Sin respuestas planas" = "El mismo modelo, sin las personas que marcaron la misma alternativa en toda una escala.",
  "Puntajes observados (HC3)" = "Regresi\u00f3n con los puntajes promedio en lugar de latentes, con errores robustos. Suele dar \u03b2 menores: el error de medida aten\u00faa las relaciones.")

# Qué significa cada fila conocida (se busca por cómo empieza la etiqueta)
.glosario_filas <- c(
  "Respuestas recibidas" = "Todas las respuestas que llegaron del formulario.",
  "No cumple" = "Respuestas que salieron porque no cumpl\u00edan este criterio de elegibilidad. Los criterios se aplican en orden.",
  "Escala incompleta" = "Personas que dejaron vac\u00eda m\u00e1s del 20 % de una escala: no se puede calcular su puntaje.",
  "Env\u00edo repetido" = "La misma persona envi\u00f3 el formulario m\u00e1s de una vez: se conserva un solo env\u00edo.",
  "Participantes analizados" = "Las personas que quedan para el an\u00e1lisis.",
  "Unidimensional" = "Todos los \u00edtems miden un solo factor.",
  "Medida conjunta" = "Todas las latentes juntas, solo correlacionadas, sin hip\u00f3tesis: comprueba que la medici\u00f3n funciona.",
  "Estructural" = "El modelo con las flechas de las hip\u00f3tesis.",
  "Propuesto" = "El modelo de tus hip\u00f3tesis.",
  "Rival" = "Otra explicaci\u00f3n razonable de los mismos datos. Si ajusta peor, tu modelo sale fortalecido.",
  "Configural" = "La misma estructura en todos los grupos, con todo libre: es el punto de partida.",
  "Umbrales" = "Adem\u00e1s, los grupos usan igual las alternativas de respuesta.",
  "Cargas" = "Adem\u00e1s, cada \u00edtem pesa lo mismo en todos los grupos. Si se cumple, los puntajes se pueden comparar.",
  "\u03b2 libres" = "El modelo con cada \u03b2 estimado por separado en cada grupo.",
  "\u03b2 iguales" = "El modelo con los \u03b2 iguales en todos los grupos. Si no empeora el ajuste, las relaciones son las mismas.")

.ayuda_columna <- function(nombre) {
  a <- .glosario[nombre]
  if (!is.na(a)) return(unname(a))
  if (grepl("^[0-9]+$", nombre))
    return(sprintf("Correlaci\u00f3n de cada variable con la variable n\u00famero %s de la lista.", nombre))
  if (grepl("^[0-9]+ \\(%\\)$", nombre))
    return(sprintf("Porcentaje de personas que eligieron la alternativa %s. Los \u00edtems son ordinales: se describen con porcentajes, no con medias.",
                   sub(" .*", "", nombre)))
  if (startsWith(nombre, "\u03b2 ")) return(sprintf("El \u03b2 estimado solo en el grupo %s.", substring(nombre, 3)))
  NULL
}
.ayuda_fila <- function(etiqueta) {
  e <- trimws(etiqueta)
  i <- which(startsWith(e, names(.glosario_filas)))
  if (length(i)) return(unname(.glosario_filas[i[1]]))
  if (grepl("factores relacionados$", e)) return("Cada grupo de \u00edtems mide su propio factor, y los factores se correlacionan entre s\u00ed.")
  p <- trimws(strsplit(e, "\u2190|\u2192|\u2194")[[1]])
  if (grepl("\u2192", e) && length(p) == 3)
    return(sprintf("%s influye en %s, que a su vez influye en %s: %s es el mediador.", p[1], p[2], p[3], p[2]))
  if (grepl("\u2190", e) && length(p) == 2) return(sprintf("Efecto de %s sobre %s.", p[2], p[1]))
  if (grepl("\u2194", e) && length(p) == 2) return(sprintf("Relaci\u00f3n entre %s y %s.", p[1], p[2]))
  NULL
}

.html_esc <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  gsub("\"", "&quot;", s, fixed = TRUE)
}

# Celdas (fila, columna) que comenta cada nota: lista con fila, columna y números
.marcas <- function(tabla, notas) {
  m <- list()
  for (k in seq_along(notas)) {
    f <- notas[[k]]$fila; cl <- .col_nota(notas[[k]]$columna, names(tabla))
    if (!is.null(f) && !is.null(cl) && cl %in% names(tabla) && f >= 1 && f <= nrow(tabla)) {
      clave <- paste(f, cl, sep = "\r")
      if (is.null(m[[clave]])) m[[clave]] <- list(fila = f, columna = cl, k = integer())
      m[[clave]]$k <- c(m[[clave]]$k, k)
    }
  }
  m
}

# Estilo de las ayudas: va una sola vez por documento
.estado <- new.env(parent = emptyenv())
.css_ayuda <- function() {
  doc <- tryCatch(knitr::current_input(), error = function(e) "")
  if (identical(.estado$doc, doc)) return(character())
  .estado$doc <- doc
  paste0("<style>",
    ".ps-tabla{overflow:visible;margin:.4em 0 .2em}",
    "table.ps{border-collapse:collapse;width:100%;font-size:.92em}",
    "table.ps th{border-top:2px solid #222;border-bottom:1px solid #222;padding:.35em .7em;font-weight:600;white-space:nowrap}",
    "table.ps td{padding:.35em .7em;border-bottom:1px solid #eee;font-variant-numeric:tabular-nums}",
    "table.ps tr:last-child td{border-bottom:2px solid #222}",
    "table.ps .der{text-align:right}",
    ".ps-tip{position:relative;cursor:help}",
    "th.ps-tip,td.ps-tip.ps-fila{text-decoration:underline dotted #999;text-underline-offset:3px}",
    "td.ps-marca{background:#fdecec;border-radius:4px}",
    "td.ps-marca sup{color:#BB1715;font-weight:700}",
    ".tip-fijo{position:fixed;z-index:10000;max-width:340px;white-space:normal;text-align:left;font-weight:400;",
    "font-size:.85rem;line-height:1.4;background:#263238;color:#fff;padding:.55em .75em;border-radius:6px;",
    "box-shadow:0 4px 14px rgba(0,0,0,.25);pointer-events:none}",
    "</style>", .js_ayuda)
}

# El globo se dibuja sobre la pagina (position: fixed) para que el contenedor de
# la tabla no lo recorte; se acomoda arriba o abajo y dentro de la ventana
.js_ayuda <- paste0("<script>(function(){if(window.tipFijo)return;window.tipFijo=1;var d=null,a=null;",
  "function quitar(){if(d)d.remove();d=null;a=null;}",
  "function ver(e){var t=e.target.closest?e.target.closest('[data-tip]'):null;if(!t||t===a)return;quitar();a=t;",
  "d=document.createElement('div');d.className='tip-fijo';d.textContent=t.getAttribute('data-tip');document.body.appendChild(d);",
  "var r=t.getBoundingClientRect(),w=d.offsetWidth,h=d.offsetHeight,x=t.classList.contains('der')?r.right-w:r.left;",
  "x=Math.min(Math.max(8,x),window.innerWidth-w-8);var y=r.top-h-8;if(y<8)y=r.bottom+8;",
  "d.style.left=x+'px';d.style.top=y+'px';}",
  "document.addEventListener('mouseover',ver);document.addEventListener('focusin',ver);",
  "document.addEventListener('mouseout',function(e){if(a&&!(e.relatedTarget&&a.contains(e.relatedTarget)))quitar();});",
  "document.addEventListener('focusout',quitar);window.addEventListener('scroll',quitar,true);})();</script>")

# Tabla HTML: encabezados y filas conocidas con su explicación; las celdas que
# comenta una nota, resaltadas y con la lectura al pasar el mouse
.html_tabla <- function(tabla, notas = list()) {
  t <- as.data.frame(lapply(tabla, function(v) ifelse(is.na(v), "", as.character(v))),
                     stringsAsFactors = FALSE, check.names = FALSE)
  derecha <- vapply(t, .es_numerica, logical(1))
  marcas <- .marcas(t, notas)
  celda <- function(etiqueta, contenido, clases, ayuda) {
    clases <- clases[nzchar(clases)]
    if (!is.null(ayuda)) clases <- c(clases, "ps-tip")
    sprintf("<%s%s%s>%s</%s>", etiqueta,
            if (length(clases)) sprintf(" class=\"%s\"", paste(clases, collapse = " ")) else "",
            if (!is.null(ayuda)) sprintf(" tabindex=\"0\" data-tip=\"%s\"", .html_esc(ayuda)) else "",
            contenido, etiqueta)
  }
  cab <- vapply(seq_along(t), function(j) celda("th", .html_esc(names(t)[j]), if (derecha[j]) "der" else "",
                                                .ayuda_columna(names(t)[j])), "")
  filas <- vapply(seq_len(nrow(t)), function(i) {
    tds <- vapply(seq_along(t), function(j) {
      v <- t[i, j]
      k <- attr(regexpr("^ +", v), "match.length")
      txt <- .html_esc(trimws(v, "left"))
      if (derecha[j]) txt <- gsub(" ", "&nbsp;", txt, fixed = TRUE)
      if (k > 0) txt <- paste0(strrep("&nbsp;", k), txt)
      m <- marcas[[paste(i, names(t)[j], sep = "\r")]]
      if (!is.null(m)) {
        ayuda <- paste(vapply(m$k, function(q) sprintf("(%d) %s", q, notas[[q]]$texto), ""), collapse = "  ")
        return(celda("td", paste0(txt, "<sup>", paste(m$k, collapse = ","), "</sup>"),
                     c(if (derecha[j]) "der" else "", "ps-marca"), ayuda))
      }
      ayuda <- if (j == 1) .ayuda_fila(v) else NULL
      celda("td", txt, c(if (derecha[j]) "der" else "", if (!is.null(ayuda)) "ps-fila" else ""), ayuda)
    }, "")
    paste0("<tr>", paste(tds, collapse = ""), "</tr>")
  }, "")
  c(.css_ayuda(), "<div class=\"ps-tabla\"><table class=\"ps\" data-quarto-disable-processing=\"true\">",
    paste0("<thead><tr>", paste(cab, collapse = ""), "</tr></thead>"),
    "<tbody>", filas, "</tbody></table></div>")
}
