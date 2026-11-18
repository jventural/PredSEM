# Partes de una fórmula estructural: "SAT ~ ADA + AE" → criterio y predictores
.partes <- function(modelo) {
  ec <- trimws(unlist(strsplit(modelo, "[;\n]")))
  ec <- ec[nzchar(ec)]
  lapply(ec, function(e) {
    lr <- trimws(strsplit(e, "~", fixed = TRUE)[[1]])
    if (length(lr) != 2) stop("Escribe cada ecuaci\u00f3n como \"CRITERIO ~ PREDICTOR1 + PREDICTOR2\".", call. = FALSE)
    list(y = lr[1], x = trimws(strsplit(lr[2], "+", fixed = TRUE)[[1]]))
  })
}

# Sintaxis estructural con etiquetas y efectos indirectos X -> M -> Y
.sintaxis_estructural <- function(partes, covariables = NULL) {
  lin <- character(); rutas <- list()
  for (p in partes) {
    rhs <- c(p$x, covariables)
    et <- paste0("b_", p$y, "_", rhs)
    lin <- c(lin, sprintf("%s ~ %s", p$y, paste(sprintf("%s*%s", et, rhs), collapse = " + ")))
    for (k in seq_along(p$x)) rutas[[length(rutas) + 1]] <- c(y = p$y, x = p$x[k], et = et[k])
  }
  R <- do.call(rbind, rutas)
  ind <- character(); def <- list()
  for (i in seq_len(nrow(R))) for (j in seq_len(nrow(R)))
    if (R[i, "y"] == R[j, "x"]) {          # x -> m (i)  y  m -> y (j)
      nom <- paste0("ind_", R[i, "x"], "_", R[i, "y"], "_", R[j, "y"])
      ind <- c(ind, sprintf("%s := %s*%s", nom, R[i, "et"], R[j, "et"]))
      def[[length(def) + 1]] <- c(nombre = nom, x = unname(R[i, "x"]), m = unname(R[i, "y"]), y = unname(R[j, "y"]))
    }
  list(sintaxis = paste(c(lin, ind), collapse = "\n"), rutas = R,
       indirectos = if (length(def)) do.call(rbind, def) else NULL)
}

# HTMT con la matriz de correlaciones de los indicadores
.htmt <- function(R, ind, a, b) {
  het <- mean(abs(R[ind[[a]], ind[[b]]]))
  mono <- function(f) { r <- R[ind[[f]], ind[[f]]]; mean(abs(r[lower.tri(r)])) }
  het / sqrt(mono(a) * mono(b))
}

#' Modelo de ecuaciones estructurales (pasos 5 a 7)
#'
#' Estima en una sola llamada el modelo de medida conjunto (todas las
#' variables latentes), la validez discriminante entre ellas (correlación
#' latente con su IC y HTMT), el modelo estructural (coeficientes beta
#' estandarizados con IC 95 % y R2 de cada criterio) y, si una variable es a la
#' vez criterio y predictor, los efectos indirectos (mediación) con su IC.
#'
#' @param base La base que devuelve [ps_preparar()].
#' @param modelo Las hipótesis como fórmula: `"SAT ~ ADA + AE"`. Varias
#'   ecuaciones se separan con punto y coma: `"AE ~ ADA; SAT ~ ADA + AE"`
#'   (aquí AE es mediadora y el efecto indirecto se calcula solo).
#' @param ... Las variables latentes con su definición, igual que en
#'   [ps_medida()]: `SAT = "ASS"`, `ADA = "BERQ1:BERQ4 | BERQ5:BERQ8"`
#'   (parcelas). Las parcelas pueden llevar nombre, que es el que aparece en
#'   el modelo y en `plot()`: `ADA = "DIS = BERQ1:BERQ4 | AFR = BERQ5:BERQ8"`.
#'   Así una variable con dimensiones se forma con sus dimensiones (parceling).
#' @param covariables Variables observadas de la base que entran como
#'   controles en todas las ecuaciones (por ejemplo `"edad"`). Opcional.
#'
#' @details Con ítems ordinales se usa WLSMV (correlaciones policóricas); los
#'   indicadores que son parcelas se tratan como continuos. Los efectos
#'   indirectos se calculan como producto de coeficientes, con IC por el método
#'   delta sobre la solución estandarizada.
#' @return Un objeto `ps_modelo`: lista con `ajuste`, `discriminante`,
#'   `estructural` e `indirectos` (cada uno un `ps_resultado` con tabla y
#'   lectura), el ajuste de `lavaan` (`fit`) y lo necesario para
#'   [ps_comparar()], [ps_grupos()] y [ps_sensibilidad()]. Con `plot()` se
#'   dibuja el modelo estimado.
#' @examples
#' base <- ps_preparar(ps_ejemplo(), escalas = c(BERQ = "1-5", EAP = "1-4", ASS = "1-3"))
#' m <- ps_modelo(base, "AE ~ ADA; SAT ~ ADA + AE",
#'                ADA = "BERQ1, BERQ6, BERQ11 | BERQ3, BERQ8, BERQ13 | BERQ4, BERQ9, BERQ14", AE = "EAP", SAT = "ASS")
#' m
#' plot(m)
#' @export
ps_modelo <- function(base, modelo, ..., covariables = NULL) {
  .comprobar_base(base)
  defs <- list(...)
  partes <- .partes(modelo)
  lat <- unique(unlist(lapply(partes, function(p) c(p$y, p$x))))
  falta <- setdiff(lat, names(defs))
  if (length(falta)) stop("Falta definir: ", paste(falta, collapse = ", "),
                          " (por ejemplo, ", falta[1], " = \"PREFIJO\").", call. = FALSE)
  pr <- .indicadores(base, defs[lat])
  ord <- pr$ordinales
  est <- if (length(ord)) "WLSMV" else "MLR"
  args <- list(data = pr$datos, ordered = if (length(ord)) ord else NULL, estimator = est, std.lv = TRUE,
               missing = if (length(ord)) "pairwise" else "ml")
  med <- do.call(lavaan::cfa, c(list(model = .sintaxis_medida(pr$ind)), args))
  se <- .sintaxis_estructural(partes, covariables)
  sint <- paste(.sintaxis_medida(pr$ind), se$sintaxis, sep = "\n")
  fit <- do.call(lavaan::sem, c(list(model = sint), args))
  .comprobar_estimacion(med, "El modelo de medida conjunto")
  .comprobar_estimacion(fit, "El modelo estructural")

  # --- 1. Ajuste del modelo de medida conjunto y del estructural ---
  am <- .ajuste(med); ae <- .ajuste(fit)
  fila_aj <- function(n, a) data.frame(Modelo = n, "\u03c7\u00b2 (gl)" = sprintf("%s (%d)", .f2(a["chi2"]), as.integer(a["gl"])),
                                       CFI = .f3(a["cfi"]), TLI = .f3(a["tli"]),
                                       "RMSEA [IC 90 %]" = sprintf("%s %s", .f3(a["rmsea"]), .ic3(a["rmsea_li"], a["rmsea_ls"])),
                                       SRMR = .f3(a["srmr"]), check.names = FALSE)
  t_aj <- rbind(fila_aj("Medida conjunta (todas las latentes)", am), fila_aj("Estructural (las hip\u00f3tesis)", ae))
  n_aj <- list(.nota(1, "CFI", sprintf("Medida conjunta: %s. Primero se demuestra que la medici\u00f3n funciona; reci\u00e9n despu\u00e9s se leen las flechas.", .lee_ajuste(am))),
               .nota(2, "RMSEA [IC 90 %]", if (abs(ae["gl"] - am["gl"]) < .5)
                 "El modelo estructural tiene los mismos grados de libertad que el de medida: las hip\u00f3tesis no imponen restricciones extra, as\u00ed que su ajuste es el mismo." else
                 sprintf("Estructural: %s.", .lee_ajuste(ae))))

  # --- 2. Validez discriminante ---
  ssm <- lavaan::standardizedSolution(med)
  cr <- ssm[ssm$op == "~~" & ssm$lhs != ssm$rhs & ssm$lhs %in% lat & ssm$rhs %in% lat, ]
  R <- lavaan::lavInspect(med, "sampstat")$cov; R <- stats::cov2cor(R)
  t_dis <- data.frame(Par = paste(cr$lhs, "\u2194", cr$rhs), "r latente" = .f2(cr$est.std),
                      "IC 95 %" = .ic(cr$ci.lower, cr$ci.upper),
                      HTMT = .f2(mapply(function(a, b) .htmt(R, pr$ind, a, b), cr$lhs, cr$rhs)), check.names = FALSE)
  im <- which.max(abs(cr$est.std))
  hm <- as.numeric(t_dis$HTMT)
  n_dis <- list(.nota(im, "IC 95 %", sprintf("%s: r = %s, IC hasta %s. %s", t_dis$Par[im], .f2(cr$est.std[im]), .f2(max(abs(c(cr$ci.lower[im], cr$ci.upper[im])))),
    if (max(abs(c(cr$ci.lower[im], cr$ci.upper[im]))) < .85) "El IC no llega a .85: son constructos distintos." else
    "El IC alcanza .85 o m\u00e1s: los constructos se solapan y sus coeficientes deben leerse con cautela.")),
    .nota(which.max(hm), "HTMT", sprintf("HTMT m\u00e1ximo = %s: %s", .f2(max(hm)), if (max(hm) < .85)
      "por debajo de .85, otra se\u00f1al de que las variables se distinguen." else "por encima de .85: problema de validez discriminante.")))

  # --- 3. Estructural ---
  ss <- lavaan::standardizedSolution(fit); pe <- lavaan::parameterEstimates(fit)
  reg <- ss[ss$op == "~", ]; rp <- pe[pe$op == "~", ]
  r2 <- lavaan::lavInspect(fit, "r2")
  t_est <- data.frame(Ruta = paste(reg$lhs, "\u2190", reg$rhs), "\u03b2" = .f2(reg$est.std), "IC 95 %" = .ic(reg$ci.lower, reg$ci.upper),
                      B = .f2(rp$est), EE = .f2(rp$se), p = .fp(rp$pvalue), "R\u00b2 del criterio" = "", check.names = FALSE)
  for (y in unique(reg$lhs)) t_est[match(y, reg$lhs), "R\u00b2 del criterio"] <- .f3(r2[y])
  hip <- which(reg$rhs %in% lat)
  imax <- hip[which.max(abs(reg$est.std[hip]))]
  y1 <- partes[[length(partes)]]$y
  n_est <- list(
    .nota(imax, "\u03b2", sprintf("\u03b2 = %s: por cada desviaci\u00f3n est\u00e1ndar m\u00e1s en %s, %s %s %s DE, manteniendo constante lo dem\u00e1s. Efecto %s y %s.",
      .f2(reg$est.std[imax]), reg$rhs[imax], reg$lhs[imax], if (reg$est.std[imax] > 0) "sube" else "baja",
      .f2(abs(reg$est.std[imax])), .magnitud_beta(reg$est.std[imax]),
      if (.incluye0(reg$ci.lower[imax], reg$ci.upper[imax])) "su IC incluye el cero" else "su IC no incluye el cero")),
    .nota(match(y1, reg$lhs), "R\u00b2 del criterio", sprintf("R\u00b2 = %s: el modelo explica el %s %% de la variaci\u00f3n de %s.",
      .f3(r2[y1]), .f2(100 * r2[y1]), y1)))
  ns <- hip[.incluye0(reg$ci.lower[hip], reg$ci.upper[hip])]
  if (length(ns)) n_est[[3]] <- .nota(ns[1], "IC 95 %", sprintf(
    "%s: el IC incluye el cero, as\u00ed que esa hip\u00f3tesis no recibe apoyo con estos datos.", t_est$Ruta[ns[1]]))

  # --- 4. Indirectos ---
  ind_res <- NULL
  if (!is.null(se$indirectos)) {
    di <- ss[ss$op == ":=", ]
    D <- se$indirectos
    t_ind <- data.frame(Efecto = sprintf("%s \u2192 %s \u2192 %s", D[, "x"], D[, "m"], D[, "y"]),
                        "\u03b2 indirecto" = .f2(di$est.std[match(D[, "nombre"], di$lhs)]),
                        "IC 95 %" = .ic(di$ci.lower[match(D[, "nombre"], di$lhs)], di$ci.upper[match(D[, "nombre"], di$lhs)]),
                        "\u03b2 directo" = .f2(reg$est.std[match(paste(D[, "y"], D[, "x"]), paste(reg$lhs, reg$rhs))]),
                        check.names = FALSE)
    li <- di$ci.lower[match(D[, "nombre"], di$lhs)]; ls <- di$ci.upper[match(D[, "nombre"], di$lhs)]
    dir_ic <- reg[match(paste(D[, "y"], D[, "x"]), paste(reg$lhs, reg$rhs)), c("ci.lower", "ci.upper")]
    n_ind <- list(.nota(1, "IC 95 %", sprintf("Efecto indirecto %s: el IC %s el cero, %s.", t_ind$Efecto[1],
      if (.incluye0(li[1], ls[1])) "incluye" else "no incluye",
      if (.incluye0(li[1], ls[1])) "no hay evidencia de mediaci\u00f3n" else sprintf("as\u00ed que %s transmite parte del efecto", D[1, "m"]))))
    if (!any(is.na(dir_ic[1, ])) && !.incluye0(li[1], ls[1]))
      n_ind[[2]] <- .nota(1, "\u03b2 directo", if (.incluye0(dir_ic[1, 1], dir_ic[1, 2]))
        "El efecto directo deja de distinguirse de cero: mediaci\u00f3n casi total." else
        "El efecto directo sigue siendo distinto de cero: mediaci\u00f3n parcial.")
    ind_res <- .resultado(t_ind, "Efectos indirectos (mediaci\u00f3n)", n_ind)
  }
  structure(list(
    ajuste = .resultado(t_aj, "Ajuste: medida conjunta y modelo estructural", n_aj),
    discriminante = .resultado(t_dis, "Validez discriminante entre las latentes", n_dis),
    estructural = .resultado(t_est, "Modelo estructural: las hip\u00f3tesis", n_est),
    indirectos = ind_res,
    fit = fit, medida = med, base = base, defs = defs[lat], modelo = modelo, partes = partes,
    covariables = covariables, estimador = est, lat = lat), class = "ps_modelo")
}

#' @export
print.ps_modelo <- function(x, ...) {
  cat("\nModelo de ecuaciones estructurales (ps_modelo)\n==============================================\n")
  cat(.resumen_modelo(x), "\n", sep = "")
  for (k in c("ajuste", "discriminante", "estructural", "indirectos")) if (!is.null(x[[k]])) print(x[[k]])
  invisible(x)
}

# Una línea con el modelo, el estimador y la n (consola y Quarto)
.resumen_modelo <- function(x)
  sprintf("Modelo: %s | estimador %s | n = %d", gsub("\n", "; ", x$modelo), x$estimador, lavaan::lavInspect(x$fit, "nobs"))

#' Diagrama del modelo estimado
#'
#' Dibuja el modelo como se presenta en un artículo: las variables latentes
#' (círculos), sus indicadores (rectángulos: ítems o parcelas) con la carga
#' estandarizada, las correlaciones entre latentes (flecha curva de dos
#' puntas) y las flechas de las hipótesis con su beta estandarizado (línea
#' continua si su IC no incluye el cero; punteada si lo incluye, con "ns"),
#' además del R2 de cada criterio. Los predictores van a la izquierda con sus
#' indicadores, los criterios a la derecha y las mediadoras arriba.
#'
#' @param x El resultado de [ps_modelo()].
#' @param indicadores `TRUE` (por defecto) dibuja también los ítems o
#'   parcelas con sus cargas; `FALSE` deja solo las latentes y sus relaciones.
#' @param ... No se usa.
#' @return Un gráfico `ggplot` (se guarda con `ggplot2::ggsave()` o `png()`).
#' @examples
#' base <- ps_preparar(ps_ejemplo(), escalas = c(BERQ = "1-5", EAP = "1-4", ASS = "1-3"))
#' m <- ps_modelo(base, "SAT ~ ADA + AE",
#'                ADA = "DIS = BERQ1, BERQ6, BERQ11 | REE = BERQ3, BERQ8, BERQ13",
#'                AE = "EAP", SAT = "ASS")
#' plot(m)
#' plot(m, indicadores = FALSE)
#' @export
plot.ps_modelo <- function(x, indicadores = TRUE, ...) {
  ss <- lavaan::standardizedSolution(x$fit)
  lat <- x$lat
  reg <- ss[ss$op == "~" & ss$lhs %in% lat & ss$rhs %in% lat, ]
  cvs <- ss[ss$op == "~~" & ss$lhs != ss$rhs & ss$lhs %in% lat & ss$rhs %in% lat, ]
  car <- ss[ss$op == "=~" & ss$lhs %in% lat, ]
  r2 <- lavaan::lavInspect(x$fit, "r2")
  endo <- unique(reg$lhs); exo <- setdiff(lat, endo)
  med <- intersect(endo, reg$rhs); fin <- setdiff(endo, med)
  rad <- .62; anc <- 1.05; alt <- .40; paso <- .50; sep <- 2.3
  n_ind <- vapply(lat, function(v) sum(car$lhs == v), 1)

  # --- Posición de las latentes: exógenas | mediadoras (arriba) | criterios ---
  bloque <- function(v) if (indicadores && !(v %in% med)) max(n_ind[v] * paso, 2 * rad + .5) + .5 else 2 * rad + 1.1
  apilar <- function(vs, x0) {
    if (!length(vs)) return(NULL)
    h <- vapply(vs, bloque, 1); tope <- sum(h) / 2
    data.frame(var = vs, x = x0, y = tope - cumsum(h) + h / 2)
  }
  dx <- if (indicadores) 4.6 else 3.4
  pos <- rbind(apilar(exo, 0), apilar(fin, if (length(med)) 2 * dx else dx))
  if (length(med)) {
    pm <- apilar(med, dx)
    pm$y <- pm$y - min(pm$y) + max(pos$y) + rad + 1.1
    pos <- rbind(pos, pm)
  }
  xy <- function(v) unlist(pos[match(v, pos$var), c("x", "y")])

  # --- Indicadores: izquierda (exógenas), derecha (criterios), arriba (mediadoras) ---
  cajas <- NULL; cargas <- NULL
  if (indicadores) {
    for (v in lat) {
      cv <- car[car$lhs == v, ]; k <- nrow(cv); c0 <- xy(v)
      off <- seq_len(k) - (k + 1) / 2
      if (v %in% med) {
        xi <- c0[1] + off * (anc + .12); yi <- rep(c0[2] + rad + 1.25, k)
      } else {
        xi <- rep(c0[1] + if (v %in% exo) -sep else sep, k); yi <- c0[2] - off * paso
      }
      cajas <- rbind(cajas, data.frame(lab = cv$rhs, x = xi, y = yi))
      # la flecha llega al centro del lado del recuadro que mira a la latente
      if (v %in% med) { xe <- xi; ye <- yi - alt / 2 } else { xe <- xi - sign(xi - c0[1]) * anc / 2; ye <- yi }
      u <- cbind(xe - c0[1], ye - c0[2]); u <- u / sqrt(rowSums(u^2))
      cargas <- rbind(cargas, data.frame(xs = c0[1] + rad * u[, 1], ys = c0[2] + rad * u[, 2],
                                         xe = xe, ye = ye, lab = .f2(cv$est.std)))
    }
    cargas$xl <- cargas$xs + .62 * (cargas$xe - cargas$xs)
    cargas$yl <- cargas$ys + .62 * (cargas$ye - cargas$ys)
  }

  # --- Hipótesis (betas) ---
  fl <- NULL
  if (nrow(reg)) {
    p0 <- t(vapply(reg$rhs, xy, c(0, 0))); p1 <- t(vapply(reg$lhs, xy, c(0, 0)))
    u <- p1 - p0; u <- u / sqrt(rowSums(u^2))
    fl <- data.frame(xs = p0[, 1] + rad * u[, 1], ys = p0[, 2] + rad * u[, 2],
                     xe = p1[, 1] - rad * u[, 1], ye = p1[, 2] - rad * u[, 2],
                     sig = !.incluye0(reg$ci.lower, reg$ci.upper))
    fl$lab <- sprintf("%s%s", .f2(reg$est.std), ifelse(fl$sig, "", " (ns)"))
    fl$xl <- fl$xs + .45 * (fl$xe - fl$xs); fl$yl <- fl$ys + .45 * (fl$ye - fl$ys)
  }

  # --- Correlaciones entre latentes (curvas de dos puntas, abiertas hacia afuera del modelo) ---
  cu <- NULL
  if (nrow(cvs)) {
    cu <- do.call(rbind, lapply(seq_len(nrow(cvs)), function(i) {
      a <- xy(cvs$lhs[i]); b <- xy(cvs$rhs[i])
      if (a[2] < b[2]) { tmp <- a; a <- b; b <- tmp }
      lado <- if (cvs$lhs[i] %in% exo) -1 else 1        # -1: hacia la izquierda (predictores); +1: derecha (criterios)
      ang <- atan2(b[2] - a[2], b[1] - a[1])
      gira <- lado * .35
      xs <- a[1] + rad * cos(ang + gira); ys <- a[2] + rad * sin(ang + gira)
      xe <- b[1] + rad * cos(ang + pi - gira); ye <- b[2] + rad * sin(ang + pi - gira)
      L <- sqrt((xe - xs)^2 + (ye - ys)^2)
      nx <- (ye - ys) / L; ny <- -(xe - xs) / L
      if (nx * lado < 0) { nx <- -nx; ny <- -ny }
      data.frame(xs = xs, ys = ys, xe = xe, ye = ye, nx = nx, ny = ny, L = L, lab = .f2(cvs$est.std[i]))
    }))
    curv <- .45
    cu$xl <- (cu$xs + cu$xe) / 2 + cu$nx * curv * cu$L / 2
    cu$yl <- (cu$ys + cu$ye) / 2 + cu$ny * curv * cu$L / 2
    # signo de geom_curve: positivo = curva hacia la derecha del sentido de avance
    cu$curv <- ifelse((cu$xe - cu$xs) * cu$ny - (cu$ye - cu$ys) * cu$nx > 0, -curv, curv)
  }

  # --- Dibujo ---
  ang <- seq(0, 2 * pi, length.out = 100)
  circ <- do.call(rbind, lapply(seq_len(nrow(pos)), function(i)
    data.frame(var = pos$var[i], x = pos$x[i] + rad * cos(ang), y = pos$y[i] + rad * sin(ang))))
  pos$lab <- ifelse(pos$var %in% endo, sprintf("%s\nR\u00b2 = %s", pos$var, .f2(r2[pos$var])), pos$var)
  g <- ggplot2::ggplot()
  if (!is.null(cargas))
    g <- g + ggplot2::geom_segment(data = cargas, ggplot2::aes(x = xs, y = ys, xend = xe, yend = ye),
                                   arrow = ggplot2::arrow(length = ggplot2::unit(.16, "cm"), type = "closed"),
                                   linewidth = .45, colour = "#606060") +
      ggplot2::geom_rect(data = cajas, ggplot2::aes(xmin = x - anc / 2, xmax = x + anc / 2, ymin = y - alt / 2, ymax = y + alt / 2),
                         fill = "white", colour = "#404040", linewidth = .4) +
      ggplot2::geom_text(data = cajas, ggplot2::aes(x, y, label = lab), size = 3) +
      ggplot2::geom_label(data = cargas, ggplot2::aes(xl, yl, label = lab), size = 2.6, linewidth = 0,
                          fill = "white", label.padding = ggplot2::unit(.08, "lines"))
  if (!is.null(cu)) {
    for (i in seq_len(nrow(cu)))
      g <- g + ggplot2::geom_curve(data = cu[i, ], ggplot2::aes(x = xs, y = ys, xend = xe, yend = ye),
                                   curvature = cu$curv[i], linewidth = .6, colour = "#404040",
                                   arrow = ggplot2::arrow(length = ggplot2::unit(.18, "cm"), type = "closed", ends = "both"))
    g <- g + ggplot2::geom_label(data = cu, ggplot2::aes(xl, yl, label = lab), size = 3.6, linewidth = 0,
                                 fill = "white", label.padding = ggplot2::unit(.12, "lines"))
  }
  if (!is.null(fl))
    g <- g + ggplot2::geom_segment(data = fl, ggplot2::aes(x = xs, y = ys, xend = xe, yend = ye, linetype = sig),
                                   arrow = ggplot2::arrow(length = ggplot2::unit(.25, "cm"), type = "closed"),
                                   linewidth = .8, colour = "#404040") +
      ggplot2::geom_label(data = fl, ggplot2::aes(xl, yl, label = lab), size = 3.8, linewidth = 0,
                          fill = "white", label.padding = ggplot2::unit(.15, "lines"))
  g + ggplot2::geom_polygon(data = circ, ggplot2::aes(x, y, group = var), fill = "#FBE3E3", colour = "#BB1715", linewidth = .6) +
    ggplot2::geom_text(data = pos, ggplot2::aes(x, y, label = lab), size = 3.9, lineheight = .9) +
    ggplot2::scale_linetype_manual(values = c(`TRUE` = "solid", `FALSE` = "dashed"), guide = "none") +
    ggplot2::coord_equal(clip = "off") + ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 10))
}
