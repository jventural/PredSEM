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
#'   (parcelas).
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
  cat(sprintf("Modelo: %s | estimador %s | n = %d\n", gsub("\n", "; ", x$modelo), x$estimador, lavaan::lavInspect(x$fit, "nobs")))
  for (k in c("ajuste", "discriminante", "estructural", "indirectos")) if (!is.null(x[[k]])) print(x[[k]])
  invisible(x)
}

#' Diagrama del modelo estimado
#'
#' Dibuja las variables latentes y las flechas de las hipótesis con su
#' coeficiente beta estandarizado (línea continua si su IC no incluye el cero;
#' punteada si lo incluye) y el R2 de cada criterio.
#'
#' @param x El resultado de [ps_modelo()].
#' @param ... No se usa.
#' @return Un gráfico `ggplot`.
#' @export
plot.ps_modelo <- function(x, ...) {
  ss <- lavaan::standardizedSolution(x$fit)
  reg <- ss[ss$op == "~" & ss$rhs %in% x$lat, ]
  r2 <- lavaan::lavInspect(x$fit, "r2")
  # Capas: exógenas a la izquierda, mediadoras al centro, criterios finales a la derecha
  endo <- unique(reg$lhs); exo <- setdiff(x$lat, endo)
  med <- intersect(endo, reg$rhs); fin <- setdiff(endo, med)
  capas <- list(exo, med, fin); capas <- capas[lengths(capas) > 0]
  pos <- do.call(rbind, lapply(seq_along(capas), function(k) {
    v <- capas[[k]]; data.frame(var = v, x = (k - 1) * 3, y = if (length(v) == 1) 0 else seq(1.6, -1.6, length.out = length(v)))
  }))
  if (length(med) == 1 && length(capas) == 3) pos$y[pos$var == med] <- 1.6
  e <- merge(merge(reg, pos, by.x = "rhs", by.y = "var"), pos, by.x = "lhs", by.y = "var", suffixes = c("0", "1"))
  dx <- e$x1 - e$x0; dy <- e$y1 - e$y0; l <- sqrt(dx^2 + dy^2); k <- .75
  e$xs <- e$x0 + k * dx / l; e$ys <- e$y0 + k * .55 * dy / l; e$xe <- e$x1 - k * dx / l; e$ye <- e$y1 - k * .55 * dy / l
  e$sig <- !.incluye0(e$ci.lower, e$ci.upper)
  e$lab <- sprintf("%s%s", .f2(e$est.std), ifelse(e$sig, "", " (ns)"))
  pos$lab <- ifelse(pos$var %in% endo, sprintf("%s\nR\u00b2 = %s", pos$var, .f2(r2[pos$var])), pos$var)
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = e, ggplot2::aes(x = xs, y = ys, xend = xe, yend = ye, linetype = sig),
                          arrow = ggplot2::arrow(length = ggplot2::unit(.25, "cm"), type = "closed"), linewidth = .8, colour = "#404040") +
    ggplot2::geom_label(data = e, ggplot2::aes(x = xs + .38 * (xe - xs), y = ys + .38 * (ye - ys), label = lab), size = 4.4,
                        linewidth = 0, fill = "white", label.padding = ggplot2::unit(.15, "lines")) +
    ggplot2::geom_point(data = pos, ggplot2::aes(x, y), shape = 21, size = 30, fill = "#FBE3E3", colour = "#BB1715", stroke = 1) +
    ggplot2::geom_text(data = pos, ggplot2::aes(x, y, label = lab), size = 4.2, lineheight = .9) +
    ggplot2::scale_linetype_manual(values = c(`TRUE` = "solid", `FALSE` = "dashed"), guide = "none") +
    ggplot2::coord_cartesian(xlim = c(-1, max(pos$x) + 1), ylim = c(-2.4, 2.4)) + ggplot2::theme_void()
}
