#' Modelo de medida de una escala (paso 3)
#'
#' Análisis factorial confirmatorio de una escala: con ítems ordinales usa el
#' estimador WLSMV sobre correlaciones policóricas; con parcelas, MLR. Informa
#' el ajuste (chi cuadrado, CFI, TLI, RMSEA con su IC y SRMR) y, por cada
#' factor, las cargas y la fiabilidad (omega y alfa sobre la misma matriz). La
#' fiabilidad solo se interpreta si el modelo ajusta.
#'
#' @param base La base que devuelve [ps_preparar()].
#' @param ... Los factores de la escala con su definición, por ejemplo
#'   `SAT = "ASS"` (todos los ítems ASS) o, si tiene dimensiones,
#'   `Distraccion = "BERQ1:BERQ4", Afrontamiento = "BERQ5:BERQ8"`.
#' @return Un resultado `ps_resultado` con el ajuste y su lectura; en
#'   `fiabilidad`, otro con las cargas, omega y alfa por factor; en `modelo`, el
#'   ajuste de `lavaan`.
#' @examples
#' base <- ps_preparar(ps_ejemplo(), escalas = c(BERQ = "1-5", EAP = "1-4", ASS = "1-3"))
#' ps_medida(base, SAT = "ASS")
#' @export
ps_medida <- function(base, ...) {
  .comprobar_base(base)
  defs <- list(...)
  if (!length(defs) || is.null(names(defs)) || any(names(defs) == ""))
    stop("Indica cada factor con nombre, por ejemplo: ps_medida(base, SAT = \"ASS\").", call. = FALSE)
  pr <- .indicadores(base, defs)
  ord <- pr$ordinales
  fit <- lavaan::cfa(.sintaxis_medida(pr$ind), data = pr$datos, ordered = if (length(ord)) ord else NULL,
                     estimator = if (length(ord)) "WLSMV" else "MLR", std.lv = TRUE, missing = if (length(ord)) "pairwise" else "ml")
  a <- .ajuste(fit)
  tabla <- data.frame(Modelo = if (length(defs) == 1) "Unidimensional" else sprintf("%d factores relacionados", length(defs)),
                      Estimador = if (length(ord)) "WLSMV" else "MLR",
                      "\u03c7\u00b2 (gl)" = sprintf("%s (%d)", .f2(a["chi2"]), as.integer(a["gl"])),
                      CFI = .f3(a["cfi"]), TLI = .f3(a["tli"]),
                      "RMSEA [IC 90 %]" = sprintf("%s %s", .f3(a["rmsea"]), .ic3(a["rmsea_li"], a["rmsea_ls"])),
                      SRMR = .f3(a["srmr"]), check.names = FALSE)
  # Fiabilidad por factor: omega con la solución estandarizada; alfa con la misma matriz
  ss <- lavaan::standardizedSolution(fit)
  R <- lavaan::lavInspect(fit, "sampstat")$cov
  if (is.null(R)) R <- lavaan::lavInspect(fit, "sampstat")[[1]]$cov
  R <- stats::cov2cor(R)
  fi <- do.call(rbind, lapply(names(pr$ind), function(f) {
    i <- pr$ind[[f]]; l <- ss$est.std[ss$op == "=~" & ss$lhs == f]
    th <- 1 - l^2; k <- length(i); r <- R[i, i]; rbar <- mean(r[lower.tri(r)])
    data.frame(Factor = f, "\u00cdtems" = k, "Cargas (m\u00edn.\u2013m\u00e1x.)" = sprintf("%s\u2013%s", .f2(min(l)), .f2(max(l))),
               "\u03c9" = .f3(sum(l)^2 / (sum(l)^2 + sum(th))), "\u03b1" = .f3(k * rbar / (1 + (k - 1) * rbar)),
               lmin = min(l), check.names = FALSE)
  }))
  ok <- .ajuste_ok(a) || .ajuste_aceptable(a)
  notas <- list(
    .nota(1, "CFI", sprintf("CFI = %s, TLI = %s, RMSEA = %s, SRMR = %s: %s.", .f3(a["cfi"]), .f3(a["tli"]), .f3(a["rmsea"]), .f3(a["srmr"]), .lee_ajuste(a))),
    .nota(1, "Estimador", if (length(ord))
      "WLSMV: los \u00edtems tipo Likert son ordinales; se analizan sus correlaciones polic\u00f3ricas." else
      "MLR: los indicadores son parcelas (promedios de \u00edtems) y se tratan como continuos."))
  iw <- which.min(as.numeric(sub("^[.]", "0.", fi[["\u03c9"]])))
  nf <- list(.nota(iw, "\u03c9", sprintf("\u03c9 = %s en %s%s", fi[["\u03c9"]][iw], fi$Factor[iw],
    if (!ok) ": el modelo no ajusta, as\u00ed que la fiabilidad todav\u00eda NO se interpreta." else
    if (as.numeric(sub("^[.]", "0.", fi[["\u03c9"]][iw])) >= .80) ": el factor con menor fiabilidad, y aun as\u00ed adecuada (\u2265 .80)." else
    ": fiabilidad por debajo de .80; interpretar con cautela.")))
  il <- which.min(fi$lmin)
  nf[[2]] <- .nota(il, "Cargas (m\u00edn.\u2013m\u00e1x.)", if (fi$lmin[il] >= .50)
    sprintf("Todas las cargas son \u2265 .50 (la menor, %s en %s): los \u00edtems representan bien a su factor.", .f2(fi$lmin[il]), fi$Factor[il]) else
    sprintf("La carga m\u00e1s baja es %s (%s): un \u00edtem con carga < .50 aporta poco; rev\u00edsalo antes de decidir.", .f2(fi$lmin[il]), fi$Factor[il]))
  .resultado(tabla, sprintf("Modelo de medida: %s (ps_medida)", paste(names(defs), collapse = ", ")), notas,
             fiabilidad = .resultado(fi[, 1:5], "Cargas y fiabilidad por factor", nf), modelo = fit)
}

#' Descriptivos y correlaciones de los puntajes (paso 4)
#'
#' Calcula el puntaje de cada variable (media de sus ítems) y presenta media,
#' desviación estándar, asimetría, curtosis y la matriz de correlaciones de
#' Pearson entre puntajes.
#'
#' @param base La base que devuelve [ps_preparar()].
#' @param ... Las variables con su definición, igual que en [ps_medida()], por
#'   ejemplo `ADA = "BERQ1:BERQ12", AE = "EAP", SAT = "ASS"`.
#' @return Un resultado `ps_resultado` con la tabla de descriptivos y
#'   correlaciones y su lectura.
#' @examples
#' base <- ps_preparar(ps_ejemplo(), escalas = c(BERQ = "1-5", EAP = "1-4", ASS = "1-3"))
#' ps_descriptivos(base, AE = "EAP", SAT = "ASS")
#' @export
ps_descriptivos <- function(base, ...) {
  .comprobar_base(base)
  defs <- list(...); d <- as.data.frame(base)
  P <- sapply(names(defs), function(f) rowMeans(d[, .definir(defs[[f]], names(d))$items, drop = FALSE], na.rm = TRUE))
  asim <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^3) / stats::sd(x)^3 }
  curt <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^4) / stats::sd(x)^4 - 3 }
  R <- stats::cor(P, use = "pairwise.complete.obs")
  k <- ncol(P)
  tabla <- data.frame(Variable = paste0(seq_len(k), ". ", colnames(P)), M = .f2(colMeans(P, na.rm = TRUE)),
                      DE = .f2(apply(P, 2, stats::sd, na.rm = TRUE)), g1 = .f2(apply(P, 2, asim)), g2 = .f2(apply(P, 2, curt)),
                      check.names = FALSE)
  for (j in seq_len(k - 1)) tabla[[as.character(j)]] <- c(rep("", j), .f2(R[(j + 1):k, j]))
  R2 <- R; R2[upper.tri(R2, diag = TRUE)] <- NA
  im <- which(abs(R2) == max(abs(R2), na.rm = TRUE), arr.ind = TRUE)[1, ]
  g1 <- apply(P, 2, asim)
  notas <- list(
    .nota(im[1], as.character(im[2]), sprintf("r = %s entre %s y %s: la relaci\u00f3n m\u00e1s fuerte entre puntajes. Es una relaci\u00f3n observada; el modelo SEM la estima libre de error de medida.",
                                           .f2(R[im[1], im[2]]), colnames(P)[im[2]], colnames(P)[im[1]])),
    .nota(which.max(abs(g1)), "g1", sprintf("g1 y g2 entre -1 y 1 indican distribuciones cercanas a la normal (aqu\u00ed, la asimetr\u00eda m\u00e1s alta es %s en %s).",
                                            .f2(g1[which.max(abs(g1))]), colnames(P)[which.max(abs(g1))])))
  .resultado(tabla, "Descriptivos y correlaciones de los puntajes (ps_descriptivos)", notas, puntajes = P)
}
