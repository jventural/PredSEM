#' Ruta de la base de ejemplo
#'
#' Base SIMULADA (ficticia) de un estudio predictivo con variables latentes:
#' regulación emocional conductual (BERQ, 20 ítems) y autoeficacia académica
#' (EAP) como predictores de la satisfacción académica (ASS) en universitarios.
#' Una fila por persona, como llega de un formulario.
#'
#' @return La ruta (texto) del archivo Excel incluido en el paquete.
#' @examples
#' ps_ejemplo()
#' @export
ps_ejemplo <- function() system.file("extdata", "satisfaccion_academica.xlsx", package = "PredSEM", mustWork = TRUE)

#' Tamaño de muestra para el modelo (paso 1)
#'
#' Calcula cuántos participantes hacen falta para detectar
#' una relación estructural de tamaño `beta` con la potencia deseada, para el
#' MISMO tipo de modelo que se estimará (variables latentes con sus
#' indicadores). Si se da `n`, informa además la potencia lograda con ese n.
#' Sigue el procedimiento de `semPower::semPower.powerRegression()`
#' (comparación restringida), calculado con lavaan.
#'
#' @param criterio Número de indicadores del criterio.
#' @param predictores Número de indicadores de cada predictor, por ejemplo
#'   `c(12, 8)`.
#' @param beta Coeficiente estandarizado más pequeño que interesa detectar.
#' @param carga Carga factorial típica de los indicadores.
#' @param potencia Potencia deseada.
#' @param alfa Nivel de significación.
#' @param n Opcional: el tamaño de muestra que se tiene.
#' @return Un resultado `ps_resultado` con el N necesario (y la potencia con
#'   `n`) y su lectura.
#' @examples
#' \donttest{
#' ps_muestra(criterio = 7, predictores = c(12, 8))
#' }
#' @export
ps_muestra <- function(criterio, predictores, beta = .20, carga = .70, potencia = .80, alfa = .05, n = NULL) {
  # Mismo procedimiento que semPower (powerRegression, comparación restringida):
  # matriz poblacional del modelo, ajuste del modelo con el beta del primer
  # predictor fijado en 0 y potencia con la chi cuadrado no central (1 gl).
  # Se calcula con lavaan para no depender de la versión de semPower.
  pw <- .potencia_regresion(criterio, predictores, beta, carga, alfa)
  N <- 10
  while (pw(N) < potencia) N <- N + 1
  tabla <- data.frame("\u03b2 m\u00ednimo" = .sin0(beta), Carga = .sin0(carga), Potencia = .sin0(potencia),
                      "N necesario" = N, check.names = FALSE)
  notas <- list(.nota(1, "N necesario", sprintf(
    "Para detectar \u03b2 = %s con potencia %s (cargas de %s; %d indicadores en el criterio y %s en los predictores) hacen falta %d participantes.",
    .sin0(beta), .sin0(potencia), .sin0(carga), criterio, paste(predictores, collapse = " y "), N)))
  if (!is.null(n)) {
    pw <- pw(n)
    tabla[["n que tienes"]] <- n; tabla[["Potencia lograda"]] <- .sin0(pw)
    notas[[2]] <- .nota(1, "Potencia lograda", sprintf("Con %d participantes la potencia para \u03b2 = %s es %s%s.", n, .sin0(beta), .sin0(pw),
      if (pw < potencia) ": por debajo de lo planeado; el IC de cada \u03b2 ser\u00e1 m\u00e1s ancho" else ""))
  }
  .resultado(tabla, "Tama\u00f1o de muestra para el modelo (ps_muestra)", notas)
}

#' Comparar con un modelo rival (paso 8)
#'
#' Estima un modelo alternativo plausible con las mismas variables (por
#' ejemplo, sin la mediación o con otra dirección) y lo compara con el
#' propuesto: diferencia de chi cuadrado escalada y cambio en CFI y RMSEA.
#'
#' @param m El resultado de [ps_modelo()].
#' @param rival Las ecuaciones del modelo rival, como en [ps_modelo()], por
#'   ejemplo `"SAT ~ ADA + AE"` (sin mediación).
#' @return Un resultado `ps_resultado` con el ajuste de ambos modelos, la
#'   prueba de diferencia y su lectura.
#' @examples
#' \donttest{
#' base <- ps_preparar(ps_ejemplo(), escalas = c(BERQ = "1-5", EAP = "1-4", ASS = "1-3"))
#' m <- ps_modelo(base, "AE ~ ADA; SAT ~ ADA + AE",
#'                ADA = "BERQ1, BERQ6, BERQ11 | BERQ3, BERQ8, BERQ13 | BERQ4, BERQ9, BERQ14", AE = "EAP", SAT = "ASS")
#' ps_comparar(m, "AE ~ ADA; SAT ~ AE")
#' }
#' @export
ps_comparar <- function(m, rival) {
  .comprobar_modelo(m)
  pr <- .indicadores(m$base, m$defs); ord <- pr$ordinales
  se <- .sintaxis_estructural(.partes(rival), m$covariables)
  fr <- lavaan::sem(paste(.sintaxis_medida(pr$ind), se$sintaxis, sep = "\n"), data = pr$datos,
                    ordered = if (length(ord)) ord else NULL, estimator = m$estimador, std.lv = TRUE,
                    missing = if (length(ord)) "pairwise" else "ml")
  a1 <- .ajuste(m$fit); a2 <- .ajuste(fr)
  dif <- tryCatch(lavaan::lavTestLRT(m$fit, fr), error = function(e) NULL)
  fila <- function(nm, f, a) data.frame(Modelo = nm, Ecuaciones = gsub("\n", "; ", f),
                                        "\u03c7\u00b2 (gl)" = sprintf("%s (%d)", .f2(a["chi2"]), as.integer(a["gl"])),
                                        CFI = .f3(a["cfi"]), RMSEA = .f3(a["rmsea"]), check.names = FALSE)
  tabla <- rbind(fila("Propuesto", m$modelo, a1), fila("Rival", rival, a2))
  tabla[["\u0394\u03c7\u00b2 (gl), p"]] <- ""
  if (!is.null(dif) && nrow(dif) == 2 && !is.na(dif[2, "Pr(>Chisq)"]))
    tabla[2, "\u0394\u03c7\u00b2 (gl), p"] <- sprintf("%s (%d), p %s", .f2(dif[2, "Chisq diff"]), as.integer(dif[2, "Df diff"]), .fpe(dif[2, "Pr(>Chisq)"]))
  dcfi <- a2["cfi"] - a1["cfi"]
  p <- if (!is.null(dif)) dif[2, "Pr(>Chisq)"] else NA
  notas <- list(.nota(2, "\u0394\u03c7\u00b2 (gl), p", if (abs(a1["gl"] - a2["gl"]) < .5)
    "Mismos grados de libertad: los modelos son EQUIVALENTES (ajustan igual a cualquier dato). Los datos no pueden elegir entre ellos; se decide por la teor\u00eda. Un rival \u00fatil quita una ruta (por ejemplo, mediaci\u00f3n total sin el efecto directo)." else if (is.na(p))
    "Los modelos no est\u00e1n anidados: comp\u00e1ralos por su ajuste (CFI y RMSEA) y por la teor\u00eda." else if (p < .05)
    sprintf("p %s: el modelo rival ajusta PEOR (\u0394CFI = %s). Se conserva el propuesto.", .fpe(p), .f3(dcfi)) else
    sprintf("p %s: el rival, m\u00e1s simple, ajusta igual (\u0394CFI = %s). Por parsimonia, la ruta que lo distingue no es necesaria.", .fpe(p), .f3(dcfi))))
  .resultado(tabla, "Modelo propuesto frente a un rival (ps_comparar)", notas, rival = fr)
}

#' Comparación por grupos: invarianza y multigrupo (paso 9)
#'
#' Primero comprueba que la medición funciona igual en los grupos
#' (invarianza: configural, umbrales y cargas; criterio: Delta CFI no menor que
#' -.010 y Delta RMSEA no mayor que .015). Si se sostiene, compara si las
#' relaciones estructurales (los beta) son iguales en los grupos. Solo se
#' realiza si cada grupo tiene al menos `minimo` participantes.
#'
#' @param m El resultado de [ps_modelo()].
#' @param grupo Columna de la base con los grupos, por ejemplo `"sexo"`.
#' @param minimo Participantes mínimos por grupo.
#' @return Un resultado `ps_resultado` con la secuencia de modelos, sus
#'   cambios de ajuste y la lectura; en `betas`, los coeficientes por grupo.
#' @examples
#' \donttest{
#' base <- ps_preparar(ps_ejemplo(), escalas = c(BERQ = "1-5", EAP = "1-4", ASS = "1-3"))
#' m <- ps_modelo(base, "SAT ~ ADA + AE",
#'                ADA = "BERQ1, BERQ6, BERQ11 | BERQ3, BERQ8, BERQ13 | BERQ4, BERQ9, BERQ14", AE = "EAP", SAT = "ASS")
#' ps_grupos(m, "sexo")
#' }
#' @export
ps_grupos <- function(m, grupo, minimo = 100) {
  .comprobar_modelo(m)
  pr <- .indicadores(m$base, m$defs); d <- pr$datos; ord <- pr$ordinales
  if (!grupo %in% names(d)) stop("La columna \"", grupo, "\" no est\u00e1 en la base.", call. = FALSE)
  d <- d[!is.na(d[[grupo]]), ]; d[[grupo]] <- as.character(d[[grupo]])
  ng <- table(d[[grupo]])
  if (min(ng) < minimo) {
    tabla <- data.frame(Grupo = names(ng), n = as.integer(ng), check.names = FALSE)
    return(.resultado(tabla, sprintf("Comparaci\u00f3n por %s (ps_grupos)", grupo), list(.nota(which.min(ng), "n", sprintf(
      "El grupo %s tiene %d participantes (< %d): la invarianza y el multigrupo no son confiables con tan pocos casos. NO se realiza; se declara en Resultados y en Limitaciones.",
      names(ng)[which.min(ng)], min(ng), minimo)))))
  }
  base_args <- list(data = d, group = grupo, ordered = if (length(ord)) ord else NULL, estimator = m$estimador,
                    std.lv = TRUE, missing = if (length(ord)) "pairwise" else "ml")
  if (length(ord)) base_args$parameterization <- "theta"
  ajustar <- function(sint, eq) tryCatch(do.call(lavaan::cfa, c(list(model = sint, group.equal = eq), base_args)),
                                         error = function(e) NULL)
  sm <- .sintaxis_medida(pr$ind)
  pasos <- list(Configural = character(), Umbrales = if (length(ord)) "thresholds" else "intercepts",
                Cargas = c(if (length(ord)) "thresholds" else "intercepts", "loadings"))
  fits <- lapply(pasos, function(eq) ajustar(sm, eq))
  se <- .sintaxis_estructural(m$partes, m$covariables)
  sint_e <- paste(sm, gsub("b_[^*]+\\*", "", se$sintaxis), sep = "\n")
  sint_e <- paste(grep(":=", strsplit(sint_e, "\n")[[1]], value = TRUE, invert = TRUE), collapse = "\n")
  # con [<- list() un modelo que no converge queda como NULL en su lugar (con $<- se borraría)
  fits["\u03b2 libres"] <- list(ajustar(sint_e, pasos$Cargas))
  fits["\u03b2 iguales"] <- list(ajustar(sint_e, c(pasos$Cargas, "regressions")))
  A <- lapply(fits, function(f) if (is.null(f)) rep(NA, 9) else .ajuste(f))
  tabla <- data.frame(Modelo = names(fits),
                      "\u03c7\u00b2 (gl)" = vapply(A, function(a) if (is.na(a[1])) "no convergi\u00f3" else sprintf("%s (%d)", .f2(a["chi2"]), as.integer(a["gl"])), ""),
                      CFI = vapply(A, function(a) .f3(a["cfi"]), ""), RMSEA = vapply(A, function(a) .f3(a["rmsea"]), ""),
                      "\u0394CFI" = "", "\u0394RMSEA" = "", check.names = FALSE)
  tabla[["\u0394\u03c7\u00b2 (gl), p"]] <- ""
  comp <- c(NA, 1, 2, NA, 4)
  pdif <- rep(NA, length(fits))
  for (i in which(!is.na(comp))) if (!is.na(A[[i]][1]) && !is.na(A[[comp[i]]][1])) {
    tabla[i, "\u0394CFI"] <- .f3(A[[i]]["cfi"] - A[[comp[i]]]["cfi"])
    tabla[i, "\u0394RMSEA"] <- .f3(A[[i]]["rmsea"] - A[[comp[i]]]["rmsea"])
    dl <- tryCatch(suppressWarnings(lavaan::lavTestLRT(fits[[comp[i]]], fits[[i]])), error = function(e) NULL)
    if (!is.null(dl) && !is.na(dl[2, "Pr(>Chisq)"])) {
      pdif[i] <- dl[2, "Pr(>Chisq)"]
      tabla[i, "\u0394\u03c7\u00b2 (gl), p"] <- sprintf("%s (%d), p %s", .f2(dl[2, "Chisq diff"]), as.integer(dl[2, "Df diff"]), .fpe(pdif[i]))
    }
  }
  # invarianza por ΔCFI y ΔRMSEA (el Δχ² es muy sensible al tamaño de muestra); los β, además, por Δχ²
  inv <- function(i) !is.na(A[[i]][1]) && (A[[i]]["cfi"] - A[[comp[i]]]["cfi"]) >= -.010 && (A[[i]]["rmsea"] - A[[comp[i]]]["rmsea"]) <= .015 &&
    (i != 5 || is.na(pdif[5]) || pdif[5] >= .05)
  inv_ok <- inv(2) && inv(3)
  notas <- list(.nota(3, "\u0394CFI", sprintf("Grupos: %s. %s", paste(sprintf("%s (n = %d)", names(ng), ng), collapse = ", "), if (inv_ok)
    "\u0394CFI \u2265 -.010 y \u0394RMSEA \u2264 .015 en umbrales y cargas: la medici\u00f3n funciona igual en los grupos y sus puntajes se pueden comparar." else
    "La invarianza no se sostiene en todos los pasos: las comparaciones entre grupos deben leerse con cautela.")))
  if (!is.na(A[[5]][1])) notas[[2]] <- .nota(5, "\u0394\u03c7\u00b2 (gl), p", if (inv(5))
    "Igualar los \u03b2 entre grupos no empeora el ajuste (\u0394\u03c7\u00b2 con p \u2265 .05): las relaciones son las mismas en ambos grupos." else
    "Igualar los \u03b2 empeora el ajuste: al menos una relaci\u00f3n cambia seg\u00fan el grupo (moderaci\u00f3n); mira la tabla de \u03b2 por grupo.")
  betas <- NULL
  if (!is.null(fits[[4]])) {
    sg <- lavaan::standardizedSolution(fits[[4]])
    sg <- sg[sg$op == "~" & sg$rhs %in% m$lat, ]
    gl <- lavaan::lavInspect(fits[[4]], "group.label")
    bt <- stats::reshape(data.frame(Ruta = paste(sg$lhs, "\u2190", sg$rhs), g = gl[sg$group], b = .f2(sg$est.std)),
                         idvar = "Ruta", timevar = "g", direction = "wide")
    names(bt) <- sub("^b\\.", "\u03b2 ", names(bt))
    betas <- .resultado(bt, sprintf("\u03b2 por %s (modelo con \u03b2 libres)", grupo), list())
  }
  .resultado(tabla, sprintf("Invarianza y multigrupo por %s (ps_grupos)", grupo), notas, betas = betas)
}

#' Análisis de sensibilidad (paso 10)
#'
#' Repite las hipótesis con otras decisiones para ver si la conclusión se
#' mantiene: el mismo modelo tratando los ítems como continuos (MLR), sin los
#' participantes con respuestas planas, y una regresión con los puntajes
#' observados (media de los ítems) con errores robustos HC3.
#'
#' @param m El resultado de [ps_modelo()].
#' @return Un resultado `ps_resultado` con el beta de cada hipótesis en cada
#'   análisis y su lectura.
#' @examples
#' \donttest{
#' base <- ps_preparar(ps_ejemplo(), escalas = c(BERQ = "1-5", EAP = "1-4", ASS = "1-3"))
#' m <- ps_modelo(base, "SAT ~ ADA + AE",
#'                ADA = "BERQ1, BERQ6, BERQ11 | BERQ3, BERQ8, BERQ13 | BERQ4, BERQ9, BERQ14", AE = "EAP", SAT = "ASS")
#' ps_sensibilidad(m)
#' }
#' @export
ps_sensibilidad <- function(m) {
  .comprobar_modelo(m)
  pr <- .indicadores(m$base, m$defs); d <- pr$datos; ord <- pr$ordinales
  sint <- paste(.sintaxis_medida(pr$ind), .sintaxis_estructural(m$partes, m$covariables)$sintaxis, sep = "\n")
  betas <- function(f) { s <- lavaan::standardizedSolution(f); s <- s[s$op == "~" & s$rhs %in% m$lat, ]
    stats::setNames(s$est.std, paste(s$lhs, "\u2190", s$rhs)) }
  b0 <- betas(m$fit)
  f_mlr <- tryCatch(lavaan::sem(sint, data = d, estimator = "MLR", std.lv = TRUE, missing = "ml"), error = function(e) NULL)
  dp <- d[!d$respuesta_plana, ]
  f_pl <- tryCatch(lavaan::sem(sint, data = dp, ordered = if (length(ord)) ord else NULL, estimator = m$estimador,
                               std.lv = TRUE, missing = if (length(ord)) "pairwise" else "ml"), error = function(e) NULL)
  # Regresión con puntajes observados estandarizados y errores HC3
  P <- as.data.frame(scale(sapply(m$lat, function(f) rowMeans(d[, .definir(m$defs[[f]], names(d))$items, drop = FALSE], na.rm = TRUE))))
  b_obs <- c(); s_obs <- c()
  for (p in m$partes) {
    fo <- stats::lm(stats::reformulate(p$x, p$y), data = P)
    X <- stats::model.matrix(fo); e <- stats::residuals(fo); h <- stats::hatvalues(fo); XtXi <- solve(crossprod(X))
    V <- XtXi %*% crossprod(X * (e / (1 - h))) %*% XtXi
    b_obs[paste(p$y, "\u2190", p$x)] <- stats::coef(fo)[p$x]
    s_obs[paste(p$y, "\u2190", p$x)] <- sqrt(diag(V))[p$x]
  }
  get <- function(f) if (is.null(f)) rep(NA, length(b0)) else betas(f)[names(b0)]
  bm <- get(f_mlr); bp <- get(f_pl)
  tabla <- data.frame(Ruta = names(b0), "Principal" = .f2(b0), "MLR (\u00edtems continuos)" = .f2(bm),
                      "Sin respuestas planas" = .f2(bp), "Puntajes observados (HC3)" = .f2(b_obs[names(b0)]), check.names = FALSE)
  M <- cbind(b0, bm, bp, b_obs[names(b0)])
  mismo <- all(apply(M, 1, function(r) length(unique(sign(r[!is.na(r)]))) == 1))
  ib <- which.max(abs(b0))
  notas <- list(
    .nota(ib, "Puntajes observados (HC3)", sprintf("Con puntajes observados el \u03b2 de %s es %s, %s que en el modelo latente (%s): el error de medida aten\u00faa las relaciones observadas.",
      names(b0)[ib], .f2(b_obs[names(b0)][ib]), if (abs(b_obs[names(b0)][ib]) < abs(b0[ib])) "menor" else "mayor", .f2(b0[ib]))),
    .nota(nrow(tabla), "Principal", if (mismo)
      "Todas las variantes mantienen el signo de cada \u03b2: la conclusi\u00f3n no depende de una sola decisi\u00f3n." else
      "Alguna variante cambia el signo de un \u03b2: esa conclusi\u00f3n depende de decisiones del an\u00e1lisis; inf\u00f3rmalo."))
  .resultado(tabla, "An\u00e1lisis de sensibilidad (ps_sensibilidad)", notas)
}


# Potencia para detectar un beta en un modelo de regresión con variables
# latentes: devuelve una función de N. Predictores no correlacionados, criterio
# con varianza 1 y todas las cargas iguales (como semPower.powerRegression).
.potencia_regresion <- function(criterio, predictores, beta, carga, alfa) {
  k <- length(predictores); ni <- c(criterio, predictores); nf <- k + 1
  Phi <- diag(nf); Phi[1, -1] <- Phi[-1, 1] <- beta
  fin <- cumsum(ni); ini <- fin - ni + 1
  Lambda <- matrix(0, sum(ni), nf)
  for (j in seq_len(nf)) Lambda[ini[j]:fin[j], j] <- carga
  Sigma <- Lambda %*% Phi %*% t(Lambda); diag(Sigma) <- 1
  x <- paste0("x", seq_len(sum(ni))); dimnames(Sigma) <- list(x, x)
  f <- paste0("f", seq_len(nf))
  med <- vapply(seq_len(nf), function(j) paste0(f[j], " =~ ", paste(x[ini[j]:fin[j]], collapse = " + ")), "")
  reg <- paste0("f1 ~ 0*f2", if (k > 1) paste0(" + ", paste(f[3:nf], collapse = " + ")))
  h0 <- lavaan::sem(c(med, reg), sample.cov = Sigma, sample.nobs = 1000, std.lv = TRUE, likelihood = "wishart")
  F0 <- 2 * unname(lavaan::fitMeasures(h0, "fmin"))
  critico <- stats::qchisq(1 - alfa, 1)
  function(N) 1 - stats::pchisq(critico, 1, ncp = (N - 1) * F0)
}
