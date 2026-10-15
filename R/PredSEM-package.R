#' PredSEM: modelos de ecuaciones estructurales sencillos para estudios predictivos
#'
#' Pocas funciones, una por paso del análisis de un estudio predictivo o
#' explicativo con variables latentes. Cada una imprime su tabla y una sección
#' "Cómo se lee" con la interpretación de los valores clave.
#'
#' \itemize{
#'   \item [ps_preparar()]: limpieza, criterios, ítems inversos y respuestas planas (paso 2).
#'   \item [ps_muestra()]: tamaño de muestra para el modelo con semPower (paso 1).
#'   \item [ps_items()]: porcentaje de cada alternativa por ítem.
#'   \item [ps_medida()]: análisis factorial confirmatorio y fiabilidad de cada escala (paso 3).
#'   \item [ps_descriptivos()]: descriptivos y correlaciones de los puntajes (paso 4).
#'   \item [ps_modelo()]: medida conjunta, validez discriminante, hipótesis y mediación (pasos 5 a 7).
#'   \item [ps_comparar()]: modelo propuesto frente a un rival (paso 8).
#'   \item [ps_grupos()]: invarianza y multigrupo (paso 9).
#'   \item [ps_sensibilidad()]: ¿cambia la conclusión? (paso 10).
#'   \item [ps_ejemplo()]: ruta de la base simulada de ejemplo.
#' }
#' @importFrom utils globalVariables
#' @keywords internal
"_PACKAGE"
