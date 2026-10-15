# PredSEM

Modelos de ecuaciones estructurales **sencillos** para estudios predictivos o
explicativos con variables latentes. Pocas funciones, una por paso del
análisis; cada una imprime su tabla y una sección **"Cómo se lee"** con la
interpretación de los valores clave.

## Instalación

```r
install.packages("remotes")
remotes::install_github("jventural/PredSEM")
```

## Dos ideas para escribir el modelo

**1. Cada variable latente se define con un texto corto**

| Texto | Significa |
|---|---|
| `"ASS"` | todos los ítems ASS1, ASS2, … |
| `"EPA2:EPA5, EPA8:EPA12"` | rangos e ítems sueltos |
| `"BERQ1, BERQ6, BERQ11 \| BERQ3, BERQ8, BERQ13"` | parcelas: cada tramo separado por `\|` es el promedio de sus ítems |

**2. Las hipótesis se escriben como fórmula**

```r
"SAT ~ ADA + AE"                   # dos predictores
"AE ~ ADA; SAT ~ ADA + AE"         # AE es mediadora: el efecto indirecto se calcula solo
```

## El análisis completo

```r
library(PredSEM)

# Paso 2 · Limpiar: rangos, ítems inversos, criterios y respuestas planas
base <- ps_preparar(ps_ejemplo(), escalas = c(BERQ = "1-5", EAP = "1-4", ASS = "1-3"),
                    criterios = c("Consentimiento" = "consentimiento", "Edad 18 o más" = "edad >= 18"))
base

ps_muestra(criterio = 6, predictores = c(3, 2, 9), n = nrow(base))  # Paso 1 · tamaño de muestra
ps_items(base, "ASS")                                               # % por alternativa
ps_medida(base, SAT = "ASS")                                        # Paso 3 · AFC y fiabilidad

ADA <- "BERQ1, BERQ6, BERQ11, BERQ16 | BERQ3, BERQ8, BERQ13, BERQ18 | BERQ4, BERQ9, BERQ14, BERQ19"
DES <- "BERQ2, BERQ7, BERQ12, BERQ17 | BERQ5, BERQ10, BERQ15, BERQ20"
ps_descriptivos(base, ADA = ADA, DES = DES, AE = "EAP", SAT = "ASS")  # Paso 4

m <- ps_modelo(base, "AE ~ ADA + DES; SAT ~ ADA + DES + AE",          # Pasos 5 a 7
               ADA = ADA, DES = DES, AE = "EAP", SAT = "ASS")
m          # ajuste, validez discriminante, hipótesis y mediación
plot(m)    # diagrama con los beta

ps_comparar(m, "AE ~ ADA + DES; SAT ~ AE")   # Paso 8 · rival (mediación total)
ps_grupos(m, "sexo")                         # Paso 9 · invarianza y multigrupo
ps_sensibilidad(m)                           # Paso 10 · ¿cambia la conclusión?
```

## Funciones

| Función | Paso | Qué hace |
|---|---|---|
| `ps_preparar()` | 2 | limpieza, rangos, inversos, criterios en orden, respuestas planas |
| `ps_muestra()` | 1 | N necesario con `semPower` para el mismo modelo |
| `ps_items()` | — | porcentaje de cada alternativa por ítem |
| `ps_medida()` | 3 | AFC (WLSMV con ítems; MLR con parcelas), cargas, omega y alfa |
| `ps_descriptivos()` | 4 | descriptivos y correlaciones de los puntajes |
| `ps_modelo()` | 5-7 | medida conjunta, validez discriminante, beta, R² y mediación |
| `ps_comparar()` | 8 | modelo propuesto frente a un rival |
| `ps_grupos()` | 9 | invarianza (configural, umbrales, cargas) y multigrupo |
| `ps_sensibilidad()` | 10 | MLR, sin respuestas planas, puntajes observados con HC3 |
| `ps_ejemplo()` | — | ruta de la base simulada incluida |

La base de ejemplo es **simulada** (ficticia): no corresponde a ninguna persona.

## Autor

José Ventura-León
