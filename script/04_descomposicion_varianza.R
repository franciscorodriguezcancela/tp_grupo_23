# =============================================================================
# 04 Descomposición_varianza — método 1 (unidad 3): descomposición de la varianza
# TP Ciencia de Datos para Economía y Negocios (FCE-UBA) — Grupo 23
#
# Este es el método que contrasta la hipótesis central de forma directa.
# La exposición de una ocupación en un país puede variar por dos motivos:
# porque esa ocupación es más automatizable que otras, o porque ese país está
# más expuesto que otros. La descomposición separa las dos cosas y les pone
# un número, en lugar de dejarlo en una impresión visual.
#
# Va primero entre los métodos porque establece el fenómeno que los demás
# después describen: el clustering agrupa países y la regresión explica el
# nivel de cada país, y las dos cosas solo tienen sentido si el componente
# país pesa.
#
# Entradas: input/base_limpia.rds
# Salidas:  output/tablas/descomposicion_varianza.csv
# =============================================================================

library(here)
library(tidyverse)

source(here("utils", "reset_par.R"))

dir_raw   <- here("raw")
dir_aux   <- here("auxiliar")
dir_input <- here("input")
dir_tab   <- here("output", "tablas")
dir_graf  <- here("output", "graficos")

base <- readRDS(file.path(dir_input, "base_limpia.rds"))


# =============================================================================
# 1. POR QUÉ ESTE MÉTODO Y QUÉ SUPONE
# =============================================================================
# El panel es balanceado: las mismas 923 ocupaciones en los 124 países, sin
# celdas faltantes. Eso es lo que permite descomponer la suma de cuadrados en
# tres partes ortogonales, sin que el reparto dependa del orden en que se
# calculen:
#
#   SC total = SC entre países + SC entre ocupaciones + SC residual
#
# La parte residual es la interacción: cuánto se desvía cada celda de lo que
# predicen el promedio de su país y el promedio de su ocupación juntos. Si la
# interacción es chica, el efecto país es casi un desplazamiento parejo sobre
# todas las ocupaciones.

n_paises     <- n_distinct(base$iso3)
n_ocupaciones <- n_distinct(base$onet_soc_code)
nrow(base) == n_paises * n_ocupaciones    # TRUE confirma que el panel es balanceado


# =============================================================================
# 2. DESCOMPOSICIÓN
# =============================================================================

media_general <- mean(base$mean_exposure)

medias_pais <- base |>
  group_by(iso3) |>
  summarise(media = mean(mean_exposure), n = n(), .groups = "drop")

medias_ocupacion <- base |>
  group_by(onet_soc_code) |>
  summarise(media = mean(mean_exposure), n = n(), .groups = "drop")

sc_total     <- sum((base$mean_exposure - media_general)^2)
sc_pais      <- sum(medias_pais$n      * (medias_pais$media      - media_general)^2)
sc_ocupacion <- sum(medias_ocupacion$n * (medias_ocupacion$media - media_general)^2)
sc_residual  <- sc_total - sc_pais - sc_ocupacion

# Grados de libertad de un diseño cruzado sin repetición
gl_pais      <- n_paises - 1
gl_ocupacion <- n_ocupaciones - 1
gl_residual  <- nrow(base) - 1 - gl_pais - gl_ocupacion

descomposicion <- tibble(
  fuente = c("Entre países", "Entre ocupaciones", "Interacción (residuo)", "Total"),
  suma_cuadrados = c(sc_pais, sc_ocupacion, sc_residual, sc_total),
  gl = c(gl_pais, gl_ocupacion, gl_residual, nrow(base) - 1)
) |>
  mutate(
    porcentaje     = 100 * suma_cuadrados / sc_total,
    cuadrado_medio = suma_cuadrados / gl,
    across(where(is.numeric), ~ round(.x, 2))
  )

descomposicion
write_csv(descomposicion, file.path(dir_tab, "descomposicion_varianza.csv"))

# Contraste formal: F de cada factor contra el cuadrado medio residual.
cm_residual <- sc_residual / gl_residual
f_pais      <- (sc_pais / gl_pais) / cm_residual
f_ocupacion <- (sc_ocupacion / gl_ocupacion) / cm_residual

c(F_pais      = f_pais,
  p_pais      = pf(f_pais, gl_pais, gl_residual, lower.tail = FALSE),
  F_ocupacion = f_ocupacion,
  p_ocupacion = pf(f_ocupacion, gl_ocupacion, gl_residual, lower.tail = FALSE))

# Supuestos: el estadístico F supone independencia entre celdas, varianza
# constante y residuos normales. Con 114.452 observaciones el F es robusto al
# incumplimiento de la normalidad, pero la independencia es discutible (las
# ocupaciones de una misma familia comparten tareas). Por eso el resultado que
# usamos en el argumento es el REPARTO DE LA VARIANZA, que es descriptivo, y no
# el p-valor: con esta cantidad de datos cualquier diferencia da significativa.


# =============================================================================
# 3. ROBUSTEZ: PONDERANDO POR CANTIDAD DE TAREAS
# =============================================================================
# El promedio de una ocupación de 54 tareas es más preciso que el de una de 5.
# Repetimos la descomposición ponderando cada celda por su cantidad de tareas,
# para ver si el reparto se sostiene.

w  <- base$task_count
mg <- weighted.mean(base$mean_exposure, w)

medias_pais_w <- base |>
  group_by(iso3) |>
  summarise(media = weighted.mean(mean_exposure, task_count), peso = sum(task_count),
            .groups = "drop")

medias_ocup_w <- base |>
  group_by(onet_soc_code) |>
  summarise(media = weighted.mean(mean_exposure, task_count), peso = sum(task_count),
            .groups = "drop")

sct_w <- sum(w * (base$mean_exposure - mg)^2)
scp_w <- sum(medias_pais_w$peso * (medias_pais_w$media - mg)^2)
sco_w <- sum(medias_ocup_w$peso * (medias_ocup_w$media - mg)^2)

round(100 * c(pais = scp_w, ocupacion = sco_w,
              residuo = sct_w - scp_w - sco_w) / sct_w, 1)


# =============================================================================
# 4. INTERPRETACIÓN
# =============================================================================
# En nuestra run el reparto es, aproximadamente:
#   entre ocupaciones .... 49%
#   entre países ......... 44%
#   interacción ...........  7%
#
# Tres lecturas, en orden de importancia para el trabajo:
#
# 1. El país explica casi tanto como la ocupación. Esto MATIZA la hipótesis
#    inicial, que decía que la exposición dependía más del país que de la
#    ocupación: los dos factores pesan parecido. Lo decimos así en la
#    conclusión, en vez de forzar el dato.
#
# 2. La interacción es chiquísima. Eso quiere decir que el efecto país es casi
#    un desplazamiento parejo: un país más expuesto lo está en todas sus
#    ocupaciones más o menos en la misma proporción, y no en unas sí y en otras
#    no. Es la versión numérica de lo que muestra el gráfico comunicativo, donde
#    la nube conserva su forma por debajo de la línea de 45 grados.
#
# 3. Que el componente país sea de ese tamaño es lo que justifica los dos
#    métodos siguientes: si el país no explicara nada, agrupar países
#    (clustering) y modelar su nivel (regresión) no tendría sentido.
#
# Limitaciones: la descomposición describe cómo se reparte la variabilidad
# observada, no por qué. No dice qué hace que un país esté más expuesto, y no
# tiene ninguna lectura causal.
