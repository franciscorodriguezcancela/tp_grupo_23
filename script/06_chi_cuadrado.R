# =============================================================================
# 06 Chi_cuadrado — método 3 (unidad 2): chi cuadrado de independencia
# TP Ciencia de Datos para Economía y Negocios (FCE-UBA) — Grupo 23
#
# Los grupos del clustering se construyeron SOLO con variables de exposición:
# el ingreso del país nunca entró. Que después coincidan con el grupo de
# ingreso del Banco Mundial es, entonces, una validación externa y no una
# consecuencia del armado. Este test le pone un p-valor a esa coincidencia.
#
# Va después del clustering porque usa los grupos que el clustering produjo, y
# antes de la regresión siguiendo el orden jerárquico de la consigna.
#
# Entradas: input/pais_clusters.rds
# Salidas:  output/tablas/{chi_cuadrado_tabla, chi_cuadrado_residuos}.csv
# =============================================================================

library(here)
library(tidyverse)

source(here("utils", "reset_par.R"))

dir_raw   <- here("raw")
dir_aux   <- here("auxiliar")
dir_input <- here("input")
dir_tab   <- here("output", "tablas")
dir_graf  <- here("output", "graficos")

pais <- readRDS(file.path(dir_input, "pais_clusters.rds"))


# =============================================================================
# 1. LA TABLA DE CONTINGENCIA
# =============================================================================
# H0: el grupo de exposición y el grupo de ingreso son independientes.
# H1: están asociados.
#
# Dejamos afuera a los 2 países "Sin clasificar": no son un faltante aleatorio
# sino países que el Banco Mundial no clasifica, y con 2 casos esa categoría
# no aporta y sí rompe las frecuencias esperadas.

datos_chi <- pais |>
  filter(ingreso != "Sin clasificar") |>
  mutate(ingreso = droplevels(ingreso))

tabla <- table(datos_chi$ingreso, datos_chi$cluster3)
addmargins(tabla)

# Perfil de filas: dado un grupo de ingreso, cómo se reparten los países
round(prop.table(tabla, margin = 1) * 100, 1)

write_csv(as.data.frame.matrix(tabla) |> rownames_to_column("ingreso"),
          file.path(dir_tab, "chi_cuadrado_tabla.csv"))


# =============================================================================
# 2. SUPUESTOS
# =============================================================================
# El chi cuadrado de Pearson exige observaciones independientes (cada país
# aparece una sola vez, se cumple) y frecuencias esperadas suficientes: la
# regla habitual pide que ninguna celda esté por debajo de 1 y que no más del
# 20% esté por debajo de 5.

test_inicial <- chisq.test(tabla)
round(test_inicial$expected, 1)

mean(test_inicial$expected < 5) * 100   # porcentaje de celdas por debajo de 5
min(test_inicial$expected)

# En nuestra corrida, 3 de las 12 celdas (25%) quedan por debajo de 5, apenas
# por encima del límite tolerado. El supuesto NO se cumple del todo, así que
# no usamos el p-valor asintótico.


# =============================================================================
# 3. EL TEST, CON EL SUPUESTO CORREGIDO
# =============================================================================
# Solución: p-valor por simulación de Monte Carlo. En vez de apoyarse en la
# distribución teórica, genera 10.000 tablas al azar con los mismos márgenes y
# ve en qué proporción de ellas el estadístico es tan extremo como el
# observado. No necesita frecuencias esperadas mínimas.

set.seed(23)
test <- chisq.test(tabla, simulate.p.value = TRUE, B = 10000)
test

test_inicial    # se reporta también el asintótico, para mostrar que coinciden

# Tamaño del efecto: V de Cramér. El chi cuadrado crece con el n, así que el
# p-valor solo dice si hay asociación, no cuán fuerte es.
v_cramer <- sqrt(as.numeric(test_inicial$statistic) /
                   (sum(tabla) * (min(dim(tabla)) - 1)))
v_cramer

# Referencia habitual para gl = 2: 0,07 chico; 0,21 mediano; 0,35 grande.


# =============================================================================
# 4. DE DÓNDE VIENE LA ASOCIACIÓN
# =============================================================================
# Los residuos estandarizados dicen qué celdas se apartan de la independencia.
# Por encima de |2| la celda tiene bastante más (o bastante menos) casos de los
# que habría si las dos variables fueran independientes.

residuos <- as.data.frame.table(round(test_inicial$stdres, 2)) |>
  rename(ingreso = Var1, grupo = Var2, residuo = Freq) |>
  arrange(desc(abs(residuo)))

residuos
write_csv(residuos, file.path(dir_tab, "chi_cuadrado_residuos.csv"))


# =============================================================================
# 5. INTERPRETACIÓN Y LIMITACIONES
# =============================================================================
# Resultado esperado: chi cuadrado alrededor de 147 con 6 grados de libertad,
# p < 0,001 por las dos vías, y V de Cramér cerca de 0,78, que es una
# asociación muy fuerte.
#
# Qué significa para el trabajo: los tres grupos que salieron de las variables
# de exposición reproducen casi exactamente la escalera de ingresos del Banco
# Mundial. Los 41 países de ingreso alto caen todos en el grupo de alta
# exposición y los de ingreso bajo, salvo uno, en el de baja. Como el ingreso
# nunca entró al clustering, esto es evidencia a favor de que el perfil de
# automatización de un país y su nivel de desarrollo describen el mismo
# fenómeno.
#
# Limitaciones, y son importantes:
# - El test dice que hay asociación, no que el ingreso cause la exposición ni
#   al revés. El trabajo no es causal.
# - Los grupos no son un dato observado sino el resultado de un algoritmo que
#   corrimos nosotros. Un k distinto daría otra tabla. Por eso el resultado
#   acompaña al clustering y no se sostiene solo.
# - Al sacar los 2 países sin clasificar el test habla de 122 países, no de 124.
