# =============================================================================
# 03 Faltantes_outliers — diagnóstico y tratamiento de faltantes y atípicos
# TP Ciencia de Datos para Economía y Negocios (FCE-UBA) — Grupo 23
#
# Detecta los faltantes y los outliers, documenta la decisión tomada sobre cada
# uno y compara las estadísticas descriptivas antes y después del tratamiento.
#
# Entradas: input/pais.rds
# Salidas:  input/pais_completo.rds, output/tablas/{descriptivas_pais_e2, outliers, antes_despues}.csv
# =============================================================================

library(here)
library(tidyverse)

source(here("utils", "reset_par.R"))
source(here("utils", "es_outlier.R"))
source(here("utils", "comparar_escenarios.R"))

dir_raw   <- here("raw")
dir_aux   <- here("auxiliar")
dir_input <- here("input")
dir_tab   <- here("output", "tablas")
dir_graf  <- here("output", "graficos")

pais <- readRDS(file.path(dir_input, "pais.rds"))

# =============================================================================
# 1. TABLA DE DESCRIPTIVAS (pedido de la devolución de la entrega 1)
# =============================================================================

vars_desc <- c("nivel", "cv", "sustitucion", "aumentacion",
               "gdp_per_capita", "log_gdp_per_capita", "internet_users_pct")

descriptivas <- pais |>
  summarise(across(
    all_of(vars_desc),
    list(
      n       = ~ sum(!is.na(.x)),
      na      = ~ sum(is.na(.x)),
      media   = ~ mean(.x, na.rm = TRUE),
      mediana = ~ median(.x, na.rm = TRUE),
      sd      = ~ sd(.x, na.rm = TRUE),
      min     = ~ min(.x, na.rm = TRUE),
      max     = ~ max(.x, na.rm = TRUE)
    ),
    .names = "{.col}__{.fn}"
  )) |>
  pivot_longer(everything(), names_to = c("variable", "estadistico"), names_sep = "__") |>
  pivot_wider(names_from = estadistico, values_from = value) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

descriptivas

# Las mismas medidas pero por grupo de ingreso (medias)
descriptivas_ingreso <- pais |>
  group_by(ingreso) |>
  summarise(
    paises      = n(),
    nivel       = mean(nivel),
    cv          = mean(cv),
    sustitucion = mean(sustitucion),
    aumentacion = mean(aumentacion),
    pbi_pc      = mean(gdp_per_capita),
    internet    = mean(internet_users_pct, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

descriptivas_ingreso

write_csv(descriptivas,         file.path(dir_tab, "descriptivas_pais_e2.csv"))
write_csv(descriptivas_ingreso, file.path(dir_tab, "descriptivas_por_ingreso.csv"))

# =============================================================================


# =============================================================================
# 2. VALORES FALTANTES
# =============================================================================

# 2.1 Cuántos hay en las variables que usamos
faltantes <- pais |>
  summarise(across(all_of(c(vars_desc, "ingreso")), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "faltantes")

faltantes   # solo internet_users_pct tiene 2

# 2.2 Dónde están: qué países
pais |>
  filter(is.na(internet_users_pct)) |>
  select(country_name, ingreso, gdp_per_capita, nivel)
# Turkmenistán y Venezuela. No se concentran en una región ni en un nivel de
# exposición: uno está en 0,95 y el otro en 1,12, cerca de la media (1,15).

# Además hay 2 países sin grupo de ingreso del Banco Mundial ("Sin clasificar"):
pais |> filter(ingreso == "Sin clasificar") |> select(country_name, gdp_per_capita, nivel)
# Etiopía y Venezuela. No es un NA de la base sino una categoría del Banco
# Mundial (no publica la clasificación de esos países).

# 2.3 Decisión
# - internet_users_pct: los 2 países quedan afuera SOLO de la regresión con
#   internet (122 de 124). El clustering no usa internet, así que ahí entran
#   los 124. Son el 1,6% de la muestra y no se
#   concentran en un grupo. No imputamos: es un corte transversal, no hay año
#   anterior que usar, y imputar con la media del grupo inventaría justo la
#   variable que queremos estudiar.
# - "Sin clasificar": queda como está. En la regresión no se usa el grupo de
#   ingreso sino el PBI per cápita, que sí tienen. En las tablas por grupo
#   se muestran aparte.

pais_completo <- pais |> filter(!is.na(internet_users_pct))
nrow(pais_completo)   # 122

# =============================================================================


# =============================================================================
# 3. OUTLIERS
# =============================================================================

# Criterio: regla del boxplot (1,5 x rango intercuartílico). La usamos porque
# no supone normalidad y varias variables son asimétricas (PBI per cápita).

outliers <- map_dfr(vars_desc, function(v) {
  o <- es_outlier(pais[[v]])
  tibble(
    variable = v,
    cantidad = sum(o, na.rm = TRUE),
    paises   = paste(pais$country_name[which(o)], collapse = ", ")
  )
})

outliers
# - nivel (la variable dependiente): ninguno.
# - gdp_per_capita: 9 países ricos (EE.UU., Suiza, Noruega, Irlanda, Qatar...).
#   Son valores genuinos, no errores. Con el logaritmo desaparecen (0 outliers),
#   así que en la regresión usamos log(PBI per cápita).
# - cv: 4 países de ingreso bajo (Burundi, Chad, R.D. Congo, Sudán del Sur).
#   También genuinos: tienen exposición muy baja y concentrada en pocas
#   ocupaciones, que es justo uno de los hallazgos de la entrega 1.
# Decisión: no eliminamos ninguno. Para la regresión revisamos además si hay
# observaciones influyentes (distancia de Cook) y la corremos con y sin ellas.

write_csv(outliers, file.path(dir_tab, "outliers.csv"))


# =============================================================================


# =============================================================================
# 4. DESCRIPTIVAS ANTES Y DESPUÉS DEL TRATAMIENTO
# =============================================================================
# Como decidimos NO eliminar ningún país, lo que mostramos es que esa
# decisión no distorsiona las descriptivas: comparamos la base completa (124)
# contra la que excluiría a los atípicos de cv y de PBI per cápita.

atipicos <- es_outlier(pais$cv) | es_outlier(pais$gdp_per_capita)
sum(atipicos, na.rm = TRUE)

antes_despues <- bind_rows(
  comparar_escenarios(pais,                        "Base completa (124 países)"),
  comparar_escenarios(pais[!atipicos, ],           "Sin atípicos de cv y PBI"),
  comparar_escenarios(pais[!is.na(pais$internet_users_pct), ], "Sin faltantes de internet (122)")
) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

antes_despues
write_csv(antes_despues, file.path(dir_tab, "antes_despues.csv"))

# Lectura: sacar los 13 atípicos movería la media del nivel de exposición menos
# de 0,05 puntos sobre una escala de 0 a 3, pero recortaría el rango de la
# variable justo en los dos extremos que el trabajo quiere describir. Por eso
# los conservamos: el costo de dejarlos es mínimo y el de sacarlos es perder
# los casos más informativos.

# Guardamos la base usada por la regresión (sin los 2 faltantes de internet).
saveRDS(pais_completo, file.path(dir_input, "pais_completo.rds"))
