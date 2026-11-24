# =============================================================================
# 01 Limpieza — carga, control de calidad y construcción de la base de análisis
# TP Ciencia de Datos para Economía y Negocios (FCE-UBA) — Grupo 23
#
#   -Lee los datos crudos
#   -Verifica la llave y los tipos
#   -Descarta las columnas vacías y redundantes
#   -Construye las variables derivadas y deja guardadas las dos tablas de análisis.
#
# Entradas: raw/country-occupation-panel.csv, auxiliar/countries.csv
# Salidas:  input/base_limpia.rds, input/pais.rds, output/tablas/diccionario_variables.csv
# =============================================================================

library(here)
library(tidyverse)

source(here("utils", "reset_par.R"))

# Rutas relativas a la raíz del proyecto: here() las resuelve desde cualquier
# computadora, sin setwd() y sin importar desde qué carpeta se abra R.
dir_raw   <- here("raw")
dir_aux   <- here("auxiliar")
dir_input <- here("input")
dir_tab   <- here("output", "tablas")
dir_graf  <- here("output", "graficos")

# 1. CARGA DE DATOS Y DICCIONARIO DE VARIABLES
# =============================================================================


ruta_ocupaciones <- file.path(dir_raw, "country-occupation-panel.csv")
ruta_paises <- file.path(dir_aux, "countries.csv")

occ_raw <- read_csv(ruta_ocupaciones, show_col_types = FALSE)
paises_raw <- read_csv(ruta_paises, show_col_types = FALSE)

diccionario <- tribble(
  ~variable,            ~descripcion,                                                      ~tipo,        ~unidad,           ~rango,
  "iso3",               "Código ISO de 3 letras del país (parte de la llave)",             "Categórica", "-",               "124 países",
  "country_name",       "Nombre del país",                                                 "Categórica", "-",               "124 categorías",
  "onet_soc_code",      "Código O*NET-SOC de la ocupación (parte de la llave)",            "Categórica", "-",               "923 ocupaciones",
  "occupation_title",   "Nombre de la ocupación",                                          "Categórica", "-",               "923 categorías",
  "task_count",         "Cantidad de tareas O*NET que componen la ocupación",              "Numérica",   "tareas",          "5 a 54",
  "mean_exposure",      "Exposición media a la automatización de las tareas de la ocup.",  "Numérica",   "nivel 0 a 3",     "0 a 3",
  "sub_only_share",     "Proporción de tareas solo sustituibles (la IA reemplaza)",        "Numérica",   "proporción",      "0 a 1",
  "both_share",         "Proporción de tareas con sustitución y aumentación a la vez",     "Numérica",   "proporción",      "0 a 1",
  "aug_only_share",     "Proporción de tareas solo aumentables (la IA asiste)",            "Numérica",   "proporción",      "0 a 1",
  "neither_share",      "Proporción de tareas sin señal de sustitución ni aumentación",    "Numérica",   "proporción",      "0 a 1",
  "region",             "Región geográfica (Banco Mundial). Viene de countries.csv",       "Categórica", "-",               "7 categorías",
  "ingreso",            "Grupo de ingreso del país (Banco Mundial). De countries.csv",     "Ordinal",    "-",               "5 categorías",
  "gdp_per_capita",     "PBI per cápita del país. De countries.csv",                       "Numérica",   "US$ corrientes",  "219 a 112.895",
  "internet_users_pct", "Población del país que usa internet. De countries.csv",           "Numérica",   "% de población",  "0 a 100"
)

print(diccionario, n = Inf)

# =============================================================================
# 2. PRIMER VISTAZO: DIMENSIONES, LLAVE, TIPOS Y FALTANTES
# =============================================================================

dim(occ_raw)      # filas y columnas de la base principal
names(occ_raw)

glimpse(occ_raw)

dim(paises_raw)   # base auxiliar
names(paises_raw)

glimpse(paises_raw)

sum(duplicated(occ_raw[, c("iso3", "onet_soc_code")]))

n_distinct(occ_raw$iso3)             # 124 países
n_distinct(occ_raw$onet_soc_code)    # 923 ocupaciones
n_distinct(occ_raw$iso3) * n_distinct(occ_raw$onet_soc_code) == nrow(occ_raw)

occ_raw |> 
  count(iso3) |> 
  count(n, name = "cantidad_de_paises")

tipos <- tibble(
  variable = names(occ_raw),
  tipo_r   = sapply(occ_raw, function(x) class(x)[1])
)
tipos

colSums(is.na(occ_raw))

faltantes_paises <- paises_raw |>
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
  filter(pct_na > 0) |>
  arrange(desc(pct_na))

faltantes_paises

# =============================================================================
# 3. CHEQUEOS DE HIGIENE PROPIOS DE ESTA BASE
# =============================================================================

suma_margenes <- occ_raw |>
  mutate(suma = sub_only_share + both_share + aug_only_share + neither_share) |>
  summarise(min = min(suma), max = max(suma))

suma_margenes

occ_raw |>
  summarise(
    dif_sustitucion = max(abs(substitution_share - sub_only_share)),
    dif_aumentacion = max(abs(augmentation_share - aug_only_share))
  )

occ_raw |>
  summarise(
    exp_min   = min(mean_exposure),   exp_max   = max(mean_exposure),
    tareas_min = min(task_count),     tareas_max = max(task_count)
  )

occ_raw |>
  distinct(onet_soc_code, task_count) |>
  count(onet_soc_code) |>
  filter(n > 1) |>
  nrow()

setdiff(unique(occ_raw$iso3), paises_raw$iso3)
setdiff(paises_raw$iso3, unique(occ_raw$iso3))

# =============================================================================
# 4. BASE DE TRABAJO Y VARIABLES DERIVADAS
# =============================================================================

atributos_pais <- paises_raw |>
  select(
    iso3,
    region,
    ingreso = income_level,
    gdp_per_capita,
    log_gdp_per_capita,
    internet_users_pct
  ) |>
  mutate(
    # Categorías de ingreso ordenadas de menor a mayor: importa para que los
    # boxplots y las tablas salgan en un orden legible.
    ingreso = factor(
      ingreso,
      levels = c("Low income", "Lower middle income",
                 "Upper middle income", "High income", "Not classified"),
      labels = c("Ingreso bajo", "Ingreso medio-bajo",
                 "Ingreso medio-alto", "Ingreso alto", "Sin clasificar")
    ),
    region = factor(region)
  )

base <- occ_raw |>
  select(-substitution_share, -augmentation_share) |>   # redundantes (3.2)
  left_join(atributos_pais, by = "iso3") |>
  mutate(
    # La medida que el diccionario sugería: tareas con ALGÚN componente de
    # sustitución, incluyendo las que además admiten aumentación.
    sustitucion_total = sub_only_share + both_share,
    aumentacion_total = aug_only_share + both_share,
    # Proporción expuesta de la ocupación (lo contrario de "ninguna señal")
    expuesto_share    = 1 - neither_share
  )

glimpse(base)
nrow(base)

pais <- base |>
  group_by(iso3, country_name, region, ingreso,
           gdp_per_capita, log_gdp_per_capita, internet_users_pct) |>
  summarise(
    n_ocupaciones = n(),
    nivel   = weighted.mean(mean_exposure, w = task_count),  # exposición media
    sd_ocup = sd(mean_exposure),                             # dispersión absoluta
    p10     = quantile(mean_exposure, 0.10),
    p90     = quantile(mean_exposure, 0.90),
    .groups = "drop"
  ) |>
  mutate(
    cv      = sd_ocup / nivel,   # dispersión RELATIVA, comparable entre países
    p90_p10 = p90 - p10
  )

# Composición del trabajo expuesto: qué parte se inclina a la sustitución y
# qué parte a la aumentación.
# Se agrega igual que el nivel, ponderando por la cantidad de tareas de cada ocupación.
# Las usa el clustering del script 05.
composicion <- base |>
  group_by(iso3) |>
  summarise(
    sustitucion = weighted.mean(sustitucion_total, w = task_count),
    aumentacion = weighted.mean(aumentacion_total, w = task_count),
    .groups = "drop"
  )

pais <- pais |> left_join(composicion, by = "iso3")

pais

control <- pais |>
  select(iso3, nivel_calculado = nivel) |>
  left_join(paises_raw |> select(iso3, nivel_atlas = mean_task_exposure),
            by = "iso3") |>
  mutate(diferencia = abs(nivel_calculado - nivel_atlas))

summary(control$diferencia)

# =============================================================================


# =============================================================================
# 5. GUARDADO
# =============================================================================
# Cada script del repositorio es autónomo: guarda lo que produce y el siguiente
# lo lee desde el archivo, sin depender de que este haya corrido en la sesión.

saveRDS(base, file.path(dir_input, "base_limpia.rds"))
saveRDS(pais, file.path(dir_input, "pais.rds"))

write_csv(diccionario, file.path(dir_tab, "diccionario_variables.csv"))
