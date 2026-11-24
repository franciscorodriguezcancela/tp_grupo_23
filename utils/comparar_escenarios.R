# comparar_escenarios(d, etiqueta) — resume en una fila la media y el desvío de
# las variables principales para un escenario de limpieza (base completa, sin
# atípicos, sin faltantes). Sirve para armar la tabla de descriptivas antes y
# después del tratamiento de faltantes y outliers.
#
# Devuelve un tibble de una fila: escenario, cantidad de países y, para nivel,
# cv, sustitución y log del PBI per cápita, su media y su desvío.

comparar_escenarios <- function(d, etiqueta) {
  d |>
    summarise(
      escenario = etiqueta,
      paises    = n(),
      across(all_of(c("nivel", "cv", "sustitucion", "log_gdp_per_capita")),
             list(media = ~ mean(.x, na.rm = TRUE),
                  sd    = ~ sd(.x, na.rm = TRUE)),
             .names = "{.col}_{.fn}")
    )
}
