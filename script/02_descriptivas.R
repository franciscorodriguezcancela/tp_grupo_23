# =============================================================================
# 02 Descriptivas — estadísticas descriptivas y exploración inicial
# TP Ciencia de Datos para Economía y Negocios (FCE-UBA) — Grupo 23
#
# Descriptivas de las variables principales, tablas de frecuencia, forma de las
# distribuciones y las dos exploraciones que estructuran el argumento:
# qué ocupaciones están más expuestas y cómo se reparte la exposición dentro
# de cada país.
#
# Entradas: input/base_limpia.rds, input/pais.rds
# Salidas:  output/tablas/descriptivas_*.csv, frecuencia_region.csv, top10/bottom10_ocupaciones.csv
# =============================================================================

library(here)
library(tidyverse)
library(skimr)

source(here("utils", "reset_par.R"))

# Rutas relativas a la raíz del proyecto: here() las resuelve desde cualquier
# computadora, sin setwd() y sin importar desde qué carpeta se abra R.
dir_raw   <- here("raw")
dir_aux   <- here("auxiliar")
dir_input <- here("input")
dir_tab   <- here("output", "tablas")
dir_graf  <- here("output", "graficos")

base <- readRDS(file.path(dir_input, "base_limpia.rds"))
pais <- readRDS(file.path(dir_input, "pais.rds"))

# 5. DESCRIPTIVAS UNIVARIADAS
# =============================================================================

summary(base$mean_exposure)
summary(pais$nivel)

base |>
  select(mean_exposure, task_count, sub_only_share, both_share,
         aug_only_share, neither_share) |>
  skim()

pais |>
  select(nivel, sd_ocup, cv, p90_p10, gdp_per_capita, internet_users_pct) |>
  skim()

mean(base$mean_exposure) - median(base$mean_exposure)
mean(pais$nivel)         - median(pais$nivel)

vars_ocupacion <- c("mean_exposure", "task_count", "sustitucion_total",
                    "aumentacion_total", "neither_share")

descriptivas_ocupacion <- base |>
  summarise(across(
    all_of(vars_ocupacion),
    list(
      n       = ~ sum(!is.na(.x)),
      media   = ~ mean(.x, na.rm = TRUE),
      mediana = ~ median(.x, na.rm = TRUE),
      sd      = ~ sd(.x, na.rm = TRUE),
      cv      = ~ sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE),
      min     = ~ min(.x, na.rm = TRUE),
      max     = ~ max(.x, na.rm = TRUE),
      iqr     = ~ IQR(.x, na.rm = TRUE)
    ),
    .names = "{.col}__{.fn}"
  )) |>
  pivot_longer(everything(),
               names_to = c("variable", "estadistico"),
               names_sep = "__") |>
  pivot_wider(names_from = estadistico, values_from = value) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

descriptivas_ocupacion

# Lo mismo a nivel país
vars_pais <- c("nivel", "sd_ocup", "cv", "p90_p10",
               "gdp_per_capita", "internet_users_pct")

descriptivas_pais <- pais |>
  summarise(across(
    all_of(vars_pais),
    list(
      n       = ~ sum(!is.na(.x)),
      media   = ~ mean(.x, na.rm = TRUE),
      mediana = ~ median(.x, na.rm = TRUE),
      sd      = ~ sd(.x, na.rm = TRUE),
      min     = ~ min(.x, na.rm = TRUE),
      max     = ~ max(.x, na.rm = TRUE)
    ),
    .names = "{.col}__{.fn}"
  )) |>
  pivot_longer(everything(),
               names_to = c("variable", "estadistico"),
               names_sep = "__") |>
  pivot_wider(names_from = estadistico, values_from = value) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

descriptivas_pais

quantile(base$mean_exposure, probs = seq(0, 1, 0.10))
quantile(pais$nivel,         probs = c(0.10, 0.25, 0.50, 0.75, 0.90))

# Ratio p90/p10 del nivel país: cuántas veces más expuesto está el país del
# percentil 90 respecto del percentil 10. Es la medida de desigualdad más usada
# en economía aplicada y el número más comunicable que vamos a tener.
p <- quantile(pais$nivel, probs = c(0.10, 0.50, 0.90))
c(p10 = p[[1]], p50 = p[[2]], p90 = p[[3]],
  ratio_90_10 = p[[3]] / p[[1]])

# =============================================================================
# 6. TABLAS DE FRECUENCIA
# =============================================================================

table(pais$region)
table(pais$ingreso, useNA = "ifany")   # useNA muestra los NA como categoría

round(prop.table(table(pais$ingreso)) * 100, 1)

frecuencia_region <- pais |>
  count(region, name = "paises") |>
  mutate(porcentaje = round(100 * paises / sum(paises), 1)) |>
  arrange(desc(paises))

frecuencia_region

pais <- pais |>
  mutate(
    nivel_exposicion = cut(
      nivel,
      breaks = c(-Inf, 0.75, 1.15, Inf),
      labels = c("Baja (<0,75)", "Media (0,75-1,15)", "Alta (>1,15)")
    )
  )

table(pais$nivel_exposicion)

tabla_cruzada <- table(pais$ingreso, pais$nivel_exposicion)
addmargins(tabla_cruzada)

round(prop.table(tabla_cruzada, margin = 1) * 100, 1)   # dentro de cada ingreso

# =============================================================================
# 7. FORMA DE LA DISTRIBUCIÓN
# =============================================================================

hist(
  base$mean_exposure,
  breaks = 30,
  main = "Exposición a la automatización, país x ocupación",
  xlab = "Exposición media de la ocupación (0 a 3)",
  ylab = "Cantidad de casos",
  col  = "steelblue", border = "white"
)
abline(v = mean(base$mean_exposure),   col = "blue",      lwd = 2, lty = 2)
abline(v = median(base$mean_exposure), col = "darkgreen", lwd = 2, lty = 3)
legend("topright", legend = c("Media", "Mediana"),
       col = c("blue", "darkgreen"), lwd = 2, lty = c(2, 3), bty = "n")


par(mfrow = c(2, 2))
for (b in c(5, 15, 30, 60)) {
  hist(base$mean_exposure, breaks = b,
       main = paste("breaks =", b),
       xlab = "Exposición (0 a 3)", ylab = "Casos",
       col = "grey80", border = "white")
}
reset_par()

hist(
  pais$nivel,
  breaks = 20,
  main = "Exposición media por país",
  xlab = "Exposición media ponderada (0 a 3)",
  ylab = "Cantidad de países",
  col  = "steelblue", border = "white"
)
rug(pais$nivel)   # marca cada país sobre el eje

par(mar = c(9, 4, 4, 2))   # más margen abajo para las etiquetas
boxplot(
  nivel ~ ingreso,
  data = pais,
  varwidth = TRUE,   # ancho proporcional a la raíz del n: se ve qué grupo tiene pocos casos
  main = "Exposición media según grupo de ingreso",
  xlab = "", ylab = "Exposición media (0 a 3)",
  col = c("#d73027", "#fc8d59", "#91bfdb", "#4575b4", "grey80"),
  las = 2
)
reset_par()


orden <- pais |>
  group_by(region) |>
  summarise(m = median(nivel)) |>
  arrange(m) |>
  pull(region) |>
  as.character()

pais <- pais |> mutate(region_ord = factor(region, levels = orden))

par(mar = c(13, 4, 4, 2))
boxplot(
  nivel ~ region_ord,
  data = pais,
  main = "Exposición media por región (ordenado por mediana)",
  xlab = "", ylab = "Exposición media (0 a 3)",
  col = "lightsteelblue", las = 2, cex.axis = 0.8
)
reset_par()

# =============================================================================


# 8. PREGUNTA EJE B: ¿QUÉ OCUPACIONES, Y SON LAS MISMAS EN TODOS LADOS?
# =============================================================================

ocupaciones <- base |>
  group_by(onet_soc_code, occupation_title) |>
  summarise(
    exposicion_media = mean(mean_exposure),
    sd_entre_paises  = sd(mean_exposure),   # cuánto varía esa ocupación entre países
    tareas           = first(task_count),
    .groups = "drop"
  )

top10    <- ocupaciones |> slice_max(exposicion_media, n = 10)
bottom10 <- ocupaciones |> slice_min(exposicion_media, n = 10)

top10
bottom10

par(mar = c(4, 18, 4, 2))
barplot(
  rev(top10$exposicion_media),
  names.arg = rev(top10$occupation_title),
  horiz = TRUE, las = 1, cex.names = 0.7,
  xlim = c(0, 3),
  col = "#d73027", border = "white",
  main = "Las 10 ocupaciones más expuestas (promedio de 124 países)",
  xlab = "Exposición media (0 a 3)"
)
reset_par()

par(mar = c(4, 18, 4, 2))
barplot(
  rev(bottom10$exposicion_media),
  names.arg = rev(bottom10$occupation_title),
  horiz = TRUE, las = 1, cex.names = 0.7,
  xlim = c(0, 3),
  col = "#4575b4", border = "white",
  main = "Las 10 ocupaciones menos expuestas (promedio de 124 países)",
  xlab = "Exposición media (0 a 3)"
)
reset_par()

comparacion <- base |>
  filter(ingreso %in% c("Ingreso alto", "Ingreso bajo")) |>
  group_by(onet_soc_code, occupation_title, ingreso) |>
  summarise(exposicion = mean(mean_exposure), .groups = "drop") |>
  pivot_wider(names_from = ingreso, values_from = exposicion) |>
  rename(alto = `Ingreso alto`, bajo = `Ingreso bajo`) |>
  mutate(brecha = alto - bajo)

comparacion |> summarise(media_alto = mean(alto), media_bajo = mean(bajo))

cor(comparacion$alto, comparacion$bajo, method = "spearman")
cor(comparacion$alto, comparacion$bajo, method = "pearson")

plot(
  comparacion$alto, comparacion$bajo,
  pch = 19, col = rgb(0.2, 0.4, 0.7, 0.4),
  xlim = c(0, 3), ylim = c(0, 3),
  main = "La misma ocupación en países ricos y pobres",
  xlab = "Exposición media en países de ingreso alto",
  ylab = "Exposición media en países de ingreso bajo"
)
abline(0, 1, col = "grey50", lwd = 2, lty = 2)   # línea de 45 grados
abline(lm(bajo ~ alto, data = comparacion), col = "darkred", lwd = 2)
legend("topleft",
       legend = c("Igualdad (45°)", "Ajuste lineal"),
       col = c("grey50", "darkred"), lwd = 2, lty = c(2, 1), bty = "n")


comparacion |> slice_max(brecha, n = 10) |> select(occupation_title, alto, bajo, brecha)
comparacion |> slice_min(brecha, n = 10) |> select(occupation_title, alto, bajo, brecha)


ocupaciones |> slice_max(sd_entre_paises, n = 10) |> select(occupation_title, exposicion_media, sd_entre_paises)
ocupaciones |> slice_min(sd_entre_paises, n = 10) |> select(occupation_title, exposicion_media, sd_entre_paises)

# =============================================================================
# 9. PREGUNTA EJE C: NIVEL CONTRA DISPERSIÓN INTERNA
# =============================================================================

plot(
  pais$nivel, pais$sd_ocup,
  pch = 19, col = rgb(0.2, 0.4, 0.7, 0.6),
  main = "Nivel de exposición y dispersión entre ocupaciones",
  xlab = "Exposición media del país (0 a 3)",
  ylab = "Desvío estándar entre ocupaciones"
)
abline(lm(sd_ocup ~ nivel, data = pais), col = "darkred", lwd = 2)
legend("topleft",
       legend = paste("r =", round(cor(pais$nivel, pais$sd_ocup), 2)),
       bty = "n")


plot(
  pais$nivel, pais$cv,
  pch = 19, col = rgb(0.8, 0.2, 0.2, 0.6),
  main = "Nivel de exposición y dispersión RELATIVA",
  xlab = "Exposición media del país (0 a 3)",
  ylab = "Coeficiente de variación entre ocupaciones"
)
abline(lm(cv ~ nivel, data = pais), col = "darkblue", lwd = 2)
legend("topright",
       legend = paste("r =", round(cor(pais$nivel, pais$cv), 2)),
       bty = "n")


dispersion_por_ingreso <- pais |>
  group_by(ingreso) |>
  summarise(
    paises       = n(),
    nivel_mediano = round(median(nivel), 3),
    sd_mediano    = round(median(sd_ocup), 3),
    cv_mediano    = round(median(cv), 3),
    p90_p10       = round(median(p90_p10), 3),
    .groups = "drop"
  )

dispersion_por_ingreso


datos_cor <- pais |>
  select(nivel, sd_ocup, cv, p90_p10, log_gdp_per_capita, internet_users_pct)

mat_cor <- cor(datos_cor, use = "pairwise.complete.obs")
round(mat_cor, 2)

mat_spearman <- cor(datos_cor, use = "pairwise.complete.obs", method = "spearman")
round(mat_spearman - mat_cor, 2)


plot(
  pais$log_gdp_per_capita, pais$nivel,
  pch = 19, col = rgb(0.2, 0.4, 0.7, 0.6),
  main = "Desarrollo económico y exposición a la automatización",
  xlab = "log del PBI per cápita", ylab = "Exposición media (0 a 3)"
)
abline(lm(nivel ~ log_gdp_per_capita, data = pais), col = "darkred", lwd = 2)


modelo <- lm(nivel ~ log_gdp_per_capita, data = pais)
pais_resid <- pais |>
  mutate(residuo = round(residuals(modelo), 3)) |>
  select(country_name, ingreso, nivel, log_gdp_per_capita, residuo)

pais_resid |> slice_max(residuo, n = 8)    # más expuestos de lo esperado
pais_resid |> slice_min(residuo, n = 8)    # menos expuestos de lo esperado
pais_resid |> filter(country_name == "Argentina")

# =============================================================================


# =============================================================================
# 10. GUARDADO
# =============================================================================

write_csv(descriptivas_ocupacion, file.path(dir_tab, "descriptivas_ocupacion.csv"))
write_csv(descriptivas_pais,      file.path(dir_tab, "descriptivas_pais.csv"))
write_csv(frecuencia_region,      file.path(dir_tab, "frecuencia_region.csv"))
write_csv(top10,                  file.path(dir_tab, "top10_ocupaciones.csv"))
write_csv(bottom10,               file.path(dir_tab, "bottom10_ocupaciones.csv"))
write_csv(dispersion_por_ingreso, file.path(dir_tab, "dispersion_por_ingreso.csv"))

# La tabla de ocupaciones y la comparación entre grupos de ingreso las usa
# después el script de gráficos.
saveRDS(ocupaciones,  file.path(dir_input, "ocupaciones.rds"))
saveRDS(comparacion,  file.path(dir_input, "comparacion_ingreso.rds"))
