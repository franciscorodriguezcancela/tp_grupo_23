# =============================================================================
# 05 Clustering — método 2 (unidad 3): agrupamiento de países por k-means
# TP Ciencia de Datos para Economía y Negocios (FCE-UBA) — Grupo 23
#
# Agrupa los 124 países según su perfil de exposición (nivel, dispersión,
# sustitución y aumentación). Las variables de contexto quedan afuera a
# propósito, para poder usarlas como validación externa de los grupos.
#
# Entradas: input/pais.rds
# Salidas:  input/pais_clusters.rds, output/tablas/{comparacion_k, perfil_clusters_k3, arquetipos, paises_por_cluster, cluster3_vs_ingreso}.csv
# =============================================================================

library(here)
library(tidyverse)
library(cluster)
library(factoextra)
library(scales)

source(here("utils", "reset_par.R"))
source(here("utils", "tema_tp.R"))
source(here("utils", "es_outlier.R"))
source(here("utils", "guardar_grafico.R"))

dir_raw   <- here("raw")
dir_aux   <- here("auxiliar")
dir_input <- here("input")
dir_tab   <- here("output", "tablas")
dir_graf  <- here("output", "graficos")

pais <- readRDS(file.path(dir_input, "pais.rds"))

# 5. ANÁLISIS CUANTITATIVO: CLUSTERING DE PAÍSES (k-means)
# =============================================================================
# Pregunta: la distribución del nivel de exposición es bimodal (entrega 1).
# ¿Hay grupos de países bien definidos según su perfil de automatización?
#
# Esta sección fue ampliada después de la devolución: se agrega la solución con
# k = 3, los países arquetípicos de cada grupo y la validación del armado.

# --- 5.1. Armado: qué variables entran y por qué -----------------------------

# Entran SOLO variables de exposición. No metemos PBI ni internet: si los
# metiéramos, los grupos saldrían definidos por el ingreso y después "descubrir"
# que coinciden con el ingreso sería circular. Los dejamos afuera justamente
# para poder usarlos como validación externa (sección 5.6).
#   - nivel:       exposición media del país, ponderada por tareas
#   - cv:          dispersión relativa entre las 923 ocupaciones
#   - sustitucion: proporción de tareas con componente de sustitución
#   - aumentacion: proporción de tareas con componente de aumentación

vars_cluster <- pais |> select(nivel, cv, sustitucion, aumentacion)

round(cor(vars_cluster), 2)
# Las cuatro están muy correlacionadas (|r| > 0,94). Eso no invalida el
# clustering, pero sí quiere decir que casi toda la información es una sola
# dimensión: los grupos se van a ordenar sobre un eje, no a repartirse en el
# plano. Conviene decirlo antes de mostrar el gráfico de componentes.

pca <- prcomp(vars_cluster, scale. = TRUE)
summary(pca)              # proporción de varianza de cada componente
round(pca$rotation[, 1:2], 2)   # qué mide cada componente

# El primer componente concentra casi toda la varianza y carga positivo en
# nivel, sustitución y aumentación, y negativo en cv: es un eje de "exposición
# alta y repartida" contra "exposición baja y concentrada".

# k-means trabaja con distancias euclídeas, así que hay que estandarizar:
# sin estandarizar, cv (que va de 0,3 a 1,1) pesaría muchísimo menos que nivel.
z_cluster <- scale(vars_cluster)
rownames(z_cluster) <- pais$iso3


# --- 5.2. Elegir k -----------------------------------------------------------

# fviz_nbclust() devuelve un ggplot con textos en inglés: los reemplazamos y
# marcamos el k elegido (3) para que el gráfico se lea solo.
set.seed(23)
g_silueta <- fviz_nbclust(z_cluster, kmeans, method = "silhouette", k.max = 8, nstart = 50) +
  geom_vline(xintercept = 3, color = "#D73027", linewidth = 0.6) +
  annotate("text", x = 3.1, y = 0.1, label = "k elegido = 3", hjust = 0,
           color = "#D73027", size = 3.5) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, decimal.mark = ",")) +
  labs(title = "La silueta es máxima con 2 grupos y casi igual con 3",
       subtitle = "Silueta media del k-means según la cantidad de grupos. La línea punteada marca el máximo. 124 países.",
       x = "Cantidad de grupos (k)", y = "Silueta media", caption = FUENTE_TP) +
  tema_tp(11)
guardar_grafico(g_silueta, "cluster_silueta.jpg", dir_graf, 20, 12)

g_codo <- fviz_nbclust(z_cluster, kmeans, method = "wss", k.max = 8, nstart = 50) +
  geom_vline(xintercept = 3, color = "#D73027", linewidth = 0.6) +
  annotate("text", x = 3.1, y = 400, label = "k elegido = 3", hjust = 0,
           color = "#D73027", size = 3.5) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Pasar de 3 a más grupos casi no reduce la variabilidad interna",
       subtitle = "Suma de cuadrados dentro de los grupos según la cantidad de grupos (método del codo). 124 países.",
       x = "Cantidad de grupos (k)", y = "Suma de cuadrados intra-grupo", caption = FUENTE_TP) +
  tema_tp(11)
guardar_grafico(g_codo, "cluster_codo.jpg", dir_graf, 20, 12)

comparacion_k <- map_dfr(2:6, function(k) {
  set.seed(23)
  km <- kmeans(z_cluster, centers = k, nstart = 50)
  tibble(
    k              = k,
    silueta        = mean(silhouette(km$cluster, dist(z_cluster))[, 3]),
    between_total  = km$betweenss / km$totss,
    grupo_min      = min(km$size)
  )
}) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

comparacion_k
write_csv(comparacion_k, file.path(dir_tab, "comparacion_k.csv"))

# Lectura: la silueta es máxima en k = 2 (0,68) y cae poco en k = 3 (0,65),
# mientras que la varianza explicada entre grupos salta de 0,77 a 0,90. De
# k = 4 en adelante la silueta cae fuerte y no se gana casi nada.
# Los dos criterios no coinciden, y eso es normal: la silueta premia
# particiones compactas y separadas, el codo premia ajuste. Con k = 2 la
# partición es más "limpia", con k = 3 describe mejor los datos.
# Decidimos trabajar con k = 3 como especificación principal y dejar k = 2
# como contraste, porque el tercer grupo tiene lectura económica propia
# (sección 5.5) y porque mejora mucho el ajuste a un costo bajo de silueta.


# --- 5.3. Las dos soluciones -------------------------------------------------

# k = 2 (la de la versión anterior, se conserva como referencia)
set.seed(23)
km2 <- kmeans(z_cluster, centers = 2, nstart = 50)
orden2 <- order(tapply(pais$nivel, km2$cluster, mean))
pais$cluster2 <- factor(match(km2$cluster, orden2),
                        labels = c("A: baja exposición", "B: alta exposición"))

# k = 3 (especificación principal)
set.seed(23)
km3 <- kmeans(z_cluster, centers = 3, nstart = 50)
orden3 <- order(tapply(pais$nivel, km3$cluster, mean))
pais$cluster3 <- factor(match(km3$cluster, orden3),
                        labels = c("A: baja exposición",
                                   "B: exposición intermedia",
                                   "C: alta exposición"))

table(pais$cluster2)
table(pais$cluster3)

# ¿De dónde sale el tercer grupo? Cruzamos las dos particiones:
table(k2 = pais$cluster2, k3 = pais$cluster3)

# En nuestra run: el grupo A de k = 2 (45 países) se parte casi al medio
# (22 + 23), y además 8 países que estaban en B bajan al grupo intermedio.
# O sea que la heterogeneidad que resuelve el tercer grupo está sobre todo
# DENTRO del grupo de baja exposición, y en menor medida en el borde inferior
# del de alta. El núcleo del grupo de alta exposición queda intacto.

# Qué países se mueven del grupo alto al intermedio al pasar a k = 3
pais |>
  filter(cluster2 == "B: alta exposición", cluster3 == "B: exposición intermedia") |>
  select(country_name, ingreso, nivel, internet_users_pct) |>
  arrange(nivel)

# ¿Dónde está realmente la heterogeneidad? La devolución suponía que en B.
# Medimos la dispersión interna de cada grupo de k = 2 y su silueta media:
sil2 <- silhouette(as.integer(pais$cluster2), dist(z_cluster))
pais$silueta2 <- sil2[, 3]

pais |>
  group_by(cluster2) |>
  summarise(
    paises        = n(),
    sd_nivel      = round(sd(nivel), 3),
    rango_nivel   = round(max(nivel) - min(nivel), 3),
    silueta_media = round(mean(silueta2), 3),
    .groups = "drop"
  )

# El grupo A es el más disperso (sd 0,21 contra 0,16) y el
# peor definido (silueta 0,55 contra 0,75). O sea que la heterogeneidad que
# hacía difícil interpretar la partición estaba en el grupo de BAJA exposición,
# no en el de alta. Vale la pena decirlo: el tercer grupo mejora la lectura,
# pero por un motivo distinto del que suponíamos.


# --- 5.4. Perfil de cada grupo -----------------------------------------------

perfil_cluster <- pais |>
  group_by(cluster3) |>
  summarise(
    paises         = n(),
    nivel_min      = min(nivel),
    nivel_medio    = mean(nivel),
    nivel_max      = max(nivel),
    cv             = mean(cv),
    sustitucion    = mean(sustitucion),
    aumentacion    = mean(aumentacion),
    # Cociente entre las dos: ¿el trabajo expuesto se inclina más a que la IA
    # reemplace o a que asista? Esto es composición, no nivel.
    sust_sobre_aum = mean(sustitucion) / mean(aumentacion),
    # variables de contexto: NO entraron al clustering
    pbi_pc_mediano = median(gdp_per_capita),
    internet       = mean(internet_users_pct, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

perfil_cluster
write_csv(perfil_cluster, file.path(dir_tab, "perfil_clusters_k3.csv"))

# Los tres grupos forman una escalera en las dos variables que NO usamos para
# construirlos: el PBI per cápita mediano y el uso de internet (23%, 53% y 88%
# de la población). Esa es la validación externa del clustering.
#
# Y los grupos no se diferencian solo por el nivel. El cociente entre
# sustitución y aumentación baja de 1,64 en el grupo A a 1,38 en el C: en los
# países menos expuestos, la poca exposición que hay se inclina relativamente
# más hacia el reemplazo, y en los más expuestos gana terreno la asistencia.
# Es un resultado de composición, que es lo que un clustering tiene para
# aportar por encima de ordenar países en un eje.

# Lista completa de países por grupo
membresia <- pais |>
  select(country_name, ingreso, cluster3, nivel, cv) |>
  arrange(cluster3, desc(nivel)) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

membresia
write_csv(membresia, file.path(dir_tab, "paises_por_cluster.csv"))


# --- 5.5. Países arquetípicos (pedido de la devolución) ----------------------

# Arquetípico = el país más cercano al centro de su grupo en el espacio
# estandarizado. Es el que mejor "representa" al grupo, y no hay que
# confundirlo con el más extremo, que está en el borde.

centros_por_pais <- km3$centers[km3$cluster, , drop = FALSE]
pais$dist_centro <- sqrt(rowSums((z_cluster - centros_por_pais)^2))

arquetipos <- pais |>
  group_by(cluster3) |>
  slice_min(dist_centro, n = 3) |>
  select(cluster3, country_name, ingreso, nivel, cv, sustitucion, dist_centro) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  ungroup()

arquetipos
write_csv(arquetipos, file.path(dir_tab, "arquetipos.csv"))

# Los arquetipos son, aproximadamente:
#   A (baja):        Papúa Nueva Guinea, Guinea, Afganistán
#   B (intermedia):  Libia, Nigeria, Ghana
#   C (alta):        Chile, Qatar, Grecia
# Chile como arquetipo del grupo de alta exposición es un buen ejemplo de por
# qué conviene reportar el arquetipo y no el máximo: el máximo es China, que
# está en el borde del grupo y no lo representa.

# Los extremos de cada grupo, para acompañar al arquetipo
extremos <- pais |>
  group_by(cluster3) |>
  summarise(
    pais_min = country_name[which.min(nivel)],
    pais_max = country_name[which.max(nivel)],
    .groups = "drop"
  )
extremos


# --- 5.6. Validación del armado ----------------------------------------------

# a) Silueta de cada grupo por separado: ¿alguno está mal definido?
sil3 <- silhouette(as.integer(pais$cluster3), dist(z_cluster))
pais$silueta3 <- sil3[, 3]

pais |>
  group_by(cluster3) |>
  summarise(silueta_media = round(mean(silueta3), 3),
            negativas     = sum(silueta3 < 0),
            .groups = "drop")

# El grupo de alta exposición es el más compacto; el intermedio, el más
# difuso, que es lo esperable en un grupo que ocupa el medio de un eje continuo.

# b) Países de frontera: silueta más baja, son los casos discutibles
pais |>
  arrange(silueta3) |>
  select(country_name, ingreso, cluster3, nivel, silueta3) |>
  head(8)

pais |> filter(country_name == "Argentina") |>
  select(country_name, cluster3, nivel, cv, silueta3)

# c) ¿Otro algoritmo da los mismos grupos? Cluster jerárquico de Ward
hc  <- hclust(dist(z_cluster), method = "ward.D2")
hc3 <- cutree(hc, k = 3)
table(kmeans = pais$cluster3, jerarquico = hc3)

# Con 124 países las etiquetas no se leen: las sacamos y dejamos los colores.
g_dend <- fviz_dend(hc, k = 3, show_labels = FALSE, horiz = FALSE,
                    k_colors = unname(COLORES_CLUSTER),
                    labels_track_height = 2.2, main = NULL) +
  labs(title = "Cluster jerárquico de Ward sobre el perfil de exposición",
       subtitle = "124 países; corte en tres grupos. Distancia euclídea sobre las cuatro variables estandarizadas.",
       x = NULL, y = "Distancia (Ward)", caption = FUENTE_TP) +
  tema_tp(11) +
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())
guardar_grafico(g_dend, "dendrograma.jpg", dir_graf, 25, 14)

# d) ¿Cambia si sacamos los 4 outliers de cv?
sin_out_cv <- !es_outlier(pais$cv)
set.seed(23)
km3_sin <- kmeans(scale(vars_cluster[sin_out_cv, ]), centers = 3, nstart = 50)
table(con = pais$cluster3[sin_out_cv], sin = km3_sin$cluster)

# e) ¿Los grupos coinciden con el ingreso? (validación externa)
tabla_cl_ingreso <- table(pais$ingreso, pais$cluster3)
addmargins(tabla_cl_ingreso)
round(prop.table(tabla_cl_ingreso, margin = 2) * 100, 1)

write_csv(as.data.frame.matrix(tabla_cl_ingreso) |> rownames_to_column("ingreso"),
          file.path(dir_tab, "cluster3_vs_ingreso.csv"))

# El grupo intermedio es, básicamente, el escalón de ingreso medio-bajo: ahí
# está la mayoría de esos países. Con k = 2 ese escalón quedaba partido entre
# los dos grupos, y era justamente lo que hacía difícil interpretarlos.


# --- 5.7. Guardado -----------------------------------------------------------
# Los gráficos de los clusters (plano de componentes, composición por ingreso)
# se arman en 08_graficos.R con ggplot, para que todas las visualizaciones del
# trabajo compartan el mismo tema y se exporten con ggsave().

saveRDS(pais, file.path(dir_input, "pais_clusters.rds"))
