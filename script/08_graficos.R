# =============================================================================
# 08 Graficos — todos los gráficos de la presentación
# TP Ciencia de Datos para Economía y Negocios (FCE-UBA) — Grupo 23
#
# Un solo script para todas las visualizaciones, por dos motivos: comparten el
# tema y la paleta (utils/tema_tp.R), y así se regeneran todas juntas cuando
# cambia una decisión de diseño.
#
# Los gráficos cubren los tres momentos que pide la consigna:
#   descriptivas .................. g01, g02
#   faltantes y outliers .......... g03
#   resultados de los métodos ..... g04 a g08
#   instancia 3 ................... grupo_23_exploratorio, grupo_23_comunicativo
#
# Entradas: input/*.rds, output/tablas/descomposicion_varianza.csv
# Salidas:  output/graficos/*.jpg
# =============================================================================

library(here)
library(tidyverse)
library(scales)
library(ggrepel)
library(lmtest)
library(sandwich)

source(here("utils", "tema_tp.R"))
source(here("utils", "guardar_grafico.R"))

dir_raw   <- here("raw")
dir_aux   <- here("auxiliar")
dir_input <- here("input")
dir_tab   <- here("output", "tablas")
dir_graf  <- here("output", "graficos")

base     <- readRDS(file.path(dir_input, "base_limpia.rds"))
pais     <- readRDS(file.path(dir_input, "pais.rds"))
paisc    <- readRDS(file.path(dir_input, "pais_clusters.rds"))
modelos  <- readRDS(file.path(dir_input, "modelos_regresion.rds"))

FUENTE    <- FUENTE_TP
FUENTE_BM <- FUENTE_TP_BM



# =============================================================================
# g01. DESCRIPTIVA: distribución del nivel de exposición entre países
# =============================================================================

g01 <- ggplot(pais, aes(x = nivel)) +
  geom_histogram(binwidth = 0.1, boundary = 0,
                 fill = "#4575B4", color = "white", linewidth = 0.4) +
  geom_vline(xintercept = median(pais$nivel),
             linetype = "dashed", color = "grey25", linewidth = 0.5) +
  annotate("text", x = median(pais$nivel) + 0.03, y = 24, hjust = 0,
           label = paste0("Mediana: ",
                          number(median(pais$nivel), accuracy = 0.01, decimal.mark = ",")),
           size = 3.4, color = "grey25") +
  scale_x_continuous(breaks = seq(0, 2, 0.25),
                     labels = label_number(accuracy = 0.01, decimal.mark = ",")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "La exposición a la automatización no se reparte de forma pareja entre países",
    subtitle = "Exposición media de las tareas de un país, ponderada por la cantidad de tareas de cada ocupación. 124 países.",
    x = "Exposición media del país (escala 0 a 3)",
    y = "Cantidad de países",
    caption = FUENTE
  ) +
  tema_tp()

guardar_grafico(g01, "g01_distribucion_nivel.jpg", dir_graf, 24, 13)


# =============================================================================
# g02. DESCRIPTIVA: exposición por grupo de ingreso
# =============================================================================

g02 <- pais |>
  filter(ingreso != "Sin clasificar") |>
  mutate(ingreso = droplevels(ingreso)) |>
  ggplot(aes(x = ingreso, y = nivel, fill = ingreso)) +
  geom_boxplot(width = 0.55, outlier.color = "grey40", outlier.size = 1.2) +
  scale_fill_manual(values = COLORES_INGRESO, guide = "none") +
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.5),
                     labels = label_number(accuracy = 0.1, decimal.mark = ",")) +
  labs(
    title = "La exposición crece de forma escalonada con el ingreso del país",
    subtitle = "Cada caja resume los países de ese grupo. Se excluyen los 2 países que el Banco Mundial no clasifica.",
    x = "Grupo de ingreso del país",
    y = "Exposición media del país (escala 0 a 3)",
    caption = FUENTE
  ) +
  tema_tp()

guardar_grafico(g02, "g02_nivel_por_ingreso.jpg", dir_graf, 22, 13)


# =============================================================================
# g03. OUTLIERS: el PBI per cápita antes y después del logaritmo
# =============================================================================
# Es el gráfico que justifica la decisión del script 03: en niveles el PBI per
# cápita tiene 9 atípicos por la regla del boxplot, y en logaritmo ninguno.

pbi_largo <- pais |>
  select(country_name,
         `PBI per cápita (USD corrientes)` = gdp_per_capita,
         `Logaritmo natural del PBI per cápita` = log_gdp_per_capita) |>
  pivot_longer(-country_name, names_to = "escala", values_to = "valor")

g03 <- ggplot(pbi_largo, aes(x = "", y = valor)) +
  geom_boxplot(width = 0.35, fill = "#91BFDB",
               outlier.color = "#D73027", outlier.size = 1.6) +
  facet_wrap(~ escala, scales = "free_y") +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",",
                                           accuracy = 1)) +
  labs(
    title = "Los atípicos del PBI per cápita desaparecen al pasar a logaritmos",
    subtitle = "Regla del boxplot (1,5 rangos intercuartílicos). En rojo, los países marcados como atípicos. 124 países.",
    x = NULL,
    y = "Valor de la variable",
    caption = FUENTE_BM
  ) +
  tema_tp() +
  theme(strip.text = element_text(face = "bold", size = 11))

guardar_grafico(g03, "g03_outliers_pbi.jpg", dir_graf, 22, 13)


# =============================================================================
# g04. MÉTODO 1: descomposición de la varianza
# =============================================================================

descomposicion <- read_csv(file.path(dir_tab, "descomposicion_varianza.csv"),
                           show_col_types = FALSE) |>
  filter(fuente != "Total") |>
  mutate(fuente = factor(fuente, levels = c("Entre ocupaciones", "Entre países",
                                            "Interacción (residuo)")))

g04 <- ggplot(descomposicion, aes(x = "", y = porcentaje, fill = fuente)) +
  geom_col(width = 0.4) +
  geom_text(aes(label = paste0(number(porcentaje, accuracy = 0.1, decimal.mark = ","), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  coord_flip() +
  scale_fill_manual(values = c("Entre ocupaciones"     = "#4575B4",
                               "Entre países"          = "#D73027",
                               "Interacción (residuo)" = "grey65"),
                    name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(0)) +
  labs(
    title = "Qué ocupación es y en qué país se ejerce pesan casi lo mismo",
    subtitle = "Reparto de la suma de cuadrados de la exposición entre las 114.452 celdas de país por ocupación.\nLa interacción, muy chica, indica que el efecto país es casi un desplazamiento parejo sobre todas las ocupaciones.",
    x = NULL,
    y = "Porcentaje de la varianza total",
    caption = FUENTE
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +   # mismo orden que la barra
  tema_tp() +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(5.5, 20, 5.5, 5.5))         # que no se corte el 100%

guardar_grafico(g04, "g04_descomposicion_varianza.jpg", dir_graf, 24, 11)


# =============================================================================
# g05. MÉTODO 2: los tres grupos en el plano de componentes principales
# =============================================================================

vars_cluster <- paisc |> select(nivel, cv, sustitucion, aumentacion)
pca  <- prcomp(vars_cluster, scale. = TRUE)
var_pc <- trimws(format(round(100 * summary(pca)$importance[2, 1:2], 1), decimal.mark = ","))

puntos <- as_tibble(pca$x[, 1:2]) |>
  mutate(pais = paisc$country_name,
         grupo = paisc$cluster3,
         distancia = paisc$dist_centro)

arquetipos_graf <- puntos |> group_by(grupo) |> slice_min(distancia, n = 1) |> ungroup() |>
  # los nombres de la base están en inglés; traducimos los que se rotulan
  mutate(pais = recode(pais, "Papua New Guinea" = "Papúa Nueva Guinea",
                       "Libya" = "Libia", "Egypt, Arab Rep." = "Egipto",
                       "Morocco" = "Marruecos", "Mexico" = "México",
                       "Brazil" = "Brasil", "Peru" = "Perú", "Panama" = "Panamá"))

g05 <- ggplot(puntos, aes(x = PC1, y = PC2, color = grupo)) +
  stat_ellipse(type = "norm", linewidth = 0.4, alpha = 0.6, show.legend = FALSE) +
  geom_point(size = 1.9, alpha = 0.85) +
  geom_point(data = arquetipos_graf, size = 3.4, shape = 21,
             fill = "white", stroke = 1.3, show.legend = FALSE) +
  geom_text_repel(data = arquetipos_graf, aes(label = paste("Arquetipo:", pais)),
                  size = 3.3, fontface = "bold", seed = 23,
                  box.padding = 0.6, show.legend = FALSE) +
  scale_color_manual(values = COLORES_CLUSTER, name = "Grupo") +
  labs(
    title = "Los 124 países se ordenan en tres grupos a lo largo de un solo eje",
    subtitle = "k-means sobre cuatro variables de exposición estandarizadas, proyectado en las dos primeras componentes principales.\nEl arquetipo de cada grupo es el país más cercano a su centro, que no es el más extremo.",
    x = paste0("Componente principal 1 (", var_pc[1], "% de la varianza)"),
    y = paste0("Componente principal 2 (", var_pc[2], "% de la varianza)"),
    caption = FUENTE
  ) +
  tema_tp()

guardar_grafico(g05, "g05_clusters_pca.jpg", dir_graf, 24, 15)


# =============================================================================
# g06. MÉTODO 3: la asociación entre los grupos y el ingreso
# =============================================================================
# Es el gráfico del chi cuadrado: muestra la asociación que el test cuantifica.

g06 <- paisc |>
  filter(ingreso != "Sin clasificar") |>
  mutate(ingreso = droplevels(ingreso)) |>
  count(ingreso, cluster3) |>
  group_by(ingreso) |>
  mutate(porcentaje = 100 * n / sum(n)) |>
  ungroup() |>
  ggplot(aes(x = fct_rev(ingreso), y = porcentaje, fill = cluster3)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = if_else(porcentaje >= 8,
                                paste0(round(porcentaje), "%"), "")),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 3.6) +
  coord_flip() +
  scale_fill_manual(values = COLORES_CLUSTER, name = "Grupo de exposición") +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(0)) +
  labs(
    title = "Los grupos de exposición reproducen la escalera de ingresos sin haberla usado",
    subtitle = "Distribución de los países de cada grupo de ingreso entre los tres grupos del clustering. 122 países.\nChi cuadrado = 147,2 (6 gl), p < 0,001 por simulación de Monte Carlo; V de Cramér = 0,78.",
    x = "Grupo de ingreso del país",
    y = "Porcentaje de los países del grupo de ingreso",
    caption = FUENTE_BM
  ) +
  tema_tp() +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank(),
        plot.margin = margin(5.5, 20, 5.5, 5.5))         # que no se corte el 100%

guardar_grafico(g06, "g06_clusters_vs_ingreso.jpg", dir_graf, 24, 13)


# =============================================================================
# g07. MÉTODO 4: la relación que estima la regresión
# =============================================================================

pais_reg <- paisc |> filter(!is.na(internet_users_pct))

g07 <- ggplot(pais_reg, aes(x = internet_users_pct, y = nivel)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = "grey25", fill = "grey85", linewidth = 0.7) +
  geom_point(aes(color = ingreso), size = 2, alpha = 0.85) +
  scale_color_manual(values = COLORES_INGRESO, name = "Grupo de ingreso") +
  scale_x_continuous(limits = c(0, 100), labels = label_percent(scale = 1)) +
  scale_y_continuous(limits = c(0, 2),
                     labels = label_number(accuracy = 0.1, decimal.mark = ",")) +
  labs(
    title = "La conectividad acompaña a la exposición mejor que el ingreso",
    subtitle = "122 países con dato de uso de internet. La recta es el ajuste por mínimos cuadrados; la banda, su intervalo de confianza del 95%.",
    x = "Población que usa internet (% del total)",
    y = "Exposición media del país (escala 0 a 3)",
    caption = FUENTE_BM
  ) +
  tema_tp()

guardar_grafico(g07, "g07_nivel_vs_internet.jpg", dir_graf, 24, 14)


# =============================================================================
# g08. MÉTODO 4: coeficientes estandarizados con errores robustos
# =============================================================================
# Estandarizados para que los dos coeficientes sean comparables entre sí, y con
# errores robustos porque el test de Breusch-Pagan rechazó la homocedasticidad.

ct <- coeftest(modelos$m3_std, vcov. = vcovHC(modelos$m3_std, type = "HC1"))

coeficientes <- tibble(
  termino = rownames(ct),
  coef    = ct[, 1],
  ee      = ct[, 2]
) |>
  filter(termino != "(Intercept)") |>
  # Traducción por vector con nombres y no con recode(): el paquete car, que
  # carga el script 07, define su propio recode() y lo tapa si las dos
  # librerías están cargadas en la misma sesión.
  mutate(termino = unname(c(
    "scale(log_gdp_per_capita)" = "Logaritmo del PBI per cápita",
    "scale(internet_users_pct)" = "Población que usa internet"
  )[termino]))

g08 <- ggplot(coeficientes, aes(x = coef, y = fct_reorder(termino, coef))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey55") +
  geom_pointrange(aes(xmin = coef - 1.96 * ee, xmax = coef + 1.96 * ee),
                  color = "#D73027", linewidth = 0.7, size = 0.7) +
  geom_text(aes(label = number(coef, accuracy = 0.01, decimal.mark = ",")),
            vjust = -1.2, size = 3.5, color = "grey25") +
  scale_x_continuous(labels = label_number(accuracy = 0.1, decimal.mark = ","),
                     expand = expansion(mult = c(0.03, 0.08))) +
  labs(
    title = "Con las dos variables en el mismo modelo, las dos mantienen un efecto propio",
    subtitle = "Coeficientes estandarizados del modelo completo, con errores estándar robustos (HC1) e intervalos del 95%.\nCada punto indica en cuántos desvíos cambia la exposición ante un desvío más de la variable. 122 países.",
    x = "Coeficiente estandarizado (desvíos)",
    y = NULL,
    caption = FUENTE_BM
  ) +
  tema_tp() +
  theme(panel.grid.major.y = element_blank())

guardar_grafico(g08, "g08_coeficientes_regresion.jpg", dir_graf, 24, 11)


# =============================================================================
# g09 y g10. LOS DOS GRÁFICOS DE LA INSTANCIA 3
# =============================================================================
# Se mantienen tal como se entregaron, con las correcciones de la devolución
# incorporadas. En el argumento entran en dos momentos distintos: el
# exploratorio en las descriptivas, cuando se describe la composición
# ocupacional de la exposición, y el comunicativo junto a la descomposición de
# la varianza, porque muestra ocupación por ocupación el "desplazamiento".

# g09. GRÁFICO EXPLORATORIO (instancia 3): familias ocupacionales por ingreso
# =============================================================================

# --- 1.1. Nombres de las familias ocupacionales ------------------------------

# Los dos primeros dígitos del código O*NET-SOC identifican el grupo mayor.
# Los traducimos para que el eje se lea en castellano.

familias <- tribble(
  ~mg,  ~familia,
  "11", "Dirección y gerencia",
  "13", "Negocios y finanzas",
  "15", "Informática y matemática",
  "17", "Arquitectura e ingeniería",
  "19", "Ciencia",
  "21", "Servicios sociales y comunitarios",
  "23", "Derecho",
  "25", "Educación y bibliotecas",
  "27", "Arte, diseño, entretenimiento y medios",
  "29", "Salud: profesionales y técnicos",
  "31", "Salud: personal de apoyo",
  "33", "Servicios de protección",
  "35", "Preparación y servicio de comida",
  "37", "Limpieza y mantenimiento de edificios",
  "39", "Cuidado personal y servicios",
  "41", "Ventas",
  "43", "Apoyo administrativo y de oficina",
  "45", "Agricultura, pesca y actividad forestal",
  "47", "Construcción y extracción",
  "49", "Instalación, mantenimiento y reparación",
  "51", "Producción",
  "53", "Transporte y movimiento de materiales"
)


# --- 1.2. Datos del gráfico --------------------------------------------------

# Dejamos afuera los 2 países sin grupo de ingreso del Banco Mundial.
# Dentro de cada familia promediamos ponderando por la cantidad de tareas de
# cada ocupación: una ocupación de 54 tareas no puede pesar igual que una de 5.

heat <- base |>
  filter(ingreso != "Sin clasificar") |>
  mutate(mg = str_sub(onet_soc_code, 1, 2)) |>
  group_by(mg, ingreso) |>
  summarise(exposicion = weighted.mean(mean_exposure, w = task_count),
            .groups = "drop") |>
  group_by(ingreso) |>
  # Exposición relativa: cociente contra el promedio de las 22 familias DENTRO
  # de ese grupo de ingreso. Al sacar el nivel general, lo que queda es la
  # composición, que es lo que queremos comparar entre grupos.
  mutate(relativa = exposicion / mean(exposicion)) |>
  ungroup() |>
  left_join(familias, by = "mg") |>
  mutate(ingreso = droplevels(ingreso))

# Ordenamos las filas por la exposición relativa en los países de ingreso bajo:
# así la columna de la izquierda queda decreciente y se ve cómo se desarma ese
# orden al moverse hacia la derecha.
orden_familias <- heat |>
  filter(ingreso == "Ingreso bajo") |>
  arrange(relativa) |>
  pull(familia)

heat <- heat |>
  mutate(
    familia = factor(familia, levels = orden_familias),
    # El texto se pone blanco sobre los extremos de color, para que se lea
    color_texto = if_else(relativa > 1.8 | relativa < 0.55, "white", "grey15")
  )


# --- 1.3. El gráfico ---------------------------------------------------------

g_exploratorio <- ggplot(heat, aes(x = ingreso, y = familia, fill = relativa)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = number(exposicion, accuracy = 0.01, decimal.mark = ","),
                color = color_texto),
            size = 3) +
  scale_color_identity() +
  scale_fill_gradient2(
    low = "#4575B4", mid = "#F7F7F7", high = "#D73027", midpoint = 1,
    limits = c(0.35, 2.55),
    breaks = c(0.5, 1, 1.5, 2, 2.5),
    labels = label_number(accuracy = 0.1, decimal.mark = ","),
    name = "Exposición relativa\n(1 = promedio del grupo)"
  ) +
  # Etiquetas en dos renglones: en una sola línea se pisan entre columnas
  scale_x_discrete(position = "top",
                   labels = c("Ingreso\nbajo", "Ingreso\nmedio-bajo",
                              "Ingreso\nmedio-alto", "Ingreso\nalto")) +
  labs(
    title = "¿Dónde se concentra la exposición dentro de cada grupo de países?",
    # ggplot no corta el texto solo, así que los saltos van a mano, pero
    # aprovechando el ancho completo: cada renglón llega hasta el borde.
    subtitle = paste(
      "El número de cada celda es la exposición media en la escala original (0 a 3). 923 ocupaciones agrupadas en 22 familias",
      "O*NET; 122 países con grupo de ingreso asignado. Filas ordenadas por la exposición relativa en los países de ingreso bajo.",
      sep = "\n"
    ),
    x = "Grupo de ingreso del país",
    y = "Familia ocupacional",
    caption = FUENTE
  ) +
  theme_minimal(base_size = 12) +
  theme(
    # title.position = "plot" alinea título y bajada con el borde del gráfico
    # y no con el panel: así usan todo el ancho disponible.
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.title        = element_text(face = "bold", size = 13.5, margin = margin(b = 6)),
    plot.subtitle     = element_text(size = 9.5, color = "grey30", lineheight = 1.25,
                                     margin = margin(b = 12)),
    plot.caption      = element_text(size = 8.5, color = "grey40", hjust = 0,
                                     margin = margin(t = 12)),
    axis.title.x      = element_text(size = 10, color = "grey25", margin = margin(t = 10)),
    axis.title.y      = element_text(size = 10, color = "grey25", margin = margin(r = 10)),
    axis.text.x       = element_text(face = "bold", size = 10, lineheight = 1.05),
    axis.text.y       = element_text(size = 10),
    panel.grid        = element_blank(),
    legend.position   = "right",
    legend.title      = element_text(size = 9, lineheight = 1.1),
    legend.key.height = unit(1.6, "cm"),
    plot.margin       = margin(14, 14, 10, 14)
  )

ggsave(
  filename = file.path(dir_graf, "grupo_23_exploratorio.jpg"),
  plot = g_exploratorio,
  width = 25, height = 19, units = "cm", dpi = 300, bg = "white"
)

# Qué se puede leer acá (no va en el gráfico, es para la defensa oral):
#   - En los países de ingreso bajo la exposición está MUY concentrada: el
#     apoyo administrativo está 2,5 veces por encima del promedio de su grupo.
#   - En los de ingreso alto esa concentración baja a 1,6: la exposición se
#     reparte entre más familias.
#   - Hay familias que cambian de posición: preparación de comida pasa de 0,71
#     a 1,08 veces el promedio, y agricultura de 0,65 a 0,99. Son las tareas
#     que recién se vuelven automatizables cuando hay infraestructura.
#   - Servicios sociales y comunitarios es la familia menos expuesta en los
#     cuatro grupos: no hay contexto en que la IA la alcance.


# =============================================================================


# g10. GRÁFICO COMUNICATIVO (instancia 3): la misma ocupación en ricos y pobres
# =============================================================================

# --- 2.1. Datos --------------------------------------------------------------

ocupaciones <- base |>
  filter(ingreso %in% c("Ingreso alto", "Ingreso bajo")) |>
  group_by(onet_soc_code, occupation_title, ingreso) |>
  summarise(exposicion = mean(mean_exposure), .groups = "drop") |>
  pivot_wider(names_from = ingreso, values_from = exposicion) |>
  rename(alto = `Ingreso alto`, bajo = `Ingreso bajo`)

nrow(ocupaciones)                      # 923 ocupaciones
sum(ocupaciones$bajo < ocupaciones$alto)   # cuántas están por debajo de la línea
median(ocupaciones$bajo / ocupaciones$alto)

# --- 2.2. Las ocupaciones que etiquetamos ------------------------------------

# Pocas y elegidas: una de cada extremo y algunas reconocibles. Si etiquetamos
# muchas, el gráfico deja de leerse en diez segundos.

destacadas <- tribble(
  ~onet_soc_code, ~etiqueta,
  "43-3031.00",   "Empleados de contabilidad",
  "41-2011.00",   "Cajeros",
  "15-1252.00",   "Desarrolladores de software",
  "45-2092.00",   "Trabajadores agrícolas",
  "29-1141.00",   "Enfermeros",
  "21-2011.00",   "Clero"
) |>
  left_join(ocupaciones, by = "onet_soc_code")


# --- 2.3. El gráfico ---------------------------------------------------------

g_comunicativo <- ggplot(ocupaciones, aes(x = alto, y = bajo)) +
  # Sombreado de la mitad vacía: el hueco ES el mensaje
  annotate("polygon", x = c(0, 0, 3), y = c(0, 3, 3),
           fill = "#F3F5F8", color = NA) +
  # Línea de referencia: igual exposición en los dos grupos
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey55", linewidth = 0.5) +
  # El universo, en gris: está pero no compite por la atención
  geom_point(color = "grey78", alpha = 0.5, size = 1.4) +
  # Las pocas ocupaciones que sí queremos que se lean
  geom_point(data = destacadas, color = "#D73027", size = 2.6) +
  geom_text_repel(data = destacadas, aes(label = etiqueta),
                  color = "#A3201A", size = 3.4, fontface = "bold",
                  box.padding = 0.5, point.padding = 0.3,
                  min.segment.length = 0, segment.color = "grey60",
                  seed = 23) +
  # Anotaciones que dirigen la lectura
  annotate("text", x = 2.52, y = 2.70, label = "Igual exposición\nen los dos grupos",
           angle = 45, size = 3.2, color = "grey45", lineheight = 0.95) +
  annotate("text", x = 0.10, y = 2.88, hjust = 0, vjust = 1,
           label = "Esta mitad está vacía.",
           size = 4.4, fontface = "bold", color = "grey20") +
  annotate("text", x = 0.10, y = 2.62, hjust = 0, vjust = 1,
           label = paste("Ninguna de las 923 ocupaciones está más expuesta",
                         "en los países de ingreso bajo que en los de ingreso alto.",
                         sep = "\n"),
           size = 3.5, color = "grey35", lineheight = 1.15) +
  # Clave de lectura: enseña el formato con un punto que ya está etiquetado,
  # para que nadie tenga que explicar qué es la diagonal.
  annotate("text", x = 0.10, y = 2.16, hjust = 0, vjust = 1,
           label = paste("Cómo leerlo: los cajeros llegan a 2,1 en los países de ingreso alto",
                         "y a 1,0 en los de ingreso bajo.",
                         sep = "\n"),
           size = 3.3, color = "grey45", fontface = "italic", lineheight = 1.15) +
  coord_fixed(xlim = c(0, 3), ylim = c(0, 3), expand = FALSE, clip = "off") +
  scale_x_continuous(breaks = 0:3, labels = label_number(accuracy = 1)) +
  scale_y_continuous(breaks = 0:3, labels = label_number(accuracy = 1)) +
  labs(
    title = "El mismo trabajo, mucho menos expuesto en los países pobres",
    # La primera línea define la variable: sin eso, un lector externo no sabe
    # qué es "exposición" ni qué significa un 2,1 en una escala de 0 a 3.
    subtitle = paste(
      "Exposición: qué parte de las tareas de una ocupación podría hacer hoy la tecnología",
      "disponible, en una escala de 0 a 3. Cada punto es una de las 923 ocupaciones del",
      "catálogo O*NET, promediada sobre 41 países de ingreso alto y 19 de ingreso bajo.",
      "La ocupación mediana alcanza, en los de ingreso bajo, el 30% de lo que alcanza en los otros.",
      sep = "\n"
    ),
    x = "Exposición en países de ingreso alto (0 a 3)",
    y = "Exposición en países de ingreso bajo (0 a 3)",
    caption = FUENTE
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.title        = element_text(face = "bold", size = 14, margin = margin(b = 6)),
    plot.subtitle     = element_text(size = 9.5, color = "grey30", lineheight = 1.25,
                                     margin = margin(b = 14)),
    plot.caption      = element_text(size = 8.5, color = "grey40", hjust = 0,
                                     margin = margin(t = 12)),
    axis.title        = element_text(size = 10.5, color = "grey25"),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "grey92", linewidth = 0.3),
    plot.margin       = margin(14, 18, 10, 14)
  )

ggsave(
  filename = file.path(dir_graf, "grupo_23_comunicativo.jpg"),
  plot = g_comunicativo,
  width = 22, height = 22, units = "cm", dpi = 300, bg = "white"
)


# =============================================================================
