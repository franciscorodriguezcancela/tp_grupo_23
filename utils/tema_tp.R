# tema_tp() — tema de ggplot común a todos los gráficos del trabajo, para que
# la presentación tenga una sola identidad visual.
#
# base_size define el tamaño de fuente base: subirlo en los gráficos que van a
# quedar chicos en la diapositiva.

tema_tp <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", size = base_size * 1.15,
                                   margin = margin(b = 6)),
      plot.subtitle = element_text(size = base_size * 0.85, color = "grey30",
                                   lineheight = 1.25, margin = margin(b = 12)),
      plot.caption  = element_text(size = base_size * 0.72, color = "grey40",
                                   hjust = 0, margin = margin(t = 12)),
      axis.title    = element_text(size = base_size * 0.88, color = "grey25"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
      legend.position  = "right"
    )
}

# Paletas del trabajo, para no repetir los códigos de color en cada script.
COLORES_INGRESO <- c("Ingreso bajo"       = "#D73027",
                     "Ingreso medio-bajo" = "#FC8D59",
                     "Ingreso medio-alto" = "#91BFDB",
                     "Ingreso alto"       = "#4575B4",
                     "Sin clasificar"     = "grey60")

COLORES_CLUSTER <- c("A: baja exposición"       = "#D73027",
                     "B: exposición intermedia" = "#FDAE61",
                     "C: alta exposición"       = "#4575B4")

FUENTE_TP <- "Fuente: elaboración propia en base a Global Automation Atlas (Garg, Crosta y Baier, 2026)."

# Para los gráficos que usan PBI per cápita, uso de internet o grupo de
# ingreso: esos datos son del Banco Mundial, incluidos en el Atlas.
FUENTE_TP_BM <- "Fuente: elaboración propia en base a Global Automation Atlas (Garg, Crosta y Baier, 2026) y Banco Mundial."
