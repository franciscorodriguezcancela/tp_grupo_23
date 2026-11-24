# guardar_grafico(g, nombre, carpeta, ancho, alto) — exporta un ggplot con
# ggsave() siempre con la misma resolución y fondo blanco, para no repetir
# los mismos argumentos en cada gráfico.
#
# ancho y alto están en centímetros; dpi = 300 para que se lea bien en la
# presentación.

guardar_grafico <- function(g, nombre, carpeta, ancho = 24, alto = 14) {
  ggsave(file.path(carpeta, nombre), g,
         width = ancho, height = alto, units = "cm", dpi = 300, bg = "white")
}
