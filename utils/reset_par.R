# reset_par() — devuelve los parámetros gráficos de base R a su estado por
# defecto después de usar par(mfrow) o par(mar).
#
# No usamos par(no.readonly = TRUE) porque guarda también "pin" (el tamaño del
# área de dibujo) y al volver al panel de RStudio ese valor puede ser inválido.

reset_par <- function() {
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
}
