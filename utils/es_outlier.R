# es_outlier(x) — marca los valores atípicos según la regla del boxplot:
# más de 1,5 rangos intercuartílicos por fuera de Q1 o Q3.
#
# Elegimos este criterio y no el z-score porque no supone normalidad, y varias
# de nuestras variables son asimétricas (el PBI per cápita, sobre todo).
#
# Devuelve un vector lógico del mismo largo que x. Los NA quedan como NA.

es_outlier <- function(x) {
  q   <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr
}
