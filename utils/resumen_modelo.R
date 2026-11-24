# resumen_modelo(m, nombre) — pasa un modelo lm() a una tabla con sus
# coeficientes, errores estándar ROBUSTOS (HC1), p-valores, R² y n.
#
# Usamos errores robustos porque el test de Breusch-Pagan rechaza la
# homocedasticidad (ver 07_regresion.R). Necesita los paquetes lmtest y
# sandwich cargados.

resumen_modelo <- function(m, nombre) {
  ct <- coeftest(m, vcov = vcovHC(m, type = "HC1"))
  tibble(
    modelo      = nombre,
    termino     = rownames(ct),
    coef        = ct[, 1],
    ee_robusto  = ct[, 2],
    p_valor     = ct[, 4],
    r2          = summary(m)$r.squared,
    r2_ajustado = summary(m)$adj.r.squared,
    n           = nobs(m)
  )
}
