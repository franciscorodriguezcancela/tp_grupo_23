# =============================================================================
# 07 Regresion — método 4 (unidad 2): regresión lineal múltiple
# TP Ciencia de Datos para Economía y Negocios (FCE-UBA) — Grupo 23
#
# Último método del recorrido: una vez establecido que los grupos de países se
# asocian al ingreso, mide cuánto de la exposición acompaña al PBI per cápita y
# cuánto a la conectividad, con las dos variables compitiendo en el mismo modelo.
#
# Entradas: input/pais_completo.rds
# Salidas:  output/tablas/regresion.csv, input/modelos_regresion.rds
# =============================================================================

library(here)
library(tidyverse)
library(car)
library(lmtest)
library(sandwich)

source(here("utils", "reset_par.R"))
source(here("utils", "resumen_modelo.R"))

dir_raw   <- here("raw")
dir_aux   <- here("auxiliar")
dir_input <- here("input")
dir_tab   <- here("output", "tablas")
dir_graf  <- here("output", "graficos")

pais_completo <- readRDS(file.path(dir_input, "pais_completo.rds"))

# 4. INFERENCIA: REGRESIÓN LINEAL
# =============================================================================
# Pregunta: ¿la exposición media se asocia al ingreso del país, a su
# conectividad, o a las dos? (pregunta 1 de la entrega 1, propuesta del docente)
#
# H0: el coeficiente de log(PBI pc) es 0 (manteniendo internet constante)
# H1: es distinto de 0. Lo mismo para internet_users_pct.

cor(pais_completo$log_gdp_per_capita, pais_completo$internet_users_pct)   # 0,88: muy correlacionadas

m1 <- lm(nivel ~ log_gdp_per_capita, data = pais_completo)
m2 <- lm(nivel ~ internet_users_pct, data = pais_completo)
m3 <- lm(nivel ~ log_gdp_per_capita + internet_users_pct, data = pais_completo)

summary(m1)
summary(m2)
summary(m3)

# 4.1 Supuestos
vif(m3)                     # multicolinealidad
bptest(m3)                  # heterocedasticidad (Breusch-Pagan)
resettest(m3)               # forma funcional (RESET)
shapiro.test(residuals(m3)) # normalidad de los residuos

par(mfrow = c(2, 2))
plot(m3)       # diagnóstico en pantalla; la versión exportable está en 08
reset_par()

# Como el test de Breusch-Pagan rechaza, reportamos errores estándar robustos
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))

# 4.2 Coeficientes estandarizados: para comparar cuál pesa más
m3_std <- lm(scale(nivel) ~ scale(log_gdp_per_capita) + scale(internet_users_pct),
             data = pais_completo)
coeftest(m3_std, vcov = vcovHC(m3_std, type = "HC1"))

# 4.3 Observaciones influyentes (distancia de Cook > 4/n)
cook <- cooks.distance(m3)
influyentes <- pais_completo$country_name[cook > 4 / nrow(pais_completo)]
influyentes

m3_sin <- lm(nivel ~ log_gdp_per_capita + internet_users_pct,
             data = pais_completo[cook <= 4 / nrow(pais_completo), ])
coeftest(m3_sin, vcov = vcovHC(m3_sin, type = "HC1"))

# 4.4 El test RESET rechaza: puede haber algo de no linealidad (la exposición
# está acotada y la distribución es bimodal). Probamos agregar el cuadrado
# del log(PBI pc) para ver si cambia la conclusión sobre internet.
m4 <- lm(nivel ~ log_gdp_per_capita + I(log_gdp_per_capita^2) + internet_users_pct,
         data = pais_completo)
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))
resettest(m4)

# 4.5 Tabla resumen para la presentación
tabla_regresion <- bind_rows(
  resumen_modelo(m1, "M1: solo PBI"),
  resumen_modelo(m2, "M2: solo internet"),
  resumen_modelo(m3, "M3: ambos"),
  resumen_modelo(m3_sin, "M3 sin influyentes"),
  resumen_modelo(m4, "M4: con log(PBI) al cuadrado")
) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

tabla_regresion
write_csv(tabla_regresion, file.path(dir_tab, "regresion.csv"))


# =============================================================================

# Guardamos los modelos para que el script de gráficos no los vuelva a estimar.
saveRDS(list(m1 = m1, m2 = m2, m3 = m3, m3_std = m3_std, m4 = m4),
        file.path(dir_input, "modelos_regresion.rds"))
