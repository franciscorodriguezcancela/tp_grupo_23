# ¿Qué trabajo puede hacer la IA, y en qué países?

Análisis exploratorio y cuantitativo de la exposición del trabajo a la automatización por inteligencia artificial, a partir del Global Automation Atlas.

Trabajo Práctico de Ciencia de Datos para Economía y Negocios — FCE-UBA, 2º cuatrimestre de 2026.

## Integrantes

Grupo 23: 

Franco Julian Perez Del Castillo (894135)

Francisco Rodríguez Cancela (918043)

Julian Roberto Zubieta (917154)

## Objetivo e hipótesis

El trabajo describe cómo se distribuye entre países y entre ocupaciones la exposición del trabajo a la automatización por IA, entendida como la proporción de tareas que la tecnología disponible hoy podría realizar.

**Hipótesis de trabajo**

La exposición de una ocupación depende tanto de qué ocupación es como del país en que se ejerce, y el componente país está asociado al nivel de desarrollo y, sobre todo, a la conectividad. Si esto es así, deberíamos observar tres cosas:

1. que el país explique una parte sustantiva de la variabilidad de la exposición, comparable a la que explica la ocupación, y que lo haga desplazando toda la escala más que reordenando las ocupaciones;
2. que los países se agrupen en perfiles de exposición diferenciados, y que esos grupos coincidan con su nivel de ingreso aun cuando el ingreso no se haya usado para construirlos;
3. que, al considerar juntos el ingreso y la conectividad, la conectividad mantenga un efecto propio al menos tan importante como el del PBI per cápita.

Una advertencia que atraviesa todo el trabajo: **exposición no es reemplazo**. El dato mide capacidad tecnológica, no empleo efectivamente perdido. Y el análisis es **descriptivo, no causal**.

## Base de datos

| | |
| --- | --- |
| **Fuente** | Global Automation Atlas — Garg, Crosta y Baier (2026) |
| **Link** | https://automationatlas.org/data/ |
| **Base principal** | `country-occupation-panel`: 114.452 filas, 923 ocupaciones O*NET × 124 países |
| **Unidad de observación** | Un país y una ocupación |
| **Base auxiliar** | `countries`: una fila por país, 124 filas × 37 columnas, con datos del Banco Mundial. Aporta región, grupo de ingreso, PBI per cápita y uso de internet. Se une a la base principal por `iso3`: coinciden los 124 países, sin pérdida de observaciones |
| **Período** | Corte transversal. Las covariables del Banco Mundial corresponden a 2022–2024 |
| **Periodicidad** | El Atlas se publicó en 2026 y no tiene todavía una periodicidad establecida |

**Limitaciones conocidas.** El Atlas publica ocho columnas de canal tecnológico y de implementación que vienen vacías (constantes en cero) y dos columnas redundantes entre sí; todas se descartan en el script `01_limpieza.R`. El panel de ocupaciones y el resumen por país no cuentan el mismo universo de tareas (23.851 contra 18.797), de modo que el agregado que calculamos reproduce el del Atlas con una diferencia máxima de 0,011 sobre una escala de 0 a 3, pero no es idéntico. Quedan fuera de la base los países de menos de 2,8 millones de habitantes.

## Análisis realizado

El recorrido va de establecer el fenómeno a explicarlo:

1. **Estadísticas descriptivas** de la exposición por país, por ocupación y por grupo de ingreso.
2. **Descomposición de la varianza** (unidad 3): separa cuánto de la variabilidad de la exposición corresponde a la ocupación, cuánto al país y cuánto a la interacción.
3. **Clustering k-means** (unidad 3): agrupa a los 124 países por su perfil de exposición, usando solo variables de exposición.
4. **Chi cuadrado de independencia** (unidad 2): contrasta si los grupos obtenidos se asocian al grupo de ingreso del Banco Mundial, que no participó del armado.
5. **Regresión lineal múltiple** (unidad 2): mide cuánto del nivel de exposición de un país acompaña a su PBI per cápita y cuánto a su conectividad.

El orden no es arbitrario: la descomposición justifica que tenga sentido analizar países, el clustering produce los grupos que el chi cuadrado después usa, y la regresión cierra midiendo la contribución de cada covariable.

## Estructura del repositorio

```
.
├── raw/           country-occupation-panel.csv  (datos crudos, no se modifican)
├── auxiliar/      countries.csv                 (base complementaria)
├── input/         bases procesadas (.rds) que generan los scripts
├── output/
│   ├── tablas/    tablas de resultados en .csv
│   └── graficos/  visualizaciones en .jpg
├── script/        los ocho scripts del trabajo, en orden de ejecución
├── utils/         funciones propias, una por archivo
├── TP.Rproj       proyecto de RStudio: abrirlo hace que las rutas funcionen
└── README.md
```

## Reproducción

### Paquetes necesarios

```r
install.packages(c("here", "tidyverse", "skimr", "scales", "ggrepel",
                   "cluster", "factoextra", "car", "lmtest", "sandwich"))
```

### Cómo correrlo

Abrir `TP.Rproj` en RStudio y ejecutar los scripts de `script/` en orden numérico. Todas las rutas se resuelven con `here()`, así que funcionan en cualquier computadora sin tocar nada. Cada script guarda sus resultados y el siguiente los lee desde el archivo: no hace falta que la sesión de R conserve nada entre uno y otro.

| Script | Qué hace | Qué genera |
| --- | --- | --- |
| `01_limpieza.R` | Carga los datos crudos, verifica la llave y los tipos, descarta columnas vacías y redundantes, construye las variables derivadas y el agregado por país | `input/base_limpia.rds`, `input/pais.rds`, `output/tablas/diccionario_variables.csv` |
| `02_descriptivas.R` | Descriptivas, tablas de frecuencia, forma de las distribuciones, ranking de ocupaciones y dispersión interna | `input/ocupaciones.rds`, `input/comparacion_ingreso.rds`, `output/tablas/descriptivas_pais.csv`, `descriptivas_ocupacion.csv`, `top10_ocupaciones.csv`, `bottom10_ocupaciones.csv`, `dispersion_por_ingreso.csv`, `frecuencia_region.csv` |
| `03_faltantes_outliers.R` | Detecta faltantes y atípicos, documenta la decisión y compara las descriptivas antes y después | `input/pais_completo.rds`, `output/tablas/descriptivas_pais_e2.csv`, `descriptivas_por_ingreso.csv`, `outliers.csv`, `antes_despues.csv` |
| `04_descomposicion_varianza.R` | Método 1: reparto de la varianza entre país, ocupación e interacción | `output/tablas/descomposicion_varianza.csv` |
| `05_clustering.R` | Método 2: k-means, elección de k, perfil de los grupos, arquetipos y validación con Ward | `input/pais_clusters.rds`, `output/tablas/comparacion_k.csv`, `perfil_clusters_k3.csv`, `arquetipos.csv`, `paises_por_cluster.csv`, `cluster3_vs_ingreso.csv`, `output/graficos/cluster_silueta.jpg`, `cluster_codo.jpg`, `dendrograma.jpg` |
| `06_chi_cuadrado.R` | Método 3: test de independencia entre los grupos y el ingreso | `output/tablas/chi_cuadrado_tabla.csv`, `chi_cuadrado_residuos.csv` |
| `07_regresion.R` | Método 4: regresión lineal, supuestos, errores robustos e influyentes | `input/modelos_regresion.rds`, `output/tablas/regresion.csv` |
| `08_graficos.R` | Las visualizaciones de la presentación (g01 a g08) y las dos de la instancia 3 | `output/graficos/g01_*.jpg` a `g08_*.jpg`, `grupo_23_exploratorio.jpg`, `grupo_23_comunicativo.jpg` |

### Funciones propias

En `utils/`, una por archivo, llamadas con `source()`:

- `es_outlier.R`: marca atípicos con la regla del boxplot (1,5 rangos intercuartílicos).
- `reset_par.R`: restaura los parámetros gráficos de base R después de usar `par()`.
- `tema_tp.R`: tema de ggplot, paletas de colores y textos de fuente comunes a todos los gráficos.
- `guardar_grafico.R`: exporta un gráfico con `ggsave()` siempre con el mismo tamaño, resolución y fondo (lo usan `05` y `08`).
- `comparar_escenarios.R`: resume media y desvío de las variables principales para cada escenario de limpieza (tabla antes/después de `03`).
- `resumen_modelo.R`: pasa un modelo de regresión a una tabla con errores estándar robustos (`07`).

## Conclusiones principales

1. **El país y la ocupación pesan casi lo mismo.** La descomposición asigna alrededor del 49% de la varianza a la ocupación, el 44% al país y apenas el 7% a la interacción, como predecía la hipótesis.
2. **El efecto país es un desplazamiento, no un reordenamiento.** Que la interacción sea tan chica, y que el orden de las ocupaciones correlacione 0,88 entre países ricos y pobres, significa que las mismas ocupaciones son las más expuestas en todas partes; lo que cambia es la altura de toda la escala.
3. **Los países forman tres grupos que reproducen la escalera de ingresos.** El clustering, construido solo con variables de exposición, separa tres grupos cuyo uso de internet promedio es de 23%, 53% y 88%. El chi cuadrado confirma la asociación con el grupo de ingreso (V de Cramér = 0,78), pese a que el ingreso nunca entró al algoritmo.
4. **La conectividad tiene un efecto propio, al menos tan importante como el del ingreso.** Con las dos variables en el mismo modelo, las dos siguen siendo significativas y el coeficiente del PBI per cápita cae a menos de la mitad. Estandarizados, internet pesa 0,56 desvíos y el PBI 0,41, aunque sus intervalos de confianza se superponen.
5. **La exposición no solo es más alta en los países ricos: también es más pareja.** En los países de ingreso bajo se concentra en el trabajo administrativo, mientras que en los de ingreso alto alcanza también a la producción, el transporte, la agricultura y los servicios de comida.

## Próximos pasos

- Trabajar con la base de tareas (2,96 millones de filas), que es la única donde están pobladas las variables de canal tecnológico, para distinguir qué rutas de automatización explican las diferencias entre países.
- Incorporar la estructura ocupacional real de cada país: el Atlas evalúa el mismo catálogo de ocupaciones en todos, de modo que no pondera por cuánta gente trabaja efectivamente en cada una.
- Sumar una dimensión temporal cuando el Atlas publique una segunda edición, para pasar de un corte transversal a una comparación entre años.
