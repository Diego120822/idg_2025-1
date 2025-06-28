# ============================================================================
# 1. CARGA DE LIBRERÍAS
# ============================================================================
library(tidyverse)
library(sf)
library(cluster)
library(factoextra)
library(ggplot2)
library(viridis)
library(DBI)
library(RPostgres)
library(GGally)    # Para ggpairs
library(entropy)   # Para el índice de Shannon
library(vegan)     # Alternativa para el índice de Shannon (aunque entropy ya sirve)
library(tibble)    # Para crear tibbles, útil para las etiquetas de clusters
library(tidyr)     # Para pivot_wider

# ============================================================================
# 2. CONEXIÓN A LA BASE DE DATOS
# ============================================================================
con <- dbConnect(
  Postgres(),
  dbname = "censo_rm_2017",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

# ============================================================================
# 3. CARGA DE DATOS: VIVIENDA PRECARIA E INGRESO
# ============================================================================

# Consulta SQL para obtener la variable de vivienda precaria
sql_vivienda_precaria <- "
SELECT
  geocodigo::double precision AS geocodigo,
  vivienda_precaria
FROM dpa.zonas_v1;
"
df_vivienda_precaria <- dbGetQuery(con, sql_vivienda_precaria) %>%
  mutate(vivienda_precaria = replace_na(vivienda_precaria, 0))

# Consulta SQL para obtener la mediana de ingreso
sql_ingreso <- "
SELECT
  geocodigo::double precision AS geocodigo,
  mediana_ingreso
FROM dpa.tmp_ingreso_rm;
"
df_ingreso <- dbGetQuery(con, sql_ingreso)

# Unir las variables en un solo dataframe
df_clusters <- df_ingreso %>%
  left_join(df_vivienda_precaria, by = "geocodigo") %>%
  # Asegurarse de filtrar cualquier NA en las variables clave antes de escalar
  filter(!is.na(mediana_ingreso) & !is.na(vivienda_precaria))

# ============================================================================
# 4. EXPLORACIÓN DE DATOS (Ajustes para mejor visualización)
# ============================================================================

# Calcular el porcentaje de zonas con 0% de vivienda precaria
porcentaje_ceros_vivienda_precaria <- mean(df_clusters$vivienda_precaria == 0, na.rm = TRUE) * 100

# === Histograma de vivienda_precaria para valores > 0 ===
# Esto permite ver la distribución de las zonas que SÍ tienen precariedad
ggplot(df_clusters %>% filter(vivienda_precaria > 0)) +
  geom_histogram(aes(x = vivienda_precaria), bins = 30, fill = "darkorange", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribución de % viviendas precarias (solo zonas > 0%)",
    x = "% viviendas precarias",
    y = "Frecuencia"
  ) +
  annotate("text", x = Inf, y = Inf,
           label = paste0("Nota: ", round(porcentaje_ceros_vivienda_precaria, 2), "% de las zonas tienen 0% de viviendas precarias."),
           hjust = 1.05, vjust = 1.5, size = 3.5, color = "gray30")

# === Histograma de mediana_ingreso ===
ggplot(df_clusters) +
  geom_histogram(aes(x = mediana_ingreso), bins = 30, fill = "darkgreen", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribución de la Mediana de Ingreso",
    x = "Mediana de Ingreso per cápita",
    y = "Frecuencia"
  )

# ============================================================================
# 5. ESCALAR Y DETERMINAR NÚMERO ÓPTIMO DE CLUSTERS (MÉTODO DEL CODO)
# ============================================================================
vars_scaled <- df_clusters %>%
  select(vivienda_precaria, mediana_ingreso) %>%
  scale()

# Método del codo para determinar número óptimo de clusters (Confirmado en 4)
fviz_nbclust(vars_scaled, kmeans, method = "wss") +
  labs(title = "Método del Codo para Vivienda Precaria e Ingreso", x = "Número de Clusters", y = "WSS") +
  theme_minimal()

# ============================================================================
# 6. APLICAR K-MEANS Y ETIQUETADO (Ajuste para etiquetas descriptivas)
# ============================================================================
set.seed(123)
# Se ajusta 'centers' a 4 según lo obtenido en el método del codo
km <- kmeans(vars_scaled, centers = 4, nstart = 25)
df_clusters$cluster <- as.factor(km$cluster)

# Etiquetar los clusters de forma más descriptiva
# ESTAS SON LAS ETIQUETAS BASADAS EN LOS PROMEDIOS QUE VIMOS EN LA TABLA
cluster_labels_df <- tibble(
  cluster = factor(c(1, 2, 3, 4)), # Asegúrate que coincidan con los IDs de tus clusters
  etiqueta_descriptiva = c(
    "1. Ingreso Medio-Bajo - Sin Precariedad",
    "2. Ingreso Alto - Muy Baja Precariedad",
    "3. Alta Precariedad - Ingreso Medio-Bajo",
    "4. Ingreso Muy Alto - Sin Precariedad"
  )
)

# Unir estas etiquetas al dataframe principal para que ggplot las use en el mapa
df_clusters <- df_clusters %>%
  left_join(cluster_labels_df, by = "cluster")

# Resumen de los clusters para confirmación (Punto 11 en el código original)
cluster_summary <- df_clusters %>%
  group_by(cluster) %>%
  summarise(
    promedio_vivienda_precaria = mean(vivienda_precaria, na.rm = TRUE),
    promedio_ingreso = mean(mediana_ingreso, na.rm = TRUE),
    n_zonas = n()
  ) %>%
  arrange(promedio_vivienda_precaria) # Ordenar para una mejor interpretación

# ============================================================================
# 7. CARGA Y UNIÓN DE ZONAS CENSALES Y COMUNAS
# ============================================================================
sql_geom <- "
SELECT geocodigo::double precision AS geocodigo, geom, nom_comuna
FROM dpa.zonas_censales_rm
WHERE nom_provin = 'SANTIAGO' AND urbano = 1;
"
sf_zonas <- st_read(con, query = sql_geom)

# Unir los datos de clusters (con etiquetas descriptivas) con la geometría espacial
sf_mapa <- merge(sf_zonas, df_clusters, by = "geocodigo")

sql_comunas <- "
SELECT cut, nom_comuna, geom
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO';
"
sf_comunas <- st_read(con, query = sql_comunas)
bbox <- st_bbox(sf_mapa) # Usar el bbox de las zonas para el mapa principal

# ============================================================================
# 8. GRÁFICOS DE DISTRIBUCIÓN POR VARIABLES (Ajustado con Jitter y Centros)
# ============================================================================
# Calcular estadísticas de los clusters para una mejor comprensión visual de los centros
cluster_summary_plot <- df_clusters %>%
  group_by(cluster) %>%
  summarise(
    avg_vivienda_precaria = mean(vivienda_precaria, na.rm = TRUE),
    avg_mediana_ingreso = mean(mediana_ingreso, na.rm = TRUE),
    etiqueta_descriptiva = first(etiqueta_descriptiva) # Para tener las etiquetas en el summary
  )

# Gráfico mejorado de Vivienda Precaria vs Ingreso (color por cluster)
ggplot(df_clusters, aes(x = vivienda_precaria, y = mediana_ingreso, color = cluster)) +
  geom_point(position = position_jitter(width = 0.05, height = 0), size = 2, alpha = 0.6) +
  # Añadir puntos medios de los clusters para ver el "centro" de cada grupo
  geom_point(data = cluster_summary_plot,
             aes(x = avg_vivienda_precaria, y = avg_mediana_ingreso, color = NULL), # color=NULL para que el asterisco sea negro por defecto
             size = 6, shape = 8, color = "black", stroke = 1.5,
             inherit.aes = FALSE) +
  labs(
    title = "Clusters de Vivienda Precaria e Ingreso",
    subtitle = "El símbolo '*' indica el centro promedio de cada cluster",
    x = "% Viviendas Precarias (con jitter para valores 0)",
    y = "Mediana de Ingreso per cápita"
  ) +
  scale_color_brewer(palette = "Set1", name = "Cluster") + # Aquí sigue mostrando 1,2,3,4 en la leyenda
  theme_minimal()

# ============================================================================
# 9. MAPA DE CLUSTERS (Ajustado para usar etiquetas descriptivas en leyenda)
# ============================================================================
ggplot() +
  geom_sf(data = sf_mapa, aes(fill = etiqueta_descriptiva), color = NA) + # USAR la nueva columna con etiquetas
  geom_sf(data = sf_comunas, fill = NA, color = "black", size = 0.4) +
  geom_sf_text(data = st_centroid(sf_comunas), aes(label = nom_comuna), size = 2) +
  scale_fill_brewer(palette = "Set2", name = "Tipo de Zona Censal") + # Cambiar el título de la leyenda
  labs(title = "Tipología de Zonas Censales Urbanas – Gran Santiago",
       subtitle = "Clusters basados en Vivienda Precaria e Ingreso") +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
  theme_void()

# ============================================================================
# 10. MATRIZ DE CORRELACIÓN (Se mantiene igual)
# ============================================================================
df_plot <- df_clusters[, c(
  "vivienda_precaria",
  "mediana_ingreso",
  "cluster"
)]

ggpairs(
  df_plot,
  columns = 1:2, # Solo las variables numéricas para la matriz
  mapping = aes(color = cluster), # Color por el ID numérico del cluster
  upper = list(continuous = "points"),
  lower = list(continuous = "points"),
  diag = list(continuous = "densityDiag")
) +
  labs(title = "Matriz de Relación entre Vivienda Precaria e Ingreso por Cluster")

# ============================================================================
# 11. PROMEDIO DE LOS CLUSTERS (Tabla descriptiva)
# ============================================================================
# Ya se calculó en la Sección 6 como 'cluster_summary' para la interpretación
# Puedes imprimirla aquí si quieres que aparezca en el output de R
print(cluster_summary)

# ============================================================================
# 12. ANÁLISIS INTRA-COMUNAL (ÍNDICE DE SHANNON)
# ============================================================================
# Asegurarse de que sf_mapa tenga el nombre de comuna asociado a cada geocodigo
# Esto ya se hizo en la carga de sf_zonas, por lo que sf_mapa debería tener nom_comuna

# Crear tabla de frecuencia de clusters por comuna
tabla_shannon <- sf_mapa %>%
  st_drop_geometry() %>%
  group_by(nom_comuna, cluster) %>% # Usamos el ID numérico del cluster aquí
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = cluster, # Pivotar por el ID numérico del cluster
    values_from = n,
    values_fill = 0
  )

# Calcular índice de Shannon
cluster_columns_for_shannon <- colnames(tabla_shannon)[!colnames(tabla_shannon) %in% c("nom_comuna")]
tabla_shannon$shannon <- diversity(tabla_shannon[, cluster_columns_for_shannon], index = "shannon")

# Unión con geometría comunal
sf_shannon <- sf_comunas %>%
  left_join(tabla_shannon, by = "nom_comuna")

# Mapa de variabilidad intra-comunal
ggplot(sf_shannon) +
  geom_sf(aes(fill = shannon), color = "grey20", size = 0.3) +
  geom_sf_text(aes(label = nom_comuna), size = 1.5, color = "black") +
  scale_fill_viridis_c(option = "plasma", name = "Índice de Shannon") +
  labs(
    title = "Variabilidad intra-comunal de Clusters",
    subtitle = "Índice de Shannon por Comuna (Vivienda Precaria e Ingreso)"
  ) +
  theme_minimal()

# ============================================================================
# 13. DESCONEXIÓN DE LA BASE DE DATOS
# ============================================================================
dbDisconnect(con)





