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

# ============================================================================
# 2. CONEXIÓN Y CARGA DE DATOS
# ============================================================================
con <- dbConnect(
  Postgres(),
  dbname = "censo_rm_2017",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

zonas <- st_read(con, query = "SELECT * FROM dpa.zonas_v1") %>%
  mutate(vivienda_precaria = replace_na(vivienda_precaria, 0))

# ============================================================================
# 3. EXPLORACIÓN Y FILTRADO DE ZONAS
# ============================================================================
# Filtrar zonas con al menos alguna vivienda precaria
zonas_filtradas <- zonas %>%
  filter(vivienda_precaria > 0)

ggplot(zonas_filtradas) +
  geom_histogram(aes(x = vivienda_precaria), bins = 30, fill = "darkorange") +
  theme_minimal() +
  labs(
    title = "Distribución de % viviendas precarias (zonas con > 0%)",
    x = "% viviendas precarias",
    y = "Frecuencia"
  )

# ============================================================================
# 4. DETERMINAR NÚMERO ÓPTIMO DE CLUSTERS (MÉTODO DEL CODO)
# ============================================================================
zonas_scaled <- zonas_filtradas %>%
  st_drop_geometry() %>%
  select(vivienda_precaria) %>%
  scale()

fviz_nbclust(zonas_scaled, kmeans, method = "wss") +
  theme_minimal() +
  labs(title = "Número óptimo de clusters (método del codo)")

# ============================================================================
# 5. APLICAR K-MEANS Y ETIQUETADO
# ============================================================================
set.seed(123)
kmeans_res <- kmeans(zonas_scaled, centers = 4)  # Cambiar si el codo indica otro número
zonas_filtradas$cluster <- factor(kmeans_res$cluster)

# Etiquetas según promedio de viviendas precarias
cluster_summary <- zonas_filtradas %>%
  group_by(cluster) %>%
  summarise(
    media_precarias = mean(vivienda_precaria, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(media_precarias)) %>%
  mutate(etiqueta = c("Alta precariedad", "Media-alta", "Media-baja", "Baja precariedad"))

zonas_filtradas <- zonas_filtradas %>%
  st_join(cluster_summary %>% select(cluster, etiqueta), by = "cluster")

# ============================================================================
# 6. MAPA DE CLUSTERS
# ============================================================================
ggplot(zonas_filtradas) +
  geom_sf(aes(fill = etiqueta), color = "white", size = 0.1) +
  scale_fill_viridis_d(option = "C") +
  theme_minimal() +
  labs(
    title = "Clusters según % de viviendas precarias",
    fill = "Grupo"
  )

# ============================================================================
# 7. ÍNDICE DE SHANNON (FUNCIÓN PERSONALIZADA)
# ============================================================================
shannon_index <- function(x) {
  p <- x / sum(x)
  -sum(p * log2(p), na.rm = TRUE)
}

shannon_por_comuna <- zonas_filtradas %>%
  group_by(nom_comuna, etiqueta) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(nom_comuna) %>%
  summarise(shannon = shannon_index(n))

comunas <- zonas_filtradas %>%
  group_by(nom_comuna) %>%
  summarise(geometry = st_union(geometry)) %>%
  left_join(shannon_por_comuna, by = "nom_comuna")

# ============================================================================
# 8. MAPA DE VARIABILIDAD INTRA-COMUNAL
# ============================================================================
ggplot(comunas) +
  geom_sf(aes(fill = shannon)) +
  scale_fill_viridis_c(option = "D", name = "Índice de Shannon") +
  theme_minimal() +
  labs(
    title = "Variabilidad interna por comuna (mezcla de clusters)",
    subtitle = "Shannon alto = heterogeneidad territorial"
  )
