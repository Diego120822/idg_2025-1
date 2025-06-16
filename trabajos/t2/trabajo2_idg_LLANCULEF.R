# ============================================================================
# 1. CARGA DE LIBRERÍAS
# ============================================================================
library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(data.table)
library(rakeR)
library(viridis)
library(dplyr)
library(tidyr)
library(here)

# ============================================================================
# 2. CONEXIÓN A BASE DE DATOS POSTGRES
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
# 3. CARGA DE DATOS
# ============================================================================
cons_censo_df <- readRDS(here("data/cons_censo_df.rds"))
casen <- readRDS(here("trabajos/t2/casen_rm.rds"))

# ============================================================================
# 4. PREPARACIÓN Y TRANSFORMACIÓN DE CASEN
# ============================================================================
vars_base <- c("estrato", "esc", "edad", "sexo", "e6a", "v1")
casen <- casen[, vars_base]
casen$Comuna <- substr(as.character(casen$estrato), 1, 5)

# Conversión de factores a numérico
casen <- casen %>%
  mutate(
    esc = as.integer(unclass(esc)),
    edad = as.integer(unclass(edad)),
    e6a = as.numeric(unclass(e6a)),
    sexo = as.integer(unclass(sexo)),
    v1 = as.numeric(unclass(v1))
  ) %>%
  select(-estrato)

# ============================================================================
# 5. IMPUTACIÓN Y RECODIFICACIÓN
# ============================================================================
# Imputación por regresión: Escolaridad faltante imputada con e6a
idx_na <- which(is.na(casen$esc) & !is.na(casen$e6a))
if (length(idx_na) > 0) {
  modelo_esc <- lm(esc ~ e6a, data = casen[!is.na(casen$esc) & !is.na(casen$e6a), ])
  casen$esc[idx_na] <- round(predict(modelo_esc, newdata = casen[idx_na, ]))
}

# Reagrupación de v1 (tipo de vivienda)
casen <- casen %>%
  mutate(
    v1 = case_when(
      v1 %in% 1:4 ~ 1,                # Viviendas formales
      v1 == 5 ~ 2,                    # Mediaguas
      v1 %in% c(6, 7, 9, 10) ~ 3,     # Viviendas precarias
      v1 == 8 ~ 4,                    # Local no destinado a habitación
      TRUE ~ NA_real_
    )
  )

casen$ID <- as.character(seq_len(nrow(casen)))

# ============================================================================
# 6. CATEGORÍAS PARA RAKE
# ============================================================================
col_cons <- setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA"))
edad_labels <- grep("^edad_", col_cons, value = TRUE)
esc_labels <- grep("^esco_", col_cons, value = TRUE)
sexo_labels <- grep("^sexo_", col_cons, value = TRUE)

casen$edad_cat <- cut(
  casen$edad,
  breaks = c(0, 30, 40, 50, 60, 70, 80, Inf),
  labels = edad_labels,
  right = FALSE,
  include.lowest = TRUE
)

casen <- casen %>%
  mutate(
    esc_cat = case_when(
      esc < 1 ~ esc_labels[1],
      esc >= 1 & esc < 9 ~ esc_labels[2],
      esc >= 9 & esc <= 12 ~ esc_labels[3],
      esc > 12 ~ esc_labels[4],
      TRUE ~ NA_character_
    ),
    sexo_cat = factor(case_when(
      sexo == 1 ~ sexo_labels[1],
      sexo == 2 ~ sexo_labels[2],
      TRUE ~ NA_character_
    ), levels = sexo_labels)
  )

casen$esc_cat <- factor(casen$esc_cat, levels = esc_labels)

# ============================================================================
# 7. DIAGNÓSTICO DE COBERTURA ENTRE CASEN Y CENSO
# ============================================================================
inds_list <- split(casen, casen$Comuna)
cons_censo_list <- split(cons_censo_df, cons_censo_df$COMUNA)

# Diagnóstico útil para evitar errores por comunas sin correspondencia
cat("Comunas solo en CASEN:\n")
print(setdiff(names(inds_list), names(cons_censo_list)))

cat("Comunas solo en CENSO:\n")
print(setdiff(names(cons_censo_list), names(inds_list)))

# ============================================================================
# 8. MICROSIMULACIÓN ESPACIAL
# ============================================================================
sim_list <- lapply(names(cons_censo_list), function(comuna) {
  cons_i <- cons_censo_list[[comuna]]
  inds_i <- inds_list[[comuna]]
  if (is.null(inds_i)) return(NULL)
  
  cons_i <- cons_i[, c("GEOCODIGO", sort(setdiff(names(cons_i), c("COMUNA", "GEOCODIGO"))))]
  inds_sub <- inds_i[, c("ID", "edad_cat", "esc_cat", "sexo_cat")]
  colnames(inds_sub) <- c("ID", "Edad", "Escolaridad", "Sexo")
  
  w <- weight(cons_i, inds_sub, vars = c("Edad", "Escolaridad", "Sexo"))
  sim_i <- integerise(w, inds_sub, seed = 123)
  sim_i$GEOCODIGO <- cons_i$GEOCODIGO[1]
  
  merge(sim_i, inds_i[, c("ID", "v1")], by = "ID", all.x = TRUE)
})

sim_df <- rbindlist(sim_list, idcol = "COMUNA", fill = TRUE)

# ============================================================================
# 9. TABLAS RESUMEN Y ANÁLISIS EXPLORATORIO
# ============================================================================

# Tabla 1: Casos simulados por comuna
tabla_1 <- sim_df %>%
  group_by(COMUNA) %>%
  summarise(casos_simulados = n()) %>%
  arrange(desc(casos_simulados))
print(tabla_1)

# Tabla 2: Distribución por tipo de vivienda
tabla_2 <- sim_df %>%
  filter(!is.na(v1)) %>%
  group_by(COMUNA, tipo_vivienda = v1) %>%
  summarise(total = n(), .groups = "drop") %>%
  group_by(COMUNA) %>%
  mutate(porcentaje = round(100 * total / sum(total), 2)) %>%
  arrange(COMUNA, tipo_vivienda)
print(tabla_2)

# Tabla 3: Porcentaje de viviendas precarias por comuna
tabla_3 <- sim_df %>%
  filter(!is.na(v1)) %>%
  group_by(COMUNA) %>%
  summarise(porc_precarias = round(100 * sum(v1 == 3) / n(), 2)) %>%
  arrange(desc(porc_precarias))
print(tabla_3)

# Gráfico de barras: top 10 comunas con mayor % de viviendas precarias
ggplot(head(tabla_3, 10), aes(x = reorder(COMUNA, porc_precarias), y = porc_precarias)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(
    title = "Top 10 Comunas con Mayor % de Viviendas Precarias",
    x = "Comuna",
    y = "% viviendas precarias (simuladas)"
  ) +
  theme_minimal()

# ============================================================================
# 10. CÁLCULO DE PORCENTAJE DE VIVIENDAS PRECARIAS POR GEOCODIGO
# ============================================================================
zonas_v1 <- sim_df %>%
  filter(!is.na(v1)) %>%
  group_by(GEOCODIGO) %>%
  summarise(vivienda_precaria = round(100 * sum(v1 == 3) / n(), 2)) %>%
  ungroup()

# ============================================================================
# 11. EXPORTAR A BASE DE DATOS Y UNIÓN ESPACIAL
# ============================================================================
dbExecute(con, "DROP TABLE IF EXISTS dpa.tmp_v1_rm")
dbWriteTable(con, Id(schema = "dpa", table = "tmp_v1_rm"), zonas_v1, overwrite = TRUE)

# Aquí corregimos el nombre de columna a GEOCODIGO (en mayúsculas)
dbExecute(con, "CREATE INDEX ON dpa.tmp_v1_rm(\"GEOCODIGO\")")
dbExecute(con, "ANALYZE dpa.tmp_v1_rm")

# Usamos comillas para asegurar referencia correcta
dbExecute(con, "DROP TABLE IF EXISTS dpa.zonas_v1")
dbExecute(con, "
  CREATE TABLE dpa.zonas_v1 AS
  SELECT z.*, t.vivienda_precaria
  FROM dpa.zonas_censales_rm z
  LEFT JOIN dpa.tmp_v1_rm t ON z.geocodigo::text = t.\"GEOCODIGO\"
  WHERE urbano = 1 AND (
    nom_provin = 'SANTIAGO' OR
    nom_comuna IN ('PEDRO AGUIRRE CERDA', 'LO ESPEJO')
  )
")

# ============================================================================
# 12. VISUALIZACIÓN FINAL EN MAPA
# ============================================================================
zonas_v1_sf <- st_read(con, query = "SELECT * FROM dpa.zonas_v1")

zonas_v1_sf <- zonas_v1_sf %>%
  mutate(vivienda_precaria = replace_na(vivienda_precaria, 0))

ggplot(zonas_v1_sf) +
  geom_sf(aes(fill = vivienda_precaria), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "C", direction = -1,
                       name = "% viviendas precarias") +
  theme_minimal() +
  labs(
    title = "Distribución de Viviendas Precarias en la Región Metropolitana",
    subtitle = "Microsimulación espacial basada en CASEN 2022 y CENSO 2017"
  ) +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

