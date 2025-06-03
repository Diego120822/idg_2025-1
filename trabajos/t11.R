# =============================================================================
# 1) INSTALAR PAQUETES (solo una vez)
# =============================================================================
install.packages(c("DBI", "RPostgres", "sf", "ggplot2", "cowplot", "biscale"))

# =============================================================================
# 2) CARGAR LIBRERÍAS NECESARIAS
# =============================================================================
library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(cowplot)
library(biscale)

# =============================================================================
# 3) CONFIGURAR CONEXIÓN A BASE DE DATOS
# =============================================================================
db_host    = "localhost"
db_port    = 5432
db_name    = "CENSO_QUINTA_REGION_2017" 
db_user    = "postgres"
db_password = "POSTGRES"

con = dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)

# =============================================================================
# 4) EXTRAER INDICADORES DESDE CENSO (A NIVEL DE MANZANA)
# =============================================================================
#  **IMPORTANTE:** Asumo que tienes una tabla que relaciona 'personas' con 'manzanas'.
#  Necesitarás ajustar la consulta SQL según la estructura de tu base de datos.
#  Por ejemplo, podría haber una columna 'manzana_id' en la tabla 'personas'
#  o una tabla de 'asignación' que vincule personas a manzanas.

sql_indicadores_manzana = "
SELECT
    m.manzana_id,  --  O el identificador de manzana que corresponda
    AVG(p.escolaridad) as promedio_escolaridad,
    COUNT(CASE WHEN p.escolaridad IN (7, 8, 9, 10, 11, 12, 13, 14) THEN 1 ELSE NULL END) * 100.0 / COUNT(*) AS porcentaje_educacion_media_o_mas,
    COUNT(CASE WHEN p.p18 NOT IN ('98', '99') AND p.p18 IS NOT NULL THEN 1 ELSE NULL END) * 100.0 / COUNT(*) AS porcentaje_ocupados
FROM
    personas p
JOIN
    hogares h ON p.hogar_ref_id = h.hogar_ref_id
JOIN
    viviendas v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN
    zonas z ON v.zonaloc_ref_id = z.zonaloc_ref_id
JOIN
    comunas c ON z.codigo_comuna = c.codigo_comuna
JOIN
    manzanas m ON p.manzana_id = m.manzana_id  --  **AJUSTAR ESTO**
WHERE
    c.nom_comuna = 'Viña del Mar' AND p.escolaridad IS NOT NULL
GROUP BY
    m.manzana_id;
"

df_indicadores_manzana = dbGetQuery(con, sql_indicadores_manzana)

# =============================================================================
# 5) CARGAR GEOMETRÍA DE MANZANAS
# =============================================================================
#  **IMPORTANTE:** Asegúrate de que la ruta al shapefile es correcta.
#  Si el shapefile está en la misma carpeta que el script, podrías usar una ruta relativa.
ruta_shapefile_manzanas = "ruta/a/tu/shapefile/manzanas_vina_del_mar.shp"
sf_manzanas = st_read(ruta_shapefile_manzanas)

#  **IMPORTANTE:** Asegúrate de que haya una columna en el shapefile
#  que coincida con la columna 'manzana_id' en 'df_indicadores_manzana'.
#  Si las columnas tienen nombres diferentes, deberás renombrarlas
#  antes del merge.

# =============================================================================
# 6) COMBINAR DATOS TABULARES Y ESPACIALES
# =============================================================================
sf_mapa_manzanas = merge(
  x = sf_manzanas,
  y = df_indicadores_manzana,
  by = "manzana_id",  #  **AJUSTAR ESTO** (nombre de la columna en el shapefile)
  all.x = FALSE  #  Conservar solo las manzanas para las que tenemos datos
)

# =============================================================================
# 7) MAPAS TEMÁTICOS SIMPLES
# =============================================================================

# 7.1 Mapa: Porcentaje Educación Media o Más
mapa_educacion = ggplot(sf_mapa_manzanas) +
  geom_sf(aes(fill = porcentaje_educacion_media_o_mas), color = "#AAAAAA30", size = 0.1) +
  labs(
    title = "Viña del Mar: % Población con Educación Media o Más (por Manzana)",
    fill  = "% Educación Media"
  ) +
  theme_void()

print(mapa_educacion)

# 7.2 Mapa: Porcentaje Ocupados
mapa_ocupacion = ggplot(sf_mapa_manzanas) +
  geom_sf(aes(fill = porcentaje_ocupados), color = "#AAAAAA30", size = 0.1) +
  labs(
    title = "Viña del Mar: % Población Ocupada (por Manzana)",
    fill  = "% Ocupados"
  ) +
  theme_void()

print(mapa_ocupacion)

# =============================================================================
# 8) GRÁFICO DE DISPERSIÓN BIVARIADO (A NIVEL DE MANZANA)
# =============================================================================

# 8.1 Calcular medianas
mediana_educacion = median(sf_mapa_manzanas$porcentaje_educacion_media_o_mas, na.rm = TRUE)
mediana_ocupacion = median(sf_mapa_manzanas$porcentaje_ocupados, na.rm = TRUE)

# 8.2 Crear variable de cuadrante
sf_mapa_manzanas$cuadrante = with(
  sf_mapa_manzanas,
  ifelse(
    porcentaje_educacion_media_o_mas >= mediana_educacion & porcentaje_ocupados >= mediana_ocupacion, 'Q1: Alta/Alta',
    ifelse(
      porcentaje_educacion_media_o_mas < mediana_educacion & porcentaje_ocupados >= mediana_ocupacion, 'Q2: Baja/Alta',
      ifelse(
        porcentaje_educacion_media_o_mas < mediana_educacion & porcentaje_ocupados < mediana_ocupacion, 'Q3: Baja/Baja',
        'Q4: Alta/Baja'
      )
    )
  )
)

# 8.3 Paleta de colores
colores_cuadrantes = c(
  'Q1: Alta/Alta' = '#08519c',
  'Q2: Baja/Alta' = '#6baed6',
  'Q3: Baja/Baja' = '#eff3ff',
  'Q4: Alta/Baja' = '#bdd7e7'
)

# 8.4 Gráfico de dispersión
grafico_dispersion_manzanas = ggplot(
  sf_mapa_manzanas,
  aes(
    x = porcentaje_educacion_media_o_mas,
    y = porcentaje_ocupados,
    color = cuadrante
  )
) +
  geom_point(size = 1) +
  geom_vline(xintercept = mediana_educacion, linetype = 'dashed', color = 'gray50') +
  geom_hline(yintercept = mediana_ocupacion, linetype = 'dashed', color = 'gray50') +
  scale_color_manual(name = 'Cuadrante', values = colores_cuadrantes) +
  labs(x = '% Educación Media o Más', y = '% Ocupados', title = 'Relación Educación y Ocupación (por Manzana)') +
  theme_minimal() +
  theme(legend.position = "bottom")

print(grafico_dispersion_manzanas)

# =============================================================================
# 9) MAPA BIVARIADO CON BISCALE (A NIVEL DE MANZANA)
# =============================================================================

# 9.1 Clasificar datos
sf_mapa_manzanas_bi = bi_class(sf_mapa_manzanas, x = porcentaje_educacion_media_o_mas, y = porcentaje_ocupados, dim = 3, style = 'jenks')

# 9.2 Mapa bivariado
mapa_bivariado_manzanas = ggplot() +
  geom_sf(data = sf_mapa_manzanas_bi, aes(fill = bi_class), color = NA, show.legend = FALSE) +
  geom_sf(data = sf_manzanas, fill = NA, color = 'black', size = 0.1) +  #  Bordes de las manzanas
  bi_scale_fill(pal = 'DkBlue', dim = 3) +
  labs(title = 'Mapa Bivariado: Educación vs. Ocupación (por Manzana)',
       subtitle = 'Viña del Mar') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'), plot.subtitle = element_text(hjust = 0.5))

# 9.3 Leyenda bivariada
leyenda_bivariada = bi_legend(pal = 'DkBlue', dim = 3, xlab = '% Educación', ylab = '% Ocupación', size = 8)

# 9.4 Combinar mapa y leyenda
mapa_final_manzanas = ggdraw() +
  draw_plot(mapa_bivariado_manzanas, x = 0,   y = 0,   width = 1,   height = 1) +
  draw_plot(leyenda_bivariada,       x = 0.75, y = 0.05, width = 0.25, height = 0.25)

print(mapa_final_manzanas)

# =============================================================================
# 10) DESCONECTAR DE LA BASE DE DATOS
# =============================================================================
dbDisconnect(con)
