# =============================================================================
# 1) INSTALAR PAQUETES (solo una vez)
# =============================================================================
# Estos paquetes permiten conexión a BD, manejo de geometrías y visualización
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
# Definir parámetros de conexión
db_host     = "localhost"       # servidor de BD
db_port     = 5432                # puerto de escucha
db_name     = "CENSO_QUINTA_REGION_2017 "   # nombre de la base
db_user     = "postgres"        # usuario de conexión
db_password = "POSTGRES"        # clave de usuario

# Establecer conexión usando RPostgres
con = dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)

# =============================================================================
# 4) EXTRAER INDICADORES DESDE CENSO
# =============================================================================
# SQL para calcular:
# - % de personas con educación media o más (p.escolaridad entre 7 y 14)
# - % de personas ocupadas (p.p18)

sql_indicadores = "
SELECT
  z.geocodigo::double precision AS geocodigo,
  c.nom_comuna,

  -- Porcentaje de personas con educación media o más
  ROUND(
    COUNT(*) FILTER (WHERE p.escolaridad IN (7,8,9,10,11,12,13,14)) * 100.0
    / NULLIF(COUNT(*), 0),
  2) AS porcentaje_educacion_media_o_mas,

  -- Porcentaje de ocupados
  ROUND(
    COUNT(*) FILTER (WHERE p.p18 NOT IN ('98', '99') AND p.p18 IS NOT NULL) * 100.0
    / NULLIF(COUNT(*), 0),
  2) AS porcentaje_ocupados,

  -- Número de personas en agricultura
  COUNT(*) FILTER (WHERE p.p18 = 'A') AS num_agricultura,

  -- Número de personas en otras actividades
  COUNT(*) FILTER (
    WHERE p.p18 IN ('B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','Z')
  ) AS num_otras_actividades,

  -- Número de casos no aplicables
  COUNT(*) FILTER (WHERE p.p18 IN ('98', '99')) AS num_no_aplicables

FROM public.personas   AS p
JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas      AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
JOIN public.comunas    AS c ON z.codigo_comuna   = c.codigo_comuna

WHERE c.nom_comuna = 'VIÑA DEL MAR' AND p.escolaridad IS NOT NULL

GROUP BY z.geocodigo, c.nom_comuna
ORDER BY porcentaje_educacion_media_o_mas DESC;
"
# Ejecutar consulta y importar resultados a data.frame en R
df_indicadores = dbGetQuery(con, sql_indicadores)

# =============================================================================
# 5) CARGAR GEOMETRÍA DE ZONAS CENSALES
# =============================================================================
# SQL para obtener geometría espacial de zonas dentro de Santiago urbano
sql_geometria = "
SELECT
  geocodigo::double precision AS geocodigo,
  geom
FROM dpa.zonas_censales_v
WHERE nom_comuna = 'VIÑA DEL MAR';
"
# Leer la capa espacial directamente desde la BD
sf_zonas = st_read(con, query = sql_geometria)

# =============================================================================
# 6) COMBINAR DATOS TABULARES Y ESPACIALES
# =============================================================================
# Merge por geocódigo para obtener un objeto sf con atributos e indicadores
sf_mapa = merge(
  x     = sf_zonas,
  y     = df_indicadores,
  by    = "geocodigo",
  all.x = FALSE  # conservar solo combinaciones existentes
)

# =============================================================================
# 7) MAPAS TEMÁTICOS SIMPLES
# =============================================================================
# Mapa del % de Personas con educación media o más: relleno por valor de porcentaje_educacion_media_o_mas
map_porcentaje_educacion_media_o_mas= ggplot(sf_mapa) +
  geom_sf(aes(fill = porcentaje_educacion_media_o_mas), color = "#AAAAAA30", size = 0.1) +  
  labs(
    title = "Porcentaje de personas con educación media o más",   # título principal
    fill  = "% Personas con educación media o más"               # etiqueta de leyenda
  ) +
  theme_minimal()

# Mapa del % de personas ocupadas: relleno por valor de porcentaje_ocupado
map_porcentaje_ocupados = ggplot(sf_mapa) +
  geom_sf(aes(fill = porcentaje_ocupados), color = "#AAAAAA30", size = 0.1) +
  labs(
    title = "Porcentaje de personas ocupadas",
    fill  = "% personas ocupadas"
  ) +
  theme_minimal()

# Mostrar los mapas en pantalla
print(map_porcentaje_educacion_media_o_mas)
print(map_porcentaje_ocupados)

# =============================================================================
# 8) GRÁFICO DE DISPERSIÓN BIVARIADO
# =============================================================================
# 8.1 Calcular medianas para dividir cuadrantes
mediana_porcentaje_educacion_media_o_mas = median(sf_mapa$porcentaje_educacion_media_o_mas, na.rm = TRUE)
mediana_personas_ocupadas = median(sf_mapa$porcentaje_ocupados, na.rm = TRUE)

# 8.2 Crear la variable que indica el cuadrante según comparaciones con medianas
sf_mapa$cuadrante = with(
  sf_mapa,
  ifelse(
    porcentaje_educacion_media_o_mas >= mediana_porcentaje_educacion_media_o_mas & porcentaje_ocupados >= mediana_personas_ocupadas, 'Q1: Alta/Alta',
    ifelse(
      porcentaje_educacion_media_o_mas <  mediana_porcentaje_educacion_media_o_mas & porcentaje_ocupados >= mediana_personas_ocupadas, 'Q2: Baja/Alta',
      ifelse(
        porcentaje_educacion_media_o_mas <  mediana_porcentaje_educacion_media_o_mas & porcentaje_ocupados <  mediana_personas_ocupadas, 'Q3: Baja/Baja',
        'Q4: Alta/Baja'
      )
    )
  )
)

# 8.3 Definir paleta de colores manual para cada cuadrante
colores_cuadrantes = c(
  'Q1: Alta/Alta' = '#08519c',  # alto/alto
  'Q2: Baja/Alta' = '#6baed6',  # bajo/alto
  'Q3: Baja/Baja' = '#eff3ff',  # bajo/bajo
  'Q4: Alta/Baja' = '#bdd7e7'   # alto/bajo
)

# 8.4 Construir scatterplot con líneas de mediana
grafico_cuadrantes = ggplot(
  sf_mapa,
  aes(
    x     = porcentaje_educacion_media_o_mas,
    y     = porcentaje_ocupados,
    color = cuadrante
  )
) +
  geom_point(size = 2) +  # puntos de cada comuna
  geom_vline(xintercept = mediana_porcentaje_educacion_media_o_mas, linetype = 'dashed', color = 'gray50') +
  geom_hline(yintercept = mediana_personas_ocupadas,  linetype = 'dashed', color = 'gray50') +
  scale_color_manual(name = 'Cuadrante', values = colores_cuadrantes) +
  labs(x = '% Personas con educacion media o mas', y = '% Personas ocupadas', title= 'Dispersión por Cuadrantes') +
  theme_minimal()

print(grafico_cuadrantes)

# =============================================================================
# 9) MAPA BIVARIADO CON BISCALE
# =============================================================================
# 9.1 Obtener geometría comunal para Santiago
sql_comunas = "
SELECT comuna, nom_comuna, geom
FROM dpa.comunas_v
WHERE nom_comuna = 'VIÑA DEL MAR';
"
sf_comunas_viña_del_mar = st_read(con, query = sql_comunas)

# 9.2 Clasificar datos en 3 x 3 bivariado
sf_mapa_bi = bi_class(sf_mapa, x = porcentaje_educacion_media_o_mas, y = porcentaje_ocupados, dim = 3, style = 'jenks')


# 9.3 Calcular bbox y centroides para etiquetas comunales
caja = sf::st_bbox(sf_mapa_bi)
sf_comunas_centroides = st_centroid(sf_comunas_viña_del_mar)

# 9.4 Crear mapa bivariado sin bordes internos y con etiquetas
mapa_bivariado_etiquetas = ggplot() +
  geom_sf(data = sf_mapa_bi, aes(fill = bi_class), color = NA, show.legend = FALSE) +
  geom_sf(data = sf_comunas_viña_del_mar, fill = NA, color = 'black', size = 0.4) +
  geom_sf_text(data = sf_comunas_centroides, aes(label = nom_comuna), size = 2, fontface = 'bold') +
  bi_scale_fill(pal = 'DkBlue', dim = 3) +
  labs(title = 'Mapa bivariado para Personas con educación media o más vs. Personas ocupadas', subtitle= 'Provincia de Viña del Mar, V Región') +
  coord_sf(xlim = c(caja['xmin'], caja['xmax']), ylim = c(caja['ymin'], caja['ymax']), expand = FALSE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'), plot.subtitle = element_text(hjust = 0.5))

# 9.5 Generar y posicionar leyenda bivariada
leyenda_bivariada = bi_legend(pal = 'DkBlue', dim = 3, xlab = '% Personas con educacion media o mas', ylab = '% Personas ocupadas', size = 8)
mapa_final = ggdraw() +
  draw_plot(mapa_bivariado_etiquetas, x = 0,    y = 0,    width = 1,    height = 1) +
  draw_plot(leyenda_bivariada,          x = 0.75, y = 0.05, width = 0.25, height = 0.25)

print(mapa_final)


