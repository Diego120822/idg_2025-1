# =============================================================================
# 1) INSTALAR PAQUETES (solo una vez)
# =============================================================================
# Estos paquetes permiten conexión a BD, manejo de geometrías y visualización
# install.packages(c("DBI", "RPostgres", "sf", "ggplot2", "cowplot", "biscale"))

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
db_port     = 5434                # puerto de escucha
db_name     = "censo_rm_2017"   # nombre de la base
db_user     = "postgres"        # usuario de conexión
db_password = "postgres"        # clave de usuario

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
# - % de personas con nivel educativo profesional (p15 entre 12 y 14)
# - % de viviendas con indicadores de hacinamiento (v.ind_hacin_rec en {2,4})

sql_indicadores = "
SELECT
  z.geocodigo::double precision AS geocodigo,
  c.nom_comuna,
  ROUND(  -- porcentaje de profesionales mayores de 18 años
    COUNT(*) FILTER (WHERE p.p15 BETWEEN 12 AND 14) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.p09 > 18), 0)
  , 2) AS total_profesionales,
  ROUND(  -- porcentaje de hacinamiento en viviendas
    COUNT(*) FILTER (WHERE v.ind_hacin_rec IN (2,4)) * 100.0
    / NULLIF(COUNT(*), 0)
  , 2) AS ptje_hacin
FROM public.personas   AS p
JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas      AS z ON v.zonaloc_ref_id   = z.zonaloc_ref_id
JOIN public.comunas    AS c ON z.codigo_comuna    = c.codigo_comuna
GROUP BY z.geocodigo, c.nom_comuna
ORDER BY total_profesionales DESC;
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
FROM dpa.zonas_censales_rm
WHERE nom_provin = 'SANTIAGO'
  AND urbano     = 1;
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
# Mapa del % de profesionales: relleno por valor de total_profesionales
map_total_profesionales = ggplot(sf_mapa) +
  geom_sf(aes(fill = total_profesionales), color = "#AAAAAA30", size = 0.1) +  
  labs(
    title = "Porcentaje de Profesionales",   # título principal
    fill  = "% Profesionales"               # etiqueta de leyenda
  ) +
  theme_minimal()

# Mapa del % de hacinamiento: relleno por valor de ptje_hacin
map_ptje_hacin = ggplot(sf_mapa) +
  geom_sf(aes(fill = ptje_hacin), color = "#AAAAAA30", size = 0.1) +
  labs(
    title = "Porcentaje de Hacinamiento",
    fill  = "% Hacinamiento"
  ) +
  theme_minimal()

# Mostrar los mapas en pantalla
print(map_total_profesionales)
print(map_ptje_hacin)

# =============================================================================
# 8) GRÁFICO DE DISPERSIÓN BIVARIADO
# =============================================================================
# 8.1 Calcular medianas para dividir cuadrantes
mediana_profesionales = median(sf_mapa$total_profesionales, na.rm = TRUE)
mediana_hacinamiento  = median(sf_mapa$ptje_hacin,          na.rm = TRUE)

# 8.2 Crear la variable que indica el cuadrante según comparaciones con medianas
sf_mapa$cuadrante = with(
  sf_mapa,
  ifelse(
    total_profesionales >= mediana_profesionales & ptje_hacin >= mediana_hacinamiento, 'Q1: Alta/Alta',
    ifelse(
      total_profesionales <  mediana_profesionales & ptje_hacin >= mediana_hacinamiento, 'Q2: Baja/Alta',
      ifelse(
        total_profesionales <  mediana_profesionales & ptje_hacin <  mediana_hacinamiento, 'Q3: Baja/Baja',
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
    x     = total_profesionales,
    y     = ptje_hacin,
    color = cuadrante
  )
) +
  geom_point(size = 2) +  # puntos de cada comuna
  geom_vline(xintercept = mediana_profesionales, linetype = 'dashed', color = 'gray50') +
  geom_hline(yintercept = mediana_hacinamiento,  linetype = 'dashed', color = 'gray50') +
  scale_color_manual(name = 'Cuadrante', values = colores_cuadrantes) +
  labs(x = '% Profesionales', y = '% Hacinamiento', title = 'Dispersión por Cuadrantes') +
  theme_minimal()

print(grafico_cuadrantes)

# =============================================================================
# 9) MAPA BIVARIADO CON BISCALE
# =============================================================================
# 9.1 Obtener geometría comunal para Santiago
sql_comunas = "
SELECT cut, nom_comuna, geom
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO';
"
sf_comunas_santiago = st_read(con, query = sql_comunas)

# 9.2 Clasificar datos en 3 x 3 bivariado
sf_mapa_bi = bi_class(sf_mapa, x = total_profesionales, y = ptje_hacin, dim = 3, style = 'jenks')


# 9.3 Calcular bbox y centroides para etiquetas comunales
caja = sf::st_bbox(sf_mapa_bi)
sf_comunas_centroides = st_centroid(sf_comunas_santiago)

# 9.4 Crear mapa bivariado sin bordes internos y con etiquetas
mapa_bivariado_etiquetas = ggplot() +
  geom_sf(data = sf_mapa_bi, aes(fill = bi_class), color = NA, show.legend = FALSE) +
  geom_sf(data = sf_comunas_santiago, fill = NA, color = 'black', size = 0.4) +
  geom_sf_text(data = sf_comunas_centroides, aes(label = nom_comuna), size = 2, fontface = 'bold') +
  bi_scale_fill(pal = 'DkBlue', dim = 3) +
  labs(title = 'Mapa bivariado para Profesionales vs. Hacinamiento', subtitle = 'Provincia de Santiago, RM') +
  coord_sf(xlim = c(caja['xmin'], caja['xmax']), ylim = c(caja['ymin'], caja['ymax']), expand = FALSE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'), plot.subtitle = element_text(hjust = 0.5))

# 9.5 Generar y posicionar leyenda bivariada
leyenda_bivariada = bi_legend(pal = 'DkBlue', dim = 3, xlab = '% Profesionales', ylab = '% Hacinamiento', size = 8)
mapa_final = ggdraw() +
  draw_plot(mapa_bivariado_etiquetas, x = 0,    y = 0,    width = 1,    height = 1) +
  draw_plot(leyenda_bivariada,          x = 0.75, y = 0.05, width = 0.25, height = 0.25)

print(mapa_final)
