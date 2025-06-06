# =============================================================================
# 1) INSTALAR PAQUETES (solo una vez)
# =============================================================================
install.packages(c("DBI", "RPostgres", "sf", "ggplot2", "cowplot", "ggfortify", "factoextra"))

# =============================================================================
# 2) CARGAR LIBRERÍAS NECESARIAS
# =============================================================================
library(factoextra)
library(ggfortify)
library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(cowplot)

# =============================================================================
# 3) CONFIGURAR CONEXIÓN A BASE DE DATOS
# =============================================================================
db_host    = "localhost"
db_port    = 5432
db_name    = "censo_rm_2017" 
db_user    = "postgres"
db_password = "postgres"

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

sql_indicadores = "
SELECT
  z.geocodigo::double precision AS geocodigo,
  c.nom_comuna,

  -- Porcentaje de migrantes
  ROUND(
    COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99)) * 100.0
    / NULLIF(COUNT(*), 0),
  2) AS ptje_migrantes,

  -- Porcentaje de personas con escolaridad mayor a 12 años
  ROUND(
    COUNT(*) FILTER (WHERE p.escolaridad >= 12) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.escolaridad IS NOT NULL), 0),
  2) AS ptje_esc_mayor_12,

  -- Porcentaje de adultos mayores
  ROUND(
    COUNT(*) FILTER (WHERE p.p09 >= 65) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.p09 IS NOT NULL), 0),
  2) AS ptje_adulto_mayor

FROM public.personas   AS p
JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas      AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
JOIN public.comunas    AS c ON z.codigo_comuna   = c.codigo_comuna

GROUP BY z.geocodigo, c.nom_comuna
ORDER BY ptje_esc_mayor_12 DESC;

"

# Ejecutar consulta y importar resultados a data.frame en R
df_indicadores = dbGetQuery(con, sql_indicadores)

# ====================================================================================
# 5) Seleccionar variables y escarlatas
# =====================================================================================
vars_clusters = df_indicadores[,c("ptje_migrantes", "ptje_esc_mayor_12", "ptje_adulto_mayor")]

# Se escalan las variables
vars_scaled = scale(vars_clusters)

# 5) Metodo del codo, para elegir k
# visualiza la suma de cuadrados dentro del cluster (wss) para varios k
fviz_nbclust(vars_scaled, kmeans, method = "wss") +
  labs(tittle = "Método del codo", x = "Número de clusters (k)", y = "wss")

# 5) K-means
set.seed(123)  # para reproducibilidad
km = kmeans(vars_scaled, centers = 4, nstart = 25)

df_indicadores$cluster = as.factor(km$cluster)

# Escolaridad v/s Migración
ggplot(df_indicadores_manzana, aes(x = ptje_esc_mayor_12, y = ptje_migrantes, color = cluster)) + 
  geom_point(size = 2) +
  labs(tittle = "escolaridad v/s migrantes",
       x = "% población con 12 años de escolaridad",
       y = "% población migrante") + 
  theme_minimal()

# C1: Escolaridad madia-baja, migración baja, pablación joven
# C2: Escolaridad media-alta , alta migración, población joven
# C3: Escolaridad alta, migración media baja, población mayor
# C4: Escolaridad baja, migración baja, población mayor

