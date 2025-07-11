library(haven)
library(dplyr)
library(ggplot2)
library(pROC)
library(sf)
library(cluster)
library(factoextra)
library(tidyr)

# Modelamiento con EPF: Logit y Regresión
# Cargar datos EPF
personas <- read_dta(".../data/datos_epf/base-personas-ix-epf-stata.dta")
gastos <- read_dta(".../data/datos_epf/base-gastos-ix-epf-stata.dta")

# Filtro Gran Santiago, jefes de hogar y limpieza
personas_gs <- subset(personas, macrozona == 2 & sprincipal == 1)
valores_invalidos <- c(-99, -88, -77)
personas_gs <- subset(personas_gs,
                      !(edad %in% valores_invalidos) &
                        !(edue %in% valores_invalidos) &
                        ing_disp_hog_hd_ai >= 0)
personas_gs$ing_pc <- personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas

# Gasto en hoteles
gastos$ccif_str <- as.character(gastos$ccif)
gastos_gs <- subset(gastos, macrozona == 2)
gastos_hoteles <- subset(gastos_gs, ccif_str == "11.2.1.01.01")
gasto_hoteles_por_folio <- aggregate(gasto ~ folio, data = gastos_hoteles, sum)
names(gasto_hoteles_por_folio)[2] <- "gasto_total_hoteles"

# Merge gasto con personas
gastos_joined <- merge(personas_gs, gasto_hoteles_por_folio, by = "folio", all.x = TRUE)
gastos_joined$gasto_total_hoteles[is.na(gastos_joined$gasto_total_hoteles)] <- 0
gastos_joined$incurre_gasto_hoteles <- ifelse(gastos_joined$gasto_total_hoteles > 0, 1, 0)

# Preparar variables
tabla_modelo <- gastos_joined %>%
  mutate(
    grupo_escolaridad = cut(edue, breaks = c(-Inf, 8, 12, 16, Inf),
                            labels = c("Básica o menos", "Media-baja", "Media-alta", "Alta")),
    grupo_edad = cut(edad, breaks = c(0, 29, 39, 49, 59, 69, 120),
                     labels = c("0–29", "30–39", "40–49", "50–59", "60–69", "70+")),
    log_ing_pc = log1p(ing_pc)
  )

# Modelo Logit
modelo_logit <- glm(incurre_gasto_hoteles ~ sexo + grupo_edad + log_ing_pc + grupo_escolaridad,
                    data = tabla_modelo, family = binomial)
# Modelo Lineal
gastadores <- subset(tabla_modelo, incurre_gasto_hoteles == 1)
gastadores <- gastadores %>%
  mutate(log_gasto_total_hoteles = log1p(gasto_total_hoteles))
modelo_lm <- lm(log_gasto_total_hoteles ~ sexo + grupo_edad + log_ing_pc + grupo_escolaridad, data = gastadores)


# APLICACION CASEN 2022
casen <- readRDS("data/casen_rm.rds")
casen <- casen %>%
  filter(edad >= 18 & edad < 100 & ypc > 0) %>%
  mutate(
    grupo_edad = cut(edad, breaks = c(0, 29, 39, 49, 59, 69, Inf),
                     labels = c("0–29", "30–39", "40–49", "50–59", "60–69", "70+")),
    grupo_escolaridad = cut(esc, breaks = c(-Inf, 8, 12, 16, Inf),
                            labels = c("Básica o menos", "Media-baja", "Media-alta", "Alta")),
    log_ing_pc = log1p(ypc),
    sexo = factor(ifelse(sexo == 1, "Hombre", "Mujer"))
  )

# Imputación con modelo logit y lineal
casen$prob_gastar <- predict(modelo_logit, newdata = casen, type = "response")
set.seed(123)
casen$gasta <- rbinom(nrow(casen), 1, casen$prob_gastar)
casen$log_gasto_pred <- NA
casen$log_gasto_pred[casen$gasta == 1] <- predict(modelo_lm, newdata = casen[casen$gasta == 1, ])
casen$gasto_imputado <- ifelse(casen$gasta == 1, expm1(casen$log_gasto_pred), 0)


# SIMULACION ESPACIAL
set.seed(42)
zonas <- paste0("ZC_", sample(1000:1999, nrow(casen), replace = TRUE))
casen$zone <- zonas

gasto_zona <- casen %>%
  group_by(zone) %>%
  summarise(
    gasto_total = sum(gasto_imputado),
    gasto_promedio = mean(gasto_imputado[gasto_imputado > 0]),
    n_personas = n()
  )

# sIMULACION DE COBERTURA (ISOCRONAS)
set.seed(101)
gasto_zona$cobertura_centroide <- sample(c(TRUE, FALSE), nrow(gasto_zona), replace = TRUE, prob = c(0.7, 0.3))
gasto_zona$oportunidad <- ifelse(!gasto_zona$cobertura_centroide & gasto_zona$gasto_total > quantile(gasto_zona$gasto_total, 0.75), "Alta", "Baja")

# CLUSTERS K-MEANS POR ZONA CENSAL
cluster_data <- gasto_zona %>%
  select(gasto_total, gasto_promedio, n_personas) %>%
  scale()

set.seed(123)
km <- kmeans(cluster_data, centers = 4)
gasto_zona$cluster <- factor(km$cluster)

# Visualizar clusters
fviz_cluster(km, data = cluster_data)

# {r clustering}
# Variables para cluster
# cluster_data <- gasto_zona %>%
# st_drop_geometry() %>%
# select(gasto_total, gasto_promedio, n_personas) %>%
# scale()
# Determinar número óptimo de clusters
# fviz_nbclust(cluster_data, kmeans, method = "wss")
# Aplicar k-means con k = 4
# set.seed(123)
# km <- kmeans(cluster_data, centers = 4, nstart = 25)
#gasto_zona$cluster <- factor(km$cluster)
# {r mapa-clusters}
#ggplot(gasto_zona) +
# geom_sf(aes(fill = cluster)) +
# theme_minimal() +
# labs(title = "Clusters de zonas según características y gasto") #####

