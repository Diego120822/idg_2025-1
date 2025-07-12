# ------------------------
# CARGA DE LIBRERÍAS
# ------------------------
library(haven)
library(dplyr)
library(ggplot2)
library(pROC)
library(sf)
library(cluster)
library(factoextra)
library(tidyr)

# ------------------------
# CARGA Y PREPARACIÓN DE EPF
# ------------------------
setwd("C:/Users/Personal/Desktop/IDG 2025/idg_2025-1/")
personas <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos   <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")

# Filtro Gran Santiago y limpieza
personas <- subset(personas, macrozona == 2 & sprincipal == 1)
valores_invalidos <- c(-99, -88, -77)
personas <- subset(personas, !(edad %in% valores_invalidos) &
                     !(edue %in% valores_invalidos) &
                     ing_disp_hog_hd_ai >= 0)
personas$ing_pc <- personas$ing_disp_hog_hd_ai / personas$npersonas

# Cargar gasto en hoteles
gastos$ccif_str <- as.character(gastos$ccif)
gastos <- subset(gastos, macrozona == 2 & ccif_str == "11.2.1.01.01")
gasto_hoteles <- aggregate(gasto ~ folio, data = gastos, sum)
names(gasto_hoteles)[2] <- "gasto_total_hoteles"

# Unir y crear variables
personas <- merge(personas, gasto_hoteles, by = "folio", all.x = TRUE)
personas$gasto_total_hoteles[is.na(personas$gasto_total_hoteles)] <- 0
personas$incurre_gasto <- ifelse(personas$gasto_total_hoteles > 0, 1, 0)
personas$grupo_escolaridad <- cut(personas$edue, breaks = c(-Inf, 12, 14, 16, Inf),
                                  labels = c("Escolar", "Tecnico", "Universitaria", "Postgrado"))
personas$rango_edad <- cut(personas$edad, breaks = c(0, 29, 44, 64, Inf),
                           labels = c("jovenes", "adultos_jovenes", "adultos", "adultos_mayores"))
personas$log_ing_pc <- log1p(personas$ing_pc)

# ------------------------
# MODELO LOGIT
# ------------------------
modelo_logit <- glm(
  incurre_gasto ~ factor(sexo) + rango_edad + grupo_escolaridad + log_ing_pc,
  data = personas,
  family = binomial
)
personas$prob_predicha <- predict(modelo_logit, type = "response")
personas$clasificacion_05 <- ifelse(personas$prob_predicha >= 0.5, 1, 0)
conf_05 <- table(Real = personas$incurre_gasto, Predicha = personas$clasificacion_05)
accuracy_05 <- mean(personas$incurre_gasto == personas$clasificacion_05)

# ROC, AUC y Youden
roc_obj <- roc(personas$incurre_gasto, personas$prob_predicha)
auc_roc <- auc(roc_obj)
plot(roc_obj, col = "blue", main = "Curva ROC - Gasto en Hoteles")
coords_opt <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
umbral_opt <- as.numeric(coords_opt["threshold"])

# Clasificación con umbral óptimo
personas$clasificacion_opt <- ifelse(personas$prob_predicha >= umbral_opt, 1, 0)
conf_opt <- table(Real = personas$incurre_gasto, Predicha = personas$clasificacion_opt)
accuracy_opt <- mean(personas$incurre_gasto == personas$clasificacion_opt)
sensibilidad_opt <- conf_opt["1", "1"] / sum(conf_opt["1", ])
especificidad_opt <- conf_opt["0", "0"] / sum(conf_opt["0", ])

# ------------------------
# MODELO LINEAL ENTRE QUIENES GASTAN
# ------------------------
tabla_gasto <- subset(personas, incurre_gasto == 1)
q_ing <- quantile(tabla_gasto$ing_pc, probs = c(0.01, 0.99))
q_gasto <- quantile(tabla_gasto$gasto_total_hoteles, probs = c(0.01, 0.99))
tabla_gasto <- subset(tabla_gasto, ing_pc >= q_ing[1] & ing_pc <= q_ing[2] &
                        gasto_total_hoteles >= q_gasto[1] & gasto_total_hoteles <= q_gasto[2])
tabla_gasto$log_gasto <- log1p(tabla_gasto$gasto_total_hoteles)

modelo_lineal <- lm(
  log_gasto ~ grupo_escolaridad + log_ing_pc + rango_edad + factor(sexo),
  data = tabla_gasto
)
pred_lm <- predict(modelo_lineal, newdata = tabla_gasto)
rmse <- sqrt(mean((expm1(pred_lm) - tabla_gasto$gasto_total_hoteles)^2))
mae <- mean(abs(expm1(pred_lm) - tabla_gasto$gasto_total_hoteles))
r2_ajustado <- summary(modelo_lineal)$adj.r.squared

cat("RMSE:", rmse, "\nMAE:", mae, "\nR² Ajustado:", r2_ajustado, "\n")

# ------------------------
# IMPUTACIÓN SOBRE CASEN 2022
# ------------------------
casen <- readRDS("data/casen_rm.rds")

casen <- casen %>%
  filter(edad >= 18 & edad < 100 & ypc > 0) %>%
  mutate(
    rango_edad = factor(
      cut(edad, breaks = c(0, 29, 44, 64, Inf),
          labels = c("jovenes", "adultos_jovenes", "adultos", "adultos_mayores")),
      levels = c("jovenes", "adultos_jovenes", "adultos", "adultos_mayores")
    ),
    grupo_escolaridad = factor(
      cut(esc, breaks = c(-Inf, 12, 14, 16, Inf),
          labels = c("Escolar", "Tecnico", "Universitaria", "Postgrado")),
      levels = c("Escolar", "Tecnico", "Universitaria", "Postgrado")
    ),
    log_ing_pc = log1p(ypc),
    sexo = as.numeric(sexo)
  ) %>%
  filter(!is.na(rango_edad), !is.na(grupo_escolaridad), !is.na(sexo), !is.na(log_ing_pc))

# Imputación con modelo logit
casen$prob_gastar <- predict(modelo_logit, newdata = casen, type = "response")
set.seed(123)
casen$gasta <- rbinom(nrow(casen), 1, casen$prob_gastar)

# Imputación con modelo lineal (sólo quienes gastan)
casen$log_gasto_pred <- NA
casen$log_gasto_pred[casen$gasta == 1] <- predict(modelo_lineal, newdata = casen[casen$gasta == 1, ])
casen$gasto_imputado <- ifelse(casen$gasta == 1, expm1(casen$log_gasto_pred), 0)

# ------------------------
# AGREGADO POR ZONA CENSAL
# ------------------------
set.seed(42)
casen$zone <- paste0("ZC_", sample(1000:1999, nrow(casen), replace = TRUE))

gasto_zona <- casen %>%
  group_by(zone) %>%
  summarise(
    gasto_total = sum(gasto_imputado),
    gasto_promedio = mean(gasto_imputado[gasto_imputado > 0]),
    n_personas = n()
  )

# ------------------------
# COBERTURA ESPACIAL (SIMULADA)
# ------------------------
set.seed(101)
gasto_zona$cobertura_centroide <- sample(c(TRUE, FALSE), nrow(gasto_zona), replace = TRUE, prob = c(0.7, 0.3))
gasto_zona$oportunidad <- ifelse(!gasto_zona$cobertura_centroide & gasto_zona$gasto_total > quantile(gasto_zona$gasto_total, 0.75), "Alta", "Baja")

# ------------------------
# CLUSTERING K-MEANS
# ------------------------
gasto_zona_clean <- gasto_zona %>%
  filter(!is.na(gasto_total), !is.na(gasto_promedio), !is.na(n_personas),
         !is.nan(gasto_total), !is.nan(gasto_promedio), !is.nan(n_personas)) %>%
  mutate(
    gasto_total = ifelse(is.nan(gasto_total), 0, gasto_total),
    gasto_promedio = ifelse(is.nan(gasto_promedio), 0, gasto_promedio)
  )

cluster_data <- gasto_zona_clean %>%
  select(gasto_total, gasto_promedio, n_personas) %>%
  scale()
cluster_data <- cluster_data[is.finite(rowSums(cluster_data)), ]
set.seed(123)
km <- kmeans(cluster_data, centers = 4)
gasto_zona_clean$cluster <- factor(km$cluster)

# Visualización opcional
fviz_cluster(km, data = cluster_data, main = "Clusters de zonas censales según gasto y tamaño")
