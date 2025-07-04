# --- INSTALACIÓN Y LIBRERÍAS NECESARIAS ---
install.packages("pROC")
library(haven)
library(ggplot2)
library(pROC)

# --- CARGA DE DATOS ---
personas <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos   <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")

# --- FILTRO GRAN SANTIAGO Y VALIDACIÓN DE VARIABLES ---
personas_gs <- subset(personas, macrozona == 2 & sprincipal == 1)
valores_invalidos <- c(-99, -88, -77)
personas_gs <- subset(personas_gs,
                      !(edad %in% valores_invalidos) &
                        !(edue %in% valores_invalidos) &
                        ing_disp_hog_hd_ai >= 0)
personas_gs$ing_pc <- personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas

# --- UNIÓN CON GASTOS EN HOTELES ---
gastos$ccif_str <- as.character(gastos$ccif)
gastos_gs <- subset(gastos, macrozona == 2)
gastos_hoteles <- subset(gastos_gs, ccif_str == "11.2.1.01.01")

gasto_hoteles_por_folio <- aggregate(gasto ~ folio, data = gastos_hoteles, sum)
names(gasto_hoteles_por_folio)[2] <- "gasto_total_hoteles"
personas_gs <- merge(personas_gs, gasto_hoteles_por_folio, by = "folio", all.x = TRUE)
personas_gs$gasto_total_hoteles[is.na(personas_gs$gasto_total_hoteles)] <- 0
personas_gs$incurre_gasto_hoteles <- ifelse(personas_gs$gasto_total_hoteles > 0, 1, 0)

# --- VARIABLES PARA MODELADO ---
tabla_modelo <- personas_gs[, c("sexo", "edad", "edue", "ing_pc", "incurre_gasto_hoteles", "gasto_total_hoteles")]
tabla_modelo$grupo_escolaridad <- cut(tabla_modelo$edue, breaks = c(-Inf, 8, 12, 16, Inf),
                                      labels = c("Básica o menos", "Media-baja", "Media-alta", "Alta"))
tabla_modelo$grupo_edad <- cut(tabla_modelo$edad, breaks = c(0, 29, 39, 49, 59, 69, 120),
                               labels = c("0–29", "30–39", "40–49", "50–59", "60–69", "70+"))
tabla_modelo$log_ing_pc <- log1p(tabla_modelo$ing_pc)

# --- SEPARAR QUIENES GASTAN Y LIMPIAR OUTLIERS ---
gastadores <- subset(tabla_modelo, incurre_gasto_hoteles == 1)
percentiles <- quantile(gastadores$gasto_total_hoteles, probs = c(0.01, 0.99), na.rm = TRUE)
gastadores_limpio <- subset(gastadores,
                            gasto_total_hoteles >= percentiles[1] &
                              gasto_total_hoteles <= percentiles[2])
gastadores_limpio$log_ing_pc <- log1p(gastadores_limpio$ing_pc)
gastadores_limpio$log_gasto_total_hoteles <- log1p(gastadores_limpio$gasto_total_hoteles)
gastadores_limpio <- na.omit(gastadores_limpio)

# --- GRÁFICOS EXPLORATORIOS ---

# Histograma del gasto
hist(gastadores_limpio$gasto_total_hoteles,
     breaks = 50,
     col = "steelblue",
     main = "Distribución del Gasto en Hoteles (solo gastadores limpios)",
     xlab = "Gasto en Hoteles")

# Edad vs Gasto
plot(gastadores_limpio$edad, gastadores_limpio$gasto_total_hoteles,
     main = "Edad vs Gasto en Hoteles",
     xlab = "Edad", ylab = "Gasto",
     pch = 20, col = rgb(0, 0, 0, 0.3))
lines(lowess(gastadores_limpio$edad, gastadores_limpio$gasto_total_hoteles),
      col = "red", lwd = 2)

# Ingreso vs Gasto
plot(gastadores_limpio$ing_pc, gastadores_limpio$gasto_total_hoteles,
     main = "Ingreso vs Gasto en Hoteles",
     xlab = "Ingreso per cápita", ylab = "Gasto",
     pch = 20, col = rgb(0, 0, 0, 0.3))
lines(lowess(gastadores_limpio$ing_pc, gastadores_limpio$gasto_total_hoteles),
      col = "blue", lwd = 2)

# Boxplot por escolaridad
boxplot(gasto_total_hoteles ~ grupo_escolaridad,
        data = gastadores_limpio,
        main = "Gasto en Hoteles según Escolaridad",
        xlab = "Escolaridad", col = "lightblue")

# Proporción de gasto
barplot(prop.table(table(tabla_modelo$incurre_gasto_hoteles)),
        main = "Proporción de Hogares que Gastan en Hoteles",
        names.arg = c("No Gasta", "Gasta"),
        col = c("gray", "steelblue"))

# Ingreso per cápita según incurrencia
boxplot(ing_pc ~ incurre_gasto_hoteles, data = tabla_modelo,
        main = "Ingreso per cápita según Incurrencia en Gasto de Hoteles",
        xlab = "Incurrencia en Gasto", ylab = "Ingreso per cápita",
        col = c("orange", "steelblue"))

# --- MODELO LOGIT ---
modelo_logit <- glm(incurre_gasto_hoteles ~ sexo + grupo_edad + log_ing_pc + grupo_escolaridad,
                    data = tabla_modelo, family = binomial(link = "logit"))
summary(modelo_logit)

# Curva ROC y AUC
prob_pred <- predict(modelo_logit, type = "response")
roc_hoteles <- roc(tabla_modelo$incurre_gasto_hoteles, prob_pred)
plot(roc_hoteles, main = "Curva ROC - Logit Hoteles")
auc_hoteles <- auc(roc_hoteles)
print(paste("AUC Logit Hoteles:", round(auc_hoteles, 3)))

# Matriz de confusión
pred_clase <- ifelse(prob_pred > 0.5, 1, 0)
matriz_conf <- table(Actual = tabla_modelo$incurre_gasto_hoteles, Predicho = pred_clase)
print("Matriz de Confusión:")
print(matriz_conf)

# Métricas adicionales
if ("1" %in% colnames(matriz_conf) && matriz_conf[2, "1"] > 0) {
  precision <- matriz_conf[2, 2] / sum(matriz_conf[, 2])
  recall <- matriz_conf[2, 2] / sum(matriz_conf[2, ])
  f1 <- 2 * (precision * recall) / (precision + recall)
  print(paste("Precisión:", round(precision, 3)))
  print(paste("Recall:", round(recall, 3)))
  print(paste("F1-Score:", round(f1, 3)))
} else {
  print("Advertencia: No se puede calcular F1-score (modelo no predijo clase 1 o hay muy pocos casos positivos).")
}

# --- MODELO REGRESIÓN LINEAL ---
modelo_lm <- lm(log_gasto_total_hoteles ~ sexo + grupo_edad + log_ing_pc + grupo_escolaridad,
                data = gastadores_limpio)
summary(modelo_lm)

# Métricas de desempeño
pred_lm <- predict(modelo_lm, newdata = gastadores_limpio)
rmse <- sqrt(mean((expm1(pred_lm) - gastadores_limpio$gasto_total_hoteles)^2))
mae <- mean(abs(expm1(pred_lm) - gastadores_limpio$gasto_total_hoteles))
print(paste("R² Ajustado:", round(summary(modelo_lm)$adj.r.squared, 3)))
print(paste("RMSE:", round(rmse, 2)))
print(paste("MAE:", round(mae, 2)))

