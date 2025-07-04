# --- INSTALACIÓN Y LIBRERÍAS NECESARIAS ---
# Instalar y cargar librerías requeridas
install.packages("pROC")
library(haven)     # Para leer archivos .dta
library(ggplot2)   # Para visualización\mlibrary(pROC)      # Para curvas ROC y AUC

# --- CARGA DE DATOS EPF ---
# Cargar base de personas y gastos de la EPF
personas <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos   <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")

# --- FILTRO GRAN SANTIAGO Y LIMPIEZA DE VARIABLES ---
# Filtrar a personas del Gran Santiago y jefes de hogar (sprincipal == 1)
personas_gs <- subset(personas, macrozona == 2 & sprincipal == 1)

# Eliminar observaciones con valores inválidos y calcular ingreso per cápita
valores_invalidos <- c(-99, -88, -77)
personas_gs <- subset(personas_gs,
                      !(edad %in% valores_invalidos) &
                        !(edue %in% valores_invalidos) &
                        ing_disp_hog_hd_ai >= 0)
personas_gs$ing_pc <- personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas

# --- FILTRAR GASTOS EN HOTELES ---
# Filtrar datos de gasto para la categoría de hoteles
# ccif_str se crea para comparar como texto y evitar errores

# Filtrar macrozona 2 (Gran Santiago)
gastos$ccif_str <- as.character(gastos$ccif)
gastos_gs <- subset(gastos, macrozona == 2)

# Filtrar gastos relacionados con hoteles
# En este caso, usamos la categoría "11.2.1.01.01"
gastos_hoteles <- subset(gastos_gs, ccif_str == "11.2.1.01.01")

# Calcular gasto total en hoteles por folio (hogar)
gasto_hoteles_por_folio <- aggregate(gasto ~ folio, data = gastos_hoteles, sum)
names(gasto_hoteles_por_folio)[2] <- "gasto_total_hoteles"

# Unir gasto total a la base de personas y crear variable binaria de incurrencia
gastos_joined <- merge(personas_gs, gasto_hoteles_por_folio, by = "folio", all.x = TRUE)
gastos_joined$gasto_total_hoteles[is.na(gastos_joined$gasto_total_hoteles)] <- 0
gastos_joined$incurre_gasto_hoteles <- ifelse(gastos_joined$gasto_total_hoteles > 0, 1, 0)

# --- VARIABLES PARA EL MODELADO ---
# Selección de variables y creación de variables categóricas

tabla_modelo <- gastos_joined[, c("sexo", "edad", "edue", "ing_pc",
                                  "incurre_gasto_hoteles", "gasto_total_hoteles")]
tabla_modelo$grupo_escolaridad <- cut(tabla_modelo$edue, breaks = c(-Inf, 8, 12, 16, Inf),
                                      labels = c("Básica o menos", "Media-baja", "Media-alta", "Alta"))
tabla_modelo$grupo_edad <- cut(tabla_modelo$edad, breaks = c(0, 29, 39, 49, 59, 69, 120),
                               labels = c("0–29", "30–39", "40–49", "50–59", "60–69", "70+"))
tabla_modelo$log_ing_pc <- log1p(tabla_modelo$ing_pc)

# --- FILTRADO DE QUIENES GASTAN Y LIMPIEZA DE OUTLIERS ---
# Mantener solo quienes incurren en gasto y remover valores extremos

gastadores <- subset(tabla_modelo, incurre_gasto_hoteles == 1)
percentiles <- quantile(gastadores$gasto_total_hoteles, probs = c(0.01, 0.99), na.rm = TRUE)
gastadores_limpio <- subset(gastadores,
                            gasto_total_hoteles >= percentiles[1] &
                              gasto_total_hoteles <= percentiles[2])
gastadores_limpio$log_ing_pc <- log1p(gastadores_limpio$ing_pc)
gastadores_limpio$log_gasto_total_hoteles <- log1p(gastadores_limpio$gasto_total_hoteles)
gastadores_limpio <- na.omit(gastadores_limpio)

# --- GRÁFICOS EXPLORATORIOS ---

# Histograma del gasto limpio
hist(gastadores_limpio$gasto_total_hoteles,
     breaks = 50, col = "steelblue",
     main = "Distribución del Gasto en Hoteles",
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

# Proporción de gasto en la población
barplot(prop.table(table(tabla_modelo$incurre_gasto_hoteles)),
        main = "Proporción de Hogares que Gastan en Hoteles",
        names.arg = c("No Gasta", "Gasta"),
        col = c("gray", "steelblue"))

# Ingreso per cápita por incurrencia en gasto
boxplot(ing_pc ~ incurre_gasto_hoteles, data = tabla_modelo,
        main = "Ingreso per cápita según Incurrencia en Gasto de Hoteles",
        xlab = "Incurrencia en Gasto", ylab = "Ingreso per cápita",
        col = c("orange", "steelblue"))

# --- MODELO LOGIT: Probabilidad de gastar ---
modelo_logit <- glm(incurre_gasto_hoteles ~ sexo + grupo_edad + log_ing_pc + grupo_escolaridad,
                    data = tabla_modelo, family = binomial(link = "logit"))
summary(modelo_logit)

# Curva ROC y AUC
prob_pred <- predict(modelo_logit, type = "response")
roc_hoteles <- roc(tabla_modelo$incurre_gasto_hoteles, prob_pred)
plot(roc_hoteles, main = "Curva ROC - Logit Hoteles")
auc_hoteles <- auc(roc_hoteles)
print(paste("AUC Logit Hoteles:", round(auc_hoteles, 3)))

# Matriz de confusión y métricas de desempeño
pred_clase <- ifelse(prob_pred > 0.5, 1, 0)
matriz_conf <- table(Actual = tabla_modelo$incurre_gasto_hoteles, Predicho = pred_clase)
print("Matriz de Confusión:")
print(matriz_conf)

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

# --- MODELO REGRESIÓN LINEAL: Monto del gasto ---
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

# --- IMPUTACIÓN DEL GASTO EN CASEN 2022 ---

# Cargar base CASEN 2022 (ajustar ruta si es necesario)
casen <- read_dta("data/casen/casen2022.dta")

# Filtrado de casos válidos y cálculo de log ingreso
casen <- subset(casen, edad >= 18 & edad < 100 & ypc > 0)
casen$log_ypc <- log1p(casen$ypc)

# Categorización de edad y escolaridad similar a EPF
casen$grupo_edad <- cut(casen$edad,
                        breaks = c(0, 29, 39, 49, 59, 69, Inf),
                        labels = c("0–29", "30–39", "40–49", "50–59", "60–69", "70+"))
casen$grupo_escolaridad <- cut(casen$esc,
                               breaks = c(-Inf, 8, 12, 16, Inf),
                               labels = c("Básica o menos", "Media-baja", "Media-alta", "Alta"))

# Mantener solo registros completos
casen_modelo <- casen[!is.na(casen$grupo_edad) &
                        !is.na(casen$grupo_escolaridad) &
                        !is.na(casen$sexo) &
                        !is.na(casen$log_ypc), ]

# Predicción de probabilidad de incurrir en gasto
casen_modelo$prob_gastar <- predict(modelo_logit, newdata = casen_modelo, type = "response")

# Simulación binaria de incurrencia en gasto
set.seed(123)
casen_modelo$gasta <- rbinom(nrow(casen_modelo), 1, casen_modelo$prob_gastar)

# Predicción del monto de gasto para quienes incurren
gasto_pred <- rep(0, nrow(casen_modelo))
gasto_pred[casen_modelo$gasta == 1] <- predict(modelo_lm, newdata = casen_modelo[casen_modelo$gasta == 1, ])
casen_modelo$gasto_imputado <- ifelse(casen_modelo$gasta == 1, expm1(gasto_pred), 0)

# Estadísticas descriptivas del gasto imputado
cat("Resumen del gasto imputado en hoteles (CASEN 2022):\n")
summary(casen_modelo$gasto_imputado)

# Histograma del gasto imputado
hist(casen_modelo$gasto_imputado,
     breaks = 50, col = "darkgreen",
     main = "Distribución del Gasto Imputado en Hoteles (CASEN 2022)",
     xlab = "Gasto Imputado")

# Boxplot por quintiles de ingreso
casen_modelo$quintil_ingreso <- cut(casen_modelo$ypc,
                                    breaks = quantile(casen_modelo$ypc, probs = seq(0, 1, 0.2), na.rm = TRUE),
                                    include.lowest = TRUE,
                                    labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))

boxplot(gasto_imputado ~ quintil_ingreso, data = casen_modelo,
        main = "Gasto Imputado según Quintil de Ingreso",
        col = "skyblue", ylab = "Gasto en Hoteles")
