install.packages("pROC")
library(haven)
library(ggplot2)
library(pROC)
library(mgcv)

# --- CARGA DE DATOS ---
personas <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos   <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")
# Considera cargar 'ccif_codes' si necesitas consultar los códigos CCIF.
# ccif_codes <- read_dta("data/datos_epf/ccif-ix-epf-stata.dta")

# --- FILTRO GRAN SANTIAGO (Macrozona 2) ---
personas_gs = subset(personas, macrozona == 2 & sprincipal == 1)

# --- Limpieza de valores inválidos y cálculo de ingreso per cápita ---
valores_invalidos <- c(-99, -88, -77)
personas_gs = subset(personas_gs, !(edad %in% valores_invalidos) &
                       !(edue %in% valores_invalidos) &
                       ing_disp_hog_hd_ai >= 0)
personas_gs$ing_pc = personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas

# --- Identificar y sumar gastos de MASAS DULCES, TORTAS, etc. ---
# CCIF para Masas Dulces, Tortas, Tartaletas y otros productos de masas dulces
# adquiridos en restaurantes, cafés y similares, con servicio completo
CCIF_MASAS_DULCES <- "11.1.1.01.03"

# Filtra los gastos de la macrozona 2
gastos_gs <- subset(gastos, macrozona == 2)
gastos_gs$ccif_str <- as.character(gastos_gs$ccif) # Asegura que ccif sea tipo caracter para comparación

# Filtrar por el CCIF específico de Masas Dulces
gastos_masas_dulces <- subset(gastos_gs, ccif_str == CCIF_MASAS_DULCES)

# Sumar el gasto total en masas dulces por folio (hogar)
gasto_masas_dulces_por_folio <- aggregate(gasto ~ folio, data = gastos_masas_dulces, sum)
names(gasto_masas_dulces_por_folio)[2] <- "gasto_total_masas_dulces"

# --- UNIR GASTO DE MASAS DULCES CON DATOS DE PERSONAS ---
personas_gs <- merge(personas_gs, gasto_masas_dulces_por_folio, by = "folio", all.x = TRUE)

# --- CREAR VARIABLE BINARIA DE GASTO (modelo logit) ---
# Se rellenan con 0 los hogares que no reportaron gasto en esta categoría.
personas_gs$gasto_total_masas_dulces[is.na(personas_gs$gasto_total_masas_dulces)] <- 0
personas_gs$incurre_gasto_masas_dulces <- ifelse(personas_gs$gasto_total_masas_dulces > 0, 1, 0)

# *** ¡¡¡VERIFIQUE LA DISTRIBUCIÓN DE ESTA VARIABLE AQUÍ ANTES DE CONTINUAR!!! ***
# Esto le mostrará cuántos hogares tienen gasto > 0 y cuántos tienen gasto = 0.
# Es fundamental para evaluar la idoneidad del modelo Logit.
print("Distribución de hogares que incurren en gasto de Masas Dulces (0=No Gasta, 1=Gasta):")
print(table(personas_gs$incurre_gasto_masas_dulces))


# --- SELECCIÓN DE VARIABLES FINALES PARA LA TABLA DE MODELADO ---
tabla_para_modelo <- personas_gs[, c("sexo", "edad", "edue", "ing_pc", "incurre_gasto_masas_dulces", "gasto_total_masas_dulces")]

# --- Preparación para la regresión lineal (solo para quienes gastan) ---
tabla_solo_gastadores <- subset(tabla_para_modelo, incurre_gasto_masas_dulces == 1)

# --- Agrupación de variables categóricas (para ambas tablas) ---
tabla_para_modelo$grupo_escolaridad <- cut(tabla_para_modelo$edue, breaks = c(-Inf, 8, 12, 16, Inf), labels = c("Básica o menos", "Media-baja", "Media-alta", "Alta"), right = TRUE)
tabla_para_modelo$grupo_edad <- cut(tabla_para_modelo$edad, breaks = c(0, 29, 39, 49, 59, 69, 120), labels = c("0–29", "30–39", "40–49", "50–59", "60–69", "70+"), right = TRUE, include.lowest = TRUE)

tabla_solo_gastadores$grupo_escolaridad <- cut(tabla_solo_gastadores$edue, breaks = c(-Inf, 8, 12, 16, Inf), labels = c("Básica o menos", "Media-baja", "Media-alta", "Alta"), right = TRUE)
tabla_solo_gastadores$grupo_edad <- cut(tabla_solo_gastadores$edad, breaks = c(0, 29, 39, 49, 59, 69, 120), labels = c("0–29", "30–39", "40–49", "50–59", "60–69", "70+"), right = TRUE, include.lowest = TRUE)

# --- Transformación logarítmica para el gasto y el ingreso ---
tabla_solo_gastadores$log_gasto_total_masas_dulces <- log1p(tabla_solo_gastadores$gasto_total_masas_dulces)
tabla_para_modelo$log_ing_pc <- log1p(tabla_para_modelo$ing_pc)
tabla_solo_gastadores$log_ing_pc <- log1p(tabla_solo_gastadores$ing_pc)


# --- GRÁFICOS EXPLORATORIOS (adaptar títulos y variables para "Gasto en Masas Dulces") ---
hist(tabla_solo_gastadores$gasto_total_masas_dulces, breaks = 50, col = "purple", main = "Distribución del Gasto en Masas Dulces (solo gastadores)", xlab = "Gasto en Masas Dulces")
plot(tabla_solo_gastadores$edad, tabla_solo_gastadores$gasto_total_masas_dulces, main = "Edad vs Gasto en Masas Dulces", xlab = "Edad", ylab = "Gasto", pch = 20, col = rgb(0,0,0,0.3))
lines(lowess(tabla_solo_gastadores$edad, tabla_solo_gastadores$gasto_total_masas_dulces), col = "red", lwd = 2)
plot(tabla_solo_gastadores$ing_pc, tabla_solo_gastadores$gasto_total_masas_dulces, main = "Ingreso vs Gasto en Masas Dulces", xlab = "Ingreso per cápita", ylab = "Gasto", pch = 20, col = rgb(0,0,0,0.3))
lines(lowess(tabla_solo_gastadores$ing_pc, tabla_solo_gastadores$gasto_total_masas_dulces), col = "blue", lwd = 2)
boxplot(gasto_total_masas_dulces ~ grupo_escolaridad, data = tabla_solo_gastadores, main = "Gasto en Masas Dulces según Escolaridad", xlab = "Escolaridad", col = "plum")

barplot(prop.table(table(tabla_para_modelo$incurre_gasto_masas_dulces)), main = "Proporción de Hogares que Gastan en Masas Dulces", names.arg = c("No Gasta", "Gasta"), col = c("gray", "purple"))
boxplot(ing_pc ~ incurre_gasto_masas_dulces, data = tabla_para_modelo, main = "Ingreso per cápita según Incurrencia en Gasto de Masas Dulces", xlab = "Incurrencia en Gasto", ylab = "Ingreso per cápita", col = c("darkorange", "mediumpurple"))

# --- MODELO DE DOS PARTES ---

# 1. Modelo Logit (probabilidad de incurrir en gasto de Masas Dulces)
# Se utiliza la variable 'incurre_gasto_masas_dulces'
modelo_logit_masas_dulces <- glm(incurre_gasto_masas_dulces ~ sexo + grupo_edad + log_ing_pc + grupo_escolaridad, data = tabla_para_modelo, family = binomial(link = "logit"))
summary(modelo_logit_masas_dulces)

# Métricas de desempeño para el Modelo Logit
prob_predicha_logit_masas_dulces <- predict(modelo_logit_masas_dulces, type = "response")
roc_curve_masas_dulces <- roc(tabla_para_modelo$incurre_gasto_masas_dulces, prob_predicha_logit_masas_dulces)
plot(roc_curve_masas_dulces, main = "Curva ROC para el Modelo Logit (Gasto en Masas Dulces)")
auc_logit_masas_dulces <- auc(roc_curve_masas_dulces)
print(paste("AUC del Modelo Logit (Gasto en Masas Dulces):", round(auc_logit_masas_dulces, 3)))

prediccion_clase_logit_masas_dulces <- ifelse(prob_predicha_logit_masas_dulces > 0.5, 1, 0)
matriz_conf_logit_masas_dulces <- table(Actual = tabla_para_modelo$incurre_gasto_masas_dulces, Predicho = prediccion_clase_logit_masas_dulces)
print("Matriz de Confusión Logit (Gasto en Masas Dulces):")
print(matriz_conf_logit_masas_dulces)

# Este bloque condicional evita errores si el modelo no predice la clase '1'
# (es decir, si la columna '1' en Predicho no existe o es todo cero en la matriz).
if ("1" %in% colnames(matriz_conf_logit_masas_dulces) && matriz_conf_logit_masas_dulces[2,"1"] > 0) {
  precision_logit_masas_dulces <- matriz_conf_logit_masas_dulces[2,2] / sum(matriz_conf_logit_masas_dulces[,2])
  recall_logit_masas_dulces <- matriz_conf_logit_masas_dulces[2,2] / sum(matriz_conf_logit_masas_dulces[2,])
  f1_score_logit_masas_dulces <- 2 * (precision_logit_masas_dulces * recall_logit_masas_dulces) / (precision_logit_masas_dulces + recall_logit_masas_dulces)
  
  print(paste("Precisión Logit (Gasto en Masas Dulces):", round(precision_logit_masas_dulces, 3)))
  print(paste("Recall Logit (Gasto en Masas Dulces):", round(recall_logit_masas_dulces, 3)))
  print(paste("F1-Score Logit (Gasto en Masas Dulces):", round(f1_score_logit_masas_dulces, 3)))
} else {
  print("Nota: No se pueden calcular Precisión, Recall y F1-Score para la clase '1' (gasta) de forma estándar.")
  print("Esto puede deberse a que el modelo Logit no predijo ningún '1' o a un desequilibrio extremo de clases donde hay muy pocos verdaderos positivos.")
  print("Considere ajustar el umbral de clasificación (0.5) o la adecuación de un modelo Logit para esta variable si los '1's son muy pocos.")
}


# 2. Regresión Lineal (monto del gasto en Masas Dulces, solo para quienes gastan)
# Se utiliza la variable 'log_gasto_total_masas_dulces' y la tabla 'tabla_solo_gastadores'
modelo_lm_masas_dulces <- lm(log_gasto_total_masas_dulces ~ sexo + grupo_edad + log_ing_pc + grupo_escolaridad, data = tabla_solo_gastadores)
summary(modelo_lm_masas_dulces)

# Métricas de desempeño para la Regresión Lineal
predicciones_lm_masas_dulces <- predict(modelo_lm_masas_dulces, newdata = tabla_solo_gastadores)
rmse_lm_masas_dulces <- sqrt(mean((expm1(predicciones_lm_masas_dulces) - tabla_solo_gastadores$gasto_total_masas_dulces)^2))
mae_lm_masas_dulces <- mean(abs(expm1(predicciones_lm_masas_dulces) - tabla_solo_gastadores$gasto_total_masas_dulces))

print(paste("RMSE del Modelo Lineal (Gasto en Masas Dulces, transformado inversamente):", round(rmse_lm_masas_dulces, 3)))
print(paste("MAE del Modelo Lineal (Gasto en Masas Dulces, transformado inversamente):", round(mae_lm_masas_dulces, 3)))