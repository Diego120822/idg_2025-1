# abrimos CASEN

ruta_rds = "data/casen_rm.rds"
casen_rm = readRDS(ruta_rds)

# Hacer una regresión que de el Ingreso
# Variables Categoricas ccomo por ejemplos Nivel/Año de escolaridad (educ/esc), Edad, Sexo, 
# Análisis exploratorio, Betas significativos y R2
# Limpieza de los dataframe (eliminar variables que no sirvan y valores atipicos), análisis de correlación y regresión

# Vizualización de variables
## Ingreso per capita

hist(casen_rm$ypc)
xlab = "Ingreso per capita"
col = ("lightblue")

boxplot(casen_rm$ypc)
umbral = quantile(casen_rm$ypc, 0.9, na.rm = TRUE)
casen_clean = casen_rm[(casen_rm$ypc <= umbral) & (casen_rm$edad >=15), ]

boxplot(casen_clean$ypc)
hist(casen_clean$ypc)
