library(haven)

# Leer archivos Stata
personas <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos   <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")
cantidades <- read_dta("data/datos_epf/base-cantidades-ix-epf-stata.dta")
ccif     <- read_dta("data/datos_epf/ccif-ix-epf-stata.dta")

# Se filtran registros de personas para el Gran Santiago
personas_gs = personas[personas$macrozona == 2, ]

# Filtro para valores inválidos.
valores_invalidos <- c(-99, -88, -77)

# Edad y escolaridad
personas_gs = personas_gs[!(personas_gs$edad %in% valores_invalidos), ]
personas_gs = personas_gs[!(personas_gs$edue %in% valores_invalidos), ]  # cantidad de años que estudio la persona
personas_gs = personas_gs[!(personas_gs$ing_disp_hog_hd_ai < 0), ]

#  Se calcula el ingreso per cápita
personas_gs$ing_pc = personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas

# Filtrar la base de cantidades en funcion de mi servicio
gastos_servicio =
  gastos[
    gastos$ccif == "09.4.6.01.08",
  ]

# Sumar gasto total en el servicio, por hogar
gasto_hogar_servicio = merge(gastos_servicio, personas_gs, by = "folio")

tabla_gastos = gasto_hogar_servicio[, c("sexo", "edad", "edue", "cse", "ing_pc", "gasto")]
