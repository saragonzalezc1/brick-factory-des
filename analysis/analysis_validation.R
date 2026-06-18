library(readxl)

simulados <- read_excel("metricas_diarias_2.xlsx", sheet = "simuladas")

### Intervalos de confianza SIMULACIÓN

t.test(simulados$toneladas_molidas, conf.level = 0.95)
t.test(simulados$unidades_produccion, conf.level = 0.95)
t.test(simulados$unidades_deshornadas, conf.level = 0.95)
t.test(simulados$unidades_producto_final, conf.level = 0.95)

###Intervalos de confianza REALES

reales <- read_excel("metricas_diarias_2.xlsx", sheet = "reales")
real_toneladas_molidas <- na.omit(reales$toneladas_molidas)
real_unidades_produccion <- na.omit(reales$unidades_produccion)
real_unidades_deshornadas <- na.omit(reales$unidades_deshornadas)

t.test(real_toneladas_molidas, conf.level = 0.95)
t.test(real_unidades_produccion, conf.level = 0.95)
t.test(real_unidades_deshornadas, conf.level = 0.95)

### Diferencia de medias

t.test(simulados$toneladas_molidas, real_toneladas_molidas, var.equal = FALSE, conf.level = 0.90)
t.test(simulados$unidades_produccion, real_unidades_produccion, var.equal = FALSE, conf.level = 0.90)
t.test(simulados$unidades_deshornadas, real_unidades_deshornadas, var.equal = FALSE, conf.level = 0.90)

#### IC mediana

library(boot)
library(readxl)

#-------------------------------------------------
# Función bootstrap para la mediana
#-------------------------------------------------

boot_mediana <- function(data, indices) {
  median(data[indices], na.rm = TRUE)
}

#-------------------------------------------------
# INTERVALOS DE CONFIANZA - SIMULACIÓN
#-------------------------------------------------

boot_toneladas <- boot(simulados$toneladas_molidas,
                       statistic = boot_mediana,
                       R = 2000)

boot_produccion <- boot(simulados$unidades_produccion,
                        statistic = boot_mediana,
                        R = 2000)

boot_deshornadas <- boot(simulados$unidades_deshornadas,
                         statistic = boot_mediana,
                         R = 2000)

boot_producto_final <- boot(simulados$unidades_producto_final,
                            statistic = boot_mediana,
                            R = 2000)

# IC 95%
boot.ci(boot_toneladas, type = "perc")
boot.ci(boot_produccion, type = "perc")
boot.ci(boot_deshornadas, type = "perc")
boot.ci(boot_producto_final, type = "perc")


#-------------------------------------------------
# INTERVALOS DE CONFIANZA - REALES
#-------------------------------------------------

reales <- read_excel("metricas_diarias.xlsx", sheet = "reales")

real_toneladas_molidas <- na.omit(reales$toneladas_molidas)
real_unidades_produccion <- na.omit(reales$unidades_produccion)
real_unidades_deshornadas <- na.omit(reales$unidades_deshornadas)

boot_real_toneladas <- boot(real_toneladas_molidas,
                            statistic = boot_mediana,
                            R = 2000)

boot_real_produccion <- boot(real_unidades_produccion,
                             statistic = boot_mediana,
                             R = 2000)

boot_real_deshornadas <- boot(real_unidades_deshornadas,
                              statistic = boot_mediana,
                              R = 2000)

# IC 95%
boot.ci(boot_real_toneladas, type = "perc")
boot.ci(boot_real_produccion, type = "perc")
boot.ci(boot_real_deshornadas, type = "perc")


#-------------------------------------------------
# DIFERENCIA DE MEDIANAS
#-------------------------------------------------

# Función bootstrap para diferencia de medianas
diff_medianas <- function(data, indices) {
  d <- data[indices, ]
  median(d$sim, na.rm = TRUE) - median(d$real, na.rm = TRUE)
}

# Toneladas molidas
df_ton <- data.frame(
  sim = simulados$toneladas_molidas,
  real = sample(real_toneladas_molidas,
                length(simulados$toneladas_molidas),
                replace = TRUE)
)

boot_diff_ton <- boot(df_ton,
                      statistic = diff_medianas,
                      R = 2000)

boot.ci(boot_diff_ton, type = "perc")


# Unidades producción
df_prod <- data.frame(
  sim = simulados$unidades_produccion,
  real = sample(real_unidades_produccion,
                length(simulados$unidades_produccion),
                replace = TRUE)
)

boot_diff_prod <- boot(df_prod,
                       statistic = diff_medianas,
                       R = 2000)

boot.ci(boot_diff_prod, type = "perc")


# Unidades deshornadas
df_desh <- data.frame(
  sim = simulados$unidades_deshornadas,
  real = sample(real_unidades_deshornadas,
                length(simulados$unidades_deshornadas),
                replace = TRUE)
)

boot_diff_desh <- boot(df_desh,
                       statistic = diff_medianas,
                       R = 2000)

boot.ci(boot_diff_desh, type = "perc")






