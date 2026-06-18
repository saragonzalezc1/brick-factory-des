library(dplyr)
library(ggplot2)
library(zoo)
library(readxl)

datos <- read.csv("welch_data.csv")
DIR_DATOS    <- "data/"          # coloca aquí los CSV del output de Python
DIR_GRAFICOS <- "output/figures/"
datos <- read.csv("welch_data.csv")
guardar_grafico <- function(nombre, ancho = 20, alto = 14) {
  ruta <- file.path(DIR_GRAFICOS, paste0(nombre, ".png"))
  ggsave(ruta, width = ancho, height = alto, units = "cm", dpi = 300, bg = "white")
  message("  Gráfico guardado: ", ruta)
}

#################### almacen prequema #########################
welch <- datos %>%
  group_by(tiempo_dias) %>%
  summarise(
    promedio = mean(almacen_pre_quema, na.rm = TRUE)
  )
k <- 10

welch$suavizado <- zoo::rollmean(
  welch$promedio,
  k = k,
  fill = NA,
  align = "center"
)
ggplot(welch, aes(x = tiempo_dias)) +
  
  geom_line(aes(y = promedio),
            alpha = 0.4) +
  
  geom_line(aes(y = suavizado),
            linewidth = 1.2) +
  
  labs(
    title = "Método de Welch – Almacén pre-quema",
    x = "Tiempo de Simulación (días)",
    y = "Promedio de inventario pre-quema"
  ) +
  
  theme_minimal()

ggplot(welch, aes(x = tiempo_dias)) +
  
  geom_line(aes(y = promedio), alpha = 0.3) +
  
  geom_line(aes(y = suavizado),
            linewidth = 1.2) +
  
  coord_cartesian(xlim = c(0, 20)) +
  
  theme_minimal()

#################### buffer molienda #########################

welch <- datos %>%
  group_by(tiempo_dias) %>%
  summarise(
    promedio = mean(buffer_molienda, na.rm = TRUE)
  )
k <- 10

welch$suavizado <- zoo::rollmean(
  welch$promedio,
  k = k,
  fill = NA,
  align = "center"
)
ggplot(welch, aes(x = tiempo_dias)) +
  
  geom_line(aes(y = promedio),
            alpha = 0.4) +
  
  geom_line(aes(y = suavizado),
            linewidth = 1.2) +
  
  labs(
    title = "Método de Welch - Análisis Warm-up",
    x = "Tiempo de Simulación (días)",
    y = "buffer_molienda promedio"
  ) +
  
  theme_minimal()

ggplot(welch, aes(x = tiempo_dias)) +
  
  geom_line(aes(y = promedio), alpha = 0.3) +
  
  geom_line(aes(y = suavizado),
            linewidth = 1.2) +
  
  coord_cartesian(xlim = c(0, 20)) +
  
  theme_minimal()
######################## cola estantes #####################

welch <- datos %>%
  group_by(tiempo_dias) %>%
  summarise(
    promedio = mean(cola_estantes, na.rm = TRUE)
  )
k <- 10

welch$suavizado <- zoo::rollmean(
  welch$promedio,
  k = k,
  fill = NA,
  align = "center"
)
ggplot(welch, aes(x = tiempo_dias)) +
  
  geom_line(aes(y = promedio),
            alpha = 0.4) +
  
  geom_line(aes(y = suavizado),
            linewidth = 1.2) +
  
  labs(
    title = "Método de Welch – Cola de estantes",
    x = "Tiempo de Simulación (días)",
    y = "Promedio de cola de estantes"
  ) +
  
  theme_minimal()

guardar_grafico("cola_estantes", ancho = 26, alto = 16)

############################################################################
ggplot(welch, aes(x = tiempo_dias)) +
  
  # Línea gris original
  geom_line(
    aes(y = promedio),
    color = "grey55",
    linewidth = 0.5,
    alpha = 0.8
  ) +
  
  # Línea negra suavizada
  geom_line(
    aes(y = suavizado),
    color = "black",
    linewidth = 1.2
  ) +
  
  labs(
    x = "Tiempo de Simulación (días)",
    y = "Promedio de cola de estantes"
  ) +
  
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  
  theme_minimal(base_size = 18) +
  
  theme(
    # Sin título
    plot.title = element_blank(),
    
    # Texto ejes
    axis.title = element_text(
      size = 20,
      color = "black"
    ),
    
    axis.text = element_text(
      size = 14,
      color = "grey30"
    ),
    
    # Grid suave
    panel.grid.major = element_line(
      color = "grey85",
      linewidth = 0.6
    ),
    
    panel.grid.minor = element_blank(),
    
    # Fondo exactamente minimal
    panel.background = element_blank(),
    plot.background = element_blank()
  )

guardar_grafico(
  "cola_estantes3",
  ancho = 26,
  alto = 16
)



ggplot(welch, aes(x = tiempo_dias)) +
  
  geom_line(aes(y = promedio), alpha = 0.3) +
  
  geom_line(aes(y = suavizado),
            linewidth = 1.2) +
  
  coord_cartesian(xlim = c(0, 50)) +
  
  theme_minimal()

######################## ocupacion secado artificial #####################

welch <- datos %>%
  group_by(tiempo_dias) %>%
  summarise(
    promedio = mean(secado_art_ocup, na.rm = TRUE)
  )
k <- 10

welch$suavizado <- zoo::rollmean(
  welch$promedio,
  k = k,
  fill = NA,
  align = "center"
)
ggplot(welch, aes(x = tiempo_dias)) +
  
  geom_line(aes(y = promedio),
            alpha = 0.4) +
  
  geom_line(aes(y = suavizado),
            linewidth = 1.2) +
  
  labs(
    title = "Método de Welch – Ocupación del secado artificial",
    x = "Tiempo de Simulación (días)",
    y = "Ocupación promedio del secado artificial (%)"
  ) +
  
  theme_minimal()

ggplot(welch, aes(x = tiempo_dias)) +
  
  geom_line(aes(y = promedio), alpha = 0.3) +
  
  geom_line(aes(y = suavizado),
            linewidth = 1.2) +
  
  coord_cartesian(xlim = c(0, 50)) +
  
  theme_minimal()






