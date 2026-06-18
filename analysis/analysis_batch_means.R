# =============================================================================
# ANÁLISIS ESTADÍSTICO — BATCH MEANS
# Comparación de tres escenarios de simulación discreta (SimPy)
# Escenario Base (EB) | Escenario 1 Intercalado (E1) | Escenario 2 Campañas (E2)
#
# Estructura esperada de archivos (en DIR_DATOS):
#   EB_batches.csv  EB_globales.csv
#   E1_batches.csv  E1_globales.csv
#   E2_batches.csv  E2_globales.csv
#
# Salidas:
#   resultados/tablas/   → CSV con estadísticos, ANOVA, Tukey, IC
#   resultados/graficos/ → PNG listos para tesis
# =============================================================================

# ── 0. PAQUETES ───────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(tidyverse)   # dplyr, tidyr, ggplot2, readr, stringr, purrr
  library(broom)       # tidy() para modelos
  library(car)         # leveneTest()
  library(rstatix)     # shapiro_test(), tukey_hsd()
  library(scales)      # label_percent(), label_comma()
  library(patchwork)   # componer paneles de ggplot
})

# ── 1. RUTAS Y DIRECTORIOS ────────────────────────────────────────────────────
DIR_DATOS    <- "data/"          # coloca aquí los CSV del output de Python
DIR_TABLAS   <- "output/tables/"
DIR_GRAFICOS <- "output/figures/"

walk(c(DIR_TABLAS, DIR_GRAFICOS), dir.create, recursive = TRUE, showWarnings = FALSE)

# ── 2. PALETA Y TEMA CORPORATIVO ──────────────────────────────────────────────
# Colores asignados a cada escenario; se usan en todos los gráficos.
COLORES_ESC <- c(
  "EB_base"        = "#2C7BB6",   # azul
  "E1_intercalado" = "#D7191C",   # rojo
  "E2_campanas"    = "#1A9641"    # verde
)

ETIQUETAS_ESC <- c(
  "EB_base"        = "Base",
  "E1_intercalado" = "E1 Intercalado",
  "E2_campanas"    = "E2 Campañas"
)

TEMA_TESIS <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, color = "gray40"),
    axis.title       = element_text(size = 11),
    legend.title     = element_text(face = "bold", size = 10),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold")
  )

# Función auxiliar: guardar gráfico con tamaño estándar para tesis
guardar_grafico <- function(nombre, ancho = 20, alto = 14) {
  ruta <- file.path(DIR_GRAFICOS, paste0(nombre, ".png"))
  ggsave(ruta, width = ancho, height = alto, units = "cm", dpi = 300, bg = "white")
  message("  Gráfico guardado: ", ruta)
}

# =============================================================================
# SECCIÓN 1 — CARGA Y UNIÓN DE DATOS
# =============================================================================
message("\n── Cargando datos ──────────────────────────────────────────────────")

# Prefijos y nombres completos de escenario
escenarios <- tibble(
  prefijo  = c("EB", "E1", "E2"),
  escenario = c("EB_base", "E1_intercalado", "E2_campanas")
)

# Función genérica para leer un tipo de CSV y unir los tres escenarios
leer_consolidado <- function(tipo) {
  escenarios |>
    pmap_dfr(function(prefijo, escenario) {
      ruta <- file.path(DIR_DATOS, paste0(prefijo, "_", tipo, ".csv"))
      if (!file.exists(ruta)) {
        warning("No encontrado: ", ruta)
        return(tibble())
      }
      read_csv(ruta, show_col_types = FALSE) |>
        mutate(escenario = escenario)   # garantiza columna aunque ya exista
    })
}

df_batches  <- leer_consolidado("batches")
df_globales <- leer_consolidado("globales")
df_autocorr <- leer_consolidado("autocorrelacion")

# Convertir escenario a factor ordenado (Base < E1 < E2)
nivel_esc <- c("EB_base", "E1_intercalado", "E2_campanas")
df_batches  <- df_batches  |> mutate(escenario = factor(escenario, levels = nivel_esc))
df_globales <- df_globales |> mutate(escenario = factor(escenario, levels = nivel_esc))

# Pivote ancho: un KPI por columna — útil para análisis univariado
batches_wide  <- df_batches  |> pivot_wider(names_from = kpi, values_from = valor)
globales_wide <- df_globales |> pivot_wider(names_from = kpi, values_from = valor)

message("  Batches:  ", nrow(df_batches),  " filas | ",
        n_distinct(df_batches$kpi), " KPIs distintos")
message("  Globales: ", nrow(df_globales), " filas | ",
        n_distinct(df_globales$kpi), " KPIs distintos")

# =============================================================================
# SECCIÓN 2 — ESTADÍSTICA DESCRIPTIVA
# =============================================================================
message("\n── Estadística descriptiva ─────────────────────────────────────────")

# Función: IC95% con t-Student (n-1 grados de libertad)
ic95 <- function(x, na.rm = TRUE) {
  x  <- x[!is.na(x)]
  n  <- length(x)
  if (n < 2) return(c(li = NA_real_, ls = NA_real_))
  se <- sd(x) / sqrt(n)
  me <- qt(0.975, df = n - 1) * se
  c(li = mean(x) - me, ls = mean(x) + me)
}

# ── 2a. Descriptivos de batches (todos los KPIs) ──────────────────────────────
desc_batches <- df_batches |>
  group_by(escenario, kpi) |>
  summarise(
    n      = sum(!is.na(valor)),
    media  = mean(valor, na.rm = TRUE),
    sd     = sd(valor,   na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    minimo  = min(valor,   na.rm = TRUE),
    maximo  = max(valor,   na.rm = TRUE),
    ic95_li = ic95(valor)["li"],
    ic95_ls = ic95(valor)["ls"],
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 6)))

write_csv(desc_batches,
          file.path(DIR_TABLAS, "descriptivos_batches.csv"))
message("  Exportado: descriptivos_batches.csv")

# ── 2b. Descriptivos de globales ──────────────────────────────────────────────
desc_globales <- df_globales |>
  group_by(escenario, kpi) |>
  summarise(
    n       = sum(!is.na(valor)),
    media   = mean(valor, na.rm = TRUE),
    sd      = sd(valor,   na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    minimo  = min(valor,   na.rm = TRUE),
    maximo  = max(valor,   na.rm = TRUE),
    ic95_li = ic95(valor)["li"],
    ic95_ls = ic95(valor)["ls"],
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 6)))

write_csv(desc_globales,
          file.path(DIR_TABLAS, "descriptivos_globales.csv"))
message("  Exportado: descriptivos_globales.csv")

# =============================================================================
# SECCIÓN 3 — INTERVALOS DE CONFIANZA (IC95%)
# =============================================================================
message("\n── Intervalos de confianza ─────────────────────────────────────────")

# KPIs de interés para los IC (mezcla de batches y globales)
kpis_ic_batches <- c(
  "throughput_bloques_por_hora",
  "utilizacion_extrusora",
  "ocupacion_secador_artificial",
  "tasa_defecto_horno"
)

kpis_ic_globales <- c(
  "throughput_dia_global",
  "ciclo_media_h",
  "ciclo_p95_h",
  "tasa_def_horno_global"
)

calcular_ic <- function(df, kpis_sel, origen) {
  df |>
    filter(kpi %in% kpis_sel) |>
    group_by(escenario, kpi) |>
    summarise(
      n       = sum(!is.na(valor)),
      media   = mean(valor, na.rm = TRUE),
      sd      = sd(valor,   na.rm = TRUE),
      ic95_li = ic95(valor)["li"],
      ic95_ls = ic95(valor)["ls"],
      .groups = "drop"
    ) |>
    mutate(origen = origen,
           across(where(is.numeric), \(x) round(x, 6)))
}

ic_batches  <- calcular_ic(df_batches,  kpis_ic_batches,  "batches")
ic_globales <- calcular_ic(df_globales, kpis_ic_globales, "globales")
ic_total    <- bind_rows(ic_batches, ic_globales)

write_csv(ic_total, file.path(DIR_TABLAS, "intervalos_confianza_95.csv"))
message("  Exportado: intervalos_confianza_95.csv")

# =============================================================================
# SECCIÓN 4 — ANOVA DE UN FACTOR
# =============================================================================
message("\n── ANOVA ───────────────────────────────────────────────────────────")

# KPIs sobre los que se realiza ANOVA
# Usamos globales para los KPIs de réplica y batches para los de nivel/ocupación
kpis_anova_globales <- c(
  "throughput_dia_global",
  "ciclo_media_h",
  "ciclo_p95_h",
  "tasa_def_horno_global"
)

kpis_anova_batches <- c(
  "utilizacion_extrusora",
  "ocupacion_secador_artificial"
)

# Función central: valida supuestos y corre ANOVA + Tukey si procede
analizar_kpi <- function(df_long, kpi_nombre) {

  datos <- df_long |>
    filter(kpi == kpi_nombre, !is.na(valor)) |>
    mutate(escenario = factor(escenario))

  if (nrow(datos) < 6 || n_distinct(datos$escenario) < 2) {
    message("  [", kpi_nombre, "] Datos insuficientes — omitido")
    return(NULL)
  }

  resultado <- list(kpi = kpi_nombre)

  # ── 4a. Shapiro-Wilk por escenario ──────────────────────────────────────────
  sw <- datos |>
    group_by(escenario) |>
    summarise(
      shapiro_stat = tryCatch(shapiro.test(valor)$statistic, error = \(e) NA_real_),
      shapiro_p    = tryCatch(shapiro.test(valor)$p.value,   error = \(e) NA_real_),
      .groups = "drop"
    ) |>
    mutate(kpi = kpi_nombre,
           normal_95 = shapiro_p > 0.05)

  resultado$shapiro <- sw

  # ── 4b. Levene test ──────────────────────────────────────────────────────────
  levene_res <- tryCatch(
    {
      lt  <- leveneTest(valor ~ escenario, data = datos, center = median)
      tibble(
        kpi         = kpi_nombre,
        F_levene    = lt$`F value`[1],
        p_levene    = lt$`Pr(>F)`[1],
        homogeneo_95 = lt$`Pr(>F)`[1] > 0.05
      )
    },
    error = \(e) tibble(kpi = kpi_nombre, F_levene = NA, p_levene = NA, homogeneo_95 = NA)
  )
  resultado$levene <- levene_res

  # ── 4c. ANOVA de un factor ───────────────────────────────────────────────────
  modelo <- aov(valor ~ escenario, data = datos)
  anova_tbl <- tidy(modelo) |>
    mutate(kpi = kpi_nombre,
           significativo_95 = p.value < 0.05)

  resultado$anova <- anova_tbl

  # ── 4d. Tukey HSD (solo si ANOVA significativo) ──────────────────────────────
  p_anova <- anova_tbl |> filter(term == "escenario") |> pull(p.value)

  if (!is.na(p_anova) && p_anova < 0.05) {
    tukey_res <- tryCatch(
      {
        tk <- TukeyHSD(modelo)$escenario |>
          as_tibble(rownames = "comparacion") |>
          mutate(kpi = kpi_nombre,
                 significativo_95 = `p adj` < 0.05)
        tk
      },
      error = \(e) { message("  Tukey falló: ", e$message); NULL }
    )
    resultado$tukey <- tukey_res
  } else {
    resultado$tukey <- NULL
    message("  [", kpi_nombre, "] ANOVA no significativo (p = ",
            round(p_anova, 4), ") — Tukey omitido")
  }

  message("  [", kpi_nombre, "] ANOVA p = ", round(p_anova, 4),
          if (!is.na(p_anova) && p_anova < 0.05) " *** SIGNIFICATIVO" else "")
  resultado
}

# Ejecutar sobre KPIs de globales
resultados_anova_g <- map(kpis_anova_globales,
                           \(k) analizar_kpi(df_globales, k))
names(resultados_anova_g) <- kpis_anova_globales

# Ejecutar sobre KPIs de batches
resultados_anova_b <- map(kpis_anova_batches,
                           \(k) analizar_kpi(df_batches, k))
names(resultados_anova_b) <- kpis_anova_batches

# Combinar todos los resultados
todos_resultados <- c(resultados_anova_g, resultados_anova_b)

# ── Exportar tablas consolidadas ──────────────────────────────────────────────

# Shapiro-Wilk
tbl_shapiro <- map_dfr(todos_resultados, "shapiro") |>
  mutate(across(where(is.numeric), \(x) round(x, 6)))
write_csv(tbl_shapiro, file.path(DIR_TABLAS, "shapiro_wilk.csv"))

# Levene
tbl_levene <- map_dfr(todos_resultados, "levene") |>
  mutate(across(where(is.numeric), \(x) round(x, 6)))
write_csv(tbl_levene, file.path(DIR_TABLAS, "levene_test.csv"))

# ANOVA
tbl_anova <- map_dfr(todos_resultados, "anova") |>
  mutate(across(where(is.numeric), \(x) round(x, 6)))
write_csv(tbl_anova, file.path(DIR_TABLAS, "anova_resultados.csv"))

# Tukey
tbl_tukey <- map_dfr(todos_resultados, "tukey") |>
  mutate(across(where(is.numeric), \(x) round(x, 6)))
write_csv(tbl_tukey, file.path(DIR_TABLAS, "tukey_hsd.csv"))

message("  Exportados: shapiro_wilk.csv | levene_test.csv | anova_resultados.csv | tukey_hsd.csv")

# =============================================================================
# SECCIÓN 5 — AUTOCORRELACIÓN (resumen para validación Batch Means)
# =============================================================================
message("\n── Autocorrelación lag-1 ───────────────────────────────────────────")

resumen_autocorr <- df_autocorr |>
  mutate(escenario = factor(escenario, levels = nivel_esc)) |>
  group_by(escenario, kpi) |>
  summarise(
    media_lag1   = mean(autocorr_lag1, na.rm = TRUE),
    max_lag1     = max(abs(autocorr_lag1), na.rm = TRUE),
    n_advertencias = sum(advertencia == "REVISAR", na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 4)))

write_csv(resumen_autocorr, file.path(DIR_TABLAS, "autocorrelacion_resumen.csv"))
message("  Exportado: autocorrelacion_resumen.csv")

# =============================================================================
# SECCIÓN 6 — GRÁFICOS
# =============================================================================
message("\n── Generando gráficos ──────────────────────────────────────────────")

# Función auxiliar: etiquetas del eje X limpias
etiq_esc <- function(x) { m <- ETIQUETAS_ESC; ifelse(as.character(x) %in% names(m), m[as.character(x)], as.character(x)) }

# ── 6.1 BOXPLOTS — KPIs globales ─────────────────────────────────────────────
# Definición de los 6 KPIs con metadatos de visualización
kpis_boxplot_globales <- tribble(
  ~kpi,                   ~etiqueta,                        ~unidad,          ~escala,
  "throughput_dia_global",  "Throughput diario",             "bloques/día",    "comma",
  "ciclo_media_h",          "Tiempo de ciclo — Media",       "horas",          "comma",
  "ciclo_p95_h",            "Tiempo de ciclo — P95",         "horas",          "comma",
  "tasa_def_horno_global",  "Tasa de defectos — Horno",      "proporción",     "percent"
)

kpis_boxplot_batches <- tribble(
  ~kpi,                        ~etiqueta,                     ~unidad,         ~escala,
  "utilizacion_extrusora",      "Utilización extrusora",       "fracción",      "percent",
  "ocupacion_secador_artificial","Ocupación secador artificial","fracción",      "percent"
)

# Función genérica para boxplot + jitter por escenario
grafico_boxplot <- function(df_long, kpi_meta, titulo_panel, nombre_archivo) {

  datos_plot <- df_long |>
    filter(kpi %in% kpi_meta$kpi, !is.na(valor)) |>
    left_join(kpi_meta, by = "kpi") |>
    mutate(
      escenario = factor(escenario, levels = nivel_esc),
      etiqueta  = factor(etiqueta, levels = kpi_meta$etiqueta)
    )

  p <- ggplot(datos_plot, aes(x = escenario, y = valor,
                               fill = escenario, color = escenario)) +
    geom_boxplot(alpha = 0.35, outlier.shape = NA, width = 0.5, linewidth = 0.6) +
    geom_jitter(width = 0.12, alpha = 0.55, size = 1.4, shape = 16) +
    facet_wrap(~ etiqueta, scales = "free_y", ncol = 2) +
    scale_fill_manual(values  = COLORES_ESC, labels = ETIQUETAS_ESC, name = "Escenario") +
    scale_color_manual(values = COLORES_ESC, labels = ETIQUETAS_ESC, name = "Escenario") +
    scale_x_discrete(labels = etiq_esc) +
    labs(
      title    = titulo_panel,
      subtitle = "Distribución por réplica — Batch Means | IC95% implícito en la caja",
      x        = NULL,
      y        = "Valor"
    ) +
    TEMA_TESIS

  # Aplicar formato de eje Y por panel
  if (any(kpi_meta$escala == "percent")) {
    p <- p + scale_y_continuous(labels = label_percent(accuracy = 0.1))
  }

  print(p)
  guardar_grafico(nombre_archivo, ancho = 22, alto = 16)
}

grafico_boxplot(
  df_globales, kpis_boxplot_globales,
  "Boxplots — KPIs globales por réplica",
  "01_boxplot_kpis_globales"
)

grafico_boxplot(
  df_batches, kpis_boxplot_batches,
  "Boxplots — KPIs de recursos (por batch × réplica)",
  "02_boxplot_kpis_batches"
)

# ── 6.2 GRÁFICOS DE IC95% ─────────────────────────────────────────────────────
message("  Generando: gráficos IC95%")

# Preparar datos de IC para los KPIs seleccionados
ic_plot <- ic_total |>
  mutate(
    kpi_label = case_match(kpi,
      "throughput_bloques_por_hora"  ~ "Throughput\n(bloques/h)",
      "throughput_dia_global"        ~ "Throughput\n(bloques/dia)",
      "utilizacion_extrusora"        ~ "Utilizacion\nextrusora",
      "ocupacion_secador_artificial" ~ "Ocupacion\nsecador",
      "tasa_defecto_horno"           ~ "Tasa defectos\nhorno (batch)",
      "tasa_def_horno_global"        ~ "Tasa defectos\nhorno (global)",
      "ciclo_media_h"                ~ "Ciclo medio\n(h)",
      "ciclo_p95_h"                  ~ "Ciclo P95\n(h)",
      .default = kpi
    ),
    escenario = factor(escenario, levels = nivel_esc)
  )

p_ic <- ggplot(ic_plot,
               aes(x = escenario, y = media,
                   ymin = ic95_li, ymax = ic95_ls,
                   color = escenario, group = escenario)) +
  geom_point(size = 3.5) +
  geom_errorbar(width = 0.22, linewidth = 1.0) +
  facet_wrap(~ kpi_label, scales = "free_y", ncol = 4) +
  scale_color_manual(values = COLORES_ESC, labels = ETIQUETAS_ESC, name = "Escenario") +
  scale_x_discrete(labels = etiq_esc) +
  labs(
    title    = "Intervalos de confianza 95% — KPIs seleccionados",
    subtitle = "Media ± t(0.975, n-1) · SE  |  Basado en distribución t-Student",
    x        = NULL,
    y        = "Valor"
  ) +
  TEMA_TESIS +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p_ic)
guardar_grafico("03_ic95_kpis", ancho = 26, alto = 16)

# ── 6.3 HISTOGRAMAS — Throughput y Tiempo de ciclo ───────────────────────────
message("  Generando: histogramas")

# Throughput diario global
hist_tp <- df_globales |>
  filter(kpi == "throughput_dia_global", !is.na(valor)) |>
  mutate(escenario = factor(escenario, levels = nivel_esc))

p_hist_tp <- ggplot(hist_tp, aes(x = valor, fill = escenario)) +
  geom_histogram(bins = 8, alpha = 0.75, color = "white", linewidth = 0.3) +
  geom_vline(data = hist_tp |> group_by(escenario) |>
               summarise(media = mean(valor, na.rm = TRUE), .groups = "drop"),
             aes(xintercept = media, color = escenario),
             linewidth = 1.0, linetype = "dashed") +
  facet_wrap(~ escenario, labeller = as_labeller(ETIQUETAS_ESC), ncol = 1) +
  scale_fill_manual(values  = COLORES_ESC, labels = ETIQUETAS_ESC, guide = "none") +
  scale_color_manual(values = COLORES_ESC, labels = ETIQUETAS_ESC, guide = "none") +
  scale_x_continuous(labels = label_comma()) +
  labs(
    title    = "Distribución del throughput diario por réplica",
    subtitle = "Línea discontinua = media por escenario",
    x        = "Bloques/día",
    y        = "Frecuencia"
  ) +
  TEMA_TESIS

# Tiempo de ciclo medio
hist_ciclo <- df_globales |>
  filter(kpi == "ciclo_media_h", !is.na(valor)) |>
  mutate(escenario = factor(escenario, levels = nivel_esc))

p_hist_ciclo <- ggplot(hist_ciclo, aes(x = valor, fill = escenario)) +
  geom_histogram(bins = 8, alpha = 0.75, color = "white", linewidth = 0.3) +
  geom_vline(data = hist_ciclo |> group_by(escenario) |>
               summarise(media = mean(valor, na.rm = TRUE), .groups = "drop"),
             aes(xintercept = media, color = escenario),
             linewidth = 1.0, linetype = "dashed") +
  facet_wrap(~ escenario, labeller = as_labeller(ETIQUETAS_ESC), ncol = 1) +
  scale_fill_manual(values  = COLORES_ESC, labels = ETIQUETAS_ESC, guide = "none") +
  scale_color_manual(values = COLORES_ESC, labels = ETIQUETAS_ESC, guide = "none") +
  labs(
    title    = "Distribución del tiempo de ciclo medio por réplica",
    subtitle = "Línea discontinua = media por escenario",
    x        = "Horas",
    y        = "Frecuencia"
  ) +
  TEMA_TESIS

p_histogramas <- p_hist_tp | p_hist_ciclo
print(p_histogramas)
guardar_grafico("04_histogramas_throughput_ciclo", ancho = 24, alto = 18)

# ── 6.4 BARRAS AGRUPADAS — Niveles almacén pre-quema ────────────────────────
message("  Generando: barras almacén pre-quema")

# EB solo tiene "nivel_almacen_pre_quema" (N4); E1 y E2 tienen N4/N3/N5.
# Normalizamos: para EB asignamos el nivel al slot N4 y dejamos N3/N5 en NA.

# Construir tabla unificada de niveles APQ
apq_e1e2 <- df_batches |>
  filter(kpi %in% c("nivel_almacen_preq_N4",
                     "nivel_almacen_preq_N3",
                     "nivel_almacen_preq_N5"),
         escenario %in% c("E1_intercalado", "E2_campanas")) |>
  mutate(referencia = str_extract(kpi, "N[345]"))

apq_eb <- df_batches |>
  filter(kpi == "nivel_almacen_pre_quema",
         escenario == "EB_base") |>
  mutate(referencia = "N4")

apq_all <- bind_rows(apq_e1e2, apq_eb) |>
  mutate(referencia = factor(referencia, levels = c("N4", "N3", "N5")))

# Medias por escenario y referencia
apq_resumen <- apq_all |>
  group_by(escenario, referencia) |>
  summarise(
    media   = mean(valor, na.rm = TRUE),
    ic95_li = ic95(valor)["li"],
    ic95_ls = ic95(valor)["ls"],
    .groups = "drop"
  )

p_apq <- ggplot(apq_resumen,
                aes(x = referencia, y = media,
                    fill = escenario, group = escenario)) +
  geom_col(position = position_dodge(0.75), width = 0.65, alpha = 0.85) +
  geom_errorbar(aes(ymin = ic95_li, ymax = ic95_ls),
                position = position_dodge(0.75),
                width = 0.22, linewidth = 0.7) +
  scale_fill_manual(values = COLORES_ESC, labels = ETIQUETAS_ESC,
                    name = "Escenario") +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title    = "Nivel promedio del almacén pre-quema por referencia",
    subtitle = "Media de muestras horarias durante la fase efectiva | Barras de error = IC95%",
    x        = "Referencia",
    y        = "Unidades en almacén"
  ) +
  TEMA_TESIS

print(p_apq)
guardar_grafico("05_barras_almacen_prequema", ancho = 18, alto = 12)

# ── 6.5 EVOLUCIÓN TEMPORAL — Perfil por batch ─────────────────────────────────
message("  Generando: gráficos de evolución temporal")

kpis_temporal <- c(
  "nivel_almacen_preq_N4",
  "utilizacion_extrusora",
  "ocupacion_secador_artificial"
)

# Para EB, "nivel_almacen_preq_N4" no existe; usar "nivel_almacen_pre_quema"
# y renombrarlo para que coincida con los demás.
df_batches_temp <- df_batches |>
  mutate(kpi = if_else(kpi == "nivel_almacen_pre_quema" & escenario == "EB_base",
                       "nivel_almacen_preq_N4", kpi)) |>
  filter(kpi %in% kpis_temporal)

kpi_labels_temp <- c(
  "nivel_almacen_preq_N4"       = "Nivel almacén pre-quema N4 (uds.)",
  "utilizacion_extrusora"       = "Utilización extrusora (fracción)",
  "ocupacion_secador_artificial" = "Ocupación secador artificial (fracción)"
)

# Definir batches válidos por escenario
batches_validos <- tibble::tribble(
  ~escenario,   ~max_batch,
  "EB_base",    13,   # 24 días × 13 ≈ 312 días
  "E1",         13,   # 24 días × 13 ≈ 312 días
  "E2",         12    # 27 días × 12 = 324 días ← solo 12 batches completos
)

evol_media <- df_batches_temp |>
  left_join(batches_validos, by = "escenario") |>
  filter(batch <= max_batch) |>          # <-- elimina batch 13 de E2
  select(-max_batch) |>
  group_by(escenario, batch, kpi) |>
  summarise(
    media   = mean(valor, na.rm = TRUE),
    ic95_li = ic95(valor)["li"],
    ic95_ls = ic95(valor)["ls"],
    .groups = "drop"
  ) |>
  mutate(
    kpi_label = case_match(kpi,
                           "nivel_almacen_preq_N4"        ~ "Nivel almacen pre-quema N4 (uds.)",
                           "utilizacion_extrusora"        ~ "Utilizacion extrusora (fraccion)",
                           "ocupacion_secador_artificial" ~ "Ocupacion secador artificial (fraccion)",
                           .default = kpi
    ),
    escenario = factor(escenario, levels = nivel_esc)
  )

# Media por batch y escenario (promediando réplicas)
evol_media <- df_batches_temp |>
  group_by(escenario, batch, kpi) |>
  summarise(
    media   = mean(valor, na.rm = TRUE),
    ic95_li = ic95(valor)["li"],
    ic95_ls = ic95(valor)["ls"],
    .groups = "drop"
  ) |>
  mutate(
    kpi_label = case_match(kpi,
      "nivel_almacen_preq_N4"        ~ "Nivel almacen pre-quema N4 (uds.)",
      "utilizacion_extrusora"        ~ "Utilizacion extrusora (fraccion)",
      "ocupacion_secador_artificial" ~ "Ocupacion secador artificial (fraccion)",
      .default = kpi
    ),
    escenario = factor(escenario, levels = nivel_esc)
  )

p_evol <- ggplot(evol_media,
                 aes(x = batch, y = media,
                     color = escenario, fill = escenario)) +
  geom_ribbon(aes(ymin = ic95_li, ymax = ic95_ls),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  facet_wrap(~ kpi_label, scales = "free_y", ncol = 1) +
  scale_color_manual(values = COLORES_ESC, labels = ETIQUETAS_ESC, name = "Escenario") +
  scale_fill_manual(values  = COLORES_ESC, labels = ETIQUETAS_ESC, name = "Escenario") +
  scale_x_continuous(breaks = seq(1, max(evol_media$batch), by = 1)) +
  labs(
    title    = "Evolución temporal de KPIs por batch",
    subtitle = "Media sobre réplicas ± IC95% | Cada punto = 1 batch (24 días EB/E1, 27 días E2)",
    x        = "Número de batch",
    y        = "Valor"
  ) +
  TEMA_TESIS

print(p_evol)
guardar_grafico("06_evolucion_temporal_batches", ancho = 22, alto = 22)

# ── 6.6 GRÁFICO TUKEY — Diferencias entre escenarios ────────────────────────
message("  Generando: gráfico Tukey HSD")

if (nrow(tbl_tukey) > 0) {

  tukey_plot <- tbl_tukey |>
    mutate(
      comparacion_label = str_replace_all(comparacion,
        c("EB_base"        = "Base",
          "E1_intercalado" = "E1",
          "E2_campanas"    = "E2")),
      kpi_label = case_match(kpi,
        "throughput_dia_global"        ~ "Throughput/dia",
        "ciclo_media_h"                ~ "Ciclo medio (h)",
        "ciclo_p95_h"                  ~ "Ciclo P95 (h)",
        "tasa_def_horno_global"        ~ "Tasa def. horno",
        "utilizacion_extrusora"        ~ "Util. extrusora",
        "ocupacion_secador_artificial" ~ "Ocup. secador",
        .default = kpi
      ),
      significativo = if_else(`p adj` < 0.05, "Si (p<0.05)", "No")
    )

  p_tukey <- ggplot(tukey_plot,
                    aes(x = diff, y = comparacion_label,
                        color = significativo)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = lwr, xmax = upr),
                   height = 0.25, linewidth = 0.8) +
    geom_point(size = 3) +
    facet_wrap(~ kpi_label, scales = "free_x", ncol = 2) +
    scale_color_manual(
      values = c("Sí (p<0.05)" = "#D7191C", "No" = "#74ADD1"),
      name   = "Diferencia significativa"
    ) +
    labs(
      title    = "Tukey HSD — Diferencias entre escenarios",
      subtitle = "Intervalos que no cruzan el 0 indican diferencia significativa (α = 0.05)",
      x        = "Diferencia de medias",
      y        = "Comparación"
    ) +
    TEMA_TESIS

  print(p_tukey)
  guardar_grafico("07_tukey_hsd", ancho = 22, alto = 16)
} else {
  message("  Sin resultados Tukey disponibles — gráfico omitido")
}

# ── 6.7 AUTOCORRELACIÓN LAG-1 — Mapa de calor ────────────────────────────────
message("  Generando: mapa de calor autocorrelación")

p_autocorr <- ggplot(
  resumen_autocorr |>
    mutate(
      escenario = factor(escenario, levels = nivel_esc),
      kpi_short = str_replace_all(kpi, c(
        "throughput_bloques"           = "Throughput",
        "tasa_defecto_"                = "Def. ",
        "tasa_def_"                    = "Def. ",
        "nivel_"                       = "Nivel ",
        "utilizacion_"                 = "Util. ",
        "ocupacion_"                   = "Ocup. ",
        "_artificial"                  = " art.",
        "_natural"                     = " nat.",
        "_produccion"                  = " prod.",
        "_endague"                     = " end.",
        "_deshorne"                    = " des.",
        "_global"                      = "",
        "extrusion"                    = "Ext.",
        "horno"                        = "Horno",
        "buffer_molienda"              = "Buffer mol.",
        "cola_estantes"                = "Cola est.",
        "almacen_pre_quema"            = "APQ",
        "almacen_preq_N4"              = "APQ N4",
        "_"                            = " "
      ))
    ),
  aes(x = escenario, y = kpi_short, fill = media_lag1)
) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(media_lag1, 2),
                color  = abs(media_lag1) > 0.3),
            size = 3) +
  scale_fill_gradient2(
    low      = "#313695",
    mid      = "white",
    high     = "#A50026",
    midpoint = 0,
    limits   = c(-1, 1),
    name     = "Autocorr.\nlag-1"
  ) +
  scale_color_manual(values = c("FALSE" = "gray30", "TRUE" = "black"),
                     guide = "none") +
  scale_x_discrete(labels = ETIQUETAS_ESC) +
  labs(
    title    = "Autocorrelación lag-1 promedio por KPI y escenario",
    subtitle = "Valores |r| > 0.3 (en negrita) sugieren aumentar el tamaño de batch",
    x        = NULL,
    y        = NULL
  ) +
  TEMA_TESIS +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        axis.text.y = element_text(size = 9))

print(p_autocorr)
guardar_grafico("08_autocorrelacion_heatmap", ancho = 20, alto = 18)

# ── 6.8 PANEL RESUMEN — 4 KPIs clave en un solo gráfico ─────────────────────
message("  Generando: panel resumen")

kpis_panel <- tribble(
  ~fuente,     ~kpi,                        ~label,
  "globales",  "throughput_dia_global",      "Throughput (bloques/día)",
  "globales",  "ciclo_media_h",              "Tiempo de ciclo medio (h)",
  "batches",   "utilizacion_extrusora",      "Utilización extrusora",
  "batches",   "ocupacion_secador_artificial","Ocupación secador artificial"
)

lista_paneles <- pmap(kpis_panel, function(fuente, kpi, label) {
  df_src <- if (fuente == "globales") df_globales else df_batches
  datos  <- df_src |>
    filter(kpi == !!kpi, !is.na(valor)) |>
    mutate(escenario = factor(escenario, levels = nivel_esc))

  es_pct <- fuente == "batches"

  ggplot(datos, aes(x = escenario, y = valor,
                    fill = escenario, color = escenario)) +
    geom_boxplot(alpha = 0.3, outlier.shape = NA, width = 0.5) +
    geom_jitter(width = 0.12, alpha = 0.6, size = 1.2) +
    scale_fill_manual(values  = COLORES_ESC, labels = ETIQUETAS_ESC, name = NULL) +
    scale_color_manual(values = COLORES_ESC, labels = ETIQUETAS_ESC, name = NULL) +
    scale_x_discrete(labels = etiq_esc) +
    {if (es_pct) scale_y_continuous(labels = label_percent(accuracy = 0.1))
     else scale_y_continuous(labels = label_comma())} +
    labs(title = label, x = NULL, y = NULL) +
    TEMA_TESIS +
    theme(legend.position = "none",
          plot.title = element_text(size = 11))
})

p_panel <- wrap_plots(lista_paneles, ncol = 2) +
  plot_annotation(
    title    = "Panel resumen — Comparación de los tres escenarios",
    subtitle = "Boxplot + jitter | Batch Means (10 réplicas)",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray40")
    )
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(p_panel)
guardar_grafico("09_panel_resumen_4kpis", ancho = 24, alto = 18)

# =============================================================================
# SECCIÓN 7 — TABLA RESUMEN FINAL PARA TESIS
# =============================================================================
message("\n── Tabla resumen final ─────────────────────────────────────────────")

# KPIs que aparecerán en la tabla maestra de comparación
kpis_tabla_maestra <- c(
  "throughput_dia_global",
  "ciclo_media_h",
  "ciclo_p95_h",
  "tasa_def_horno_global",
  "tasa_def_extrusion_global",
  "tasa_def_secart_global",
  "tasa_def_secnat_global"
)

tabla_maestra <- df_globales |>
  filter(kpi %in% kpis_tabla_maestra) |>
  group_by(escenario, kpi) |>
  summarise(
    media   = mean(valor, na.rm = TRUE),
    sd      = sd(valor,   na.rm = TRUE),
    ic95_li = ic95(valor)["li"],
    ic95_ls = ic95(valor)["ls"],
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from  = escenario,
    values_from = c(media, sd, ic95_li, ic95_ls),
    names_glue  = "{escenario}_{.value}"
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 4)))

# Añadir p-valor ANOVA a la tabla maestra
p_valores <- tbl_anova |>
  filter(term == "escenario") |>
  select(kpi, p_value_anova = p.value)

tabla_maestra <- tabla_maestra |>
  left_join(p_valores, by = "kpi") |>
  mutate(significativo_anova = p_value_anova < 0.05)

write_csv(tabla_maestra, file.path(DIR_TABLAS, "tabla_maestra_comparacion.csv"))
message("  Exportado: tabla_maestra_comparacion.csv")

# =============================================================================
# RESUMEN FINAL EN CONSOLA
# =============================================================================
message("\n", strrep("=", 65))
message("  ANÁLISIS COMPLETADO")
message(strrep("=", 65))

message("\n  Tablas exportadas en: ", DIR_TABLAS, "/")
walk(dir(DIR_TABLAS, full.names = FALSE), \(f) message("    • ", f))

message("\n  Gráficos exportados en: ", DIR_GRAFICOS, "/")
walk(dir(DIR_GRAFICOS, full.names = FALSE), \(f) message("    • ", f))

message("\n  Resultados ANOVA (p < 0.05 = significativo):")
tbl_anova |>
  filter(term == "escenario") |>
  select(kpi, statistic, p.value, significativo_95) |>
  mutate(across(where(is.numeric), \(x) round(x, 4))) |>
  print(n = Inf)

if (nrow(tbl_tukey) > 0) {
  message("\n  Comparaciones Tukey significativas (p.adj < 0.05):")
  tbl_tukey |>
    filter(`p adj` < 0.05) |>
    select(kpi, comparacion, diff, `p adj`) |>
    mutate(across(where(is.numeric), \(x) round(x, 4))) |>
    print(n = Inf)
}

message("\n", strrep("=", 65))

# =============================================================================
# SECCIÓN 8 — ANOVA DE DOS FACTORES: escenario × referencia  (v2)
# Solo E1 vs E2 (ambos tienen N4, N3, N5 en producción intercalada y campañas).
# EB se excluye porque solo produce N4: no es un diseño factorial completo.
#
# Modelo: throughput_ref_por_hora ~ escenario * referencia
# Unidad de observación: un batch (con réplica como bloque de ruido).
# =============================================================================
message("\n── ANOVA dos factores: escenario × referencia (E1 vs E2) ──────────")

# KPIs con desglose por referencia disponibles en E1 y E2
kpis_2factores <- c(
  "throughput_N4_por_hora",   "throughput_N3_por_hora",   "throughput_N5_por_hora",
  "producto_final_N4_por_hora","producto_final_N3_por_hora","producto_final_N5_por_hora"
)

# Reshapear a formato largo con columna 'referencia'
# Ej: kpi = "throughput_N4_por_hora"  →  metrica = "throughput_por_hora", referencia = "N4"
df_2f <- df_batches |>
  filter(escenario %in% c("E1_intercalado", "E2_campanas"),
         kpi %in% kpis_2factores,
         !is.na(valor)) |>
  mutate(
    referencia = str_extract(kpi, "N[345]"),
    metrica    = str_replace(kpi, "_N[345]", ""),
    escenario  = factor(escenario, levels = c("E1_intercalado", "E2_campanas")),
    referencia = factor(referencia, levels = c("N4", "N3", "N5"))
  )

# Función: ANOVA 2 factores + Tukey sobre interacción
anova_dos_factores <- function(df_long, metrica_nombre) {

  datos <- df_long |> filter(metrica == metrica_nombre)
  if (nrow(datos) < 6) return(NULL)

  message("  Métrica: ", metrica_nombre)

  # ── Modelo con interacción ──────────────────────────────────────────────────
  modelo <- aov(valor ~ escenario * referencia, data = datos)
  tbl    <- tidy(modelo) |>
    mutate(metrica = metrica_nombre,
           significativo_95 = p.value < 0.05)

  # ── Tukey sobre todos los efectos ──────────────────────────────────────────
  tk_raw  <- TukeyHSD(modelo)
  tk_list <- imap_dfr(tk_raw, function(mat, nombre_efecto) {
    as_tibble(mat, rownames = "comparacion") |>
      mutate(efecto = nombre_efecto, metrica = metrica_nombre,
             significativo_95 = `p adj` < 0.05)
  })

  # Imprimir resumen en consola
  inter_p <- tbl |> filter(str_detect(term, ":")) |> pull(p.value)
  if (length(inter_p) && !is.na(inter_p))
    message("    Interacción escenario:referencia p = ", round(inter_p, 4),
            if (inter_p < 0.05) " *** SIGNIFICATIVA" else "")

  list(anova = tbl, tukey = tk_list)
}

metricas_2f <- unique(df_2f$metrica)
res_2f      <- map(metricas_2f, \(m) anova_dos_factores(df_2f, m))
names(res_2f) <- metricas_2f

# Exportar
tbl_anova_2f <- map_dfr(res_2f, "anova") |>
  mutate(across(where(is.numeric), \(x) round(x, 6)))
tbl_tukey_2f <- map_dfr(res_2f, "tukey") |>
  mutate(across(where(is.numeric), \(x) round(x, 6)))

write_csv(tbl_anova_2f, file.path(DIR_TABLAS, "anova_2factores_resultados.csv"))
write_csv(tbl_tukey_2f, file.path(DIR_TABLAS, "tukey_2factores_hsd.csv"))
message("  Exportados: anova_2factores_resultados.csv | tukey_2factores_hsd.csv")

# =============================================================================
# SECCIÓN 9 — GRÁFICOS PARA ANOVA DE DOS FACTORES  (v2)
# =============================================================================
message("\n── Gráficos ANOVA dos factores ─────────────────────────────────────")

# ── 9.1 Gráfico de interacción escenario × referencia ────────────────────────
# ── 9.1 Gráfico de interacción escenario × referencia (solo throughput) ────────
media_2f_tp <- df_2f |>
  filter(metrica == "throughput_por_hora") |>
  group_by(escenario, referencia, metrica) |>
  summarise(
    media   = mean(valor, na.rm = TRUE),
    ic95_li = ic95(valor)["li"],
    ic95_ls = ic95(valor)["ls"],
    .groups = "drop"
  )

p_interaccion <- ggplot(media_2f_tp,
                        aes(x = referencia, y = media,
                            color = escenario, group = escenario)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ic95_li, ymax = ic95_ls),
                width = 0.18, linewidth = 0.7) +
  scale_color_manual(
    values = c("E1_intercalado" = "#D7191C", "E2_campanas" = "#1A9641"),
    labels = c("E1_intercalado" = "E1 Intercalado", "E2_campanas" = "E2 Campañas"),
    name   = "Escenario"
  ) +
  labs(
    title    = "Gráfico de interacción: escenario × referencia de bloque",
    subtitle = "Throughput (bloques/h) — Media ± IC95% por batch | Líneas paralelas = sin interacción",
    x        = "Referencia de bloque",
    y        = "Bloques / hora"
  ) +
  TEMA_TESIS

print(p_interaccion)
guardar_grafico("10_interaccion_escenario_referencia", ancho = 16, alto = 12)
# ── 9.2 Boxplot throughput por referencia comparando E1 vs E2 ─────────────────
p_box_ref <- ggplot(
  df_2f |> filter(metrica == "throughput_por_hora"),
  aes(x = referencia, y = valor, fill = escenario)
) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA,
               position = position_dodge(0.75), width = 0.6) +
  geom_jitter(aes(color = escenario),
              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75),
              alpha = 0.55, size = 1.3) +
  scale_fill_manual(
    values = c("E1_intercalado" = "#D7191C", "E2_campanas" = "#1A9641"),
    labels = c("E1_intercalado" = "E1 Intercalado", "E2_campanas" = "E2 Campañas"),
    name = "Escenario"
  ) +
  scale_color_manual(
    values = c("E1_intercalado" = "#D7191C", "E2_campanas" = "#1A9641"),
    guide  = "none"
  ) +
  labs(
    title    = "Throughput por referencia de bloque — E1 vs E2",
    subtitle = "Distribución por batch × réplica | ANOVA 2 factores: escenario × referencia",
    x        = "Referencia de bloque",
    y        = "Throughput (bloques/hora)"
  ) +
  TEMA_TESIS

print(p_box_ref)
guardar_grafico("11_boxplot_throughput_por_referencia", ancho = 20, alto = 13)

# ── 9.3 Boxplot producto final por referencia ─────────────────────────────────
p_box_pf <- ggplot(
  df_2f |> filter(metrica == "producto_final_por_hora"),
  aes(x = referencia, y = valor, fill = escenario)
) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA,
               position = position_dodge(0.75), width = 0.6) +
  geom_jitter(aes(color = escenario),
              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75),
              alpha = 0.55, size = 1.3) +
  scale_fill_manual(
    values = c("E1_intercalado" = "#D7191C", "E2_campanas" = "#1A9641"),
    labels = c("E1_intercalado" = "E1 Intercalado", "E2_campanas" = "E2 Campañas"),
    name = "Escenario"
  ) +
  scale_color_manual(
    values = c("E1_intercalado" = "#D7191C", "E2_campanas" = "#1A9641"),
    guide  = "none"
  ) +
  labs(
    title    = "Producto final por referencia de bloque — E1 vs E2",
    subtitle = "Bloques buenos salidos del horno por batch | ANOVA 2 factores",
    x        = "Referencia de bloque",
    y        = "Producto final (bloques/hora)"
  ) +
  TEMA_TESIS

print(p_box_pf)
guardar_grafico("12_boxplot_producto_final_por_referencia", ancho = 20, alto = 13)

# ── 9.4 Tukey 2 factores — forest plot de comparaciones significativas ─────────
if (nrow(tbl_tukey_2f) > 0) {
  tukey2_plot <- tbl_tukey_2f |>
    filter(str_detect(efecto, "referencia|escenario")) |>
    mutate(
      significativo = if_else(`p adj` < 0.05, "Si (p<0.05)", "No"),
      metrica_label = case_match(metrica,
        "throughput_por_hora"    ~ "Throughput/hora",
        "producto_final_por_hora"~ "Producto final/hora",
        .default = metrica
      ),
      comparacion_corta = str_replace_all(comparacion,
        c("E1_intercalado" = "E1", "E2_campanas" = "E2"))
    )

  p_tukey2 <- ggplot(tukey2_plot,
    aes(x = diff, y = comparacion_corta, color = significativo)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = lwr, xmax = upr),
                   height = 0.3, linewidth = 0.8) +
    geom_point(size = 2.5) +
    facet_grid(efecto ~ metrica_label, scales = "free") +
    scale_color_manual(
      values = c("Si (p<0.05)" = "#D7191C", "No" = "#74ADD1"),
      name   = "Diferencia significativa"
    ) +
    labs(
      title    = "Tukey HSD — ANOVA de dos factores (E1 vs E2)",
      subtitle = "Comparaciones por efecto principal e interacción | α = 0.05",
      x        = "Diferencia de medias (bloques/hora)",
      y        = NULL
    ) +
    TEMA_TESIS +
    theme(axis.text.y = element_text(size = 8))

  print(p_tukey2)
  guardar_grafico("13_tukey_2factores", ancho = 26, alto = 20)
}

message("\n  Gráficos adicionales (ANOVA 2 factores): 10, 11, 12, 13")
message("  Tablas adicionales: anova_2factores_resultados.csv | tukey_2factores_hsd.csv")
