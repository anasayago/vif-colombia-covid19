# =============================================================================
# ANÁLISIS DE VIOLENCIA INTRAFAMILIAR CONTRA MUJERES EN COLOMBIA
# Diplomado en Analítica y Modelamiento en Salud de las Mujeres — PSM CO
# Datos: SIVIGILA 2019–2023
# Herramienta: R + tidyverse + ggplot2
# =============================================================================

# -----------------------------------------------------------------------------
# 0. INSTALACIÓN Y CARGA DE PAQUETES
# -----------------------------------------------------------------------------
# Ejecuta esto solo la primera vez:
# install.packages(c("tidyverse", "readxl", "janitor", "scales",
#                    "sf", "ggmap", "viridis", "patchwork", "writexl"))

library(tidyverse)    # manipulación y visualización de datos
library(readxl)       # leer archivos Excel
library(janitor)      # limpiar nombres de columnas
library(scales)       # formatear ejes de gráficos
library(viridis)      # paletas de colores accesibles
library(patchwork)    # combinar múltiples gráficos
library(writexl)      # exportar resultados a Excel

# -----------------------------------------------------------------------------
# 1. CONFIGURACIÓN 
# -----------------------------------------------------------------------------

RUTA_DATOS <- "~/PSM-COVID19/SIVIGILA"

archivos <- c(
  "2018" = "Datos_2018_875.xlsx",
  "2019" = "Datos_2019_875.xlsx",
  "2020" = "Datos_2020_875.xlsx",
  "2021" = "Datos_2021_875.xlsx",
  "2022" = "Datos_2022_875.xlsx",
  "2023" = "Datos_2023_875.xlsx"
)

RUTA_SALIDA <- file.path(RUTA_DATOS, "resultados")
dir.create(RUTA_SALIDA, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 2. CARGA Y UNIÓN DE ARCHIVOS
# -----------------------------------------------------------------------------

cat(">>> Cargando archivos...\n")

datos_raw <- map_dfr(
  names(archivos),
  function(anio) {
    ruta_archivo <- file.path(RUTA_DATOS, archivos[anio])
    cat("  Leyendo:", archivos[anio], "\n")
    read_xlsx(ruta_archivo) %>%
      clean_names() %>%           # estandariza nombres a minúsculas
      mutate(archivo_anio = anio)  # columna auxiliar
  }
)

cat(">>> Total de filas cargadas:", nrow(datos_raw), "\n")
cat(">>> Columnas disponibles:", ncol(datos_raw), "\n")

# -----------------------------------------------------------------------------
# 3. LIMPIEZA Y FILTRADO
# -----------------------------------------------------------------------------

cat("\n>>> Filtrando y limpiando datos...\n")

datos <- datos_raw %>%

  # 3.1 Filtrar solo mujeres
  filter(str_to_upper(sexo) == "F") %>%

  # 3.2 Filtrar solo eventos de VIF
  # El evento 875 corresponde a VCM VIF VSX en SIVIGILA
  filter(cod_eve == 875 | str_detect(str_to_upper(nombre_evento), "VIF")) %>%

  # 3.3  inicializar variable de año
  mutate(anio = as.integer(ano)) %>%

  # 3.4 Codificar zona urbano/rural
  # SIVIGILA: AREA 1=Cabecera municipal (urbano), 2=Centro poblado, 3=Rural disperso
  mutate(
    zona = case_when(
      area == 1 ~ "Urbano",
      area == 2 ~ "Centro poblado",
      area == 3 ~ "Rural",
      TRUE      ~ "Sin información"
    ),
    zona_dicot = case_when(
      area == 1 ~ "Urbano",
      area %in% c(2, 3) ~ "Rural/Semi-rural",
      TRUE ~ NA_character_
    )
  ) %>%

  # 3.5 variable PERÍODO 
  mutate(
    periodo = case_when(
      anio %in% c(2018, 2019) ~ "Pre-pandemia\n(2018–2019)",
      anio %in% c(2020, 2021) ~ "Durante pandemia\n(2020–2021)",
      anio %in% c(2022, 2023) ~ "Post-pandemia\n(2022–2023)",
      TRUE ~ NA_character_
    ),
    periodo = factor(periodo, levels = c(
      "Pre-pandemia\n(2018–2019)",
      "Durante pandemia\n(2020–2021)",
      "Post-pandemia\n(2022–2023)"
    ))
  ) %>%

  # 3.6 Crear grupos de edad
  mutate(
    grupo_edad = case_when(
      edad < 12              ~ "< 12 años",
      edad >= 12 & edad < 18 ~ "12–17 años (adolescentes)",
      edad >= 18 & edad < 30 ~ "18–29 años",
      edad >= 30 & edad < 45 ~ "30–44 años",
      edad >= 45 & edad < 60 ~ "45–59 años",
      edad >= 60             ~ "60+ años",
      TRUE                   ~ "Sin información"
    ),
    grupo_edad = factor(grupo_edad, levels = c(
      "< 12 años", "12–17 años (adolescentes)", "18–29 años",
      "30–44 años", "45–59 años", "60+ años", "Sin información"
    ))
  ) %>%

  # 3.7 Limpiar nombres de departamento
  mutate(
    departamento = str_to_title(departamento_ocurrencia)
  )

cat(">>> Registros después del filtrado:", nrow(datos), "\n")
cat(">>> Años presentes:", paste(sort(unique(datos$anio)), collapse = ", "), "\n")

# -----------------------------------------------------------------------------
# 4. ANÁLISIS DESCRIPTIVO GENERAL
# -----------------------------------------------------------------------------

cat("\n>>> Calculando estadísticas descriptivas...\n")

# 4.1 Casos por año
tabla_anual <- datos %>%
  count(anio, name = "casos") %>%
  arrange(anio)

cat("\nCasos por año:\n")
print(tabla_anual)

# 4.2 Casos por período
tabla_periodo <- datos %>%
  count(periodo, name = "casos") %>%
  mutate(porcentaje = round(casos / sum(casos) * 100, 1))

cat("\nCasos por período:\n")
print(tabla_periodo)

# 4.3 Casos por período y zona
tabla_periodo_zona <- datos %>%
  filter(!is.na(zona_dicot)) %>%
  count(periodo, zona_dicot, name = "casos") %>%
  group_by(periodo) %>%
  mutate(porcentaje = round(casos / sum(casos) * 100, 1)) %>%
  ungroup()

# 4.4 Distribución por grupo de edad
tabla_edad <- datos %>%
  count(grupo_edad, name = "casos") %>%
  mutate(porcentaje = round(casos / sum(casos) * 100, 1))

# -----------------------------------------------------------------------------
# 5. VISUALIZACIONES 
# -----------------------------------------------------------------------------

# Paleta de colores consistente con enfoque de género
colores_periodo <- c(
  "Pre-pandemia\n(2018–2019)"     = "#5B2C8A",
  "Durante pandemia\n(2020–2021)" = "#C0392B",
  "Post-pandemia\n(2022–2023)"    = "#1A7A4A"
)
colores_zona <- c("Urbano" = "#1A237E", "Rural/Semi-rural" = "#E65100")

# --- GRÁFICO 1: Tendencia temporal anual ---
g1 <- ggplot(tabla_anual, aes(x = anio, y = casos)) +
  geom_line(color = "#5B2C8A", linewidth = 1.2) +
  geom_point(size = 4, color = "#5B2C8A") +
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "#C0392B", linewidth = 0.8) +
  geom_vline(xintercept = 2021.5, linetype = "dashed", color = "#1A7A4A", linewidth = 0.8) +
  annotate("text", x = 2019.6, y = max(tabla_anual$casos) * 0.95,
           label = "Inicio\nconfinamiento", hjust = 0, size = 3.2, color = "#C0392B") +
  annotate("text", x = 2021.6, y = max(tabla_anual$casos) * 0.95,
           label = "Post-\npandemia", hjust = 0, size = 3.2, color = "#1A7A4A") +
  scale_x_continuous(breaks = 2018:2023) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Tendencia anual de casos de VIF en mujeres",
    subtitle = "Colombia 2018–2023 | Fuente: SIVIGILA",
    x = "Año", y = "Número de casos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#5B2C8A"))

ggsave(file.path(RUTA_SALIDA, "g1_tendencia_anual.png"), g1,
       width = 8, height = 5, dpi = 300)

# --- GRÁFICO 2: Comparación por período (barras) ---
g2 <- ggplot(tabla_periodo, aes(x = periodo, y = casos, fill = periodo)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = comma(casos)), vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = colores_periodo) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Casos de VIF por período",
    subtitle = "Pre-pandemia vs. Pandemia vs. Post-pandemia",
    x = NULL, y = "Número de casos"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", color = "#5B2C8A"))

ggsave(file.path(RUTA_SALIDA, "g2_casos_por_periodo.png"), g2,
       width = 7, height = 5, dpi = 300)

# --- GRÁFICO 3: Brecha urbano/rural por período ---
g3 <- datos %>%
  filter(!is.na(zona_dicot)) %>%
  count(anio, zona_dicot) %>%
  ggplot(aes(x = anio, y = n, color = zona_dicot, group = zona_dicot)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.5) +
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 2021.5, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = colores_zona, name = "Zona") +
  scale_x_continuous(breaks = 2018:2023) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Evolución de VIF por zona: Urbano vs. Rural",
    subtitle = "Colombia 2018–2023 | Fuente: SIVIGILA",
    x = "Año", y = "Número de casos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#5B2C8A"),
        legend.position = "bottom")

ggsave(file.path(RUTA_SALIDA, "g3_brecha_urbano_rural.png"), g3,
       width = 8, height = 5, dpi = 300)

# --- GRÁFICO 4: Distribución por grupo de edad ---
g4 <- datos %>%
  filter(grupo_edad != "Sin información") %>%
  count(grupo_edad, periodo) %>%
  ggplot(aes(x = grupo_edad, y = n, fill = periodo)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = colores_periodo, name = "Período") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Casos de VIF por grupo de edad y período",
    subtitle = "Colombia 2018–2023 | Fuente: SIVIGILA",
    x = "Grupo de edad", y = "Número de casos"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(face = "bold", color = "#5B2C8A"),
    legend.position = "bottom"
  )

ggsave(file.path(RUTA_SALIDA, "g4_edad_por_periodo.png"), g4,
       width = 9, height = 5, dpi = 300)

# --- GRÁFICO 5: Top 10 departamentos ---
g5 <- datos %>%
  count(departamento, periodo) %>%
  group_by(departamento) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  slice_max(order_by = total, n = 30) %>%   # top 10 departamentos × 3 períodos
  mutate(departamento = fct_reorder(departamento, total)) %>%
  ggplot(aes(x = n, y = departamento, fill = periodo)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = colores_periodo, name = "Período") +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Top 10 departamentos con más casos de VIF",
    subtitle = "Colombia 2018–2023 | Fuente: SIVIGILA",
    x = "Número de casos", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", color = "#5B2C8A"),
    legend.position = "bottom"
  )

ggsave(file.path(RUTA_SALIDA, "g5_top_departamentos.png"), g5,
       width = 9, height = 6, dpi = 300)

# --- GRÁFICO 6: Panel resumen (patchwork) ---
panel <- (g1 + g2) / (g3 + g4) +
  plot_annotation(
    title = "Violencia Intrafamiliar contra Mujeres en Colombia 2018–2023",
    subtitle = "Análisis por período, zona y grupo de edad | Fuente: SIVIGILA",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, color = "#5B2C8A"),
      plot.subtitle = element_text(size = 10, color = "grey40")
    )
  )

ggsave(file.path(RUTA_SALIDA, "panel_resumen.png"), panel,
       width = 14, height = 10, dpi = 300)

# -----------------------------------------------------------------------------
# 6. ANÁLISIS DE CAMBIO ENTRE PERÍODOS (OE2)
# -----------------------------------------------------------------------------

cat("\n>>> Calculando cambios entre períodos por departamento...\n")

cambio_dpto <- datos %>%
  count(departamento, periodo) %>%
  pivot_wider(names_from = periodo, values_from = n, values_fill = 0) %>%
  clean_names() %>%
  rename(
    pre    = pre_pandemia_2018_2019,
    durante = durante_pandemia_2020_2021,
    post   = post_pandemia_2022_2023
  ) %>%
  mutate(
    cambio_pct_pandemia  = round((durante - pre) / pre * 100, 1),
    cambio_pct_postpan   = round((post - durante) / durante * 100, 1),
    cambio_pct_total     = round((post - pre) / pre * 100, 1)
  ) %>%
  arrange(desc(cambio_pct_pandemia))

cat("\nTop 10 departamentos con mayor aumento durante pandemia:\n")
print(head(cambio_dpto %>% select(departamento, pre, durante, post,
                                   cambio_pct_pandemia, cambio_pct_total), 10))

# -----------------------------------------------------------------------------
# 7. ANÁLISIS URBANO-RURAL DETALLADO (OE3)
# -----------------------------------------------------------------------------

cat("\n>>> Análisis de brecha urbano-rural...\n")

brecha_zona <- datos %>%
  filter(!is.na(zona_dicot)) %>%
  count(anio, periodo, zona_dicot) %>%
  pivot_wider(names_from = zona_dicot, values_from = n, values_fill = 0) %>%
  mutate(
    total          = Urbano + `Rural/Semi-rural`,
    pct_urbano     = round(Urbano / total * 100, 1),
    pct_rural      = round(`Rural/Semi-rural` / total * 100, 1),
    ratio_urb_rural = round(Urbano / `Rural/Semi-rural`, 2)
  )

cat("\nDistribución urbano-rural por año:\n")
print(brecha_zona)

# Casos en gestantes por período (vulnerabilidad especial)
gestantes <- datos %>%
  filter(gp_gestan == 1) %>%
  count(periodo, zona_dicot, name = "casos_gestantes") %>%
  filter(!is.na(zona_dicot))

cat("\nCasos en gestantes por período y zona:\n")
print(gestantes)

# -----------------------------------------------------------------------------
# 8. EXPORTAR TABLAS DE RESULTADOS
# -----------------------------------------------------------------------------

cat("\n>>> Exportando tablas a Excel...\n")
# -----------------------------------------------------------------------------
# TABLA CLAVE PARA EL CRUCE CON IPM Y GEIH 
# Una fila por departamento-año con casos de VIF y período
# -----------------------------------------------------------------------------
casos_depto_anio <- datos %>%
  count(departamento, anio, name = "casos_vif") %>%
  mutate(
    periodo = case_when(
      anio %in% c(2018, 2019) ~ "Pre-pandemia",
      anio %in% c(2020, 2021) ~ "Durante pandemia",
      anio %in% c(2022, 2023) ~ "Post-pandemia"
    ),
    periodo = factor(periodo, levels = c(
      "Pre-pandemia", "Durante pandemia", "Post-pandemia"
    ))
  ) %>%
  arrange(departamento, anio)

cat(">>> Tabla cruce generada:", nrow(casos_depto_anio),
    "filas (departamento × año)\n")

write_xlsx(
  list(
    "Casos_por_año"          = tabla_anual,
    "Casos_por_período"      = tabla_periodo,
    "Período_x_zona"         = tabla_periodo_zona,
    "Distribución_edad"      = tabla_edad,
    "Cambio_departamentos"   = cambio_dpto,
    "Brecha_urbano_rural"    = brecha_zona,
    "Gestantes"              = gestantes,
    "Casos_depto_anio"       = casos_depto_anio   # ← tabla para cruce OE3
  ),
  path = file.path(RUTA_SALIDA, "tablas_resultados_VIF.xlsx")
)

# -----------------------------------------------------------------------------
# 9. RESUMEN FINAL EN CONSOLA
# -----------------------------------------------------------------------------

cat("\n")
cat("=============================================================\n")
cat("  RESUMEN DEL ANÁLISIS\n")
cat("=============================================================\n")
cat("  Total de registros analizados :", nrow(datos), "\n")
cat("  Años cubiertos                :", paste(sort(unique(datos$anio)), collapse = ", "), "\n")
cat("  Departamentos                 :", n_distinct(datos$departamento), "\n")
cat("  Casos en zona urbana          :", sum(datos$zona_dicot == "Urbano", na.rm = TRUE), "\n")
cat("  Casos en zona rural           :", sum(datos$zona_dicot == "Rural/Semi-rural", na.rm = TRUE), "\n")
cat("  Archivos guardados en        :", RUTA_SALIDA, "\n")
cat("=============================================================\n")
cat("  Gráficos generados:\n")
cat("    g1_tendencia_anual.png\n")
cat("    g2_casos_por_periodo.png\n")
cat("    g3_brecha_urbano_rural.png\n")
cat("    g4_edad_por_periodo.png\n")
cat("    g5_top_departamentos.png\n")
cat("    panel_resumen.png\n")
cat("  Tablas exportadas:\n")
cat("    tablas_resultados_VIF.xlsx\n")
cat("=============================================================\n")
