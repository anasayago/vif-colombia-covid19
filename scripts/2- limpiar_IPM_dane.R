# =============================================================================
# LIMPIEZA Y UNIFICACIÓN DEL IPM (DANE) + CRUCE CON SIVIGILA
# Diplomado en Analítica y Modelamiento en Salud de las Mujeres — PSM CO
# =============================================================================

library(tidyverse)
library(readxl)
library(janitor)
library(writexl)
library(ggplot2)
library(scales)

# -----------------------------------------------------------------------------
# 1. CONFIGURACIÓN 
# -----------------------------------------------------------------------------

# Archivo IPM más reciente (ya incluye 2018–2023)
RUTA_IPM     <- "~/PSM-COVID19/DANE/anex-PMultidimensional-Departamental-2023.xlsx"

# Archivo GEIH Mujeres (mercado laboral)
RUTA_GEIH    <- "~/PSM-COVID19/DANE/Departamentos Anual Mujeres.ods"

# Resultado del script de SIVIGILA 
RUTA_SIVIGILA <- "~/PSM-COVID19/SIVIGILA/resultados/tablas_resultados_VIF.xlsx"

# Carpeta de salida
RUTA_SALIDA  <- "~/PSM-COVID19/resultados_OE3"
dir.create(RUTA_SALIDA, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 2. EXTRAER IPM DEPARTAMENTAL (hoja IPM_Departamentos)
# Estructura: fila 11=encabezado años, fila 12=subencabezado Total/Cab/Rural
# Datos desde fila 13. Grupos de 3 columnas por año: Total, Cabeceras, Rural
# -----------------------------------------------------------------------------

cat(">>> Leyendo IPM departamental...\n")

ipm_raw <- read_xlsx(RUTA_IPM, sheet = "IPM_Departamentos", col_names = FALSE)

anios_ipm  <- c(2018, 2019, 2020, 2021, 2022, 2023)
notas_skip <- c("Fuente", "Nota", "Datos", "Actualizado")

# Extraer fila por fila
ipm_lista <- map_dfr(11:nrow(ipm_raw), function(i) {
  depto <- as.character(ipm_raw[[i, 1]])
  if (is.na(depto) || depto == "NA") return(NULL)
  if (any(str_detect(depto, notas_skip))) return(NULL)
  # Saltar filas de encabezado
  if (depto %in% c("Departamento", "Total", "Cabeceras")) return(NULL)
  
  map_dfr(seq_along(anios_ipm), function(j) {
    col_total <- 1 + (j - 1) * 3 + 1   # columna Total del año j
    col_cab   <- col_total + 1           # Cabeceras
    col_rural <- col_total + 2           # Rural disperso
    
    tibble(
      departamento  = depto,
      anio          = anios_ipm[j],
      ipm_total     = as.numeric(ipm_raw[[i, col_total]]),
      ipm_cabecera  = as.numeric(ipm_raw[[i, col_cab]]),
      ipm_rural     = as.numeric(ipm_raw[[i, col_rural]])
    )
  })
})

cat(">>> IPM extraído:", nrow(ipm_lista), "filas |",
    n_distinct(ipm_lista$departamento), "departamentos\n")

# Agregar período y limpiar
ipm_limpio <- ipm_lista %>%
  mutate(
    departamento = str_trim(departamento),
    periodo = case_when(
      anio %in% c(2018, 2019) ~ "Pre-pandemia",
      anio %in% c(2020, 2021) ~ "Durante pandemia",
      anio %in% c(2022, 2023) ~ "Post-pandemia"
    ),
    periodo = factor(periodo, levels = c("Pre-pandemia", "Durante pandemia", "Post-pandemia")),
    # Brecha territorial: diferencia entre IPM rural y cabecera
    brecha_urb_rural = round(ipm_rural - ipm_cabecera, 2)
  )

# -----------------------------------------------------------------------------
# 3. EXTRAER TASA DE DESOCUPACIÓN FEMENINA (GEIH)
# Estructura: bloques de ~18 filas por departamento
# -----------------------------------------------------------------------------

cat("\n>>> Leyendo GEIH mujeres...\n")

library(readODS)
geih_raw <- read_ods(RUTA_GEIH, col_names = FALSE)

notas_geih <- c("Fuente", "Nota", "Actualizado")

pos_deptos <- which(
  !is.na(geih_raw[[1]]) &
    is.na(geih_raw[[2]]) &
    !str_detect(geih_raw[[1]], paste(notas_geih, collapse = "|")) &
    geih_raw[[1]] != "Concepto"
)

anios_geih <- c(2018, 2019, 2020, 2021, 2022, 2023)

geih_limpio <- map_dfr(pos_deptos, function(pos) {
  nombre <- geih_raw[[1]][pos]
  bloque <- geih_raw[pos:(pos + 17), ]
  colnames(bloque) <- c("concepto", as.character(anios_geih))
  
  bloque %>%
    filter(concepto == "Tasa de Desocupación (TD)") %>%
    pivot_longer(cols = as.character(anios_geih),
                 names_to = "anio", values_to = "td_mujeres") %>%
    mutate(
      departamento = nombre,
      anio         = as.integer(anio),
      td_mujeres   = as.numeric(td_mujeres)
    ) %>%
    select(departamento, anio, td_mujeres)
})

cat(">>> GEIH extraído:", nrow(geih_limpio), "filas\n")

# -----------------------------------------------------------------------------
# 4. CARGAR CASOS VIF DE SIVIGILA
# -----------------------------------------------------------------------------

cat("\n>>> Cargando datos SIVIGILA...\n")

sivigila_dpto <- read_xlsx(RUTA_SIVIGILA, sheet = "Casos_depto_anio")

# -----------------------------------------------------------------------------
# 5. ESTANDARIZAR NOMBRES Y CRUZAR LAS TRES FUENTES
# -----------------------------------------------------------------------------
library(stringi)
estandarizar <- function(x) {
  x %>%
    str_to_title() %>%
    str_trim() %>%
    # Eliminar tildes usando stringi (funciona independiente del encoding)
    stri_trans_general("Latin-ASCII") %>%
    # Corregir nombres incompletos
    str_replace_all("^Valle$",         "Valle Del Cauca") %>%
    str_replace_all("^Guajira$",       "La Guajira") %>%
    str_replace_all("Norte Santander", "Norte De Santander") %>%
    str_replace_all("^Bogota.*",       "Bogota") %>%
    # Excluir registros sin departamento válido
    if_else(. %in% c("Exterior", "Procedencia Desconocida"),
            NA_character_, .)
}



ipm_limpio     <- ipm_limpio     %>% mutate(depto_std = estandarizar(departamento))
geih_limpio    <- geih_limpio    %>% mutate(depto_std = estandarizar(departamento))
sivigila_dpto  <- sivigila_dpto  %>% mutate(depto_std = estandarizar(departamento))

# Cruce triple: SIVIGILA + IPM + GEIH
datos_oe3 <- sivigila_dpto %>%
  left_join(
    ipm_limpio %>% select(depto_std, anio, ipm_total, ipm_cabecera,
                          ipm_rural, brecha_urb_rural),
    by = c("depto_std", "anio")
  ) %>%
  left_join(
    geih_limpio %>% select(depto_std, anio, td_mujeres),
    by = c("depto_std", "anio")
  ) %>%
  # Reconstruir periodo desde anio para evitar NAs del join
  mutate(
    periodo = case_when(
      anio %in% c(2018, 2019) ~ "Pre-pandemia",
      anio %in% c(2020, 2021) ~ "Durante pandemia",
      anio %in% c(2022, 2023) ~ "Post-pandemia"
    ),
    periodo = factor(periodo, levels = c(
      "Pre-pandemia", "Durante pandemia", "Post-pandemia"
    ))
  )

cat(">>> Filas en datos cruzados:", nrow(datos_oe3), "\n")
cat(">>> Con IPM disponible:", sum(!is.na(datos_oe3$ipm_total)), "\n")
cat(">>> Con TD disponible :", sum(!is.na(datos_oe3$td_mujeres)), "\n")

# Verificar departamentos sin cruce
sin_ipm <- datos_oe3 %>% filter(is.na(ipm_total)) %>%
  distinct(departamento) %>% pull()
if (length(sin_ipm) > 0)
  cat("⚠️  Sin IPM:", paste(sin_ipm, collapse = ", "), "\n")

# -----------------------------------------------------------------------------
# 6. ANÁLISIS DE CORRELACIÓN OE3
# -----------------------------------------------------------------------------

cat("\n>>> Correlaciones OE3...\n")

# 6.1 IPM total vs casos VIF por período
cor_ipm <- datos_oe3 %>%
  filter(!is.na(ipm_total), !is.na(casos_vif), !is.na(periodo)) %>%
  group_by(periodo) %>%
  summarise(
    cor_ipm_total  = round(cor(ipm_total,  casos_vif, method = "spearman"), 3),
    cor_ipm_rural  = round(cor(ipm_rural,  casos_vif, method = "spearman", use = "complete.obs"), 3),
    cor_ipm_cab    = round(cor(ipm_cabecera, casos_vif, method = "spearman", use = "complete.obs"), 3),
    cor_td_mujeres = round(cor(td_mujeres, casos_vif, method = "spearman", use = "complete.obs"), 3),
    n = n()
  )

cat("\nCorrelaciones Spearman por período:\n")
print(cor_ipm)

# 6.2 Correlación global con p-valor
cat("\nPrueba global IPM total ~ casos VIF:\n")
cor_global <- cor.test(datos_oe3$ipm_total, datos_oe3$casos_vif,
                       method = "spearman", , exact = FALSE)
cat("  Rho =", round(cor_global$estimate, 3),
    "| p-valor =", round(cor_global$p.value, 4), "\n")

cat("\nPrueba global TD mujeres ~ casos VIF:\n")
cor_td <- cor.test(datos_oe3$td_mujeres, datos_oe3$casos_vif,
                   method = "spearman", , exact = FALSE)
cat("  Rho =", round(cor_td$estimate, 3),
    "| p-valor =", round(cor_td$p.value, 4), "\n")

# -----------------------------------------------------------------------------
# 7. VISUALIZACIONES OE3
# -----------------------------------------------------------------------------

colores_periodo <- c(
  "Pre-pandemia"     = "#5B2C8A",
  "Durante pandemia" = "#C0392B",
  "Post-pandemia"    = "#1A7A4A"
)

# GRÁFICO 1: IPM total vs VIF por período (dispersograma)
g1 <- datos_oe3 %>%
  filter(!is.na(ipm_total), !is.na(casos_vif), !is.na(periodo)) %>%
  ggplot(aes(x = ipm_total, y = casos_vif, color = periodo)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  facet_wrap(~periodo) +
  scale_color_manual(values = colores_periodo) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Correlación entre Índice de Pobreza Multidimensional y casos de VIF",
    subtitle = "Correlación de Spearman | Colombia 2018–2023 | Fuentes: DANE-ECV y SIVIGILA",
    x = "IPM total (%)", y = "Casos VIF"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", color = "#5B2C8A"),
        strip.text = element_text(face = "bold"))

ggsave(file.path(RUTA_SALIDA, "g_ipm_vs_vif.png"), g1, width = 11, height = 4.5, dpi = 300)

# GRÁFICO 2: Brecha urbano-rural del IPM por departamento (pandemia)
g2 <- ipm_limpio %>%
  filter(periodo == "Durante pandemia") %>%
  group_by(departamento) %>%
  summarise(brecha_media = mean(brecha_urb_rural, na.rm = TRUE)) %>%
  slice_max(brecha_media, n = 15) %>%
  mutate(departamento = fct_reorder(departamento, brecha_media)) %>%
  ggplot(aes(x = brecha_media, y = departamento)) +
  geom_col(fill = "#C0392B", alpha = 0.85) +
  geom_text(aes(label = paste0(round(brecha_media, 1), "pp")),
            hjust = -0.1, size = 3.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top 15: mayor brecha IPM rural vs cabecera (pandemia 2020–2021)",
    subtitle = "Puntos porcentuales de diferencia | Fuente: DANE-ECV",
    x = "Brecha IPM (rural − cabecera, pp)", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#5B2C8A"))

ggsave(file.path(RUTA_SALIDA, "g_brecha_ipm_rural.png"), g2, width = 9, height = 6, dpi = 300)

# GRÁFICO 3: Evolución IPM rural vs casos VIF promedio nacional
g3 <- ipm_limpio %>%
  filter(anio >= 2018) %>%
  group_by(anio) %>%
  summarise(ipm_rural_prom = mean(ipm_rural, na.rm = TRUE)) %>%
  left_join(
    datos_oe3 %>% group_by(anio) %>%
      summarise(vif_prom = mean(casos_vif, na.rm = TRUE)),
    by = "anio"
  ) %>%
  pivot_longer(cols = c(ipm_rural_prom, vif_prom),
               names_to = "variable", values_to = "valor") %>%
  mutate(variable = recode(variable,
                           "ipm_rural_prom" = "IPM rural promedio (%)",
                           "vif_prom"       = "Casos VIF promedio"
  )) %>%
  ggplot(aes(x = anio, y = valor, color = variable, group = variable)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 2021.5, linetype = "dashed", color = "grey60") +
  scale_x_continuous(breaks = 2018:2023) +
  scale_color_manual(values = c("#C0392B", "#1A237E"), name = NULL) +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Evolución paralela: IPM rural y VIF en mujeres",
    subtitle = "Promedio nacional por año | Colombia 2018–2023",
    x = "Año", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#5B2C8A"),
        legend.position = "none",
        strip.text = element_text(face = "bold"))

ggsave(file.path(RUTA_SALIDA, "g_evolucion_ipm_rural_vif.png"), g3, width = 10, height = 4.5, dpi = 300)

# -----------------------------------------------------------------------------
# 8. EXPORTAR RESULTADOS
# -----------------------------------------------------------------------------

cat("\n>>> Exportando resultados...\n")

write_xlsx(
  list(
    "IPM_limpio"         = ipm_limpio,
    "GEIH_TD_mujeres"    = geih_limpio,
    "Datos_OE3_cruzados" = datos_oe3,
    "Correlaciones"      = cor_ipm
  ),
  path = file.path(RUTA_SALIDA, "resultados_OE3_IPM.xlsx")
)

# -----------------------------------------------------------------------------
# 9. RESUMEN FINAL
# -----------------------------------------------------------------------------

cat("\n")
cat("================================================================\n")
cat("  RESUMEN OE3 — IPM + TD + VIF\n")
cat("================================================================\n")
cat("  Departamentos en IPM        :", n_distinct(ipm_limpio$departamento), "\n")
cat("  Años cubiertos IPM          : 2018–2023\n")
cat("  Departamentos en GEIH       :", n_distinct(geih_limpio$departamento), "\n")
cat("  Correlación IPM ~ VIF (Rho) :", round(cor_global$estimate, 3), "\n")
cat("  p-valor IPM ~ VIF           :", round(cor_global$p.value, 4), "\n")
cat("  Correlación TD  ~ VIF (Rho) :", round(cor_td$estimate, 3), "\n")
cat("  p-valor TD  ~ VIF           :", round(cor_td$p.value, 4), "\n")
cat("================================================================\n")
cat("  Archivos generados en:", RUTA_SALIDA, "\n")
cat("    g_ipm_vs_vif.png\n")
cat("    g_brecha_ipm_rural.png\n")
cat("    g_evolucion_ipm_rural_vif.png\n")
cat("    resultados_OE3_IPM.xlsx\n")
cat("================================================================\n")
