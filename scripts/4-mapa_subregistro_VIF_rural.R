# =============================================================================
# MAPA DE SUBREGISTRO POTENCIAL DE VIF EN ZONAS RURALES
# Diplomado en Analítica y Modelamiento en Salud de las Mujeres — PSM CO
# =============================================================================
# HIPÓTESIS: Departamentos con alto IPM rural (alta vulnerabilidad estructural)
# pero baja tasa de VIF rural reportada probablemente tienen subregistro —
# no menos violencia, sino menos denuncia por barreras de acceso institucional.
#
# CLASIFICACIÓN DE DEPARTAMENTOS (matriz 2x2):
#   Alta ruralidad-pobreza + Alta VIF rural   → Vulnerable visible
#   Alta ruralidad-pobreza + Baja VIF rural   → Subregistro probable  ← CLAVE
#   Baja ruralidad-pobreza + Alta VIF rural   → Alerta urbana
#   Baja ruralidad-pobreza + Baja VIF rural   → Esperado/controlado
# =============================================================================

library(tidyverse)
library(readxl)
library(sf)
library(geodata)
library(stringi)
library(janitor)
library(cowplot)
library(scales)
library(writexl)

# -----------------------------------------------------------------------------
# 1. CONFIGURACIÓN — AJUSTA ESTAS RUTAS
# -----------------------------------------------------------------------------

RUTA_SIVIGILA <- "~/PSM-COVID19/SIVIGILA"        # carpeta con archivos anuales
RUTA_IPM      <- "~/PSM-COVID19/DANE/anex-PMultidimensional-Departamental-2023.xlsx"
RUTA_MAPAS    <- "~/PSM-COVID19/mapas/"
RUTA_SALIDA   <- "~/PSM-COVID19/resultados_OE3/"
dir.create(RUTA_SALIDA, showWarnings = FALSE)

# Nombres de archivos SIVIGILA por año
archivos_sivi <- c(
  "2018" = "Datos_2018_875.xlsx",
  "2019" = "Datos_2019_875.xlsx",
  "2020" = "Datos_2020_875.xlsx",
  "2021" = "Datos_2021_875.xlsx",
  "2022" = "Datos_2022_875.xlsx",
  "2023" = "Datos_2023_875.xlsx"
)

# -----------------------------------------------------------------------------
# 2. FUNCIÓN ESTANDARIZAR NOMBRES
# -----------------------------------------------------------------------------

estandarizar <- function(x) {
  x %>%
    str_to_title() %>%
    str_trim() %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("Bogota D[.]C[.]|Bogota D[.]C|Bogota[^\\n]*", "Bogota") %>%
    str_replace_all("San Andres Y Providencia|San Andres", "San Andres") %>%
    str_replace_all("^Guajira$",           "La Guajira") %>%
    str_replace_all("Norte Santander|Norte De Santander", "Norte de Santander") %>%
    str_replace_all("^Valle$|Valle Del Cauca", "Valle del Cauca") %>%
    if_else(. %in% c("Exterior", "Procedencia Desconocida"),
            NA_character_, .)
}

# -----------------------------------------------------------------------------
# 3. CARGAR SIVIGILA Y CALCULAR VIF RURAL POR DEPARTAMENTO Y PERÍODO
# -----------------------------------------------------------------------------

cat(">>> Cargando SIVIGILA y calculando VIF rural...\n")

datos_sivi <- map_dfr(
  names(archivos_sivi),
  function(anio) {
    ruta <- file.path(RUTA_SIVIGILA, archivos_sivi[anio])
    read_xlsx(ruta) %>%
      clean_names() %>%
      mutate(anio = as.integer(anio))
  }
)

# Filtrar: mujeres + VIF + zona rural (AREA == 3) o semi-rural (AREA == 2)
vif_rural <- datos_sivi %>%
  filter(
    str_to_upper(sexo) == "F",
    cod_eve == 875,
    area %in% c(2, 3)           # 2 = centro poblado, 3 = rural disperso
  ) %>%
  mutate(
    departamento = estandarizar(str_to_title(departamento_ocurrencia)),
    periodo = case_when(
      anio %in% c(2018, 2019) ~ "Pre-pandemia",
      anio %in% c(2020, 2021) ~ "Durante pandemia",
      anio %in% c(2022, 2023) ~ "Post-pandemia"
    ),
    periodo = factor(periodo, levels = c(
      "Pre-pandemia", "Durante pandemia", "Post-pandemia"
    ))
  ) %>%
  filter(!is.na(departamento), !is.na(periodo)) %>%
  count(departamento, periodo, name = "casos_vif_rural")

cat(">>> Casos VIF rural por departamento-período:", nrow(vif_rural), "filas\n")

# -----------------------------------------------------------------------------
# 4. EXTRAER IPM RURAL DEL ARCHIVO DANE
# -----------------------------------------------------------------------------

cat("\n>>> Extrayendo IPM rural...\n")

ipm_raw  <- read_xlsx(RUTA_IPM, sheet = "IPM_Departamentos", col_names = FALSE)
anios_ipm <- c(2018, 2019, 2020, 2021, 2022, 2023)
notas_skip <- c("Fuente", "Nota", "Datos", "Actualizado", "Departamento", "Total", "Cabeceras")

ipm_rural <- map_dfr(11:nrow(ipm_raw), function(i) {
  depto <- as.character(ipm_raw[[i, 1]])
  if (is.na(depto) || depto == "NA") return(NULL)
  if (any(str_detect(depto, notas_skip))) return(NULL)

  map_dfr(seq_along(anios_ipm), function(j) {
    col_rural <- 1 + (j - 1) * 3 + 3   # columna Rural disperso del año j
    tibble(
      departamento = depto,
      anio         = anios_ipm[j],
      ipm_rural    = as.numeric(ipm_raw[[i, col_rural]])
    )
  })
}) %>%
  filter(!is.na(ipm_rural)) %>%
  mutate(
    departamento = estandarizar(departamento),
    periodo = case_when(
      anio %in% c(2018, 2019) ~ "Pre-pandemia",
      anio %in% c(2020, 2021) ~ "Durante pandemia",
      anio %in% c(2022, 2023) ~ "Post-pandemia"
    ),
    periodo = factor(periodo, levels = c(
      "Pre-pandemia", "Durante pandemia", "Post-pandemia"
    ))
  ) %>%
  group_by(departamento, periodo) %>%
  summarise(ipm_rural_prom = mean(ipm_rural, na.rm = TRUE), .groups = "drop")

cat(">>> IPM rural extraído:", nrow(ipm_rural), "filas\n")

# -----------------------------------------------------------------------------
# 5. CRUZAR VIF RURAL + IPM RURAL Y CLASIFICAR SUBREGISTRO
# -----------------------------------------------------------------------------

cat("\n>>> Clasificando departamentos...\n")

datos_sub <- ipm_rural %>%
  left_join(vif_rural, by = c("departamento", "periodo")) %>%
  mutate(casos_vif_rural = replace_na(casos_vif_rural, 0))

# Clasificar usando medianas como puntos de corte
# (más robusto que la media para datos con distribución asimétrica)
datos_sub <- datos_sub %>%
  group_by(periodo) %>%
  mutate(
    mediana_ipm = median(ipm_rural_prom, na.rm = TRUE),
    mediana_vif = median(casos_vif_rural, na.rm = TRUE),
    clasificacion = case_when(
      ipm_rural_prom >= mediana_ipm & casos_vif_rural <  mediana_vif ~ "Subregistro probable",
      ipm_rural_prom >= mediana_ipm & casos_vif_rural >= mediana_vif ~ "Vulnerable visible",
      ipm_rural_prom <  mediana_ipm & casos_vif_rural >= mediana_vif ~ "Alerta urbana",
      ipm_rural_prom <  mediana_ipm & casos_vif_rural <  mediana_vif ~ "Esperado/controlado",
      TRUE ~ NA_character_
    ),
    clasificacion = factor(clasificacion, levels = c(
      "Subregistro probable",
      "Vulnerable visible",
      "Alerta urbana",
      "Esperado/controlado"
    ))
  ) %>%
  ungroup()

# Resumen por período
cat("\nDistribución de categorías por período:\n")
datos_sub %>%
  count(periodo, clasificacion) %>%
  print()

# -----------------------------------------------------------------------------
# 6. CARGAR SHAPEFILE Y UNIR DATOS
# -----------------------------------------------------------------------------

cat("\n>>> Cargando shapefile...\n")

colombia_gadm <- gadm("COL", level = 1, path = RUTA_MAPAS)
colombia      <- st_as_sf(colombia_gadm) %>%
  mutate(departamento = estandarizar(NAME_1))

# -----------------------------------------------------------------------------
# 7. CONSTRUIR MAPA DE SUBREGISTRO POR PERÍODO
# -----------------------------------------------------------------------------

cat("\n>>> Generando mapas de subregistro...\n")

colores_sub <- c(
  "Subregistro probable"  = "#C0392B",   # rojo — el hallazgo clave
  "Vulnerable visible"    = "#E67E22",   # naranja
  "Alerta urbana"         = "#8E44AD",   # morado
  "Esperado/controlado"   = "#27AE60"    # verde
)

periodos <- levels(datos_sub$periodo)

lista_mapas <- map(periodos, function(per) {

  mapa_sf <- colombia %>%
    left_join(
      datos_sub %>% filter(periodo == per),
      by = "departamento"
    )

  ggplot(mapa_sf) +
    geom_sf(aes(fill = clasificacion), color = "white", linewidth = 0.3) +
    scale_fill_manual(
      values   = colores_sub,
      na.value = "grey85",
      name     = NULL,
      drop     = FALSE
    ) +
    labs(title = per) +
    theme_void(base_size = 10) +
    theme(
      plot.title    = element_text(face = "bold", size = 11,
                                   color = "#2C3E50", hjust = 0.5),
      legend.position = "none"
    )
})

# Leyenda compartida
leyenda <- ggplot(
  data.frame(
    cat = factor(names(colores_sub), levels = names(colores_sub)),
    x = 1, y = seq_along(colores_sub)
  ),
  aes(x, y, color = cat, label = cat)
) +
  geom_point(size = 5) +
  geom_text(hjust = 0, nudge_x = 0.1, size = 3.2) +
  scale_color_manual(values = colores_sub) +
  scale_x_continuous(limits = c(0.8, 3)) +
  theme_void() +
  theme(legend.position = "none")

# Panel final
panel <- plot_grid(
  lista_mapas[[1]],
  lista_mapas[[2]],
  lista_mapas[[3]],
  nrow = 1
)

panel_final <- ggdraw() +
  draw_plot(panel,   x = 0,    y = 0.08, width = 0.78, height = 0.86) +
  draw_plot(leyenda, x = 0.78, y = 0.20, width = 0.22, height = 0.50) +
  draw_label(
    "Subregistro potencial de VIF en mujeres — zonas rurales de Colombia",
    x = 0.5, y = 0.99, vjust = 1, hjust = 0.5,
    fontface = "bold", size = 13, color = "#2C3E50"
  ) +
  draw_label(
    "Clasificacion basada en IPM rural (DANE-ECV) y casos VIF rural (SIVIGILA) | 2018-2023",
    x = 0.5, y = 0.95, vjust = 1, hjust = 0.5,
    fontface = "italic", size = 9, color = "grey40"
  ) +
  draw_label(
    "Rojo = alta pobreza rural + baja denuncia VIF: posible subregistro por barreras de acceso institucional",
    x = 0.5, y = 0.04, vjust = 0, hjust = 0.5,
    fontface = "italic", size = 8, color = "#C0392B"
  )

ggsave(
  file.path(RUTA_SALIDA, "mapa_subregistro_VIF_rural.png"),
  panel_final,
  width = 16, height = 9, dpi = 300, bg = "white"
)

# -----------------------------------------------------------------------------
# 8. DISPERSOGRAMA COMPLEMENTARIO: IPM rural vs VIF rural
# Muestra la misma historia en formato de puntos con etiquetas
# -----------------------------------------------------------------------------

g_scatter <- datos_sub %>%
  filter(periodo == "Durante pandemia") %>%
  ggplot(aes(x = ipm_rural_prom, y = casos_vif_rural,
             color = clasificacion, label = departamento)) +
  geom_point(size = 3.5, alpha = 0.85) +
  ggrepel::geom_text_repel(size = 2.8, max.overlaps = 20) +
  geom_vline(aes(xintercept = mediana_ipm),
             linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_hline(aes(yintercept = mediana_vif),
             linetype = "dashed", color = "grey50", linewidth = 0.7) +
  annotate("text", x = Inf, y = Inf,
           label = "Vulnerable visible", hjust = 1.1, vjust = 1.5,
           size = 3, color = "#E67E22", fontface = "italic") +
  annotate("text", x = -Inf, y = Inf,
           label = "Alerta urbana", hjust = -0.1, vjust = 1.5,
           size = 3, color = "#8E44AD", fontface = "italic") +
  annotate("text", x = Inf, y = -Inf,
           label = "Subregistro probable", hjust = 1.1, vjust = -0.5,
           size = 3, color = "#C0392B", fontface = "italic") +
  annotate("text", x = -Inf, y = -Inf,
           label = "Esperado/controlado", hjust = -0.1, vjust = -0.5,
           size = 3, color = "#27AE60", fontface = "italic") +
  scale_color_manual(values = colores_sub, name = NULL) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Subregistro potencial: IPM rural vs VIF rural durante pandemia",
    subtitle = "Departamentos en zona roja (alto IPM + baja denuncia) tienen mayor probabilidad de subregistro",
    x        = "IPM rural promedio (%)",
    y        = "Casos VIF rural reportados",
    caption  = "Lineas punteadas = medianas | Fuentes: DANE-ECV y SIVIGILA"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", color = "#2C3E50"),
    plot.subtitle = element_text(color = "grey40", size = 9),
    legend.position = "bottom"
  )

# Verificar si ggrepel está disponible
if (!requireNamespace("ggrepel", quietly = TRUE)) {
  cat(">>> Instala ggrepel para etiquetas: install.packages('ggrepel')\n")
  g_scatter <- g_scatter +
    geom_text(size = 2.5, check_overlap = TRUE)
}

ggsave(
  file.path(RUTA_SALIDA, "scatter_subregistro_pandemia.png"),
  g_scatter,
  width = 10, height = 8, dpi = 300, bg = "white"
)

# -----------------------------------------------------------------------------
# 9. EXPORTAR TABLA DE CLASIFICACIÓN
# -----------------------------------------------------------------------------

writexl::write_xlsx(
  list("Clasificacion_subregistro" = datos_sub %>%
         select(departamento, periodo, ipm_rural_prom,
                casos_vif_rural, mediana_ipm, mediana_vif, clasificacion) %>%
         arrange(periodo, clasificacion, departamento)),
  path = file.path(RUTA_SALIDA, "tabla_subregistro_potencial.xlsx")
)

# -----------------------------------------------------------------------------
# 10. RESUMEN EN CONSOLA
# -----------------------------------------------------------------------------

cat("\n")
cat("================================================================\n")
cat("  MAPA DE SUBREGISTRO POTENCIAL\n")
cat("================================================================\n")
cat("  Departamentos analizados  :", n_distinct(datos_sub$departamento), "\n")
cat("\n  Durante pandemia:\n")
datos_sub %>%
  filter(periodo == "Durante pandemia") %>%
  count(clasificacion) %>%
  mutate(label = paste0("    ", clasificacion, ": ", n)) %>%
  pull(label) %>% cat(sep = "\n")
cat("\n")
cat("  Archivos generados:\n")
cat("    mapa_subregistro_VIF_rural.png\n")
cat("    scatter_subregistro_pandemia.png\n")
cat("    tabla_subregistro_potencial.xlsx\n")
cat("  Guardados en:", RUTA_SALIDA, "\n")
cat("================================================================\n")
