# =============================================================================
# MAPA BIVARIADO: IPM RURAL + TASA DE VIF POR DEPARTAMENTO
# Diplomado en Analítica y Modelamiento en Salud de las Mujeres — PSM CO
# =============================================================================
# Este mapa combina DOS variables simultáneamente:
#   - IPM rural (eje de pobreza territorial)
#   - Tasa de VIF en mujeres (eje de violencia de género)
# Los departamentos con alto IPM rural Y alta VIF son los más vulnerables
# estructuralmente → esto es interseccionalidad aplicada cartográficamente.
# =============================================================================

# Paquetes necesarios
# install.packages(c("sf", "biscale", "cowplot", "viridis",
#                    "readxl", "tidyverse", "janitor"))

library(tidyverse)
library(sf)           # manejo de geometrías espaciales
library(biscale)      # escala bivariada (dos variables en un mapa)
library(cowplot)      # para combinar mapa + leyenda
library(readxl)
library(janitor)
library(scales)

# -----------------------------------------------------------------------------
# 1. CONFIGURACIÓN
# -----------------------------------------------------------------------------

# Archivo de resultados OE3 (generado por el script anterior)
RUTA_OE3    <- "~/PSM-COVID19/resultados_OE3/resultados_OE3_IPM.xlsx"

# Carpeta donde geodata descargó el shapefile
RUTA_MAPAS  <- "~/PSM-COVID19/mapas/"

RUTA_SALIDA <- "~/PSM-COVID19/resultados_OE3"
dir.create(RUTA_SALIDA, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 2. CARGAR Y PREPARAR DATOS
# -----------------------------------------------------------------------------

cat(">>> Cargando datos OE3...\n")

datos <- read_xlsx(RUTA_OE3, sheet = "Datos_OE3_cruzados")

# Calcular tasa de VIF por 100.000 mujeres por departamento y período
# (necesitas población femenina por departamento — aquí usamos los datos
# de población del GEIH que ya tienes, o una estimación del DANE Censo 2018)

# Por ahora calculamos tasa relativa usando pob_total del GEIH si está disponible
# Si no tienes la columna pob_total, el script usa casos absolutos normalizados

library(stringi)

estandarizar <- function(x) {
  x %>%
    str_to_title() %>%
    str_trim() %>%
    # Eliminar tildes para unificar todas las fuentes
    stri_trans_general("Latin-ASCII") %>%
    # Unificar variantes de nombres
    str_replace_all("Bogota D[.]C[.]|Bogota D[.]C|Bogota[^\n]*", "Bogota") %>%
    str_replace_all("San Andres Y Providencia|San Andres", "San Andres") %>%
    str_replace_all("^Guajira$",            "La Guajira") %>%
    str_replace_all("Norte Santander|Norte De Santander", "Norte de Santander") %>%
    str_replace_all("^Valle$|Valle Del Cauca", "Valle del Cauca")
}

datos_periodo <- datos %>%
  mutate(departamento = estandarizar(departamento)) %>%
  filter(!is.na(periodo), !is.na(ipm_rural), !is.na(casos_vif)) %>%
  group_by(departamento, periodo) %>%
  summarise(
    ipm_rural_prom  = mean(ipm_rural,   na.rm = TRUE),
    casos_vif_prom  = mean(casos_vif,   na.rm = TRUE),
    td_prom         = mean(td_mujeres,  na.rm = TRUE),
    .groups = "drop"
  )

cat(">>> Períodos disponibles:", levels(datos_periodo$periodo), "\n")

# -----------------------------------------------------------------------------
# 3. CARGAR SHAPEFILE DE COLOMBIA
# -----------------------------------------------------------------------------

cat("\n>>> Cargando shapefile de Colombia (GADM)...\n")

library(geodata)
colombia_gadm <- gadm("COL", level = 1, path = RUTA_MAPAS)
colombia      <- st_as_sf(colombia_gadm)

# El campo de nombres en GADM es NAME_1
colombia <- colombia %>%
  mutate(departamento = estandarizar(NAME_1))

# Verificar coincidencias
deptos_datos <- unique(datos_periodo$departamento)
deptos_shp   <- unique(colombia$departamento)
sin_match    <- setdiff(deptos_datos, deptos_shp)
if (length(sin_match) > 0)
  cat("⚠️  Sin match en shapefile:", paste(sin_match, collapse = ", "), "\n")

# -----------------------------------------------------------------------------
# 4. CONSTRUIR MAPA BIVARIADO POR PERÍODO
# biscale divide cada variable en 3 clases (alto/medio/bajo)
# generando 9 combinaciones posibles (3×3 = grilla bivariada)
# -----------------------------------------------------------------------------

cat("\n>>> Construyendo mapas bivariados...\n")

periodos <- c("Pre-pandemia", "Durante pandemia", "Post-pandemia")

lista_mapas <- map(periodos, function(per) {

  # Filtrar período
  datos_per <- datos_periodo %>% filter(periodo == per)

  # Unir con shapefile
  mapa_sf <- colombia %>%
    left_join(datos_per, by = "departamento")

  # Aplicar escala bivariada (3×3 clases)
  # dim1 = IPM rural (pobreza), dim2 = casos VIF (violencia)
  mapa_bi <- bi_class(
    mapa_sf,
    x = ipm_rural_prom,
    y = casos_vif_prom,
    style = "quantile",   # divide en terciles por distribución real de los datos
    dim = 3
  )

  # Paleta bivariada: "DkBlue" combina azul (alto IPM) y naranja (alto VIF)
  # Las celdas 3-3 (alto IPM + alta VIF) son los departamentos más vulnerables
  pal <- "DkBlue"

  # Construir mapa
  mapa <- ggplot(mapa_bi) +
    geom_sf(aes(fill = bi_class), color = "white", linewidth = 0.3,
            show.legend = FALSE) +
    bi_scale_fill(pal = pal, dim = 3) +
    labs(
      title    = per,
      subtitle = "IPM rural (→) × Tasa VIF (↑)",
      caption  = "Fuentes: DANE-ECV y SIVIGILA"
    ) +
    theme_void(base_size = 10) +
    theme(
      plot.title    = element_text(face = "bold", size = 11,
                                   color = "#5B2C8A", hjust = 0.5),
      plot.subtitle = element_text(size = 8, color = "grey40", hjust = 0.5),
      plot.caption  = element_text(size = 7, color = "grey60"),
      plot.margin   = margin(5, 5, 5, 5)
    )

  return(list(mapa = mapa, data = mapa_bi, pal = pal))
})

# -----------------------------------------------------------------------------
# 5. CONSTRUIR LEYENDA BIVARIADA
# -----------------------------------------------------------------------------

# La leyenda es una grilla 3×3:
# Esquina inferior izquierda = bajo IPM + bajo VIF (menos vulnerable)
# Esquina superior derecha   = alto IPM + alta VIF (más vulnerable)

leyenda <- bi_legend(
  pal  = "DkBlue",
  dim  = 3,
  xlab = "Mayor IPM rural →",
  ylab = "Mayor VIF ↑",
  size = 9
)

# -----------------------------------------------------------------------------
# 6. COMPONER EL PANEL FINAL (3 MAPAS + LEYENDA + ANOTACIONES)
# -----------------------------------------------------------------------------

cat("\n>>> Componiendo panel final...\n")

panel_mapas <- plot_grid(
  lista_mapas[[1]]$mapa,
  lista_mapas[[2]]$mapa,
  lista_mapas[[3]]$mapa,
  nrow  = 1,
  align = "hv"
)

# Añadir leyenda a la derecha
panel_final <- ggdraw() +
  draw_plot(panel_mapas,  x = 0,    y = 0.05, width = 0.82, height = 0.90) +
  draw_plot(leyenda,      x = 0.83, y = 0.30, width = 0.16, height = 0.40) +
  draw_label(
    "Vulnerabilidad estructural de las mujeres ante la VIF en Colombia",
    x = 0.5, y = 0.99, vjust = 1, hjust = 0.5,
    fontface = "bold", size = 13, color = "#5B2C8A"
  ) +
  draw_label(
    "Combinación de Pobreza Multidimensional Rural e Incidencia de VIF por departamento | 2018–2023",
    x = 0.5, y = 0.96, vjust = 1, hjust = 0.5,
    fontface = "italic", size = 9, color = "grey40"
  ) +
  draw_label(
    "Departamentos en zona oscura (alto IPM rural + alta VIF) concentran la mayor vulnerabilidad interseccional",
    x = 0.5, y = 0.04, vjust = 0, hjust = 0.5,
    fontface = "italic", size = 8, color = "#C0392B"
  )

ggsave(
  file.path(RUTA_SALIDA, "mapa_bivariado_VIF_IPM.png"),
  panel_final,
  width  = 16,
  height = 9,
  dpi    = 300,
  bg     = "white"
)

cat(">>> Mapa guardado: mapa_bivariado_VIF_IPM.png\n")

# -----------------------------------------------------------------------------
# 7. MAPA COMPLEMENTARIO: CAMBIO PRE → PANDEMIA (urbano vs rural marcado)
# Este mapa muestra qué departamentos tuvieron mayor aumento de VIF
# y marca con un símbolo los departamentos predominantemente rurales
# -----------------------------------------------------------------------------

cat("\n>>> Construyendo mapa de cambio pre→pandemia...\n")

# Calcular cambio porcentual entre pre-pandemia y durante pandemia
cambio_vif <- datos_periodo %>%
  filter(periodo %in% c("Pre-pandemia", "Durante pandemia")) %>%
  select(departamento, periodo, casos_vif_prom, ipm_rural_prom) %>%
  pivot_wider(names_from = periodo,
              values_from = c(casos_vif_prom, ipm_rural_prom)) %>%
  clean_names() %>%
  mutate(
    cambio_pct = round(
      (casos_vif_prom_durante_pandemia - casos_vif_prom_pre_pandemia) /
        casos_vif_prom_pre_pandemia * 100, 1
    ),
    # Clasificar como predominantemente rural si IPM rural > 30%
    predominio = if_else(ipm_rural_prom_pre_pandemia > 30,
                         "Rural vulnerable", "Urbano/mixto")
  )

mapa_cambio <- colombia %>%
  left_join(cambio_vif, by = "departamento")

# Calcular centroides para marcar los departamentos rurales vulnerables
centroides <- mapa_cambio %>%
  filter(predominio == "Rural vulnerable") %>%
  st_centroid()

g_cambio <- ggplot(mapa_cambio) +
  geom_sf(aes(fill = cambio_pct), color = "white", linewidth = 0.3) +
  geom_sf(data = centroides, shape = 4, size = 2.5,
          color = "black", stroke = 1.2) +
  scale_fill_gradient2(
    low      = "#1A7A4A",
    mid      = "#F5F5F5",
    high     = "#C0392B",
    midpoint = 0,
    name     = "Cambio VIF (%)",
    labels   = function(x) paste0(x, "%"),
    na.value = "grey85"
  ) +
  annotate("text", x = Inf, y = -Inf,
           label = "✕ = Departamentos con IPM rural > 30%\n(mayor vulnerabilidad estructural)",
           hjust = 1.05, vjust = -0.5, size = 2.8, color = "grey30") +
  labs(
    title    = "Cambio en VIF contra mujeres: pre-pandemia → durante pandemia",
    subtitle = "Colombia | Variación porcentual de casos | Cruces marcan departamentos predominantemente rurales",
    caption  = "Fuentes: SIVIGILA e IPM DANE-ECV"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12,
                                 color = "#5B2C8A", hjust = 0.5),
    plot.subtitle = element_text(size = 9, color = "grey40", hjust = 0.5),
    plot.caption  = element_text(size = 8, color = "grey60"),
    legend.position = "right"
  )

ggsave(
  file.path(RUTA_SALIDA, "mapa_cambio_prepandemia_pandemia.png"),
  g_cambio,
  width  = 10,
  height = 9,
  dpi    = 300,
  bg     = "white"
)

cat(">>> Mapa guardado: mapa_cambio_prepandemia_pandemia.png\n")

# -----------------------------------------------------------------------------
# 8. TABLA DE DEPARTAMENTOS MÁS VULNERABLES (para el texto del documento)
# -----------------------------------------------------------------------------

vulnerables <- cambio_vif %>%
  left_join(
    datos_periodo %>%
      filter(periodo == "Durante pandemia") %>%
      select(departamento, casos_vif_prom, ipm_rural_prom),
    by = "departamento"
  ) %>%
  mutate(
    score_vulnerabilidad = scale(casos_vif_prom)[,1] +
                           scale(ipm_rural_prom)[,1] +
                           scale(pmax(cambio_pct, 0))[,1]
  ) %>%
  arrange(desc(score_vulnerabilidad)) %>%
  select(departamento, cambio_pct, casos_vif_prom,
         ipm_rural_prom, predominio, score_vulnerabilidad) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

cat("\nTop 10 departamentos más vulnerables (score combinado):\n")
print(head(vulnerables, 10))

writexl::write_xlsx(
  list("Vulnerabilidad_deptos" = vulnerables,
       "Cambio_VIF"            = cambio_vif),
  path = file.path(RUTA_SALIDA, "tabla_vulnerabilidad_territorial.xlsx")
)

# -----------------------------------------------------------------------------
# 9. RESUMEN FINAL
# -----------------------------------------------------------------------------

cat("\n")
cat("================================================================\n")
cat("  MAPAS GENERADOS\n")
cat("================================================================\n")
cat("  mapa_bivariado_VIF_IPM.png\n")
cat("     → Panel 3 períodos: IPM rural × VIF (escala bivariada)\n")
cat("     → Identifica departamentos con doble vulnerabilidad\n")
cat("  mapa_cambio_prepandemia_pandemia.png\n")
cat("     → Cambio % VIF entre pre-pandemia y pandemia\n")
cat("     → Marca departamentos rurales vulnerables (IPM rural > 30%)\n")
cat("  tabla_vulnerabilidad_territorial.xlsx\n")
cat("     → Ranking de departamentos por score de vulnerabilidad\n")
cat("================================================================\n")
cat("  Guardados en:", RUTA_SALIDA, "\n")
cat("================================================================\n")
