library(tidyverse)

# 1. Generación de datos reproducibles (mismo promedio, distinta dispersión)
set.seed(42)

datos_proceso <- tibble(
  linea = rep(c("Línea A", "Línea B", "Línea C", "Línea D"), each = 120),
  tiempo_ciclo = c(
    rnorm(120, mean = 24.0, sd = 2.1),
    rnorm(120, mean = 23.9, sd = 2.4),
    c(rnorm(100, mean = 20.2, sd = 1.4), runif(20, min = 36, max = 50)),
    rnorm(120, mean = 24.1, sd = 2.2)
  )
)

# 2. Resumen estadístico: Paramétrico vs. No Paramétrico
resumen <- datos_proceso |>
  group_by(linea) |>
  summarise(
    n = n(),
    media = round(mean(tiempo_ciclo), 2),
    desv_est = round(sd(tiempo_ciclo), 2),
    mediana = round(median(tiempo_ciclo), 2),
    iqr = round(IQR(tiempo_ciclo), 2),
    p90 = round(quantile(tiempo_ciclo, 0.90), 2)
  )

print(resumen)

# 3. Construcción del gráfico de alta densidad informativa
p <- ggplot(datos_proceso, aes(x = linea, y = tiempo_ciclo, fill = linea)) +
  geom_jitter(
    aes(color = linea),
    width = 0.16,
    alpha = 0.45,
    size = 1.8,
    show.legend = FALSE
  ) +
  geom_boxplot(
    width = 0.35,
    alpha = 0.75,
    outlier.shape = NA,
    color = "#F8F9FA",
    linewidth = 0.6,
    show.legend = FALSE
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 3.5,
    fill = "#FF4B4B",
    color = "#121212"
  ) +
  annotate(
    "text",
    x = 3, y = 47,
    label = "Cola crítica (>36 min)\narrastra el promedio",
    color = "#FF4B4B",
    size = 3.6,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("#3B82F6", "#3B82F6", "#F59E0B", "#3B82F6")) +
  scale_color_manual(values = c("#60A5FA", "#60A5FA", "#FBBF24", "#60A5FA")) +
  labs(
    title = "El promedio miente: 4 líneas con media idéntica (24 min)",
    subtitle = "Rombo rojo = Media | Línea blanca = Mediana | Puntos = Ciclos reales (n = 480)",
    x = NULL,
    y = "Tiempo de ciclo (minutos)",
    caption = "Fuente: Simulación operativa en R (ggplot2) | @Alquimista_Kio"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#121212", color = NA),
    panel.background = element_rect(fill = "#121212", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#2A2A2A", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    text = element_text(color = "#F8F9FA"),
    axis.text = element_text(color = "#D1D5DB", face = "bold"),
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 4)),
    plot.subtitle = element_text(color = "#9CA3AF", size = 10.5, margin = margin(b = 14)),
    plot.caption = element_text(color = "#6B7280", size = 9, margin = margin(t = 12))
  )

# 4. Exportación dual automática (300 DPI)
ggsave("grafico_x_yt_16x9.png", plot = p, width = 12, height = 6.75, dpi = 300)
ggsave("grafico_ig_4x5.png", plot = p, width = 8, height = 10, dpi = 300)
