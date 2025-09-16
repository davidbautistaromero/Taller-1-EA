# Cargar la configuración base
source(here::here("scripts", "config.R"))

# Secuencia de T (incluye 0)
T_seq <- seq(0, 10, length.out = 2000)

# 1) Nombres de columnas seguros y etiquetas limpias
df_long <- tibble(
  T     = T_seq,
  asinh = asinh(T_seq),
  log1p = log1p(T_seq),
  log   = ifelse(T_seq > 0, log(T_seq), NA_real_)
) %>%
  pivot_longer(-T, names_to = "funcion", values_to = "valor") %>%
  mutate(
    funcion = factor(
      funcion,
      levels = c("asinh", "log1p", "log"),                 # niveles en los datos
      labels = c("arcsinh(T)", "ln(T+1)", "ln(T)")         # etiquetas a mostrar
    )
  )

# 2) Escalas manuales CON nombres que coinciden con los levels (etiquetas)
cols <- c("arcsinh(T)" = "#1f77b4",  # azul
          "ln(T+1)"    = "#2ca02c",  # verde
          "ln(T)"      = "#d62728")  # rojo

lts  <- c("arcsinh(T)" = "solid",
          "ln(T+1)"    = "dashed",
          "ln(T)"      = "dotdash")

# 3) Gráfica general
png(filename = file.path("outputs", "comparacion_tranformaciones_T.png"),
    width = 1200, height = 1000)
ggplot(df_long, aes(x = T, y = valor,
                             color = funcion, linetype = funcion)) +
  geom_line(linewidth = 1, na.rm = TRUE) +  # na.rm para evitar el warning de ln(0)
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lts) +
  labs(
    title = "Comparación transformaciones de T",
    x = "T",
    y = "Valor transformado"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

dev.off()

# 4) Zoom cerca de 0
png(filename = file.path("outputs", "zoom_comparacion_tranformaciones_T.png"),
    width = 1200, height = 1000)
df_long %>%
  filter(T <= 0.5) %>%
  ggplot(aes(x = T, y = valor, color = funcion, linetype = funcion)) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = lts) +
  coord_cartesian(ylim = c(-0.1, 0.5)) +
  labs(
    title = "Zoom cerca de T = 0 (ln(T) tiende a -Inf)",
    x = "T",
    y = "Valor transformado"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

dev.off()