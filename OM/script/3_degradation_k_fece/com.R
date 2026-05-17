dt_plot <- copy(dt_k)
dt_plot <- dt_plot[Treatment %chin% c("weekly flushing", "control")]

dt_plot[, Window := factor(Day, levels = c(14, 30, 90), labels = c("14", "30", "90"))]
dt_plot[, Treatment := factor(Treatment, levels = c("control", "weekly flushing"))]

dt_plot[, Temperature := as.character(Temperature)]
dt_plot[, Temperature := factor(Temperature, levels = c("10", "17.5", "25"))]

dt_plot <- dt_plot[!(Metric %chin% c("Lipids", "NDF") & Day == 14)]
dt_plot <- dt_plot[Day %in% c(14, 30, 90)]

# ---- Split: AB (raw points + mean lines) vs CD (points only, overlaid into A) ----
dt_ab <- dt_plot[Farm %chin% c("A", "B")]
dt_cd <- dt_plot[Farm %chin% c("C", "D")]

# mean for AB only
dt_ab_mean <- dt_ab[
  , .(K_mean = mean(K, na.rm = TRUE)),
  by = .(Farm, Temperature, Metric, Window, Treatment)
]

# Keep original C/D identity for legend (shape)
dt_cd[, Farm_shape := as.character(Farm)]

# Overlay C/D into A facet
dt_cd[, Farm := "A"]

# Relabel facet strips
dt_ab[, Farm := factor(Farm, levels = c("A", "B"), labels = c("Farm A", "Farm B"))]
dt_cd[, Farm := factor(Farm, levels = c("A", "B"), labels = c("Farm A", "Farm B"))]
dt_ab_mean[, Farm := factor(Farm, levels = c("A", "B"), labels = c("Farm A", "Farm B"))]

treat_labs <- c("control" = "Control", "weekly flushing" = "Weekly flushing")

p <- ggplot() +
  # ---- AB: raw replicate points ----
geom_point(
  data = dt_ab,
  aes(x = Window, y = K, color = Treatment),
  size = 2.0,
  alpha = 0.8,
  na.rm = TRUE,
  position = position_jitter(width = 0.06, height = 0)
) +
  
  # ---- AB: mean lines only ----
geom_line(
  data = dt_ab_mean,
  aes(x = Window, y = K_mean, color = Treatment, group = Treatment),
  linewidth = 0.6,
  na.rm = TRUE
) +
  
  # ---- AB: mean points ----
geom_point(
  data = dt_ab_mean,
  aes(x = Window, y = K_mean, color = Treatment),
  size = 2.6,
  na.rm = TRUE
) +
  
  # ---- C & D: raw points only, overlaid into Farm A ----
geom_point(
  data = dt_cd,
  aes(x = Window, y = K, color = Treatment, shape = Farm_shape),
  size = 3.0,
  stroke = 0.9,
  fill = "white",
  na.rm = TRUE,
  position = position_jitter(width = 0.06, height = 0)
) +
  
  facet_nested(
    Metric ~ Farm + Temperature,
    scales = "free_y",
    nest_line = element_line(linewidth = 0.6)
  ) +
  
  scale_color_manual(
    values = c("control" = "#D55E00", "weekly flushing" = "#0072B2"),
    labels = treat_labs,
    name = "Treatment"
  ) +
  scale_shape_manual(
    values = c("C" = 21, "D" = 24),
    labels = c("C" = "Farm C", "D" = "Farm D"),
    name = "Overlaid farms"
  ) +
  labs(
    x = "Incubation days (days)",
    y = expression("First-order rate constant, "~K~"(day"^{-1}*")")
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    text = element_text(size = 12),
    axis.title = element_text(size = 13),
    axis.text  = element_text(size = 11),
    strip.text = element_text(size = 12)
  ) +
  facetted_pos_scales(
    y = list(
      Metric == "VS"  ~ scale_y_continuous(breaks = scales::pretty_breaks(n = 3)),
      Metric == "NDF" ~ scale_y_continuous(breaks = scales::pretty_breaks(n = 3))
    )
  )

print(p)