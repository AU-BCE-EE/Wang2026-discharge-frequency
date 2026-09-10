# Keepers
ind_keep   <- c("VS", "CP", "NDF", "Lipids")
treat_keep <- c("control", "weekly flushing")

# Day 90 data
dd90 <- dt_long[
  Day == 90 &
    is.finite(Removal_pct) &
    Indicator %in% ind_keep &
    Treatment %in% treat_keep
]

# Mean only (for connecting lines)
mean90 <- dd90[
  , .(
    mean_removal = mean(Removal_pct, na.rm = TRUE)
  ),
  by = .(Farm, Temperature, Treatment, Indicator)
]

# Factor order
dd90[, Indicator := factor(Indicator, levels = ind_keep)]
mean90[, Indicator := factor(Indicator, levels = ind_keep)]

# Treatment colors
treat_cols <- c(
  "control" = "#D55E00",
  "weekly flushing" = "#0072B2"
)

# Split data
dd90_A   <- dd90[Farm == "A"]
dd90_B   <- dd90[Farm == "B"]
mean90_A <- mean90[Farm == "A"]
mean90_B <- mean90[Farm == "B"]

# C & D overlaid into A
dd90_CD <- dd90[Farm %in% c("C", "D")]
dd90_CD[, Farm := factor(Farm, levels = c("C", "D"))]
dd90_CD[, Farm_facet := "A"]
setnames(dd90_CD, "Farm", "Farm_shape")
setnames(dd90_CD, "Farm_facet", "Farm")

# Plot
p90 <- ggplot() +
  
  # ---- A: all points + mean line ----
geom_point(
  data = dd90_A,
  aes(Temperature, Removal_pct, color = Treatment),
  size = 1.8,
  alpha = 0.7,
  position = position_jitter(width = 0.05, height = 0)
) +
  geom_line(
    data = mean90_A,
    aes(Temperature, mean_removal, color = Treatment, group = Treatment),
    linewidth = 1
  ) +
  geom_point(
    data = mean90_A,
    aes(Temperature, mean_removal, color = Treatment),
    size = 2.6
  ) +
  
  # ---- B: all points + mean line ----
geom_point(
  data = dd90_B,
  aes(Temperature, Removal_pct, color = Treatment),
  size = 1.8,
  alpha = 0.7,
  position = position_jitter(width = 0.05, height = 0)
) +
  geom_line(
    data = mean90_B,
    aes(Temperature, mean_removal, color = Treatment, group = Treatment),
    linewidth = 1
  ) +
  geom_point(
    data = mean90_B,
    aes(Temperature, mean_removal, color = Treatment),
    size = 2.6
  ) +
  
  # ---- C & D: all raw points only, legend-visible ----
geom_point(
  data = dd90_CD,
  aes(
    Temperature,
    Removal_pct,
    color = Treatment,
    shape = Farm_shape
  ),
  size = 2.4,
  alpha = 0.8,
  stroke = 0.9,
  position = position_jitter(width = 0.05, height = 0)
) +
  
  facet_grid(
    Indicator ~ Farm,
    scales = "fixed",
    labeller = labeller(
      Farm = c(
        "A" = "Farm A",
        "B" = "Farm B"
      )
    )
  ) +
  facet_grid(
    Indicator ~ Farm,
    scales = "fixed",
    labeller = labeller(
      Farm = c("A" = "Farm A", "B" = "Farm B")
    )
  ) +
  
  scale_y_continuous(
    breaks = seq(-25, 75, 25),
    limits = c(-37, 82),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  
  scale_color_manual(
    name   = "Treatment",
    values = treat_cols,
    labels = c("Control", "Weekly flushing")
  ) +
  
  scale_shape_manual(
    name   = "",
    values = c("C" = 21, "D" = 24),
    labels = c("Farm C", "Farm D")
  ) +
  
  labs(
    x = "Temperature (°C)",
    y = "Removal at Day 90 (%)"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "grey92", color = "grey40"),
    strip.text = element_text(size = 14),
    strip.placement = "outside",
    strip.text.y = element_text(angle = 90)
  )

print(p90)