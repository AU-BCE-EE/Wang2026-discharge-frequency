dt_plot <- dt_long[
  Metric %in% c("Cellulose", "Hemicellulose") &
    Treatment %in% c("control", "weekly flushing") &
    Day != 14
]
dt_plot_sum <- dt_plot[, .(
  Mean = mean(Value, na.rm = TRUE),
  SE   = sd(Value, na.rm = TRUE) / sqrt(sum(!is.na(Value)))
), by = .(Farm, Temperature, Treatment, Metric, Day)]
ggplot(
  dt_plot_sum,
  aes(
    x = Day,
    y = Mean,
    color = Treatment,
    group = Treatment
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(ymin = Mean - SE, ymax = Mean + SE),
    width = 2
  ) +
  facet_grid(
    Farm + Metric ~ Temperature,
    scales = "free_y"
  ) +
  labs(
    x = "Day",
    y = "Concentration",
    color = "Treatment"
  ) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )