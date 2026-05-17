ind_keep   <- c("VS", "CP", "NDF", "Lipids")
treat_keep <- c("control", "weekly flushing")

dd90 <- dt_long[
  Day == 90 &
    is.finite(Removal_pct) &
    Indicator %in% ind_keep &
    Treatment %in% treat_keep]
sum90 <- dd90[
  , .(
    n = sum(!is.na(Removal_pct)),
    mean_removal = mean(Removal_pct, na.rm = TRUE),
    sd_removal   = sd(Removal_pct, na.rm = TRUE)),
  by = .(Farm, Temperature, Treatment, Indicator)][
  , se_removal := sd_removal / sqrt(n)]
sum90[, Indicator := factor(Indicator, levels = c("VS", "CP", "NDF", "Hemicellulose", "Cellulose", "Lipids"))]
treat_cols <- c(
  "control" = "#D55E00",
  "weekly flushing" = "#0072B2")
treat_labs <- c(
  "control" = "Control",
  "weekly flushing" = "Weekly flushing")
p90 <- ggplot(
  sum90,
  aes(x = Temperature, y = mean_removal, color = Treatment, group = Treatment)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.6) +
  geom_errorbar(
    aes(ymin = mean_removal - se_removal, ymax = mean_removal + se_removal),
    width = 0.15,
    linewidth = 0.6) +
  facet_grid( Indicator~Farm , scales = "free_y") +
  scale_color_manual(
    name   = "Treatment",
    values = treat_cols,
    breaks = c("control", "weekly flushing"),
    labels = treat_labs) +
  labs(
    x = "Temperature (°C)",
    y = "Removal at Day 90 (%)",
    title = "") +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "grey92", color = "grey40", linewidth = 0.6),
    strip.text = element_text(size = 14, color = "black"),
    strip.placement = "outside",
    strip.text.y = element_text(angle = 90),
    plot.title = element_text(hjust = 0.5, size = 14))

print(p90)
