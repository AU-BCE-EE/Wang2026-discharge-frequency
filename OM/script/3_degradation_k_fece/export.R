fwrite(
  p_table1,
  file = "../../output/K_ANOVA_pvalues.csv")
fwrite(p_table, "../../output/lm_removal_rate_p.csv")

dt_wide_window <- dcast(
  dt_plot,
  Metric + Treatment + Farm + Temperature ~ Window,
  value.var = "K"
)
fwrite(dt_wide_window, "../../output/K_by_time.csv")

ggsave(
  filename = "../../figure/hydrolysis_rate.png",
  plot = p,
  width = 19,
  height = 16,
  units = "cm",
  dpi = 900
)
