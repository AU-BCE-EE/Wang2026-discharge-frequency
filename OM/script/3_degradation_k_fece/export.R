fwrite(
  p_table1,
  file = "../../output/K_ANOVA_pvalues.csv")
fwrite(p_table, "../../output/lm_removal_rate_p.csv")

dt_export <- dt_plot[
  , .(Metric, Treatment, Farm, Temperature, Day, K)
]
fwrite(dt_export, "../../output/K_all.csv")
ggsave(
  filename = "../../figure/hydrolysis_rate.png",
  plot = p,
  width = 19,
  height = 16,
  units = "cm",
  dpi = 900
)
