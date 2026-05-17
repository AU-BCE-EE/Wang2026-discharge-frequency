ind_keep <- c("CP", "VS","Lipids","NDF")
p_all_ft <- ggplot(
  dt_long[Indicator %in% ind_keep],
  aes(x = Day, y = Removal_pct,
      group = interaction(Farm, Temperature, Treatment, Replicate),
      linetype = Treatment)
) +
  geom_line(alpha = 0.35) +
  geom_point(alpha = 0.6, size = 1.6) +
  facet_grid(Farm + Temperature ~ Indicator, scales = "free_y") +
  labs(x = "Day", y = "Removal (%)",
       title = "Removal (%) over time by Farm & Temperature (weekly flushing vs control)") +
  theme_bw()
print(p_all_ft)
days_need <- c(14, 30, 90)
plot_one_day_ft <- function(d) {
  dd <- dt_long[
    Day == d & 
      is.finite(Removal_pct) & 
      Indicator %in% c("VS", "CP", "NDF", "Hemicellulose", "Cellulose", "Lipids")
  ]
  if (nrow(dd) == 0) {
    message("Day ", d, ": no data after filtering.")
    return(NULL)
  }
  keep <- dd[, .(n_non_na = sum(!is.na(Removal_pct))),
             by = .(Farm, Temperature, Indicator)][n_non_na > 0]
  dd <- merge(dd, keep[, .(Farm, Temperature, Indicator)],
              by = c("Farm", "Temperature", "Indicator"),
              all = FALSE)
  if (nrow(dd) == 0) {
    message("Day ", d, ": no indicators with data after filtering.")
    return(NULL)
  }
  ggplot(dd, aes(x = Treatment, y = Removal_pct)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.4) +
    geom_jitter(width = 0.15, alpha = 0.7, size = 1.8) +
    facet_grid(Farm + Temperature ~ Indicator, scales = "free_y") +
    labs(x = NULL, y = "Removal (%)",
         title = paste0("Removal (%) at Day ", d,
                        " by Farm & Temperature (weekly flushing vs control)")) +
    theme_bw()
}
p14_ft <- plot_one_day_ft(14); if (!is.null(p14_ft)) print(p14_ft)
p30_ft <- plot_one_day_ft(30); if (!is.null(p30_ft)) print(p30_ft)
p90_ft <- plot_one_day_ft(90); if (!is.null(p90_ft)) print(p90_ft)
