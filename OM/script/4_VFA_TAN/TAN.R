##############################################
## 3) FAN (NH3-N) from TAN, pH, T-dependent Ka
##############################################
dt[, TempK := TempC + 273.15]
R <- 8.314
Ka25_NH4 <- 10^(-9.25)

dt[, Ka_NH4 := Ka25_NH4 * exp((51965 / R) * (1/298.15 - 1/TempK))]
dt[, pKa_NH4 := -log10(Ka_NH4)]
dt[, FAN_NH3N := TAN / (1 + 10^(pKa_NH4 - pH))]

##############################################
## 4) Helper: mean ± SE plot (like your VFA style)
##############################################
plot_mean_se <- function(sumDT, title_txt, ylab_expr_or_txt) {
  ggplot(sumDT, aes(x = Day, y = mean, color = Treatment, group = Treatment)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.2) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 1.2, linewidth = 0.6) +
    facet_grid(Farm ~ Temperature, scales = "free_y") +
    labs(title = title_txt, x = "Day", y = ylab_expr_or_txt, color = NULL) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "top",
      panel.spacing = unit(0.8, "lines"),
      panel.grid.major.x = element_blank(),
      strip.text.y = element_text(face = "bold"),
      strip.text.x = element_text(face = "bold")
    )
}
######
#-----------------------------
# TAN
# same style as Figure 5
#-----------------------------

# check TAN range first
range(
  dt_plot$TAN,
  na.rm = TRUE
)

#-----------------------------
# TAN plot
#-----------------------------

p_TAN <- plot_raw_with_mean_line(
  dt_plot,
  value_col = "TAN",
  ylab_expr_or_txt = expression(
    TAN~(g~kg~slurry^{-1})
  ),
  y_breaks = c(
    0,
    2.5,
    5.5
  ),
  y_view = c(
    -0.2,
    5.5
  )
) +
  labs(
    x = "Day"
  )

print(p_TAN)