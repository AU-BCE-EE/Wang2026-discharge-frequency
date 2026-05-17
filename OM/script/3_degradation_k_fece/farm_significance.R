## ================================
## Significance test: Farm D vs W
## ================================
dt_test <- copy(dt_k)
dt_test <- dt_test[!(Metric %chin% c("Lipids", "NDF") & Day == 14)]
dt_test <- dt_test[Day %in% c(14, 30, 90)]
dt_test[, Farm := factor(Farm, levels = c("D", "W"))]
farm_test <- dt_test[
  , {
    if (length(unique(Farm)) == 2) {
      tt <- t.test(K ~ Farm)
      .(
        mean_D = mean(K[Farm == "D"], na.rm = TRUE),
        mean_W = mean(K[Farm == "W"], na.rm = TRUE),
        p_value = tt$p.value
      )
    } else {
      .(
        mean_D = NA_real_,
        mean_W = NA_real_,
        p_value = NA_real_
      )
    }
  },
  by = .(Temperature, Metric, Day)
]
farm_test[, signif := fifelse(
  p_value < 0.001, "***",
  fifelse(p_value < 0.01, "**",
          fifelse(p_value < 0.05, "*", "ns"))
)]
farm_test
