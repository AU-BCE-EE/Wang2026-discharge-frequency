metrics <- c("CP","NDF","VS","Lipids")
keep_cols <- c("Farm", "Temperature", "Treatment", "Replicate", "Day", metrics)
dt <- dt[, ..keep_cols]

dt_long <- melt(
  dt,
  id.vars = c("Farm", "Temperature", "Treatment", "Replicate", "Day"),
  variable.name = "Metric",
  value.name = "Value"
)

base <- dt_long[Day == 0, .(C0 = Value),
                by = .(Farm, Temperature, Treatment, Replicate, Metric)]

target_days <- c(14, 30, 90)
dt_k <- dt_long[Day %in% target_days]
dt_k <- merge(
  dt_k, base,
  by = c("Farm", "Temperature", "Treatment", "Replicate", "Metric"),
  all.x = TRUE
)

dt_k <- dt_k[!is.na(C0) & !is.na(Value) & C0 > 0 & Value > 0]
dt_k[, K := -log(Value / C0) / Day]
dt_k_long <- dt_k[, .(
  K  = mean(K, na.rm = TRUE),
  SE = sd(K, na.rm = TRUE) / sqrt(.N),
  N  = .N
), by = .(Farm, Temperature, Treatment, Metric, Day)]

setorder(dt_k_long, Farm, Temperature, Treatment, Metric, Day)
dt_k_wide <- dcast(dt_k_long,
                   Farm + Temperature + Treatment + Metric ~ Day,
                   value.var = "K")
setnames(dt_k_wide,
         old = intersect(as.character(target_days), names(dt_k_wide)),
         new = paste0("K", intersect(as.character(target_days), names(dt_k_wide))))

dt_fit <- copy(dt_k)
dt_fit[, y := log(Value / C0)]
dt_fit[, t := Day]

k_fit <- dt_fit[, {
  m <- lm(y ~ 0 + t)                 # 截距强制为 0
  slope <- coef(m)[["t"]]            # slope = -k
  list(
    k  = -slope,
    SE = sqrt(vcov(m)[1,1]),
    N  = .N
  )
}, by = .(Farm, Temperature, Treatment, Metric)]

#
