setDT(dt)
dt_sub <- dt[Treatment %chin% c("weekly flushing", "control")]
metrics <- c("VS", "CP", "NDF", "Hemicellulose", "Cellulose", "Lipids")
id_cols <- c("Farm", "Temperature", "Treatment", "Replicate")
base0 <- dt_sub[Day == 0, c(id_cols, metrics), with = FALSE]
setnames(base0, metrics, paste0(metrics, "_0"))
dt_rem <- merge(dt_sub, base0, by = id_cols, all.x = TRUE, sort = FALSE)
for (m in metrics) {
  dt_rem[, paste0(m, "_removal_pct") :=
           (get(paste0(m, "_0")) - get(m)) / get(paste0(m, "_0")) * 100]
}
rem_cols <- paste0(metrics, "_removal_pct")
dt_long <- melt(
  dt_rem,
  id.vars = c(id_cols, "Day"),
  measure.vars = rem_cols,
  variable.name = "Indicator",
  value.name = "Removal_pct",
  variable.factor = FALSE
)
dt_long[, Indicator := gsub("_removal_pct$", "", Indicator)]
dt_long <- dt_long[Day != 0]
dt_long <- dt_long[is.finite(Removal_pct)]
