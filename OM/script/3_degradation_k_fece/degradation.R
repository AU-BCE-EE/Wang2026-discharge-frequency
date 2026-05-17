setDT(dt)
dt32 <- dt[Treatment %in% c("Control", "Weekly flushing")]
om_vars <- c(
  "VS",
  "CP",
  "NDF",
  "Hemicellulose",
  "Cellulose",
  "Lipids")
baseline <- dt32[Day == 0,
                 .(Farm, Temperature, Treatment, Replicate,
                   VS_0 = VS,
                   CP_0 = CP,
                   NDF_0 = NDF,
                   Hemicellulose_0 = Hemicellulose,
                   Cellulose_0 = Cellulose,
                   Lipids_0 = Lipids)]
dt_rem <- merge(
  dt32[Day %in% c(14, 30, 90)],
  baseline,
  by = c("Farm", "Temperature", "Treatment", "Replicate"),
  all.x = TRUE,
  sort = FALSE
)
dt_rem[, VS_removal_pct :=
         (VS_0 - VS) / VS_0 * 100]
dt_rem[, CP_removal_pct :=
         (CP_0 - CP) / CP_0 * 100]
dt_rem[, NDF_removal_pct :=
         (NDF_0 - NDF) / NDF_0 * 100]
dt_rem[, Hemicellulose_removal_pct :=
         (Hemicellulose_0 - Hemicellulose) / Hemicellulose_0 * 100]
dt_rem[, Cellulose_removal_pct :=
         (Cellulose_0 - Cellulose) / Cellulose_0 * 100]
dt_rem[, Lipids_removal_pct :=
         (Lipids_0 - Lipids) / Lipids_0 * 100]