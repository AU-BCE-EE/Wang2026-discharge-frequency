
dt <- as.data.table(dt)

dt[, Farm := substr(Treatment, 1, 1)]

# AC/BC = Control；AW/BW/CW/DW = Weekly flushing
dt[, Treat2 := fifelse(
  endsWith(Treatment, "C"),
  "Control",
  "Weekly flushing"
)]

dt[, Farm      := factor(Farm,      levels = c("A","B","C","D"))]
dt[, Treat2    := factor(Treat2,    levels = c("Control","Weekly flushing"))]
dt[, Treatment := factor(Treatment, levels = c("AC","AW","BC","BW","CW","DW"))]

# ------------------------------------------------
# 2) long format
# ------------------------------------------------
dt_long <- melt(
  dt,
  id.vars       = c("Treatment","Replicate","Farm","Treat2"),
  measure.vars  = measure_cols,
  variable.name = "Variable",
  value.name    = "Value"
)

dt_sum <- dt_long[
  , {
    x <- Value[!is.na(Value)]
    n <- length(x)
    m <- if (n > 0) mean(x) else NA_real_
    sdv <- if (n > 1) sd(x) else NA_real_
    .(
      n    = n,
      mean = m,
      ymin = if (!is.na(sdv)) m - sdv else NA_real_,
      ymax = if (!is.na(sdv)) m + sdv else NA_real_
    )
  },
  by = .(Treatment, Treat2, Variable)
]

var_labels <- c(
  TS            = "TS~(g~kg~slurry^{-1})",
  VS            = "VS~(g~kg~DM^{-1})",
  CP            = "CP~(g~kg~DM^{-1})",
  TAN           = "TAN~(g~kg~slurry^{-1})",
  TKN           = "TKN~(g~kg~slurry^{-1})",
  pH            = "pH~(dimensionless)",
  VFAs          = "VFAs~(g~kg~slurry^{-1})",
  Acetic        = "Acetic~(g~kg~slurry^{-1})",
  Propionic     = "Propionic~(g~kg~slurry^{-1})",
  Butanoic      = "Butanoic~(g~kg~slurry^{-1})",
  NDF           = "NDF~(g~kg~DM^{-1})",
  Hemicellulose = "Hemicellulose(g~kg~DM^{-1})",
  Cellulose     = "Cellulose~(g~kg~DM^{-1})",
  Lignin        = "Lignin~(g~kg~DM^{-1})",
  Lipids        = "Lipids~(g~kg~DM^{-1})"
)



