freq_D <- results_D[variable != "(Intercept)", .(
  sig_freq = sum(`Pr(>|t|)` < 0.05, na.rm = TRUE),
  total = .N
), by = variable]

freq_D[, sig_ratio := sig_freq / total]

freq_D[order(-sig_ratio)]
freq_W <- results_W[variable != "(Intercept)", .(
  sig_freq = sum(`Pr(>|t|)` < 0.05, na.rm = TRUE),
  total = .N
), by = variable]

freq_W[, sig_ratio := sig_freq / total]

freq_W[order(-sig_ratio)]

