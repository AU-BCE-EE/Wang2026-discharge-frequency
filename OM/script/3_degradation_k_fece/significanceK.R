setDT(dt)
kdt <- copy(dt_k_long)
kdt <- kdt[Treatment %in% c("control", "weekly flushing")]
kdt <- kdt[Farm %in% c("A", "B")]
temp_levels <- sort(unique(kdt$Temperature))  
day_levels  <- sort(unique(kdt$Day))          
metric_levels <- c("CP","NDF","VS","Lipids","Hemicellulose","Cellulose","TS")
kdt[, `:=`(
  Farm      = factor(Farm),
  Treatment   = factor(Treatment, levels = c("control", "weekly flushing")),
  Temperature = factor(Temperature, levels = temp_levels),
  Day         = factor(Day, levels = day_levels),
  Metric      = factor(Metric, levels = metric_levels),
  K           = as.numeric(K),
  SE          = as.numeric(SE),
  N           = as.numeric(N)
)]

## drop missing / non-finite K
kdt <- kdt[is.finite(K)]
kdt <- kdt[!is.na(Farm) & !is.na(Treatment) & !is.na(Temperature) & !is.na(Day) & !is.na(Metric)]

## ---- 3) Optional: weight by precision ----
use_weights <- TRUE
kdt[, w := ifelse(use_weights & is.finite(SE) & SE > 0, 1/(SE^2), 1)]
run_lm_K_by_metric <- function(metric_name, adjust_method = "tukey") {
  d <- kdt[Metric == metric_name]
  form_main <- K ~ Farm * Treatment + Treatment * Temperature + Day
  m <- lm(form_main, data = d, weights = if (use_weights) w else NULL)
  a3 <- tryCatch(car::Anova(m, type = 3), error = function(e) e)
  emm_S <- emmeans(m, ~ Treatment | Farm)
  pw_S  <- pairs(emm_S, adjust = adjust_method)
  emm_ST <- emmeans(m, ~ Treatment | Farm * Temperature)
  pw_ST  <- pairs(emm_ST, adjust = adjust_method)
  emm_SD <- emmeans(m, ~ Treatment | Farm * Day)
  pw_SD  <- pairs(emm_SD, adjust = adjust_method)
  
  list(
    metric = metric_name,
    data   = d,
    model  = m,
    anova3 = a3,
    emm_Farm = emm_S,
    pairs_Farm = pw_S,
    pairs_Farm_temp = pw_ST,
    pairs_Farm_day  = pw_SD
  )
}

metrics_to_run <- intersect(levels(kdt$Metric), c("VS","CP","NDF","Lipids"))
res_list <- lapply(metrics_to_run, run_lm_K_by_metric)
names(res_list) <- metrics_to_run

## ---- Print results ----
for (mm in metrics_to_run) {
  cat("\n\n==============================\n")
  cat("METRIC:", mm, "\n")
  cat("==============================\n")
  
  cat("\nType III ANOVA:\n")
  if (inherits(res_list[[mm]]$anova3, "error")) {
    cat("Type III ANOVA failed (aliased coefficients / sparse cells).\n")
    cat("Error message:\n")
    cat(conditionMessage(res_list[[mm]]$anova3), "\n")
    cat("\nTry simplifying the model (see notes at bottom).\n")
  } else {
    print(res_list[[mm]]$anova3)
  }
  
  cat("\nWeekly vs Control within each Farm (Tukey):\n")
  print(res_list[[mm]]$pairs_Farm)
  
  cat("\nWeekly vs Control within each Farm x Temperature (Tukey):\n")
  print(res_list[[mm]]$pairs_Farm_temp)
  
  cat("\nWeekly vs Control within each Farm x Day (Tukey):\n")
  print(res_list[[mm]]$pairs_Farm_day)
}

## ---- Build a compact p-value table from Type III ANOVA (per Metric) ----
p_table1 <- rbindlist(lapply(res_list, function(res) {
  if (inherits(res$anova3, "error")) {
    return(data.table(
      Metric = res$metric,
      Effect = c("Farm","Treatment","Temperature","Day","Farm:Treatment","Treatment:Temperature"),
      `Pr(>F)` = NA_real_
    ))
  }
  tab <- as.data.frame(res$anova3)
  data.table(
    Metric = res$metric,
    Effect = rownames(tab),
    `Pr(>F)` = tab[["Pr(>F)"]]
  )
}), fill = TRUE)

cat("\n\n==============================\n")
cat("P-VALUE SUMMARY (Type III)\n")
cat("==============================\n")
print(p_table1)

mK_all <- lm(
  K ~ Metric + Farm * Treatment + Treatment * Temperature + Day,
  data = kdt,
  weights = if (use_weights) w else NULL
)

a3_all <- tryCatch(car::Anova(mK_all, type = 3), error = function(e) e)

cat("\n\n==============================\n")
cat("POOLED MODEL (all metrics)\n")
cat("==============================\n")
if (inherits(a3_all, "error")) {
  cat("Type III ANOVA failed (aliased coefficients / sparse cells).\n")
  cat("Error message:\n")
  cat(conditionMessage(a3_all), "\n")
} else {
  print(a3_all)
}

## Weekly vs Control within each Farm (pooled over metrics, temp, day)
emm_all_S <- emmeans(mK_all, ~ Treatment | Farm)
pw_all_S  <- pairs(emm_all_S, adjust = "tukey")
cat("\nWeekly vs Control within each Farm (POOLED, Tukey):\n")
print(pw_all_S)
