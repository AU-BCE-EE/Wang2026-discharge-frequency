setDT(dt)
dt <- dt[Treatment %in% c("control", "weekly flushing")]
dt <- dt[Farm %in% c("A", "B")]
run_lm_removal <- function(var,
                           day0 = 0,
                           day1 = 90,
                           treat_levels = c("control", "weekly flushing"),
                           temp_levels  = c(10, 17.5, 25),
                           adjust_method = "tukey") {
  stopifnot(var %in% names(dt))
  
  ## 1) keep only required days + columns
  d0 <- dt[
    Treatment %in% treat_levels & Day %in% c(day0, day1),
    .(Farm, Temperature, Treatment, Replicate, Day, value = get(var))
  ]
  
  ## 2) wide: Day0 / Day1 columns
  dw <- dcast(
    d0,
    Farm + Temperature + Treatment + Replicate ~ Day,
    value.var = "value"
  )
  
  ## guard against missing day columns
  if (!(as.character(day0) %in% names(dw)) || !(as.character(day1) %in% names(dw))) {
    stop(sprintf("After dcast, cannot find columns '%s' and '%s' for variable %s",
                 day0, day1, var))
  }
  
  ## 3) compute removal
  col0 <- as.character(day0)
  col1 <- as.character(day1)
  dw[, removal := (get(col0) - get(col1)) / get(col0)]
  
  ## 4) clean: finite + non-missing
  dw <- dw[is.finite(removal)]
  dw <- dw[!is.na(Temperature) & !is.na(Treatment)]
  
  ## 5) factors (AFTER dcast, as you already learned)
  dw[, `:=`(
    Temperature = factor(Temperature, levels = temp_levels),
    Treatment   = factor(Treatment,   levels = treat_levels),
    Farm      = factor(Farm),
    Replicate   = factor(Replicate)
  )]
  
  ## 6) fit LM (no random effects)
  m <- lm(removal ~ Farm * Treatment + Treatment * Temperature, data = dw)
  
  ## 7) Type III ANOVA + EMMEANS pairwise at each temperature
  a3  <- car::Anova(m, type = 3)
  emm <- emmeans(m, ~ Treatment | Farm)
  pairs(emm, adjust = "tukey")
  pw  <- pairs(emm, adjust = adjust_method)
  
  ## return everything
  list(
    var     = var,
    data_w  = dw,
    model   = m,
    anova3  = a3,
    emmeans = emm,
    pairs   = pw
  )
}

## ---- 2) run for all variables ----
vars <- c("VS", "CP", "NDF", "Lipids")

results <- lapply(vars, run_lm_removal)
names(results) <- vars

## ---- 3) print results nicely ----
for (v in vars) {
  cat("\n==============================\n")
  cat("Variable:", v, "\n")
  cat("==============================\n\n")
  
  cat("Type III ANOVA:\n")
  print(results[[v]]$anova3)
  
  cat("\nPost hoc (Treatment within each Temperature, Tukey):\n")
  print(results[[v]]$pairs)
}

## ---- 4) OPTIONAL: compact summary table of p-values ----
p_table <- rbindlist(lapply(results, function(res) {
  tab <- as.data.frame(res$anova3)
  data.table(
    Variable = res$var,
    Effect   = rownames(tab),
    `Pr(>F)` = tab[["Pr(>F)"]]
  )
}))

print(p_table)
