dt_35_clean
dt_D <- dt_35_clean[Farm == "A"]
dt_W <- dt_35_clean[Farm == "B"]

vars_pool <- c("Temperature",
               "VS", "CP", "NDF",
               "Lipids", 
               "VFAs", 
               "TAN", "pH")
results_D <- data.table()

for (k in 2:8) {
  combos <- combn(vars_pool, k, simplify = FALSE)
  
  for (vars in combos) {
    
    formula_txt <- paste("CH4 ~", paste(vars, collapse = " + "))
    formula_obj <- as.formula(formula_txt)
    
    model <- lm(formula_obj, data = dt_D)
    sm <- summary(model)
    
    if (any(is.na(coef(sm)))) next
    
    coef_tab <- as.data.table(coef(sm), keep.rownames = "variable")
    
    coef_tab[, `:=`(
      formula = formula_txt,
      R2 = sm$r.squared,
      adjR2 = sm$adj.r.squared,
      F_p = pf(sm$fstatistic[1], sm$fstatistic[2], sm$fstatistic[3], lower.tail = FALSE)
    )]
    
    results_D <- rbind(results_D, coef_tab, fill = TRUE)
  }
}

results_W <- data.table()

for (k in 2:8) {
  combos <- combn(vars_pool, k, simplify = FALSE)
  
  for (vars in combos) {
    
    formula_txt <- paste("CH4 ~", paste(vars, collapse = " + "))
    formula_obj <- as.formula(formula_txt)
    
    model <- lm(formula_obj, data = dt_W)
    sm <- summary(model)
    
    if (any(is.na(coef(sm)))) next
    
    coef_tab <- as.data.table(coef(sm), keep.rownames = "variable")
    
    coef_tab[, `:=`(
      formula = formula_txt,
      R2 = sm$r.squared,
      adjR2 = sm$adj.r.squared,
      F_p = pf(sm$fstatistic[1], sm$fstatistic[2], sm$fstatistic[3], lower.tail = FALSE)
    )]
    
    results_W <- rbind(results_W, coef_tab, fill = TRUE)
  }
}
head(results_D)
head(results_W)


