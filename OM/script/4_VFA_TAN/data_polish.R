dt <- dt[!str_detect(Treatment, regex("inoculum", ignore_case = TRUE))]
dt <- dt[Treatment != "Unit" & Replicate != "Unit"]
dt[, Treatment := factor(Treatment, levels = c("Control", "Weekly discharge"))]
dt[, Farm := factor(Farm, levels = c("A","B","C","D"))]
dt[, Temperature := str_squish(as.character(Temperature))]
dt[str_detect(Temperature, "^[0-9.]+$"),
   Temperature := paste0(Temperature, "°C")]
dt[, Temperature := factor(Temperature, levels = c("10°C", "17.5°C", "25°C"))]
dt[, Replicate := as.integer(Replicate)]
dt[, Day := as.numeric(as.character(Day))]
dt[, TempC := as.numeric(str_extract(as.character(Temperature), "[0-9.]+"))]


