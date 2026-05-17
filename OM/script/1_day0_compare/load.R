dt <- read_excel("../../data/Day0DM.xlsx")
dt <- as.data.table(dt)
dt <- dt[Treatment != "Unit" & Replicate != "Unit"]
dt[, Replicate := as.integer(Replicate)]
measure_cols <- setdiff(names(dt), c("Treatment", "Replicate"))
dt[, (measure_cols) := lapply(.SD, as.numeric), .SDcols = measure_cols]
