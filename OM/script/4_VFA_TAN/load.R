dt <- as.data.table(read_excel("../../data/correct_new.xlsx", sheet = "Sheet1"))
dt <- dt[!str_detect(Treatment, regex("inoculum", ignore_case = TRUE))]
dt[, Treatment := factor(Treatment, levels = c("Control", "Weekly discharge"))]
dt[, Temperature := factor(Temperature)]
dt[, Farm := factor(Farm)]
