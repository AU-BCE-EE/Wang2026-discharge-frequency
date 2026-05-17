file <- "../../data/final_summary_inoculum_corrected.xlsx"
dt <- as.data.table(read_xlsx(file, sheet = 1))
dt[, Farm := factor(Farm)]
dt[, Treatment := factor(Treatment)]
dt[, Replicate := as.integer(Replicate)]
dt[, Day := as.numeric(Day)]

# Temperature "10°C" -> 10
dt[, Temperature := as.numeric(gsub("[^0-9.\\-]+", "", as.character(Temperature)))]