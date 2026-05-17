setDT(dt_sum)
setDT(dt_ch4)
setnames(dt_ch4, "temp", "Temperature")
setnames(dt_ch4, "time", "Day")
setnames(dt_ch4, "mean", "CH4")    
dt_sum[, Treatment := tolower(Treatment)]
dt_ch4[, Treatment := tolower(Treatment)]
dt_sum[Treatment == "control", Treatment := "Control"]
dt_sum[grepl("week", Treatment), Treatment := "Weekly"]
dt_ch4[Treatment == "control", Treatment := "Control"]
dt_ch4[grepl("week", Treatment), Treatment := "Weekly"]
dt_sum[, Temperature := as.numeric(Temperature)]
dt_ch4[, Temperature := as.numeric(Temperature)]
dt_sum <- dt_sum[Treatment %in% c("Control", "Weekly")]
dt_ch4 <- dt_ch4[Treatment %in% c("Control", "Weekly")]
dt_sum_90 <- dt_sum[Day == 90]
dt_sum_90_mean <- dt_sum_90[, lapply(.SD, mean, na.rm = TRUE),
                            by = .(Farm, Temperature, Treatment, Replicate),
                            .SDcols = c("Hemicellulose","Cellulose","VS","Lipids","NDF",
                                        "pH","CP","VFAs","TAN",
                                        "Acetic","Propionic","Butanoic",
                                        "TS","Lignin")]
dt_ch4_90 <- dt_ch4[Day == 90, .(Farm, Temperature, Treatment, CH4)]

dt_ch4_90 <- dt_ch4_90[, .(CH4 = mean(CH4, na.rm = TRUE)),
                       by = .(Farm, Temperature, Treatment)]
setkey(dt_sum_90_mean, Farm, Temperature, Treatment)
setkey(dt_ch4_90, Farm, Temperature, Treatment)

dt_35_clean <- dt_sum_90_mean[dt_ch4_90]
dt_35_clean[, .N, by = .(Farm, Temperature, Treatment)]
