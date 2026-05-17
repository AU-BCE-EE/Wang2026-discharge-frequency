fp_sum <- "../../data/final_summary_inoculum_corrected.xlsx"
fp_ch4 <- "../../../CH4/2farmCH4_name_change.xlsx"
dt_sum <- as.data.table(readxl::read_xlsx(fp_sum, sheet = 1))
dt_ch4 <- as.data.table(readxl::read_xlsx(fp_ch4, sheet = 1))

