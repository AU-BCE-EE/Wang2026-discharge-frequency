##############################################
## 2) FFA (Free acids): T-dependent pKa (linear)
##############################################
alpha <- -0.01
pKa25_acetic    <- 4.76
pKa25_propionic <- 4.87
pKa25_butanoic  <- 4.82

dt[, `:=`(
  pKa_acetic    = pKa25_acetic    + alpha * (TempC - 25),
  pKa_propionic = pKa25_propionic + alpha * (TempC - 25),
  pKa_butanoic  = pKa25_butanoic  + alpha * (TempC - 25)
)]

dt[, `:=`(
  Ka_acetic    = 10^(-pKa_acetic),
  Ka_propionic = 10^(-pKa_propionic),
  Ka_butanoic  = 10^(-pKa_butanoic)
)]

dt[, H := 10^(-pH)]

dt[, `:=`(
  FA_Acetic    = Acetic    * (H / (Ka_acetic    + H)),
  FA_Propionic = Propionic * (H / (Ka_propionic + H)),
  FA_Butanoic  = Butanoic  * (H / (Ka_butanoic  + H))
)]
dt[, FFA_total := FA_Acetic + FA_Propionic + FA_Butanoic]
