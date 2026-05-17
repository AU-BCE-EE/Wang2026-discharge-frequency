setDT(dt_rem)
dt_rem[, Farm := factor(Farm)]
dt_rem[, Temperature := factor(Temperature)]
dt_rem[, Treatment := factor(Treatment, levels = c("control", "weekly flushing"))]
dt_rem[, Day := as.integer(Day)]

dt_A <- dt_rem[Farm == "A"]
#######CP
data_A_CP <- dt_rem[
  Farm == "A" & !is.na(CP),]
mod_A_CP_lin <- lm(
  CP ~ Temperature * Treatment,
  data = data_A_CP)
mod_A_CP_quad <- lm(
  CP ~ poly(Temperature, 2) * Treatment,
  data = data_A_CP)
Anova(mod_A_CP_lin,  type = 2)
Anova(mod_A_CP_quad, type = 2)
####VS
data_A_VS <- dt_rem[
  Farm == "A" & !is.na(VS),
]

mod_A_VS_lin <- lm(
  VS ~ Temperature * Treatment,
  data = data_A_VS
)

mod_A_VS_quad <- lm(
  VS ~ poly(Temperature, 2) * Treatment,
  data = data_A_VS
)

Anova(mod_A_VS_lin,  type = 2)
Anova(mod_A_VS_quad, type = 2)
##Lipids
data_A_Lipids <- dt_rem[
Farm == "A" & !is.na(Lipids),
]

mod_A_Lipids_lin <- lm(
  Lipids ~ Temperature * Treatment,
  data = data_A_Lipids
)

mod_A_Lipids_quad <- lm(
  Lipids ~ poly(Temperature, 2) * Treatment,
  data = data_A_Lipids
)

Anova(mod_A_Lipids_lin,  type = 2)
Anova(mod_A_Lipids_quad, type = 2)
##NDF
data_A_NDF <- dt_rem[
  Farm == "A" & !is.na(NDF),
]

mod_A_NDF_lin <- lm(
  NDF ~ Temperature * Treatment,
  data = data_A_NDF
)

mod_A_NDF_quad <- lm(
  NDF ~ poly(Temperature, 2) * Treatment,
  data = data_A_NDF
)

Anova(mod_A_NDF_lin,  type = 2)
Anova(mod_A_NDF_quad, type = 2)
##Hemicellulose
data_A_Hemi <- dt_rem[
  Farm == "A" & !is.na(Hemicellulose),
]

mod_A_Hemi_lin <- lm(
  Hemicellulose ~ Temperature * Treatment,
  data = data_A_Hemi
)

mod_A_Hemi_quad <- lm(
  Hemicellulose ~ poly(Temperature, 2) * Treatment,
  data = data_A_Hemi
)

Anova(mod_A_Hemi_lin,  type = 2)
Anova(mod_A_Hemi_quad, type = 2)
###Cellulose
data_A_Cellulose <- dt_rem[
  Farm == "A" & !is.na(Cellulose),
]

mod_A_Cellulose_lin <- lm(
  Cellulose ~ Temperature * Treatment,
  data = data_A_Cellulose
)

mod_A_Cellulose_quad <- lm(
  Cellulose ~ poly(Temperature, 2) * Treatment,
  data = data_A_Cellulose
)

Anova(mod_A_Cellulose_lin,  type = 2)
Anova(mod_A_Cellulose_quad, type = 2)
