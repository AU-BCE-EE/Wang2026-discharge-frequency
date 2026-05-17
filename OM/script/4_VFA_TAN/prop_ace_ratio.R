dt[, Prop_Ace_Ratio := Propionic / Acetic]
dt[Acetic == 0, Prop_Ace_Ratio := 0]