dt_D <- dt_35_clean[Farm == "A"]
dt_W <- dt_35_clean[Farm == "B"]

vars <- c("CH4", "Temperature",
          "VS", "CP", "NDF",
          "Lipids", 
          "VFAs", 
          "TAN", "pH")

my_col <- colorRampPalette(c("#67a9cf","white","#ef8a62"))(200)

plot_corr_export <- function(dt, filename){
  
  corr <- rcorr(as.matrix(dt[, ..vars]), type = "spearman")
  R <- corr$r
  P <- corr$P
  n <- nrow(R)
  sig_star <- matrix("", nrow = n, ncol = n)
  sig_star[P < 0.05]  <- "*"
  sig_star[P < 0.01]  <- "**"
  sig_star[P < 0.001] <- "***"
  
  png(filename, width = 6000, height = 6000, res = 900)
  
  corrplot(R,
           method = "color",
           type = "upper",
           col = my_col,
           tl.col = "black",
           tl.cex = 1.2,       
           number.cex = 1.2,    
           addCoef.col = "black",
           addgrid.col = "white",
           cl.pos = "r",
           cl.cex = 1.2)        
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (j > i && sig_star[i, j] != "") {
        y_pos <- n - i + 1
        text(x = j,
             y = y_pos + 0.30,
             labels = sig_star[i, j],
             cex = 1.2,        
             col = "black")
      }
    }
  }
  
  dev.off()
}
