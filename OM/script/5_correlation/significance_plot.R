# Farm D
plot_D <- copy(freq_D)
plot_D <- plot_D[order(sig_ratio)]   
plot_D[, variable := factor(variable, levels = variable)]

# Farm W
plot_W <- copy(freq_W)
plot_W <- plot_W[order(sig_ratio)]
plot_W[, variable := factor(variable, levels = variable)]

p_D <- ggplot(plot_D, aes(x = sig_ratio, y = variable)) +
  geom_col(width = 0.7,
           fill = "#F4A261",
           color = "black",
           linewidth = 0.4) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    x = "Significance frequency",
    y = "",
    title = ""
  ) +
  theme_bw(base_size = 20) +
  theme(
    text = element_text(family = "Arial", color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", linewidth = 0.8),
    panel.grid = element_blank()
  )

p_D


p_W <- ggplot(plot_W, aes(x = sig_ratio, y = variable)) +
  geom_col(width = 0.7,
           fill = "#F4A261",
           color = "black",
           linewidth = 0.4) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    x = "Significance frequency",
    y = "",
    title = ""
  ) +
  theme_bw(base_size = 20) +
  theme(
    text = element_text(family = "Arial", color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", linewidth = 0.8),
    panel.grid = element_blank()
  )

p_W
