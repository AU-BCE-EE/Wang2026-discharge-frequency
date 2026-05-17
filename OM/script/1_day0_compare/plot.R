dt_long <- as.data.table(dt_long)

dt_long[, Treatment := factor(
  Treatment,
  levels = c("AC","AW","BC","BW","CW","DW")
)]
dt_long[, Treat2 := factor(
  Treat2,
  levels = c("Control","Weekly flushing")
)]

bg_df <- data.frame(
  xmin   = c(0.5, 2.5, 4.5, 5.5),
  xmax   = c(2.5, 4.5, 5.5, 6.5),
  Farm   = factor(c("A","B","C","D"), levels = c("A","B","C","D"))
)

p <- ggplot(
  dt_long,
  aes(x = Treatment, y = Value, colour = Treat2)
) +
  geom_rect(
    data = bg_df,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = Farm),
    inherit.aes = FALSE,
    alpha = 0.08
  ) +
  geom_jitter(
    width = 0.12,
    height = 0,
    size = 4.5,
    alpha = 0.8
  ) +
  facet_wrap(
    ~ Variable,
    scales   = "free_y",
    ncol     = 4,
    labeller = as_labeller(var_labels, label_parsed)
  ) +
  scale_colour_manual(
    values = c(
      "Control" = "#D55E00",
      "Weekly flushing" = "#0072B2"
    ),
    name = "Treatment"
  ) +
  scale_fill_manual(
    values = c("A"="#999999","B"="white","C"="#999999","D"="white"),
    guide = "none"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.15)),
    n.breaks = 4
  ) +
  labs(
    x = "Treatment",
    y = "Concentration"
  ) +
  theme_bw(base_size = 24) +
  theme(
    strip.background   = element_rect(fill = "grey90", colour = NA),
    strip.text         = element_text(face = "bold", size = 18),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 18),
    axis.text.y        = element_text(size = 18),
    legend.position    = "top",
    legend.text        = element_text(size = 18),
    legend.title       = element_text(size = 18)
  )

print(p)