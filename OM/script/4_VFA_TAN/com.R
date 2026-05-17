library(data.table)
library(ggplot2)
library(patchwork)

#-----------------------------
# colors and basic variables
#-----------------------------
treat_cols <- c(
  "Control" = "#D55E00",
  "Weekly discharge" = "#0072B2"
)

#-----------------------------
# data prep
#-----------------------------
dt_plot <- copy(dt)

dt_plot[, Treatment := factor(Treatment, levels = names(treat_cols))]

dt_plot[, Farm2 := fifelse(
  Farm %chin% c("A", "C", "D"), "FarmA",
  fifelse(Farm == "B", "FarmB", as.character(Farm))
)]
dt_plot[, Farm2 := factor(Farm2, levels = c("FarmA", "FarmB"))]

dt_plot[, Farm_label := fifelse(
  Farm == "C", "farm C",
  fifelse(Farm == "D", "farm D", NA_character_)
)]

# optional: if Day order matters
#dt_plot[, Day := factor(Day, levels = sort(unique(as.numeric(as.character(Day)))))]
dt_plot[, Day := as.numeric(as.character(Day))]
#-----------------------------
# theme
#-----------------------------
theme_sci_arial <- theme_bw(base_size = 20, base_family = "Arial") +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 20, color = "black"),
    strip.text = element_text(size = 20),
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 1),
    legend.title = element_text(size = 20),
    legend.text  = element_text(size = 20),
    legend.position = "top",
    panel.grid.major = element_line(color = "grey85", linewidth = 0.6),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", linewidth = 1)
  )

common_scales <- list(
  scale_color_manual(
    values = treat_cols,
    drop = FALSE,
    name = "Treatment"
  )
)

#-----------------------------
# plotting function
#-----------------------------
plot_raw_with_mean_line <- function(d, value_col,
                                    title_txt = "",
                                    ylab_expr_or_txt = "") {
  
  # keep rows with non-missing values
  d0 <- copy(d)[is.finite(get(value_col))]
  
  # split AB and CD
  d_ab <- d0[Farm %chin% c("A", "B")]
  d_cd <- d0[Farm %chin% c("C", "D")]
  
  # mean lines only for A and B
  mean_ab <- d_ab[, .(
    mean_value = mean(get(value_col), na.rm = TRUE)
  ), by = .(Farm2, Temperature, Day, Treatment)]
  
  ggplot() +
    # ---- A/B raw points ----
  geom_point(
    data = d_ab,
    aes(
      x = Day,
      y = get(value_col),
      color = Treatment
    ),
    size = 2.6,
    alpha = 0.8,
    position = position_jitter(width = 0.10, height = 0)
  ) +
    
    # ---- A/B mean lines ----
  geom_line(
    data = mean_ab,
    aes(
      x = Day,
      y = mean_value,
      color = Treatment,
      group = Treatment
    ),
    linewidth = 1.4
  ) +
    
    # ---- A/B mean points ----
  geom_point(
    data = mean_ab,
    aes(
      x = Day,
      y = mean_value,
      color = Treatment
    ),
    size = 3.2
  ) +
    
    # ---- C/D raw points overlaid into FarmB ----
  geom_point(
    data = d_cd,
    aes(
      x = Day,
      y = get(value_col),
      color = Treatment,
      shape = Farm_label
    ),
    size = 3.5,
    fill = "white",
    stroke = 1.2,
    position = position_jitter(width = 0.10, height = 0)
  ) +
    
    facet_grid(Temperature ~ Farm2, scales = "free_y") +
    scale_y_continuous(n.breaks = 3) +
    scale_shape_manual(
      values = c("farm C" = 21, "farm D" = 22),
      name   = ""
    ) +
    labs(
      title = title_txt,
      x = NULL,
      y = ylab_expr_or_txt
    ) +
    common_scales +
    theme_sci_arial
}

#-----------------------------
# individual panels
#-----------------------------
p_VFA <- plot_raw_with_mean_line(
  dt_plot,
  value_col = "VFAs",
  ylab_expr_or_txt = expression(Total~VFAs~(g~kg~slurry^{-1}))
) + labs(x = NULL)

p_Ratio <- plot_raw_with_mean_line(
  dt_plot,
  value_col = "Prop_Ace_Ratio",
  ylab_expr_or_txt = "Propionic / Acetic Ratio"
) + labs(x = NULL)

p_FFA <- plot_raw_with_mean_line(
  dt_plot,
  value_col = "FFA_total",
  ylab_expr_or_txt = expression(Total~FFA~(g~kg~slurry^{-1}))
) + labs(x = NULL)

p_FAN <- plot_raw_with_mean_line(
  dt_plot,
  value_col = "FAN_NH3N",
  ylab_expr_or_txt = expression(FAN~(g~kg~slurry^{-1}))
) + labs(x = "Day")

#-----------------------------
# combine
#-----------------------------
p_all <- (p_VFA / p_Ratio / p_FFA / p_FAN) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

print(p_all)