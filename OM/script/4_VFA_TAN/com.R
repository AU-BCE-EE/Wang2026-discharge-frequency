#-----------------------------
# colors
#-----------------------------

treat_cols <- c(
  "Control" = "#D55E00",
  "Weekly discharge" = "#0072B2"
)

#-----------------------------
# data prep
#-----------------------------

dt_plot <- copy(dt)

dt_plot[, Treatment := factor(
  Treatment,
  levels = names(treat_cols)
)]

dt_plot[, Farm2 := fifelse(
  Farm %chin% c("A", "C", "D"),
  "FarmA",
  fifelse(Farm == "B", "FarmB", as.character(Farm))
)]

dt_plot[, Farm2 := factor(
  Farm2,
  levels = c("FarmA", "FarmB")
)]

dt_plot[, Farm_label := fifelse(
  Farm == "C",
  "farm C",
  fifelse(Farm == "D", "farm D", NA_character_)
)]

dt_plot[, Day := as.numeric(as.character(Day))]

dt_plot[, Temperature := factor(
  Temperature,
  levels = c("10°C", "17.5°C", "25°C")
)]

#-----------------------------
# theme
#-----------------------------

theme_sci_arial <- theme_bw(
  base_size = 20,
  base_family = "Arial"
) +
  theme(
    plot.title = element_text(
      size = 20,
      hjust = 0.5
    ),
    axis.title = element_text(
      size = 20
    ),
    axis.text = element_text(
      size = 20,
      color = "black"
    ),
    strip.text = element_text(
      size = 20
    ),
    strip.background = element_rect(
      fill = "grey90",
      color = "black",
      linewidth = 0.7
    ),
    legend.title = element_text(
      size = 20
    ),
    legend.text = element_text(
      size = 20
    ),
    legend.position = "top",
    panel.grid.major = element_line(
      color = "grey85",
      linewidth = 0.6
    ),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      color = "black",
      linewidth = 0.7
    ),
    panel.spacing.y = unit(
      0.12,
      "cm"
    )
  )

common_scales <- list(
  scale_color_manual(
    values = treat_cols,
    drop = FALSE,
    name = "Treatment"
  )
)

#-----------------------------
# helper
# hide zero tick label
#-----------------------------

hide_zero <- function(x) {
  ifelse(
    abs(x) < 1e-10,
    "",
    format(
      x,
      trim = TRUE
    )
  )
}

#-----------------------------
# plotting function
#-----------------------------

plot_raw_with_mean_line <- function(
    d,
    value_col,
    ylab_expr_or_txt,
    y_breaks,
    y_view
) {
  
  d0 <- copy(d)[
    is.finite(get(value_col))
  ]
  
  d_ab <- d0[
    Farm %chin% c("A", "B")
  ]
  
  d_cd <- d0[
    Farm %chin% c("C", "D")
  ]
  
  mean_ab <- d_ab[, .(
    mean_value = mean(
      get(value_col),
      na.rm = TRUE
    )
  ), by = .(
    Farm2,
    Temperature,
    Day,
    Treatment
  )]
  
  ggplot() +
    
    # raw observations for farms A and B
    geom_point(
      data = d_ab,
      aes(
        x = Day,
        y = get(value_col),
        color = Treatment
      ),
      size = 4,
      alpha = 0.8,
      position = position_jitter(
        width = 0.10,
        height = 0
      )
    ) +
    
    # mean lines for farms A and B
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
    
    # mean points for farms A and B
    geom_point(
      data = mean_ab,
      aes(
        x = Day,
        y = mean_value,
        color = Treatment
      ),
      size = 4.5
    ) +
    
    # raw observations for farms C and D
    geom_point(
      data = d_cd,
      aes(
        x = Day,
        y = get(value_col),
        color = Treatment,
        shape = Farm_label
      ),
      size = 4.5,
      fill = "white",
      stroke = 1.2,
      position = position_jitter(
        width = 0.10,
        height = 0
      )
    ) +
    
    facet_grid(
      Temperature ~ Farm2,
      scales = "fixed"
    ) +
    
    scale_y_continuous(
      breaks = y_breaks,
      labels = hide_zero,
      expand = c(0, 0)
    ) +
    
    # slightly extend below zero so zero values
    # are not hidden by the panel border
    coord_cartesian(
      ylim = y_view,
      clip = "on"
    ) +
    
    scale_shape_manual(
      values = c(
        "farm C" = 21,
        "farm D" = 22
      ),
      name = ""
    ) +
    
    common_scales +
    
    labs(
      x = NULL,
      y = ylab_expr_or_txt
    ) +
    
    theme_sci_arial
}

#-----------------------------
# A. VFA
# same scale for all panels
#-----------------------------

p_VFA <- plot_raw_with_mean_line(
  dt_plot,
  value_col = "VFAs",
  ylab_expr_or_txt = expression(
    Total~VFAs~(g~kg~slurry^{-1})
  ),
  y_breaks = c(
    0,
    10,
    20
  ),
  y_view = c(
    -1,
    21
  )
)

#-----------------------------
# B. Propionic / Acetic Ratio
# same scale for all panels
#-----------------------------

p_Ratio <- plot_raw_with_mean_line(
  dt_plot,
  value_col = "Prop_Ace_Ratio",
  ylab_expr_or_txt = "Propionic / Acetic Ratio",
  y_breaks = c(
    0,
    0.25,
    0.50
  ),
  y_view = c(
    -0.03,
    0.525
  )
)

#-----------------------------
# C. FFA
# same scale for all panels
#-----------------------------

p_FFA <- plot_raw_with_mean_line(
  dt_plot,
  value_col = "FFA_total",
  ylab_expr_or_txt = expression(
    Total~FFA~(g~kg~slurry^{-1})
  ),
  y_breaks = c(
    0,
    0.10,
    0.20
  ),
  y_view = c(
    -0.015,
    0.21
  )
)

#-----------------------------
# D. FAN
# same scale for all panels
#-----------------------------

p_FAN <- plot_raw_with_mean_line(
  dt_plot,
  value_col = "FAN_NH3N",
  ylab_expr_or_txt = expression(
    FAN~(g~kg~slurry^{-1})
  ),
  y_breaks = waiver(),
  y_view = NULL
) +
  facet_grid(
    Temperature ~ Farm2,
    scales = "free_y"
  ) +
  ggh4x::facetted_pos_scales(
    y = list(
      Temperature == "10°C" ~ scale_y_continuous(
        limits = c(-0.05, 0.35),
        breaks = c(0, 0.15, 0.3),
        labels = hide_zero,
        expand = c(0, 0)
      ),
      Temperature != "10°C" ~ scale_y_continuous(
        limits = c(-0.10, 1.575),
        breaks = c(0, 0.75, 1.50),
        labels = hide_zero,
        expand = c(0, 0)
      )
    )
  ) +
  labs(x = "Day")
#-----------------------------
# combine
#-----------------------------

p_all <- (
  p_VFA /
    p_Ratio /
    p_FFA /
    p_FAN
) +
  plot_layout(
    guides = "collect"
  ) &
  theme(
    legend.position = "top"
  )

print(p_all)