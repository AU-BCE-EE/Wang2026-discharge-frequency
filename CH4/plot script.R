library(data.table)
library(ggplot2)
library(readxl)

df <- read_excel("2farmCH4.xlsx")
setDT(df)
df[, farm := ifelse(substr(descrip, 1, 1) == "D", "Farm A", "Farm B")]
df[, discharge_group := ifelse(substr(treatment, 2, 2) == "C",
                              "Control",
                              "Weekly discharge")]
df[, discharge_group := factor(discharge_group,
                              levels = c("Control", "Weekly discharge"))]
df[, temp_f := factor(
  temp,
  levels = c(10, 17.5, 25),
  labels = c("10 °C", "17.5 °C", "25 °C")
)]

my_colors <- c(
  "Control" = "#D55E00",
  "Weekly discharge" = "#0072B2"
)

p <- ggplot(df, aes(x = time, y = mean,
                    color = discharge_group,
                    fill  = discharge_group,
                    group = descrip)) +
  geom_ribbon(aes(ymin = pmax(mean - se, 0),
                  ymax = mean + se),
              alpha = 0.2, colour = NA) +
  geom_line(size = 1.2) +
  facet_grid(temp_f ~ farm, switch = "y") +
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  labs(
    x = "Incubation time (days)",
    y = expression("Cumulative CH"[4] * " (g kg VS"^-1*")"),
    color = "discharge treatment",
    fill  = "discharge treatment"
  ) +
  theme_bw(base_size = 14) +
  theme(
    theme(text = element_text(family = "Arial")),
    strip.background = element_rect(fill = "grey90"),
    panel.grid = element_blank()
  )
p
ggsave("CH4_plot.png", p,
       width = 6, height = 6, dpi = 600)
