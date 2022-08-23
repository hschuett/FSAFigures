library(ggplot2)
library(dplyr)
library(viridisLite)
library(rayshader)
library(rgl)
source("R/utils.R")

pe <- function(r, g, RoE){
  (1 / (r - g)) * (1 - (g / RoE))
}

value_grid <- expand.grid(r = seq(0.11, 0.15, 0.001),
                          g = seq(0, 0.08, 0.001),
                          RoE = seq(0.05, 0.3, 0.025)
)


pe_data <- as.data.frame(value_grid)
pe_data$PE = pe(pe_data$r, pe_data$g, pe_data$RoE)
pe_data$RoE <- factor(
  paste("RoE:", scales::percent(pe_data$RoE)),
  levels = c("RoE: 5.0%", "RoE: 7.5%", "RoE: 10.0%", "RoE: 12.5%",
             "RoE: 15.0%", "RoE: 17.5%", "RoE: 20.0%", "RoE: 22.5%",
             "RoE: 25.0%", "RoE: 27.5%",  "RoE: 30.0%")
)

(
pe_plots <-
  pe_data %>%
  filter(
    is.infinite(PE) == FALSE,
    PE > 0
  ) %>%
  ggplot(aes(x = r, y = g, fill = PE)) +
  geom_raster() +
  facet_wrap(~RoE) +
  labs(
    x = expression(r[equity]),
    y = expression(g[RI]),
    fill = expression(P/E)
  ) +
  scale_fill_viridis_c(option = "A") +
  scale_x_continuous(labels = function(x) paste0(round(x*100, 0), "%")) +
  scale_y_continuous(labels=scales::percent) +
  theme(
    axis.line.y = element_line(color = fig_colors[["darkgrey"]]),
    axis.text = element_text(size = 6),
    panel.grid.major.y = element_blank()
    # panel.grid.major.x = element_line(
    #   color = fig_colors[["lightgrey"]],
    #   linetype = "dotted"
    # )
  )
)

ggsave('Figs/L07-pe.pdf', pe_plots, width=7, height=6)
ragg::agg_png("Figs/L07-pe.png", width = 7, height = 6, units = "in", res = 144)
print(pe_plots)
invisible(dev.off())


plot_gg(
  pe_plots,
  multicore = TRUE,
  width = 6,
  height = 6,
  scale = 250,
  windowsize = c(1400, 866),
  zoom = 0.45, theta = -40, phi = 30
)
#
# render_movie(
#   filename = 'pe',
#   type = "orbit",
#   phi = 30,
#   zoom = 0.45,
#   theta = -40
# )¯
