library(tidyverse)
library(lubridate)
library(patchwork)
source("R/utils.R")

start_date <- ymd("2020-12-31")
all_dates <- as_date(map_dbl(seq(0, 12, by = 2), \(x) start_date %m+% months(x)))

dta <- tibble(
  period = all_dates,
  plot1 = c(100, NA, NA, NA, NA, NA, 200),
  plot2 = c(100, 100, 100, 100, 100, 100, 200),
  plot3 = seq(100, 200, length.out = 7),
  color = as.factor(c(1, 2, 2, 2, 2, 2, 1))
)


# -------------------------------------------------------------------------


bvplot <- function(vari) {
  dta |>
    ggplot(aes(x = period, y = {{vari}}, fill = color)) +
    geom_col() +
    # annotate("rect", xmin = all_dates[1], xmax = all_dates[7], ymin = 220, ymax = 240,
             # fill = fig_colors[["accent2"]]) +
    # annotate("text", x = all_dates[4], y = 230, label = "Net income in 2021") +
    scale_x_date(breaks = all_dates,
                 labels = c(as.character(all_dates[1]), "", "", "", "", "", as.character(all_dates[7]))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       limits = c(0, 240)) +
    scale_fill_manual(values = c(fig_colors[["main1"]], "grey80")) +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(x = NULL, y = "Book value of equity")
}


p1 <- bvplot(plot1)
p2 <- bvplot(plot2)
p3 <- bvplot(plot3)

base_plot <-
  dta |>
  ggplot() +
  xlim(0, 1) +
  ylim(0, 1) +
  annotate("segment", y = 0.7, yend = 0.4, x = 0.25, xend = 0.125,
           arrow = arrow(type = "closed", length = unit(0.2, "inches")),
           lineend = "round", # See available arrow types in example above
           linejoin = "round",
           size = 1) +
  annotate("segment", y = 0.7, yend = 0.4, x = 0.75, xend = 0.925,
           arrow = arrow(type = "closed", length = unit(0.2, "inches")),
           lineend = "round", # See available arrow types in example above
           linejoin = "round",
           size = 1) +
  theme_void()

(
fig <-
  base_plot +
  inset_element(p1, left = 0.3, bottom = 0.6, right = 0.7, top = 1, align_to = 'full') +
  inset_element(p2, left = 0, bottom = 0, right = 0.4, top = 0.4, align_to = 'full') +
  inset_element(p3, left = 0.6, bottom = 0, right = 1, top = 0.4, align_to = 'full')
)


# -------------------------------------------------------------------------
ggsave('Figs/L03-averages.pdf', fig, width=7, height=6)
ragg::agg_png("Figs/L03-averages.png", width = 7, height = 6, units = "in", res = 144)
print(fig)
invisible(dev.off())

