library(tidyverse)
source('R/utils.R')

u <- function(x, alpha=0.5) {
  1 - exp(-1 * x * alpha)
}

d <- tibble(
  x = seq(0, 10, 0.001),
  `u(x)` = u(x)
)

fig <-
  ggplot(d, aes(x=x, y=`u(x)`)) +
  annotate("segment", x=0, y=0, xend=5, yend=u(5), color=fig_colors[["accent2"]]) +
  geom_line() +
  annotate("segment", x=2.5, y=0, xend=2.5, yend=u(2.5), linetype="dashed", color="grey") +
  annotate(
    geom="curve", x=5, y=0.25, xend=2.5, yend=0.5 * u(5),
    curvature=-0.3, arrow=arrow(length=unit(2, "mm"))
  ) +
  annotate(geom="text", x=5.1, y=0.25, label="50% * u(0) + 50% * u(2)", hjust="left") +
  annotate(
    geom="curve", x=2.1, y=0.8, xend=2.5, yend=u(2.5),
    curvature=-0.3, arrow=arrow(length=unit(2, "mm"))
  ) +
  annotate(geom="text", x=2, y=0.8, label="u(1)", hjust="right") +
  annotate(
    geom="curve", x=3, y=0.05, xend=2.5, yend=0,
    curvature=0.3, arrow=arrow(length=unit(2, "mm"))
  ) +
  annotate(geom="text", x=3.1, y=0.05, label="E(A) = E(B) = 1", hjust="left") +
  scale_x_continuous(breaks=seq(0, 10, 2.5), labels=seq(0, 4, 1)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(y="u(x)",
       x="x: $ million payout",
       subtitle="Expected payout of A and B same (E(A) = E(B) = 1), but expected utility different") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y=element_blank(),
        axis.line.y=element_line())

fig

ggsave("Figs/L06-risk-aversion.pdf", fig, width = 6, height = 6 * 0.618, units = "in")
save_png("Figs/L06-risk-aversion.png", fig)
