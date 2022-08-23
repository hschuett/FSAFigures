library(tidyverse)
source("R/utils.R")



simdata <-
  tibble(
    Time = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3),
    Realized = c(110, 60, 60, 110, 60, 60,  120, 50, 50, 80, 120, 70),
    Market =  c(rep("Market", 6), rep("Firm", 6)),
    Risky = c(rep("More risky", 3), rep("Less risky", 3), rep("More risky", 3), rep("Less risky", 3))
  )

expected <- tibble(
  Time = c(1, 2, 3),
  Expected = c(100, 100, 100)
)


(
fig <-
  simdata |>
  ggplot(aes(x = Time, y = Realized, color = Market)) +
  facet_wrap(~Risky) +
  geom_line(data = expected, aes(x = Time, y = Expected), color = fig_colors["accent1"]) +
  geom_point(data = expected, aes(x = Time, y = Expected), color = fig_colors["accent1"]) +
  annotate("text",
     x = 3, y = 100, label = "expected\ncash-flows",
     hjust = -0.2, color = fig_colors["accent1"],
     size = 2.5) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, 130)) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("t+1", "t+2", "t+3"), limits = c(1, 3.5)) +
  scale_color_manual(values = c(fig_colors[["main1"]], fig_colors[["accent2"]])) +
  geom_text(data = filter(simdata, Time == 3), aes(label = Market), hjust = -0.2) +
  labs(
    x = NULL,
    y = "Realized cash-flows"
  ) +
  theme(legend.position = "none")
)

ggsave("Figs/L01-risky-variance.pdf", fig, width = 6, height = 6 * 0.618, units = "in")
save_png("Figs/L01-risky-variance.png", fig)

