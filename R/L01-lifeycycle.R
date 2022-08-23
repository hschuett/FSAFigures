library(tidyverse)
source("R/utils.R")

dta <-
  tibble(
    x = seq(-2, 2, .002),
    y = case_when(
      x < -1.56 ~ -1,
      x >= -1.56 & x <= 1.56  ~ sin(x),
      x > -1.56 ~ 1
    )
  )

(
  fig <-
    dta %>%
    ggplot(aes(x = x, y = y)) +
    geom_line() +
    scale_x_continuous(breaks = seq(-2, 2, 0.1)) +
    labs(x = "Years", y = "Revenues") +
    theme(axis.text = element_blank())
)

ggsave("Figs/L01-lifecycle.pdf", fig, width = 6, height = 6 * 0.618, units = "in")
save_png("Figs/L01-lifecycle.png", fig)
