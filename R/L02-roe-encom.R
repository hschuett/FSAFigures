# Setup -------------------------------------------------------------------
library(tidyverse)
library(readxl)
source("R/utils.R")

d <- read_excel("data/encom-roes.xlsx")

colos <- c(fig_colors[["main1"]], fig_colors[["main3"]], fig_colors[["main2"]])

(p1 <-
  d %>%
  mutate(ROE = ROE *100) %>%
  ggplot(aes(x=Period, y=ROE, group=Case, color=Case)) +
  geom_hline(yintercept=16.03, color=fig_colors[["accent2"]]) +
  geom_line() +
  geom_point() +
  annotate("text", x=5, y=15, label="IRR", color=fig_colors[["accent2"]]) +
  scale_color_manual(values=colos) +
  labs(y="Retun on Equity"))


ggsave("Ressources/Figures/L2/encom-cases.pdf", p1, width=5, height=3.5)
