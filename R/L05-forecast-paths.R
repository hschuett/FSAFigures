# Setup -------------------------------------------------------------------
library(tidyverse)
source("R/utils.R")



# Data --------------------------------------------------------------------
# gen_fc <- function(start_val, term_val, nperiods, k){
#   i <- 0:nperiods
#   start_val + (term_val - start_val) * (i/(nperiods))^k
#   }
#
# nr_years <- 20
# gen_roefc <- partial(gen_fc, start_val=0.3, term_val=0.1, nperiod=nr_years)
#
# d <- map_dfc(seq(0.5, 2, 0.25), gen_roefc)
# d$Year <- 0:nr_years
#
# d %>%
# pivot_longer(starts_with("V")) %>%
# ggplot(aes(x=Year, y=value, group=name, color=name)) +
#   geom_line()
#
#
#
# # -------------------------------------------------------------------------
# gen_fc <- function(start_val, term_val, inf_val, nperiods, termyear, k1, k2){
#   i <- 0:nperiods
#   infyear <- if_else(inf_val > start_val, 3, floor(termyear / 2))
#   # inf_val <- start_val / 2
#   case_when(
#     i <= infyear ~ start_val + (inf_val - start_val) * (i/(infyear))^k1,
#     i > infyear & i < termyear ~ inf_val + (term_val - inf_val) * ((i - infyear)/(termyear - infyear))^k2,
#     i >= termyear ~ term_val
#   )
# }
#
#
# nr_years <- 30
# gen_roefc <- partial(gen_fc, start_val=0.3, term_val=0.1, nperiod=nr_years)
#
# # combinations <-
# #   expand.grid(k1=c(0.5, 1.5, 2),
# #               k2=c(0.5, 0.8, 1.3),
# #               inf_val=c(0.35, 0.2),
# #               termyear=c(5, 10, 20)) %>%
# #   distinct()
#
# combinations <-
#   tribble(
#     ~inf_val, ~termyear, ~k1, ~k2,
#     0.2,         5,       0.5, 0.5,
#     0.2,         5,       0.8, 0.3,
#     0.1,         5,       0.4, 0.4,
#     0.35,        20,      0.5, 2.5,
#     0.2,         10,      1.8, 0.8,
#     0.25,        10,      1.4, 0.7
#   )
#
#
#
# d <- pmap_dfc(combinations, gen_roefc)
# d$Year <- 0:nr_years
#
# d %>%
#   pivot_longer(starts_with("V")) %>%
#   ggplot(aes(x=Year, y=value, group=name, color=name)) +
#   geom_line()
#
# # if_else(Years < 2025,
# #         0.11 + (0.2 - 0.11) * (i/6)^0.5,
# #         0.2 + (0 - 0.2) * ((i - 6) / 15)^0.5)
#
#
# tf <- function(x, s){ 1 / (1 + 200*exp(x)) }
# x <- seq(-10, 10, 0.01)
# qplot(x, tf(x, 1))

# -------------------------------------------------------------------------
# https://en.wikipedia.org/wiki/Generalised_logistic_function
general_logistic <- function(t, A, K, B, v, Q, C = 1) {
  A + (K - A) / ((C + Q * exp(-B * t))^(1 / v))
}
Years <- seq(-6, 12, 0.01)
gen_roefc <- partial(general_logistic, A = 0, K = 1, C = 1, t = Years)

combinations <-
  expand.grid(
    B = c(0.5, 1.5, 1),
    v = c(0.5, 1, 2),
    Q = c(0.5, 1, 2)
  ) %>%
  distinct()
d <- pmap_dfc(combinations, gen_roefc)
colnames(d) <- paste0("V_", seq_len(nrow(combinations)))
d$Year <- Years

(
  fig <-
    d %>%
    pivot_longer(starts_with("V")) %>%
    mutate(value = value * (-1)) %>%
    ggplot(aes(x = Year, y = value, group = name)) +
    geom_line(alpha = 0.8) +
    labs(
      y = "Sales growth / RNOA / ROE",
      x = "Future mean reversion periods"
    ) +
    scale_x_continuous(breaks = seq(-5, 10, 5), labels = c("t=0", "t+5", "t+10", "t+20")) +
    theme(
      legend.position = "none",
      axis.text.y = element_blank()
    )
)


ggsave("Figs/L05-reversion-paths.pdf", fig, width = 6, height = 6 * 0.618, units = "in")
save_png("Figs/L05-reversion-paths.png", fig)
