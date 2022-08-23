library(rjqpd)
library(ggplot2)
library(patchwork)
library(matrixStats)
set.seed(0234)
col_lgold  = "#C3BCB2"
source("R/utils.R")

gen_trend_series <- function(start, end, nr_years, m = 0.5, max_years = 10) {
  i <- seq_len(nr_years) - 1
  # this is just to make sure that we always consider the same amount of years
  full_series <- rep.int(end, max_years)
  # m controls how fast saturation happens (see plot)
  trend <- start + (end - start) * ( i / (nr_years - 1) )^m
  full_series[1:nr_years] <- trend
  return(full_series)
}

data.frame(
  x = c(seq_len(10), seq_len(10)),
  y = c(gen_trend_series(0, 1, 9,  m = 0.5), gen_trend_series(0, 1, 9,  m = 1.2)),
  lab = c(rep.int("m = 0.5", 10), rep.int("m = 1.2", 10))
) |>
  ggplot(aes(x = x, y = y, group = lab, color = lab)) +
  geom_point() +
  labs(y = "Market saturation", x = "Year", color = "Rate")

n_sims <- 1000

params_HHmax <- jqpd(
  c(90, 100, 120), # the three benchmark points to our quantiles
  lower = 0, upper = 200,  # worst case (lower): zero market, (upper): all HH in the area (200)
  alpha = 0.05  # bounds are 5%, 50%, 95% percentiles
)
samples_HHmax <- rjqpd(n = n_sims, params_HHmax)

p1 <-
  ggplot(data.frame(x = samples_HHmax), aes(x = x)) +
  geom_histogram(binwidth = 5, color = "white", fill = fig_colors[["main1"]]) +
  labs(y = "Count of draws",
       x = expression(HH[max]),
       subtitle = "J-QPD(5% = 90, 50% = 100, 95% = 120)"
  ) +
  theme(plot.subtitle = element_text(size = 7), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(0, 200, 10))

params_share <- jqpd(
  c(0.2, 0.3, 0.4),
  lower = 0, upper = 1,
  alpha = 0.05  # bounds are 5%, 50%, 95%
)
samples_share <- rjqpd(n = n_sims, params_share)

p2 <-
  ggplot(data.frame(x = samples_share), aes(x = x)) +
  geom_histogram(binwidth = 0.05, color = "white", fill = fig_colors[["main1"]]) +
  labs(y = "Count of draws",
       x = expression(share),
       subtitle = "J-QPD(5% = 0.2, 50% = 0.3, 95% = 0.4)"
  ) +
  theme(plot.subtitle = element_text(size = 7), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  coord_cartesian(xlim = c(0, 1))

params_m <- jqpd(
  c(0.6, 1.0, 1.4),
  lower = 0.1, upper = 2,
  alpha = 0.05  # bounds are 5%, 50%, 95%
)
samples_m <- rjqpd(n = n_sims, params_m)

p3 <-
  ggplot(data.frame(x = samples_m), aes(x = x)) +
  geom_histogram(binwidth = 0.1, color = "white", fill = fig_colors[["main1"]]) +
  labs(y = "Count of draws",
       x = expression(m),
       subtitle = "J-QPD(5% = 0.2, 50% = 0.3, 95% = 0.4)"
  ) +
  theme(plot.subtitle = element_text(size = 7), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(0, 2, 0.2)) +
  coord_cartesian(xlim = c(0, 2))

N <- c("5" = 0.1, "6" = 0.2, "7" = 0.4, "8" = 0.2, "9" = 0.1)
N

samples_N <- sample(5:9, size = n_sims, replace = TRUE, prob = N)

p4 <-
  ggplot(data.frame(x = samples_N), aes(x = x)) +
  geom_histogram(bins = 5, color = "white", fill = fig_colors[["main1"]]) +
  labs(y = "Count of draws",
       x = expression(N),
       subtitle = "Discrete sampling"
  ) +
  theme(plot.subtitle = element_text(size = 7), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  coord_cartesian(xlim = c(1, 10))

sat_grade <-
  mapply(function(x, y) gen_trend_series(0, 1, nr_years = x, m = y),
         x = samples_N,
         y = samples_m)

sat_grade[, 1:3]

vol_series <- matrix(NA, nrow = 10, ncol = n_sims)
for (i in seq_len(n_sims)) {
  vol_series[, i] <- samples_HHmax[[i]] * sat_grade[, i] * samples_share[[i]]
}
vol_series[, 1:3]

plot_data <-
  as.data.frame(vol_series) |>
  stack()
plot_data$Year <- rep(1:10, times = n_sims)

ci_data <- data.frame(
  Year = 1:10,
  Median = rowMedians(vol_series),
  Q05 = rowQuantiles(vol_series, prob = 0.05),
  Q25 = rowQuantiles(vol_series, prob = 0.25),
  Q75 = rowQuantiles(vol_series, prob = 0.75),
  Q95 = rowQuantiles(vol_series, prob = 0.95)
)

p5 <-
  ggplot(plot_data, aes(x = Year, y = values, group = ind)) +
  geom_line(alpha = 0.10) +
  lab(subtitle = paste(n_sims, "indidual volume trends")) +
  scale_x_continuous(breaks = seq(1, 10, 1)) &
  labs(x = "Year", y = "Sales volume") &
  coord_cartesian(ylim = c(0, 70)) &
  theme(panel.grid.minor = element_blank())

p6 <-
  ggplot(ci_data, aes(x = Year)) +
  geom_ribbon(aes(ymin = Q05, ymax = Q95), alpha = 0.5, fill = col_lgold) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.5, fill = col_lgold) +
  geom_line(aes(y = Median), color = col_lgold)  +
  labs(subtitle = "Summary via credible intervals") +
  annotate("text",
           x = c(5, 3, 8), y = c(60, 50, 10),
           label = c("5%-95% probability", "25%-75% probability", "Median"),
           color = "gray20", size = 3) +
  annotate("segment",
           x = c(5, 3, 8), y = c(60-1, 50-1, 10+1),
           xend = c(8, 5, 7), yend = c(ci_data[8, "Q95"], ci_data[5, "Q75"], ci_data[7, "Median"]),
           color = "gray40") +
  scale_x_continuous(breaks = seq(1, 10, 1)) &
  labs(x = "Year", y = "Sales volume") &
  coord_cartesian(ylim = c(0, 70)) &
  theme(panel.grid.minor = element_blank())


fig1 <- (p1 | p2) / (p3 + p4) / (p5 | p6)
fig1
  # plot_annotation(
  #   tag_levels = 'A',
  #   title = 'Quantifying beliefs via simulation',
  #   subtitle = paste(n_sims, 'random draws (universes)')
  # ) &
  # scale_x_continuous(breaks = seq(1, 10, 1)) &
  # labs(x = "Year", y = "Sales volume") &
  # coord_cartesian(ylim = c(0, 70)) &
  # theme(panel.grid.minor = element_blank())

fig1
ggsave('Figs/L08-scenario.pdf', fig1, width=6, height=7)
ragg::agg_png("Figs/L08-sceanrio.png", width = 6, height = 7, units = "in", res = 144)
print(fig1)
invisible(dev.off())

