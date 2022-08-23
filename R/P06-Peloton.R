
# Setup -------------------------------------------------------------------
library(tidyverse)
library(haven)
library(lubridate)
source("R/utils.R")
library(quantmod)
library(patchwork)
library(ggrepel)



# Peloton stock price -----------------------------------------------------
# PTON_price <- getSymbols('PTON', src='yahoo', auto.assign=FALSE)
#
# priceplot_data <-
#   PTON_price %>%
#   as.data.frame() %>%
#   tibble::rownames_to_column() %>%
#   mutate(Date = as.Date(rowname)) %>%
#   select(-rowname)
#
# write_csv(priceplot_data, "data/peloton-price-data.csv")


priceplot_data <- read_csv("data/peloton-price-data.csv")

label_data <-
  priceplot_data |>
  filter(Date == ymd("2021-01-04") | Date == ymd("2021-12-10")) |>
  mutate(label = paste0(Date, ": ", round(PTON.Adjusted, 1)))

(p1 <-
   priceplot_data %>%
   ggplot(aes(x=Date, y=PTON.Adjusted)) +
   geom_line(color=tiu_colors['blue']) +
   ggrepel::geom_label_repel(
     data = label_data,
     aes(label = label),
     min.segment.length = 0,
     color = tiu_colors["blue"]
   ) +
   labs(
     y=NULL,
     x=NULL,
     subtitle="Peloton Interactive, Inc. Share Price",
     caption="Source: Daily PTON stock price data from Yahoo"
   )
)




# Ibes consensus shift numbers --------------------------------------------
ibes_dump <- read_dta("data/IBES Peloton.dta")
glimpse(ibes_dump)
head(ibes_dump)

ibes_clean <-
  ibes_dump |>
  filter(FPI <= 5) |>
  select(ANALYS, FPI, EPSFCST = VALUE, FPEDATS, ANNDATS_ACT, ANNDATS) |>
  arrange(FPI, FPEDATS, ANNDATS_ACT, ANALYS, ANNDATS) |>
  mutate(
    date_at_high = ymd("2021-01-01"),
    date_now = ymd("2022-02-20")
  )

most_recent_eps_fcsts_high <-
  ibes_clean |>
  filter(
    ANNDATS <= date_at_high,
    ANNDATS > ymd("2020-09-10"),
    FPEDATS > date_at_high
  ) |>
  group_by(FPI, FPEDATS, ANNDATS_ACT, ANALYS) |>
  filter(ANNDATS == last(ANNDATS)) |>
  ungroup()

consensus_eps_fcsts_high <-
  most_recent_eps_fcsts_high |>
  group_by(FPI, FPEDATS) |>
  summarize(
    med_eps = median(EPSFCST),
    num_anaylsts = n(),
    .groups = "drop"
  ) |>
  mutate(Time = "At High")

most_recent_eps_fcsts_now <-
  ibes_clean |>
  filter(
    ANNDATS <= date_now,
    ANNDATS > ymd("2021-08-26"),
    FPEDATS > date_now
  ) |>
  group_by(FPI, FPEDATS, ANNDATS_ACT, ANALYS) |>
  filter(ANNDATS == last(ANNDATS)) |>
  ungroup()

range(most_recent_eps_fcsts_now$ANNDATS)

consensus_eps_fcsts_now <-
  most_recent_eps_fcsts_now |>
  group_by(FPI, FPEDATS) |>
  summarize(med_eps = median(EPSFCST),
            num_anaylsts = n(),
            .groups = "drop"
  ) |>
  mutate(Time = "Now")

plot_data <-
  bind_rows(consensus_eps_fcsts_high, consensus_eps_fcsts_now)
plot_data

(p2 <-
  plot_data |>
  ggplot(aes(x = FPEDATS, y = med_eps, color = Time)) +
  annotate("rect",
     ymin = -4, ymax = 0, xmin = ymd("2021-01-01"), xmax = ymd("2027-01-01"),
     alpha = .2, fill = "red") +
  geom_hline(yintercept = 0, color = "grey") +
  geom_line() +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = med_eps),
                            nudge_x = 10,
                            size = 2.5,
                            na.rm = TRUE) +
  annotate("text", x = ymd("2023-02-28"), y = 3,
           label = "Forecasts (consensus) on Jan 4, 2021",
           color = tiu_colors["blue"],
           size = 3) +
  annotate("text", x = ymd("2025-06-30"), y = -1.2,
           label = "Forecasts (consensus) on Dec 10, 2021",
           color = tiu_colors["lightblue"],
           size = 3) +
  scale_color_manual(values = c(tiu_colors[["blue"]], tiu_colors[["lightblue"]])) +
  scale_x_date(
    breaks = unique(plot_data$FPEDATS),
    limits = c(ymd("2021-01-01"), ymd("2027-01-01")),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(limits = c(-4, 4), expand = expansion(mult = c(0, 0))) +
  labs(
    subtitle = "Shift in Peloton's analyst consensus from January to December 2021",
    color = NULL,
    x = "Fiscal year end and of forecast number",
    y = "Median consensus eps forecast",
    caption = "source: IBES forecast detail file"
  ) +
  theme(legend.position = "none")
)

(p3 <-
  plot_data |>
  ggplot(aes(x = FPEDATS, y = num_anaylsts, group = Time, fill = Time)) +
  geom_col(position = "dodge2") +
  scale_fill_manual(values = c(tiu_colors[["blue"]], tiu_colors[["lightblue"]])) +
  scale_x_date(
    breaks = unique(plot_data$FPEDATS),
    limits = c(ymd("2021-01-01"), ymd("2027-01-01")),
    expand = expansion(mult = c(0, 0))
  ) + labs(
    x = "Fiscal year end and of forecast number",
    y = "N. analysts",
    caption = "source: IBES forecast detail file"
  ) +
  theme(legend.position = "none")
)



layout <- "
AACC
AACC
AACC
BBCC
"
fig1 <- p2 + p3 + p1 +
  plot_layout(design = layout)
fig1

ggsave("Figs/P06-peloton.pdf", fig1, width = 12, height = 6, units = "in")
ggsave("Figs/P06-peloton.jpg", fig1, width = 12, height = 6, units = "in")


# Event Plot --------------------------------------------------------------
priceplot_data


peloton_events <- c(
  "US gyms close" = ymd("2020-03-16"),
  "Q3'2020 EA Call:\nAnalyst asks about\ndeferred revenues" = ymd("2020-05-07"),
  "Warning of\nsupply constraints" = ymd("2020-11-05"),
  "Q3'21 EA:\nRevenue beat,\nbut lower outlook" = ymd("2021-05-06"),
  "Q4'21 EA:\nCut of outlook" = ymd("2021-08-26"),
  "Q1'22 EA:\nCut\nof outlook" = ymd("2021-11-04"),
  "'And Just\nLike That'\nSeason\npremiere" = ymd("2021-12-09"),
  "Blackwell\nPresentation" = ymd("2022-02-04"),
  "New CEO, Layoffs" = ymd("2022-02-08")
)

label_data2 <-
  tibble(
    label = names(peloton_events),
    Date = peloton_events,
    ny = c(20, 30, 20, -30, 50, 30, -10, 0, 30),
    nx = c(-10, -10, -10, -10, -10, 50, -30, -60, 60)
  )

priceplot_data2 <-
  priceplot_data %>%
  left_join(
    label_data2,
    by = "Date"
  )

(fig2 <-
  priceplot_data2 |>
  ggplot(aes(x=Date, y=PTON.Adjusted)) +
  geom_line(color=fig_colors['main1']) +
  scale_x_date(date_breaks = "6 months") +
  geom_text_repel(
    aes(label = label),
    size = 2.5,
    # box.padding = 0.5,
    point.padding = 0.5,
    max.overlaps = Inf,
    nudge_y = priceplot_data2$ny,
    nudge_x = priceplot_data2$nx,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.shape = 1,
    segment.angle = 70,
    max.time = 1,
    arrow = arrow(length = unit(0.015, "npc")),
    color = "black"
  ) +
  labs(
    y="Share price",
    x=NULL,
    subtitle="Peloton Interactive, Inc. event history",
    caption="Source: Daily PTON stock price data from Yahoo Finance"
  )
)

ggsave("Figs/P06-peloton-events.pdf", fig2, width = 6, height = 6 * 0.618, units = "in")
save_png("Figs/P06-peloton-events.png", fig2)

