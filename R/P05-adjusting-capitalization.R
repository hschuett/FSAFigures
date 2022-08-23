
# Setup -------------------------------------------------------------------
library(tidyverse)
library(patchwork)
source('R/utils.R')




# Gen data ----------------------------------------------------------------

gen_investment_life <- function(start_year,
                                n_years = 10,
                                useful_life = 10,
                                investment_amount = 100) {
  tibble(
    t = 1:n_years,
    Year = start_year + t,
    BV = investment_amount - t * (investment_amount / useful_life),
    BV2 = investment_amount - (t * (investment_amount / 5)),
    Depreciation = rep((investment_amount / useful_life), times=n_years),
    Depreciation2 = c(rep((investment_amount / 5), times=5), rep(0, times=5)),
    SYear = start_year
  ) |>
    mutate(
      BV2 = if_else(BV2 < 0, 0, BV2)
    )

}

investments <-
  map_dfr(1990:2010, gen_investment_life)


company_financials <-
  investments %>%
  group_by(Year) %>%
  summarize(
    SumBV = sum(BV),
    SumBV2 = sum(BV2),
    SumDep = sum(Depreciation),
    SumDep2 = sum(Depreciation2)
  ) %>%
  ungroup() %>%
  mutate(CapEx = if_else(Year <= 2010, 100, 0),
         NoBV = 0)




y# Plotting ----------------------------------------------------------------
bv_labels <- seq(0, 500, 100)
is_labels <- seq(0, 100, 25)

p1 <-
  investments %>%
  ggplot(aes(x=Year, y=BV, group=SYear)) +
  annotate("rect", xmin=2000, xmax=2011,
           ymin=0, ymax=100,
           fill=fig_colors['accent1'],
           alpha = .2) +
  geom_line(color = fig_colors["main2"]) +
  labs(x=NULL, y=NULL,
       title="Economic book value of each investment vintage",
       subtitle='Start of each year $100 investment. Useful life of 10 years') +
  scale_y_continuous(breaks=is_labels,
                     labels=is_labels,
                     limits=c(0, 110),
                     expand=expansion(mult=c(0, 0.05)))

p2 <-
  company_financials %>%
  ggplot(aes(x=Year, y=NoBV)) +
  annotate("rect", xmin=2000, xmax=2011,
           ymin=0, ymax=500,
           fill=fig_colors['accent1'],
           alpha = .2) +
  geom_col(fill=fig_colors['main2']) +
  labs(x=NULL, y=NULL,
       title='Scenario A: Direct expensing',
       subtitle="Balance Sheet: Accounting book value") +
  scale_y_continuous(breaks=bv_labels,
                     labels=bv_labels,
                     limits=c(0, 500),
                     expand=expansion(mult=c(0, 0.05)))

p3 <-
  company_financials %>%
  ggplot(aes(x=Year, y=CapEx)) +
  annotate("rect", xmin=2000, xmax=2011,
           ymin=0, ymax=110,
           fill=fig_colors['accent1'],
           alpha = .2) +
  geom_col(fill=fig_colors['main2'])+
  labs(x=NULL, y=NULL, title='', subtitle="Income Statement: Investment expense") +
  scale_y_continuous(breaks=is_labels,
                     labels=is_labels,
                     limits=c(0, 110),
                     expand=expansion(mult=c(0, 0.05)))

p4 <-
  company_financials %>%
  ggplot(aes(x=Year, y=SumBV)) +
  annotate("rect", xmin=2000, xmax=2011,
           ymin=0, ymax=500,
           fill=fig_colors['accent1'],
           alpha = .2) +
  geom_col(fill=fig_colors['main2']) +
  labs(x=NULL, y=NULL,
       title='Scenario B: Capitalizing investments',
       subtitle="Balance Sheet: Accounting book value") +
  scale_y_continuous(breaks=bv_labels,
                     labels=bv_labels,
                     limits=c(0, 500),
                     expand=expansion(mult=c(0, 0.05)))

p5 <-
  company_financials %>%
  ggplot(aes(x=Year, y=SumDep)) +
  annotate("rect", xmin=2000, xmax=2011,
           ymin=0, ymax=110,
           fill=fig_colors['accent1'],
           alpha = .2) +
  geom_col(fill=fig_colors['main2'])+
  labs(x=NULL, y=NULL, title='', subtitle="Income Statement: Depreciation expense") +
  scale_y_continuous(breaks=is_labels,
                     labels=is_labels,
                     limits=c(0, 110),
                     expand=expansion(mult=c(0, 0.05)))

fig1 <- p1 / (p2 + p3) / (p4 + p5)  +
  plot_annotation(
    caption = 'Shaded area: no-growth, steady state region, where investment and wear-and-tear cancel out'
  )
fig1




# Save --------------------------------------------------------------------
ggsave('Figs/P03-cap-vs-expense.pdf', fig1, width=7, height=6)
ragg::agg_png("Figs/P03-cap-vs-expense.png", width = 7, height = 6, units = "in", res = 144)
print(fig1)
invisible(dev.off())
