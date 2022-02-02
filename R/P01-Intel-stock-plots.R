# Setup -------------------------------------------------------------------
library(tidyverse)
library(RPostgres)
library(dbplyr)
library(fst)
library(lubridate)
source('R/utils.R')
library(patchwork)
begin_date <- "1990-01-01"
end_date   <- "2021-12-31"



# Downloading data from crsp ----------------------------------------------
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  user=rstudioapi::askForPassword(prompt="Enter your WRDS login"),
                  password=rstudioapi::askForPassword(),
                  sslmode='require',
                  dbname='wrds')
wrds  # checking if connection exists

intel_prc <-
  tbl(wrds, in_schema("crsp", "dsf")) %>%
  filter(between(date, begin_date, end_date) &
         permno == "59328") %>%
  select(date, prc, vol, shrout) %>%
  collect()

intel_data <-
  intel_prc %>%
  mutate(Mval = prc * shrout / 1000000)


intel_data %>%
  filter(date < "2000-09-23",
         date > "2000-09-16") %>%
  as.data.frame()

intel_funda <-
  tbl(wrds, in_schema("comp", "funda")) %>%
  filter(indfmt  == 'INDL' &
         datafmt == 'STD'&
         popsrc  == 'D' &
         consol  == 'C' &
         gvkey == "006008" &
         between(datadate, begin_date, end_date)
         ) %>%
  select(gvkey, datadate, conm, fyear, ib, sale, cogs) %>%
  mutate(Sales.Growth = (sale - lag(sale)) / lag(sale),
         Gross.Margin = (sale - cogs) / sale
         ) %>%
  collect() |>
  arrange(gvkey, datadate)

intel_funda_cleaned <-
  intel_funda %>%
  mutate(Net.Margin = ib / sale) %>%
  filter(fyear >= 1999)



# Plots -------------------------------------------------------------------
(plot1 <-
    intel_data %>%
    filter(date < "2000-09-23",
           date > "1995-01-01") %>%
    ggplot(aes(x=date, y=Mval)) +
    geom_line(color=tiu_colors['blue']) +
    geom_vline(xintercept = as.numeric(as_date("2000-09-21")),
               color=tiu_colors['gold'],
               alpha=.5) +
   annotate("text", x=as_date("2000-09-21"), y=100,
            label="The Warning",
            color=tiu_colors["gold"],
            size=3,
            hjust=1.1) +
    labs(
      y=NULL,
      x=NULL,
      subtitle="Intel Corp. market value 1995/01/01 till 2000/09/23 (in $Bn)",
      caption="Source: daily stock data from CRSP. Figure code: https://github.com/hschuett/FSAFigures"
    )
)

(plot2 <-
    intel_data %>%
    ggplot(aes(x=date, y=Mval)) +
    geom_line(color=tiu_colors['blue']) +
    geom_vline(xintercept = as.numeric(as_date("2000-09-21")),
               color=tiu_colors['gold'],
               alpha=.5) +
    annotate("text", x=as_date("2000-09-21"), y=75,
             label="The Warning",
             color=tiu_colors["gold"],
             size=3,
             hjust=1.05) +
    labs(
      y=NULL,
      x=NULL,
      subtitle="Intel Corp. market value (in $Bn)",
      caption="Source: daily stock data from CRSP. Figure code: https://github.com/hschuett/FSAFigures"
    )
)

ggsave("Figs/P01-intel-1.pdf",
       plot1,
       width=7, height=4)
ggsave("Figs/P01-intel-2.pdf",
       plot2,
       width=7, height=4)


(sg_plot <-
    intel_funda_cleaned %>%
    ggplot(aes(x=fyear, y=Sales.Growth)) +
    geom_hline(yintercept=0) +
    geom_line(color=tiu_colors['blue']) +
    geom_point(color=tiu_colors['blue']) +
    labs(
      x=NULL,
      y=NULL,
      subtitle="Yearly sales growth of Intel Corp."
    )
)
(gm_plot <-
    intel_funda_cleaned %>%
    ggplot(aes(x=fyear, y=Gross.Margin)) +
    geom_line(color=tiu_colors['blue']) +
    geom_point(color=tiu_colors['blue']) +
    labs(
      x=NULL,
      y=NULL,
      subtitle="Gross margin of Intel Corp."
    )
)


fund_plot <- sg_plot / gm_plot +
  plot_annotation(
    caption = 'Data: Compustat annual file, 1990/01/01 - 2021/12/31. Figure code: https://github.com/hschuett/FSAFigures'
  )
fund_plot
ggsave("Figs/P01-intel-3.pdf",
       fund_plot,
       width=7, height=4.5)
