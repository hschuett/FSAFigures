# Setup -------------------------------------------------------------------
library(tidyverse)
library(RPostgres)
library(dbplyr)
library(lubridate)
source('R/utils.R')
begin_date <- "1968-01-01"
end_date   <- "2021-12-31"



# Downloading data from ---------------------------------------------------
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  user=rstudioapi::askForPassword(prompt="Enter your WRDS login"),
                  password=rstudioapi::askForPassword(),
                  sslmode='require',
                  dbname='wrds')
wrds  # checking if connection exists

funda <-
  tbl(wrds, in_schema("comp", "funda")) %>%
  filter(indfmt  == 'INDL' &
         datafmt == 'STD'&
         popsrc  == 'D' &
         consol  == 'C' &
         between(datadate, begin_date, end_date)
        ) %>%
  select(gvkey, datadate, conm, fyear, ib, at, sale, ceq, oiadp) %>%
  collect()


sg_development <-
  funda %>%
  arrange(gvkey, datadate) %>%
  group_by(gvkey) %>%
    mutate(MinTA = min(at)) %>%
  ungroup() %>%
  filter(MinTA > 20) %>%
  select(gvkey, datadate, fyear, sale) %>%
  group_by(gvkey) %>%
    mutate( SG_t0 = (sale - lag(sale)) / lag(sale) ) %>%
    mutate(
      SG_t1 = lead(SG_t0, 1),
      SG_t2 = lead(SG_t0, 2),
      SG_t3 = lead(SG_t0, 3),
      SG_t4 = lead(SG_t0, 4),
      SG_t5 = lead(SG_t0, 5)
    ) %>%
  ungroup() %>%
  group_by(fyear) %>%
    mutate(RankSG = ntile(SG_t0, 10)) %>%
  ungroup() %>%
  filter(complete.cases(SG_t0, SG_t1, SG_t2, SG_t3, SG_t4, SG_t5)) %>%
  group_by(RankSG) %>%
    summarise_at(vars(SG_t0, SG_t1, SG_t2, SG_t3, SG_t4, SG_t5), median) %>%
  ungroup()


# Plot data ---------------------------------------------------------------
graph_data <-
  sg_development %>%
  tidyr::pivot_longer(c(SG_t0, SG_t1, SG_t2, SG_t3, SG_t4, SG_t5),
                      names_to=c("Var", "Year"),
                      names_sep="_",
                      values_to="SG")

first_year <-
  graph_data %>%
  filter(Year == "t0") %>%
  mutate(SG = round(SG, 2))

(
plot <-
  graph_data %>%
  ggplot(aes(x=Year, y=SG, group=RankSG)) +
  geom_hline(yintercept=.05, color="#CC9933") +
  labs(y=NULL,
       x=NULL,
       subtitle="Median yearly sales growth by bins formed on sales growth in t0",
       caption="Data: Annual sales growth of US listed companies with total assets greater $20mn, 1968 - 2021") +
  geom_text(data=first_year,
            aes(label=SG),
            nudge_x=-.2, color=tiu_colors["blue"],
            size=3.5) +
  annotate("text", x=6.4, y=.07, label="5%", size=3.5, color=tiu_colors["gold"]) +
  geom_line(color="#003366") +
  scale_x_discrete(expand=expansion(mult=c(0.1, 0.15)))
)

ggsave("Figs/P01-SG-mean-reversion.pdf", plot, width=7, height=4.5)

