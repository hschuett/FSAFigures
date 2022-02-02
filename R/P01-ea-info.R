# Setup -------------------------------------------------------------------
library(RPostgres)
library(tidyverse)
library(dbplyr)
library(patchwork)
source("R/utils.R")
begin_date <- "1990-01-01"
end_date   <- "2022-12-31"


# Connecting to WRDS
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  user=rstudioapi::askForPassword(prompt="Enter your WRDS login"),
                  password=rstudioapi::askForSecret("WRDS pw"),
                  sslmode='require',
                  dbname='wrds')
wrds  # checking if connection exists
# dbDisconnect(wrds)



# Create ibes crsp linking table ------------------------------------------
ccm_link <-
  tbl(wrds, in_schema("crsp", "ccmxpf_linktable")) %>%
  filter(usedflag %in% c(0, 1),
         linktype %in% c('LU', 'LC'),
         linkprim %in% c("P", "C")) %>%
  mutate(linkenddt = if_else(is.na(linkenddt) == T,
                             date("2030-06-30"), linkenddt)) %>%
  select(gvkey, lpermno, linkdt, linkenddt) %>%
  collect()

chosen_gvkey <- unique(ccm_link$gvkey)
chosen_permno <- unique(ccm_link$lpermno)



# Getting rdq from compustat quarterly ------------------------------------
comp_dates <-
  tbl(wrds, in_schema("comp", "fundq")) %>%
  filter(indfmt  == 'INDL' &
           datafmt == 'STD'&
           popsrc  == 'D' &
           consol  == 'C' &
           between(datadate, begin_date, end_date) &
           gvkey %in% chosen_gvkey &
           is.na(rdq) == F
           ) %>%
  select(gvkey, rdq, datadate) %>%
  distinct() %>%
  mutate(DaysPast = rdq - datadate) %>%
  filter(DaysPast > 0 & DaysPast < 80) %>%
  select(-DaysPast) %>%
  collect()



# Collect return data -----------------------------------------------------
delisting_returns <-
  tbl(wrds, in_schema("crsp", "dsedelist")) %>%
  select(permno, dlstdt, dlret) %>%
  mutate(dlret = if_else(is.na(dlret), -1, dlret))

returns <-
  tbl(wrds, in_schema("crsp", "dsf")) %>%
  filter(permno %in% chosen_permno) %>%
  left_join(delisting_returns, by=c("permno", "date"="dlstdt")) %>%
  group_by(permno) %>%
  mutate(Price = abs(prc),
         MinPrc = min(Price, na.rm=T),
         Turn = vol / shrout,
         ret = if_else(is.na(ret) == T & is.na(dlret) == F, dlret, ret)
  ) %>%
  ungroup() %>%
  filter(between(date, begin_date, end_date),
         MinPrc > 1) %>%
  select(permno, date, ret, Turn)

indices <-
  tbl(wrds, in_schema("crsp", "dsi")) %>%
  select(date, MktRet=vwretd) %>%
  filter(between(date, begin_date, end_date))

compinfo <-
  tbl(wrds, in_schema("crsp", "dsenames")) %>%
  filter(shrcd %in% c(10, 11),
         exchcd %in% c(1, 2, 3),
         primexch %in% c("N", "A", "Q")
  ) %>%
  select(permno, namedt, nameendt)

crsp <-
  returns %>%
  inner_join(indices, by="date") %>%
  inner_join(compinfo, by="permno") %>%
  filter(date >= namedt & date <= nameendt) %>%
  mutate(ARet = ret - MktRet) %>%
  select(permno, date, ARet, Turn)|>
  collect()

dbDisconnect(wrds)



# Final merge -------------------------------------------------------------

full_data0 <-
    crsp %>%
    inner_join(ccm_link, by=c("permno"="lpermno")) %>%
    filter(date >= linkdt & date <= linkenddt) %>%
    filter(is.na(gvkey) == F)

data_ranges <-
  comp_dates |>
  select(gvkey, rdq) |>
  mutate(start = rdq - 45L,
         end = rdq + 45L) |>
  as_tibble() |>
  rowwise() |>
  transmute(gvkey, rdq, date = list(seq(start, end, by="day"))) |>
  unnest(date)

full_data <-
  full_data0 %>%
  inner_join(data_ranges, by = c("gvkey", "date")) %>%
  filter(date >= linkdt & date <= linkenddt) |>
  select(-linkdt, -linkenddt, -gvkey) %>%
  mutate(EventTime = date - rdq) %>%
  filter(EventTime >= -45L & EventTime <= +45L)



# Creating plot data ------------------------------------------------------
plot_data <-
  full_data %>%
  group_by(EventTime) %>%
    summarize(MedAbsRet = median(abs(ARet), na.rm=T),
              MedTurn = median(Turn, na.rm=T)) %>%
  ungroup() %>%
  mutate(EventTime = as.integer(EventTime))



# Plot --------------------------------------------------------------------
(p1 <-
    plot_data %>%
    ggplot(aes(x=EventTime, y=MedAbsRet)) +
    geom_line(color=tiu_colors["blue"]) +
    geom_point(color=tiu_colors["blue"]) +
    scale_x_continuous(breaks=seq(-45, 45, by=5)) +
    labs(x="Days relative to EA",
         y="Median absolute abnormal daily return"
    )
)
(p2 <-
    plot_data %>%
    ggplot(aes(x=EventTime, y=MedTurn)) +
    geom_line(color=tiu_colors["blue"]) +
    geom_point(color=tiu_colors["blue"]) +
    scale_x_continuous(breaks=seq(-45, 45, by=5)) +
    labs(x="Days relative to EA",
         y="Median share turnover ",
    )
)

fig <- p1 / p2 +
  plot_annotation(
    caption = 'Data: Compustat quarterly file, Crsp daily return file. Date range: 1990/01/01 - 2021/12/31.\nTurnover is daily volume scaled by shares outstanding. Figure code: https://github.com/hschuett/FSAFigures'
  )
fig

ggsave("Figs/P01-EA-importance.pdf", fig, width=6, height=7.5)
