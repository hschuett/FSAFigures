# Setup -------------------------------------------------------------------
library(tidyverse)
library(RPostgres)
library(dbplyr)
library(lubridate)
source("R/utils.R")
begin_date <- "1970-01-01"
end_date <- "2021-12-31"



# Downloading data from crsp ----------------------------------------------
wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  port = 9737,
  user = keyring::key_get("wrds_user"),
  password = keyring::key_get("wrds_pw"),
  sslmode = "require",
  dbname = "wrds"
)

frb <-
  tbl(wrds, in_schema("frb", "rates_monthly")) %>%
  select(date, tcmnom_y10) %>%
  filter(is.na(tcmnom_y10) == F) %>%
  collect() %>%
  mutate(YM = paste(year(date), month(date), sep = "-"))

indices <-
  tbl(wrds, in_schema("crsp", "msi")) %>%
  select(date, SPRet = sprtrn) %>%
  filter(between(date, begin_date, end_date)) %>%
  collect() %>%
  mutate(YM = paste(year(date), month(date), sep = "-"))



# Returns -----------------------------------------------------------------
rets <-
  indices %>%
  inner_join(frb, by = c("YM")) %>%
  mutate(
    TYield10 = tcmnom_y10 / 100,
    Year = year(date.x)
  ) %>%
  select(-tcmnom_y10, -date.y)

annual_rets <-
  rets %>%
  group_by(Year) %>%
  summarize(
    SPRet = prod((1 + SPRet)) - 1,
    TYield10 = last(TYield10)
  ) %>%
  ungroup()



# Plots -------------------------------------------------------------------
line_colors <- c(fig_colors["main1"], fig_colors["main2"])
names(line_colors) <- NULL
max_year <- max(annual_rets$Year)

(fig <-
  annual_rets %>%
  pivot_longer(c(TYield10, SPRet),
    names_to = "Series",
    values_to = "Value"
  ) %>%
  mutate(Series = case_when(
    Series == "SPRet" ~ "S&P 500 returns",
    Series == "TYield10" ~ "US 10-year Treasury bills"
  )) %>%
  ggplot(aes(x = Year, y = Value, color = Series)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = line_colors) +
  # theme(axis.text.x=element_text(angle=90)) +
  labs(
    x = NULL,
    y = "Annual Yield",
    color = NULL,
    caption = paste0("Source: Federal reserve board and CRSP, data range based",
                     " on monthly returns from 1970 - ", max_year)
  )
)


ggsave("Ressources/Figures/L1/market-risk.pdf",
  plot,
  width = 7, height = 5
)
ggsave("Figs/L01-market-risk.pdf", fig, width = 6, height = 6 * 0.618, units = "in")
save_png("Figs/L01-market-risk.png", fig)
