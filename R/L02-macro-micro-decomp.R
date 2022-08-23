library(tidyverse)
library(RPostgres)
library(dbplyr)
library(lme4)
source("R/utils.R")
begin_date <- "2018-01-01"
end_date <- "2018-12-31"



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

returns <-
  tbl(wrds, in_schema("crsp", "dsf")) %>%
  group_by(permno) %>%
  mutate(
    MVE = prc * shrout,
    MinPrc = min(prc, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(
    between(date, begin_date, end_date),
    MinPrc > 1
  ) %>%
  select(permno, date, ret, MVE)

indices <-
  tbl(wrds, in_schema("crsp", "dsi")) %>%
  select(date, MktRet = vwretd) %>%
  filter(between(date, begin_date, end_date))

compinfo <-
  tbl(wrds, in_schema("crsp", "dsenames")) %>%
  filter(
    shrcd %in% c(10, 11),
    exchcd %in% c(1, 2, 3),
    primexch %in% c("N", "A", "Q")
  ) %>%
  select(permno, namedt, nameendt, siccd)

crsp <-
  returns %>%
  inner_join(indices, by = "date") %>%
  inner_join(compinfo, by = "permno") %>%
  filter(between(date, namedt, nameendt)) %>%
  collect()

dbDisconnect(wrds)



crsp_cleaned <-
  crsp %>%
  mutate(
    ret = forecast::na.interp(ret),
    MVE = forecast::na.interp(MVE),
    Industry = trunc(siccd / 10)
  ) %>%
  group_by(permno) %>%
  mutate(MinMVE = min(MVE)) %>%
  ungroup() %>%
  filter(MinMVE > 10000) %>%
  add_count(Industry, date, name = "IndCount") %>%
  group_by(Industry) %>%
  mutate(MinIndCount = min(IndCount)) %>%
  ungroup() %>%
  filter(MinIndCount > 5)



# Computing industry returns ----------------------------------------------
industry_returns <-
  crsp_cleaned %>%
  mutate(WtdRet = ret * MVE) %>%
  group_by(Industry, date) %>%
  summarize(IndRet = median(ret - MktRet)) %>%
  ungroup()

reg_data <-
  crsp_cleaned %>%
  inner_join(industry_returns, by = c("Industry", "date")) %>%
  mutate(
    MktRet = scale(MktRet)[, 1],
    IndRet = scale(IndRet)[, 1],
    ret = scale(ret)[, 1],
  ) %>%
  select(permno, date, ret, MktRet, IndRet, MVE)

summary(reg_data)



# Compute decomposition ---------------------------------------------------
fit <- lmer(ret ~ 0 + MktRet + IndRet + (0 + MktRet + IndRet | permno), REML = F, data = reg_data)
summary(fit)

rand_coeffs <- coef(fit)[[1]]
rand_coeffs$permno <- as.integer(rownames(rand_coeffs))

reg_results <-
  reg_data %>%
  inner_join(rand_coeffs, by = "permno") %>%
  mutate(FirmRet = residuals(fit))



# Plot it -----------------------------------------------------------------
(
  fig <-
    rand_coeffs %>%
    pivot_longer(c(MktRet, IndRet),
      names_to = "Part",
      values_to = "Exposure"
    ) %>%
    mutate(Part = case_when(
      Part == "MktRet" ~ "Market Exposure (% of return)",
      Part == "IndRet" ~ "Industry Exposure (% of return)",
    )) %>%
    ggplot(aes(x = Exposure)) +
    facet_wrap(~Part) +
    geom_histogram(
      binwidth = 0.05,
      fill = fig_colors[["main1"]],
      color = "white"
    ) +
    scale_y_continuous(expand = expansion(add = c(1, 10))) +
    labs(
      x = NULL,
      y = "Nr. Firms",
      caption = "Based on firm-specfici coefficients of standardized daily returns on standardized daily market and industry returns.\n Date range: 2018-01-01 until 2018-12-31. Data: CRSP daily US stocks file. 1168 stocks."
    )
)
ggsave("Figs/L02-stock-decomposition.pdf", fig, width = 6, height = 6 * 0.618, units = "in")
save_png("Figs/L02-stock-decomposition.png", fig)
