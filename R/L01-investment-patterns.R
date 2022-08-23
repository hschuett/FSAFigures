library(RPostgres)
library(tidyverse)
library(dbplyr)
library(readxl)
source("R/utils.R")
# source("")
begin_date <- "1968-01-01"
end_date <- "2021-12-31"



gics_old <-
  read_excel(
    "data/gics-codes-effective-1999-till-2002-03-21.xls",
    skip = 2
  ) %>%
  select(GGroupValue, GroupOLD = GGroupLabel) %>%
  drop_na()

gics_new <-
  read_excel("data/gics-map-2018.xlsx", skip = 4) %>%
  select(GGroupValue, GroupNEW = GGroupLabel) %>%
  drop_na()

gics <-
  gics_old %>%
  full_join(gics_new, by = "GGroupValue") %>%
  mutate(GGroup = if_else(is.na(GroupOLD), GroupNEW, GroupOLD)) %>%
  select(GGroupValue, GGroup)


wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  port = 9737,
  user = keyring::key_get("wrds_user"),
  password = keyring::key_get("wrds_pw"),
  sslmode = "require",
  dbname = "wrds"
)

countries <- c(
  "AUT", "BEL", "CAN", "DNK", "FIN", "FRA", "DEU", "ITA",
  "NLD", "NOR", "PRT", "ESP", "SWE", "CHE", "GBR", "IRL"
)

funda <-
  tbl(wrds, in_schema("comp_global_daily", "g_funda")) %>%
  filter(
    indfmt %in% c("INDL") &
      datafmt == "HIST_STD" &
      popsrc == "I" &
      consol == "C" &
      loc %in% !!countries &
      between(datadate, begin_date, end_date)
  ) %>%
  select(gvkey, datadate, conm, fyear, loc, ib, at, sale, ceq, oiadp, xrd, capx, dp) %>%
  distinct()

company_meta <-
  tbl(wrds, in_schema("comp_global_daily", "g_company")) %>%
  select(gvkey, gsector, ggroup) %>%
  filter(is.na(gsector) == F | is.na(ggroup) == F) %>%
  distinct()

financials_eu <-
  funda %>%
  inner_join(company_meta, by = "gvkey") %>%
  collect() %>%
  mutate(Region = "Europe")

funda <-
  tbl(wrds, in_schema("comp", "funda")) %>%
  filter(indfmt == "INDL" &
    datafmt == "STD" &
    popsrc == "D" &
    consol == "C" &
    between(datadate, begin_date, end_date)) %>%
  select(gvkey, datadate, conm, fyear, ib, at, sale, ceq, oiadp, xrd, capx, dp) %>%
  distinct()

company_meta <-
  tbl(wrds, in_schema("comp", "company")) %>%
  select(gvkey, gsector, ggroup) %>%
  filter(is.na(gsector) == F | is.na(ggroup) == F) %>%
  distinct()

financials_us <-
  funda %>%
  inner_join(company_meta, by = "gvkey") %>%
  collect() %>%
  mutate(Region = "US")

investment <-
  bind_rows(financials_eu, financials_us) %>%
  filter(ceq > 50, at > 50, sale > 50) %>%
  replace_na(list(xrd = 0, capx = 0)) %>%
  mutate(InvExp = (xrd + capx - dp) / sale) %>%
  drop_na(InvExp, ggroup, Region)

industries <- gics$GGroup
names(industries) <- gics$GGroupValue

investment$Industry <-
  investment$ggroup %>%
  as.character() %>%
  str_replace_all(industries)

investment_by_industry <-
  investment %>%
  filter(is.finite(InvExp)) %>%
  group_by(Region, Industry, fyear) %>%
  summarize(Investment = mean(InvExp)) %>%
  ungroup() %>%
  rename(Year = fyear)

investment_by_industry %>% head()

colos <- c(fig_colors["main1"], fig_colors["main2"])
names(colos) <- NULL

max_year <- max(investment_by_industry$Year)
max_year

inv_plot <-
  investment_by_industry %>%
  filter(
    Industry %in% c(
      "Media", "Technology Hardware & Equipment",
      "Health Care Equipment & Services",
      "Commercial Services & Supplies",
      "Capital Goods",
      "Automobiles & Components"
    ),
    Year >= 1995
  ) %>%
  ggplot(aes(x = Year, y = Investment, color = Region, group = Region)) +
  facet_wrap(~Industry, scales = "free_x") +
  geom_hline(yintercept = 0, color = fig_colors["accent1"]) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = colos) +
  labs(
    y = "Average Excess Invesment",
    x = NULL,
    caption = paste0("Data: Compustat. Listed companies with equity, assets and",
                     " sales all greater than $50mn, 1995 - ", max_year,
                     "\nExcess Investment: (R&D + Capex - Depreciation &",
                     " Amortization) / Revenues")
  )
inv_plot

ggsave("Figs/L01-investments.pdf", inv_plot, width = 7.5, height =  7.5 * 0.618)
save_png("Figs/L01-investments.png", inv_plot, fig_width = 7.5)
