# NOTE: This seems not to work. The underlying economics drastically overlay the expected
# accounting distortions that are coupled with different investment paths. Try again next
# year once you got time to think about this more.

# Setup -------------------------------------------------------------------
library(RPostgres)
library(tidyverse)
library(dbplyr)
source("R/utils.R")
begin_date <- "1980-01-01"
end_date <- "2021-12-31"

truncate_x = function(x, cut = 0.01, sides = 'both'){

  if (sides %in% c('both', 'top')) {
    cut_point_top <- quantile(x, 1 - cut, na.rm = T)
    i = which(x >= cut_point_top)
    x[i] = NA
  }
  if (sides %in% c('both', 'bottom')) {
    cut_point_bottom <- quantile(x, cut, na.rm = T)
    j = which(x <= cut_point_bottom)
    x[j] = NA
  }
  return(x)
}




# Data --------------------------------------------------------------------
wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  port = 9737,
  user = keyring::key_get("wrds_user"),
  password = keyring::key_get("wrds_pw"),
  sslmode = "require",
  dbname = "wrds"
)

funda <-
  tbl(wrds, in_schema("comp", "funda")) %>%
  filter(indfmt == "INDL" &
    datafmt == "STD" &
    popsrc == "D" &
    consol == "C" &
    between(datadate, begin_date, end_date)) %>%
  select(
    gvkey, datadate, conm, fyear, ib, at, sale, ceq, oiadp, xrd,
    xad, lifr, sich, dlc, dltt, pstk, che, ivao, mib
  ) %>%
  distinct()

company_meta <-
  tbl(wrds, in_schema("comp", "company")) %>%
  select(gvkey, sic) %>%
  distinct()

financials <-
  funda %>%
  inner_join(company_meta, by = "gvkey") %>%
  distinct() %>%
  collect()

dbDisconnect(wrds)

sic_code_labels <-
  read_csv(
    "data/wikipedia-sic-codes.csv",
    col_names = c("sic", "IndLabel")
  )



# Data wrangling ----------------------------------------------------------
Sample <-
  financials %>%
  filter(str_sub(sic, 1, 1) != "6") %>%
  distinct() %>%
  replace_na(list(
    xrd = 0,
    xad = 0,
    lifr = 0,
    dlc = 0,
    dltt = 0,
    pstk = 0,
    ivao = 0,
    mib = 0
  )) %>%
  arrange(gvkey, datadate) %>%
  group_by(gvkey) %>%
  mutate(NOA = (ceq) + (dlc + dltt + pstk) - (che + ivao) + mib) %>%
  mutate(
    NOA = if_else(NOA < 0, NA_real_, NOA),
    RDReserve = xrd * 5 / 15 + lag(xrd) * 4 / 15
      + lag(xrd, 2) * 3 / 15
      + lag(xrd, 3) * 2 / 15
      + lag(xrd, 4) * 1 / 15,
    AdvReserve = xad * 2 / 3 + lag(xad) * 1 / 3,
    RNOA = oiadp / (0.5 * NOA + 0.5 * lag(NOA)),
    CReserve = (RDReserve + AdvReserve + lifr) / NOA,
    MinAssets = min(at),
    MinSales = min(sale)
  ) %>%
  ungroup() %>%
  filter(MinAssets > 10 & MinSales > 10) %>%
  select(-MinAssets, -MinSales)

cleaned_sample <-
  Sample %>%
  select(gvkey, datadate, fyear, sic, CReserve, RNOA, at, sale) %>%
  drop_na() %>%
  mutate(
    CReserve = truncate_x(CReserve),
    RNOA = truncate_x(RNOA)
  ) %>%
  drop_na()

plot_data <-
  cleaned_sample %>%
  group_by(gvkey) %>%
  mutate(
    QA = CReserve - lag(CReserve),
    RNOALag1 = lag(RNOA),
    RNOALag2 = lag(RNOA, 2),
    RNOALag3 = lag(RNOA, 3),
    RNOALead1 = lead(RNOA, 1),
    RNOALead2 = lead(RNOA, 2),
    RNOALead3 = lead(RNOA, 3)
  ) %>%
  ungroup() %>%
  mutate(sic2 = str_sub(1, 2)) %>%
  group_by(sic2) %>%
  mutate(QB = CReserve - median(CReserve)) %>%
  ungroup() %>%
  mutate(QScore = 0.5 * QA + 0.5 * QB) %>%
  group_by(fyear) %>%
  mutate(RNOADecile = ntile(RNOA, 10)) %>%
  ungroup() %>%
  group_by(sic2, RNOADecile) %>%
  mutate(QGroup = ntile(QScore, 3)) %>%
  ungroup() %>%
  group_by(QGroup) %>%
  summarize_at(
    .vars = vars(
      RNOALag3, RNOALag2, RNOALag1, RNOA,
      RNOALead1, RNOALead2, RNOALead3
    ),
    .funs = list(Med = ~ median(., na.rm = T), SD = ~ sd(., na.rm = T))
  ) %>%
  ungroup()


plot_data2 <-
  plot_data %>%
  drop_na() %>%
  pivot_longer(
    cols = starts_with("RNOA"),
    names_to = c("name", "Stat"),
    names_sep = "_"
  ) %>%
  mutate(Year = case_when(
    name == "RNOALag3" ~ -3L,
    name == "RNOALag2" ~ -2L,
    name == "RNOALag1" ~ -1L,
    name == "RNOA" ~ 0L,
    name == "RNOALead3" ~ 3L,
    name == "RNOALead2" ~ 2L,
    name == "RNOALead1" ~ 1L
  )) %>%
  pivot_wider(names_from = Stat, values_from = value)



# Plotting ----------------------------------------------------------------
max_year <- max(cleaned_sample$fyear)

(fig <-
  plot_data2 %>%
  filter(QGroup == 3) %>%
  ggplot(aes(x = Year, y = Med, group)) +
  geom_line(color = fig_colors[["main1"]]) +
  geom_point(color = fig_colors[["main1"]]) +
  ylim(0.13, 0.17) +
  scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  labs(
    y = "Median return on net operating assets",
    x = "Years relative to scoring year",
    caption = paste0(
      "Data: Compustat. Non-financial companies with assets and sales all greater than $10mn, 1980 - ", max_year,
      ".\nThe top third of firms according to Penman and Zhang (2002) Q-score in a given 2-digit industry",
      "\nand RNOA decile-year group are selected for this plot"
      )
  )
)
ggsave("Figs/L02-hidden-reserve.pdf", fig, width = 6, height = 6 * 0.618, units = "in")
save_png("Figs/L02-hidden-reserve.png", fig)

#
# (fig2 <-
#     plot_data2 %>%
#     filter(QGroup == 1) %>%
#     ggplot(aes(x = Year, y = Med, group)) +
#     geom_line(color = fig_colors[["main1"]]) +
#     geom_point(color = fig_colors[["main1"]]) +
#     # ylim(0.13, 0.17) +
#     scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
#     labs(
#       y = "Median return on net operating assets",
#       x = "Years relative to scoring year",
#       caption = paste0(
#         "Data: Compustat. Non-financial companies with assets and sales all greater than $10mn, 1980 - ", max_year,
#         ".\nThe top third of firms according to Penman and Zhang (2002) Q-score in a given 2-digit industry",
#         "\nand RNOA decile-year group are selected for this plot"
#       )
#     ))
