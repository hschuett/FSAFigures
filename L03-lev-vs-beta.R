# Setup -------------------------------------------------------------------
library(tidyverse)
library(RPostgres)
library(dbplyr)
library(patchwork)
source('R/utils.R')
# dyn.load('/opt/R/arm64/gfortran/lib/libgfortran.5.dylib')
begin_date <- "1990-01-01"
end_date   <- "2021-12-31"



# Wrds data ---------------------------------------------------------------
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
  group_by(gvkey) %>%
  mutate(MinCE = min(ceq, na.rm=T),
         MinSale = min(sale, na.rm=T)) %>%
  ungroup() %>%
  filter(indfmt  == 'INDL' &
           datafmt == 'STD'&
           popsrc  == 'D' &
           consol  == 'C' &
           between(datadate, begin_date, end_date) &
           MinCE > 10 &
           MinSale > 0 &
           is.na(at) == F &
           is.na(sale) == F &
           is.na(ib) == F
  ) %>%
  select(gvkey, datadate, conm, fyear, ib, at, sale, ceq, cogs, oiadp,
         xint, dltt, dlc, mib, pstk, txt, mii, dvp, prcc_f, csho) %>%
  collect()

company <- dbGetQuery(wrds, "SELECT gvkey, sic, fic, loc, gsector FROM comp.company;")

dbDisconnect(wrds)

glimpse(funda)


# Calculations ------------------------------------------------------------
funda_clean1 <-
  funda %>%
  left_join(company, by = "gvkey") |>
  filter(
    fic == "USA",
    loc == "USA"
  ) |>
  mutate(mib = if_else(mib < 0, 0, mib),
         pstk = if_else(pstk < 0, 0, pstk),
         MVE = prcc_f * csho
  ) %>%
  replace_na(list(dltt=0, dlc=0, mib=0,
                  pstk=0, dvp=0, mii=0, xint=0)) %>%
  mutate(Tax = txt / (ib + txt),
         NOI = (ib + txt + xint) * (1 - Tax),
         NFE = xint * (1 - Tax) + mii + dvp,
         NFO = dltt + dlc + mib + pstk,
         NOA = ceq + NFO
  )

funda_clean2 <-
  funda_clean1 %>%
  tsibble::as_tsibble(key=gvkey, index=fyear) %>%
  tsibble::fill_gaps(.full=FALSE) %>%
  as_tibble() %>%
  arrange(gvkey, fyear) %>%
  group_by(gvkey) %>%
  mutate(AvgCE = 0.5 * ceq + 0.5 * lag(ceq),
         AvgNOA = 0.5 * NOA + 0.5 * lag(NOA),
         AvgNFO = 0.5 * NFO + 0.5 * lag(NFO)
  ) %>%
  ungroup() %>%
  mutate(RNOA = NOI / AvgNOA,
         NOIMargin = NOI / sale,
         GMargin = cogs / sale,
         NOATurn = sale / AvgNOA,
         ROE = ib / AvgCE,
         Lev = AvgNFO / AvgCE)

plot_data <-
  funda_clean2 |>
  select(gvkey, datadate, fyear, RNOA, ROE, MVE, NFO, Tax, gsector) |>
  mutate(
    BETASpread = (1 + (1 - Tax) * (NFO / MVE)),
    ROELev = ROE - RNOA
    ) |>
  filter(is.na(BETASpread) == FALSE & is.na(ROELev) == FALSE)

skimr::skim(plot_data)





# Figure ------------------------------------------------------------------
(fig <-
  plot_data |>
  group_by(fyear, gsector) |>
  summarise(
    BETASpread = median(BETASpread),
    ROELev = median(ROELev),
    Nfirms = n(),
    .groups = "drop"
  ) |>
  filter(ROELev > -0.02) |>
  ggplot(aes(y = BETASpread, x = ROELev)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula  ='y ~ x', color = "black") +
  labs(
    x = "RoE - RNOA",
    y = expression(beta[eq] / beta[unlevered]),
    subtitle = "Scatterplot of yearly sector betas and median profitability ratios",
    caption = "Source: Compstat North America data on U.S. listed firms from 1991 to 2021.\nSector classification based on GIC sectors."
  ) +
  theme(axis.line.y = element_line(),
        panel.grid.major.y = element_blank())
)



ggsave("Figs/L03-lev-vs-beta.pdf", fig, width = 6, height = 6, units = "in")
ragg::agg_png("Figs/L03-lev-vs-beta.png", width = 6, height = 6, units = "in", res = 144)
print(fig)
invisible(dev.off())

