library(tidyverse) # plotting and data wrangling
library(RPostgres) # drivers for access Postgres SQL servers
library(dbplyr)    # translates dplyr verbs into SQL
library(ggridges)  # additional plotting options
source("R/utils.R")



wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  port = 9737,
  user =keyring::key_get("wrds_user"),
  password = keyring::key_get("wrds_pw"),
  sslmode = "require",
  dbname = "wrds"
)

compustat_raw <-
  tbl(wrds, in_schema("comp", "funda")) |>
  filter(
    indfmt == 'INDL',
    datafmt == 'STD',
    popsrc == 'D',
    consol == 'C',
    fyear > 1970
  ) |>
  mutate(mve = csho * prcc_f) %>%
  select(
    gvkey, datadate, conm, fyear, fyr, sich,
    ib, ceq, sale, at, mve
  ) |>
  collect()

company <- dbGetQuery(wrds, "SELECT gvkey, sic, fic FROM comp.company;")
dbDisconnect(wrds)


us_sample_wo_micro <-
  compustat_raw |>
  left_join(company, by = "gvkey") |>
  filter(
    fic == "USA",
    ceq > 10,
    sale > 5,
    at > 0
  )  |>
  mutate(sic = ifelse(is.na(sich), as.numeric(sic), sich)) |>
  select(-fic, -sich)

roe_sample <-
  us_sample_wo_micro |>
  arrange(gvkey, datadate) |>
  group_by(gvkey) |>
  mutate(roe = ib / ((ceq + lag(ceq))/2)) |>
  filter(
    fyear - 1 == lag(fyear),
    is.na(roe) == FALSE
  ) |>
  ungroup()

max_year <- max(roe_sample$fyear)

fig <-
  roe_sample |>
  filter(roe < .5 & roe > -.5) |>
  ggplot(aes(x = roe, y = fyear, group = fyear)) +
  geom_vline(xintercept = 0, color = fig_colors["accent1"]) +
  geom_density_ridges(scale = 15, size = 0.35,
                      rel_min_height = 0.02,
                      alpha = 0.2,
                      fill = fig_colors["main2"],
                      color = fig_colors["main1"]) +
  geom_vline(xintercept = 0.1, color = fig_colors["accent2"]) +
  scale_y_continuous(breaks = seq(1970, max_year, 5)) +
  scale_x_continuous(breaks = seq(-.5, .5, .1)) +
  labs(
    y = NULL,
    x = "Return on Equity",
    caption = paste0("Data: Annual RoE of US listed companies with common equity",
                     " greater $10mn, 1968 - ", max_year, "\nRange between -50% and 50% RoE")
    )

fig

ggsave("Figs/L01-roe-ridges.pdf", fig, width = 6, height = 6 * 0.618, units = "in")
save_png("Figs/L01-roe-ridges.png", fig)
