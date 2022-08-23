# Setup -------------------------------------------------------------------
library(tidyverse)
library(RPostgres)
library(dbplyr)
library(patchwork)
source('R/utils.R')
begin_date <- "1968-01-01"
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
         xint, dltt, dlc, mib, pstk, txt, mii, dvp) %>%
  collect()

dbDisconnect(wrds)

summary(funda)



# Cleaning and Prepping ---------------------------------------------------
funda_clean1 <-
  funda %>%
  mutate(mib = if_else(mib < 0, 0, mib),
         pstk = if_else(pstk < 0, 0, pstk),
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


summary(funda_clean2)




# Ratio Time Series -------------------------------------------------------
gen_plot_data <- function(dta, varx) {
  dta %>%
    select(gvkey, fyear, {{varx}}) %>%
    mutate(X_t0 = {{varx}}) %>%
    group_by(gvkey) %>%
      mutate(X_t1 = lead(X_t0, 1),
             X_t2 = lead(X_t0, 2),
             X_t3 = lead(X_t0, 3),
             X_t4 = lead(X_t0, 4),
             X_t5 = lead(X_t0, 5)
             ) %>%
    ungroup() %>%
    group_by(fyear) %>%
      mutate(RankX = ntile(X_t0, 10)) %>%
    ungroup() %>%
    filter(complete.cases(X_t0, X_t1, X_t2, X_t3, X_t4, X_t5)) %>%
    group_by(RankX) %>%
      summarise_at(vars(X_t0, X_t1, X_t2, X_t3, X_t4, X_t5), median) %>%
    ungroup()
}


make_plot <- function(X, dta=funda_clean2){

  plot_data <-
    funda_clean2 %>%
    gen_plot_data({{X}}) %>%
    tidyr::pivot_longer(c(X_t0, X_t1, X_t2, X_t3, X_t4, X_t5),
                        names_to=c("Var", "Year"),
                        names_sep="_",
                        values_to="Y")

  first_year <-
    plot_data %>%
    filter(Year == "t0") %>%
    mutate(Y = round(Y, 2))

  max_year <- max(dta$fyear)
  plot <-
    plot_data %>%
    ggplot(aes(x=Year, y=Y, group=RankX)) +
    geom_line(color=fig_colors["main1"]) +
    # geom_point(color=fig_colors["main1"]) +
    labs(y=paste("Median", rlang::as_label(enquo(X))),
         x=NULL,
         caption=paste0("Data: US listed companies with common equity greater $10mn, 1968 - ", max_year)
         ) +
    scale_x_discrete(labels = c("t=0", "t+1", "t+2", "t+3", "t+4", "t+5"))
    geom_text(data=first_year,
              aes(label=Y),
              nudge_x=-.25, color=fig_colors["main1"],
              size=3.5)

  return(plot)
}

# ROE Mean reversion plot
p1 <- make_plot(ROE) + ylim(-0.2, 0.3)
fig1 <-
  p1 + geom_hline(yintercept = 0.1, color = fig_colors[["accent1"]])
fig1
ggsave("Figs/L01-meanrev-roe.pdf", fig1, width = 6, height = 6 * 0.618, units = "in")
save_png("Figs/L01-meanrev-roe.png", fig1)



p2 <- make_plot(RNOA) + ylim(-0.2, 0.3)
p3 <- make_plot(GMargin) + ylim(-0.1, 1)
p4 <- make_plot(NOIMargin)  + ylim(-0.1, 1)
p5 <- make_plot(NOATurn)

fig2 <- (p1 | p2)
fig3 <- (p3 | p4) / (p5 | p1)
fig2
fig3

ggsave("Figs/L03-roe-rnoa-mr.pdf", fig2, width=7, height = 6 * 0.618, units = "in")
save_png("Figs/L03-roe-rnoa-mr.png", fig2)
ggsave("Figs/L03-rnoa-decomp-mr.pdf", fig3, width=7, height=6)
ragg::agg_png("Figs/L03-rnoa-decomp-mr.png", width = 7, height = 6, units = "in", res = 144)
print(fig3)
invisible(dev.off())




