# Setup -------------------------------------------------------------------
library(RPostgres)
library(tidyverse)
library(dbplyr)
library(bizdays)
source("R/utils.R")
begin_date <- "1990-01-01"
end_date   <- "2020-12-31"
cal <- create.calendar('mycal',
                       weekdays=c("saturday", "sunday"))

read_parquet <- arrow::read_parquet
write_parquet <- function(x, p) {
  arrow::write_parquet(x, p, compression='gzip', compression_level=5)
}


# Connecting to WRDS
wrds <- dbConnect(Postgres(),
                  host = "wrds-pgdata.wharton.upenn.edu",
                  port = 9737,
                  user = rstudioapi::askForSecret("WRDS user"),
                  password = rstudioapi::askForSecret("WRDS pw"),
                  sslmode = "require",
                  dbname = "wrds"
)
wrds  # checking if connection exists
# dbDisconnect(wrds)




# Create ibes crsp linking table ------------------------------------------
ibes_cusip_link <-
  tbl(wrds, in_schema("ibes", "idsum")) %>%
  filter(usfirm == 1 & is.na(cusip) == F) %>%
  group_by(ticker, cusip) %>%
  summarize(fdate = min(sdates, na.rm = TRUE),
            ldate = max(sdates, na.rm = TRUE),
            lname = max(cname, na.rm = TRUE)
  ) %>%
  ungroup()

crsp_cusip_link <-
  tbl(wrds, in_schema("crsp", "stocknames")) %>%
  filter(is.na(ncusip) == F) %>%
  group_by(permno, ncusip) %>%
  summarize(fdate = min(namedt, na.rm = TRUE),
            ldate = max(nameenddt, na.rm = TRUE),
            lname = max(comnam, na.rm = TRUE)
  ) %>%
  ungroup()

iclink <-
  ibes_cusip_link %>%
  inner_join(crsp_cusip_link,
             by=c("cusip" = "ncusip"),
             suffix = c("_i", "_c")) %>%
  mutate(Score = if_else(!(ldate_i < fdate_c) & !(ldate_c < fdate_i), 0, 1)) %>%
  mutate(Score = if_else(lname_i != lname_c & Score == 1, Score + 1, Score)) %>%
  filter(Score <= 1) %>%
  collect()

iclink_clean1 <-
  iclink %>%
  filter(!(str_detect(.$ticker, "/\\d"))) %>%
  arrange(ticker, permno, fdate_i, fdate_c, ldate_i, ldate_c) %>%
  group_by(ticker, permno) %>%
  summarize(fdate_i = min(fdate_i),
            ldate_i = max(ldate_i),
            fdate_c = min(fdate_c),
            ldate_c = max(ldate_c),
            lname_i = max(lname_i),
            lname_c = max(lname_c)) %>%
  ungroup()

iclink_clean2 <-
  iclink_clean1 %>%
  arrange(ticker, permno, fdate_i, fdate_c, ldate_i, ldate_c) %>%
  group_by(ticker) %>%
  mutate(lag_fdate_i = lag(fdate_i),
         lag_ldate_i = lag(ldate_i)) %>%
  ungroup() %>%
  filter(is.na(lag_fdate_i) == T | !(fdate_i >= lag_fdate_i & ldate_i <= lag_ldate_i)) %>%
  select(ticker, permno, fdate_i, fdate_c, ldate_i, ldate_c, lname_c, lname_i)

# The SCORE variable is 0 and determined to be ‘best’ when the linked CUSIP has
# intersecting dates and matching company names. Small differences in company
# names (CNAME in IBES and COMNAM in CRSP) can be checked for and tolerated
# using SPEDIS, which is the spelling distance function in SAS. SPEDIS
# (cname,comnam)=0 is a perfect score and SPEDIS < 30 is usually good enough to
# be considered a name match. Matches with intersecting CUSIP dates ranges but
# with substantially different companies names are assigned a score level of 1.
# In this exercise, 200 cases with non-intersecting CUSIP dates are
# identified. These matches are further explored by calculating the spelling
# distance between IBES and CRSP company names. Those cases are assigned a score
# level of 2 if company names match, and 3 otherwise. [wrds
# support](https://wrds-www.wharton.upenn.edu/pages/support/applications/linking-databases/linking-ibes-and-crsp-data/)

ccm_link <-
  tbl(wrds, in_schema("crsp", "ccmxpf_linktable")) %>%
  filter(usedflag %in% c(0, 1),
         linktype %in% c('LU', 'LC'),
         linkprim %in% c("P", "C")) %>%
  mutate(linkenddt = if_else(is.na(linkenddt) == T,
                             date("2030-06-30"), linkenddt)) %>%
  collect()

final_linkt_table <-
  ccm_link %>%
  inner_join(iclink_clean2, by=c("lpermno"="permno")) %>%
  filter(is.na(linkdt) == T | (linkdt <= ldate_i & ldate_i <= linkenddt) | linkdt <= ldate_c & ldate_c <= linkenddt)


chosen_gvkey <- unique(final_linkt_table$gvkey)
chosen_permno <- unique(final_linkt_table$lpermno)
chosen_ticker <- unique(final_linkt_table$ticker)

rm(ccm_link, iclink, iclink_clean1, iclink_clean2,
   ibes_cusip_link, crsp_cusip_link)



# Getting actuals + adjust for rdq from compustat quarterly ---------------

# We checked many instances, and it appears that ANNDATS and RDQE are the same,
# but in case you notice any difference between them, I recommend that you use
# RDQ as a more reliable "announcement date".
# [wrds support](https://wrds-www.wharton.upenn.edu/pages/support/support-articles/ibes/difference-between-ibes-earnings-announcement-date-and-compustat-announcement-dates/)
comp_dates <-
  tbl(wrds, in_schema("comp", "fundq")) %>%
  filter(
    indfmt  == 'INDL' &
      datafmt == 'STD'&
      popsrc  == 'D' &
      consol  == 'C' &
      between(datadate, begin_date, end_date) &
      gvkey %in% chosen_gvkey
  ) %>%
  select(gvkey, conm, rdq, datadate, ceqq) %>%
  distinct() %>%
  collect()

matched_comp_dates <-
  comp_dates %>%
  left_join(final_linkt_table, by="gvkey") %>%
  filter(
    is.na(linkdt) == T | (linkdt <= datadate & datadate <= linkenddt),
    !(linkenddt < fdate_c) & !(ldate_c < linkdt)
  ) %>%
  group_by(gvkey, datadate) %>%
  # taking only primary link if multiple permnos per gvkey
  mutate(countkey2 = n()) %>%
  ungroup() %>%
  filter(countkey2 == 1 | (countkey2 > 1 & liid == "01")) %>%
  select(-countkey2) %>%
  add_count(gvkey, datadate) %>%
  filter(n == 1) %>%
  select(ticker, pends=datadate, rdq, ceqq) %>%
  distinct()

ibes_actuals <-
  tbl(wrds, in_schema("ibes", "actu_epsus")) %>%
  filter(
    measure == "EPS",
    pdicity == "QTR",
    curr_act == "USD",
    ticker %in% chosen_ticker,
    between(pends, begin_date, end_date),
  ) %>%
  select(ticker, cname, pends, anndats, value) %>%
  collect()

actuals_fxd_dates <-
  ibes_actuals %>%
  left_join(matched_comp_dates, by = c('ticker', 'pends')) %>%
  mutate(AnnounceDate = if_else(is.na(rdq), anndats, rdq)) %>%
  select(-anndats, -rdq) %>%
  rename(
    PeriodEndDate = pends,
    ActualEPS = value
  ) %>%
  filter(is.na(ActualEPS) == FALSE)

rm(comp_dates, matched_comp_dates, ibes_actuals)



# Collect lates ibes fcst data --------------------------------------------
ltg <-
  tbl(wrds, in_schema("ibes", "statsumu_epsus")) %>%
  filter(fpi == "0",
         measure == "EPS",
         ticker %in% chosen_ticker,
         between(statpers, begin_date, end_date)
  ) %>%
  select(ticker, statpers, NumEstLTG=numest, MedLTG=medest) %>%
  distinct()

eps_fcsts <-
  tbl(wrds, in_schema("ibes", "statsumu_epsus")) %>%
  filter(fpi == "6",
         measure == "EPS",
         statpers < fpedats,
         ticker %in% chosen_ticker,
         between(fpedats, begin_date, end_date)
  ) %>%
  select(ticker, fpedats, statpers, numest, medest) %>%
  group_by(ticker, fpedats) %>%
  filter(statpers == max(statpers, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

ibes_fcsts <-
  eps_fcsts %>%
  left_join(ltg, by=c("ticker", "statpers")) %>%
  collect() %>%
  arrange(ticker, fpedats, statpers, numest, medest)

rm(ltg, eps_fcsts)



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
         MVE = Price * shrout,
         MinPrc = min(Price, na.rm=T),
         ret = if_else(is.na(ret) == T & is.na(dlret) == F, dlret, ret)
  ) %>%
  ungroup() %>%
  filter(between(date, begin_date, end_date),
         MinPrc > 1) %>%
  select(permno, date, ret, Price, MVE)

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
  filter(between(date, namedt, nameendt)) %>%
  select(-namedt, -nameendt) %>%
  collect()

rm(delisting_returns, returns, indices, compinfo)



# Disconnect --------------------------------------------------------------
dbDisconnect(wrds)



# Final return merge ------------------------------------------------------
head(actuals_fxd_dates)
head(ibes_fcsts)

permno_link <-
  final_linkt_table %>%
  select(ticker, permno=lpermno, fdate_i, ldate_i, fdate_c, ldate_c) %>%
  distinct()

ibes <-
  actuals_fxd_dates %>%
  inner_join(ibes_fcsts, by = c("ticker", "PeriodEndDate"="fpedats")) %>%
  inner_join(permno_link, by = c("ticker")) %>%
  filter(PeriodEndDate >= fdate_i & PeriodEndDate <= ldate_i) %>%
  rename(ConsensusDate = statpers) %>%
  add_count(ticker, PeriodEndDate) %>%
  arrange(-n, ticker, PeriodEndDate)

table(ibes$n)

ibes <-
  ibes %>%
  # Interestingly, all consensusdates are a Thursday? Check this for full sample
  # mutate(WeekDay = weekdays(ConsensusDate)) %>%
  inner_join(crsp %>% select(permno, date, Price, MVE),
             by=c("permno", "ConsensusDate"="date")) %>%
  # <- loosing a lot of obs last step. Probably because restriced to main US exchanges
  mutate(ESurp = (ActualEPS - medest) / Price,
         EP = medest / Price,
         BtoM = ceqq / (MVE/1000)
  ) %>%
  select(-medest, -ActualEPS, -Price, -MVE, -ceqq) %>%
  filter(is.na(ESurp) == F) %>%
  select(-n, -fdate_i, -ldate_i, -fdate_c, -ldate_c)

# Create return windows
window_length <- 5
multiply_rets <- function(ret, n = window_length) {
  # to get a [-1, 0, 1, 2, 3] -> 5 day return window
  zoo::rollapplyr(ret, width = n, FUN = prod,  fill = NA)
}

ev_rets <-
  crsp %>%
  arrange(permno, date) %>%
  mutate(ret1 = ret + 1,
         MktRet1 = MktRet + 1) %>%
  group_by(permno) %>%
  mutate(
    EvRet1 = multiply_rets(ret1),
    EvMktRet1 = multiply_rets(MktRet1)
  ) %>%
  ungroup()

ev_rets <-
  ev_rets %>%
  mutate(AbEvRet = EvRet1 - EvMktRet1,
         # ea_match_date = add.bizdays(date, 1, cal)
         ea_match_date = add.bizdays(date, -(window_length - 2), cal)
  ) %>%
  select(permno, ea_match_date, AbEvRet)

# ev_rets %>%
#   select(permno, date, ea_match_date, EvRet1, ret, ret1) %>%
#   mutate(EA_mdate = add.bizdays(date, -(window_length - 2), cal))
#   head(20)
#
# 1 * (-0.0123 + 1) * 1 * (1 + 0.00625) * (1 + 0.00621)

final_data <-
  ibes %>%
  # next business day if the given date is not a business day
  mutate(ea_match_date = adjust.next(AnnounceDate, cal)) %>%
  inner_join(ev_rets, by = c("permno", "ea_match_date"))

head(final_data)

# write_parquet(final_data, "ea-event-returns.pqt")



# Plot --------------------------------------------------------------------
colos <- c(tiu_colors["blue"], tiu_colors["lightblue"])
names(colos) <- NULL

(fig <-
    final_data %>%
    filter(BtoM > 0 & is.na(BtoM) == F) %>%
    mutate(ESurpBins = ntile(ESurp, 30),
           BtMBins = ntile(BtoM, 2)) %>%
    # filter(BtMBins %in% c(1, 5)) %>%
    mutate(Firm.Type = if_else(BtMBins == 1, "Growth", "Value")) %>%
    group_by(ESurpBins, Firm.Type) %>%
    summarize(ESurp = median(ESurp, na.rm=T),
              AbEvRet = median(AbEvRet, na.rm=T)
    ) %>%
    ungroup() %>%
    ggplot(aes(x=ESurp, y=AbEvRet,
               group=Firm.Type, color=Firm.Type, fill=Firm.Type)) +
    geom_hline(yintercept=0, color=tiu_colors["lightgold"]) +
    geom_vline(xintercept=0,  color=tiu_colors["lightgold"]) +
    geom_smooth(method='loess', formula='y ~ x', se=T) +
    scale_color_manual(values=colos) +
    scale_fill_manual(values=colos) +
    labs(x="Earnings surprise (actual EPS - forecast scaled by stock price)",
         y="Abnormal event return (day-1, day+3)",
         caption="Data from IBES, Compustat, Crsp. Date range: 1990/01/01 - 2020/12/31. Announcements are sorted in to 30 bins\nbased on earnings surprise.The plot shows smoothed lines between the median anncouncement return and earnings surprise per bin."
    )
)

ggsave("Figs/P01-announce-ret.pdf", fig, width=7.5, height=5.5)
