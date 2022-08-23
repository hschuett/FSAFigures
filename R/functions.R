download_compustat_us <- function(
    # ridge figure
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

    # inv growth figure

    funda <-
      tbl(wrds, in_schema("comp", "funda")) %>%
      filter(indfmt  == 'INDL' &
               datafmt == 'STD'&
               popsrc  == 'D' &
               consol  == 'C' &
               between(datadate, begin_date, end_date)
      ) %>%
      select(gvkey, datadate, conm, fyear, ib, at, sale, ceq, oiadp, xrd, capx, dp) %>%
      distinct()

    company_meta <-
      tbl(wrds, in_schema("comp", "company")) %>%
      select(gvkey, gsector, ggroup) %>%
      filter(is.na(gsector) == F | is.na(ggroup) == F) %>%
      distinct()

    #  Q score

    funda <-
      tbl(wrds, in_schema("comp", "funda")) %>%
      filter(indfmt  == 'INDL' &
               datafmt == 'STD'&
               popsrc  == 'D' &
               consol  == 'C' &
               between(datadate, begin_date, end_date)
      ) %>%
      select(gvkey, datadate, conm, fyear, ib, at, sale, ceq, oiadp, xrd,
             xad, lifr, sich, dlc, dltt, pstk, che, ivao, mib) %>%
      distinct()

    company_meta <-
      tbl(wrds, in_schema("comp", "company")) %>%
      select(gvkey, sic) %>%
      distinct()

    financials <-
      funda %>%
      inner_join(company_meta, by="gvkey") %>%
      distinct() %>%
      collect()


    # mean reversion graphs

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

    #

  )



####


# -------------------------------------------------------------------------

frb <-
  tbl(wrds, in_schema("frb", "rates_monthly")) %>%
  select(date, tcmnom_y10) %>%
  filter(is.na(tcmnom_y10) == F) %>%
  collect() %>%
  mutate(YM = paste(year(date), month(date), sep="-"))

indices <-
  tbl(wrds, in_schema("crspa", "msi")) %>%
  select(date, SPRet=sprtrn) %>%
  filter(between(date, begin_date, end_date)) %>%
  collect() %>%
  mutate(YM = paste(year(date), month(date), sep="-"))
