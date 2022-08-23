# Setup -------------------------------------------------------------------
library(RPostgres)
library(tidyverse)
library(dbplyr)
source("R/utils.R")
begin_date <- "2005-01-01"
end_date   <- "2020-12-31"



# Data --------------------------------------------------------------------
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  user="hschuett",
                  password=rstudioapi::askForSecret("WRDS pw"),
                  sslmode='require',
                  dbname='wrds')
wrds  # checking if connection exists



returns <-
  tbl(wrds, in_schema("crspa", "dsf")) %>%
  filter(between(date, begin_date, end_date),
         permno == 80924 ) %>%
  mutate(MVE = (prc * shrout) / 1000000) %>%
  select(permno, date, MVE) %>%
  collect()


funda <-
  tbl(wrds, in_schema("comp", "funda")) %>%
  filter(indfmt  == 'INDL' &
           datafmt == 'STD'&
           popsrc  == 'D' &
           consol  == 'C' &
           between(datadate, begin_date, end_date) &
           gvkey == "030662"
  ) %>%
  select(gvkey, datadate, conm, fyear, ib, at, sale, ceq, ni) %>%
  distinct() %>%
  collect()



# plots -------------------------------------------------------------------

(p1 <-
   returns %>%
  filter(date > "2005-01-01" & date < "2020-01-01") %>%
  ggplot(aes(x=date, y=MVE)) +
  geom_line(color=tiu_colors['blue']) +
  labs(x=NULL,
       y="Sirius market value  ($Bn)"))


ggsave("Ressources/Figures/P5/sirius-1.pdf",
       p1,
       width=7, height=4)
