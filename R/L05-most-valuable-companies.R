# Setup -------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(patchwork)
source("R/utils.R")



# Data --------------------------------------------------------------------
companies <- read_excel("data/forbes-most-valuable-companies.xlsx")
glimpse(companies)

top10_companies <-
  companies %>%
  group_by(Years) %>%
    slice(1:10) %>%
    arrange(Years, `Assets ($ bn)`) %>%
    unite("grp_ass", Years, Company, sep = "_", remove=FALSE) %>%
    data.frame() %>%
    mutate(grp_ass = factor(grp_ass, levels = grp_ass)) %>%
  ungroup() %>%
  unite("Era", Years, Era, sep = ": ") %>%
  rename(`Assets ($ bn)`=Assets....bn.)



top5_industries <-
  companies %>%
  group_by(Years, Era, Industry) %>%
    summarize(`Assets ($ bn)` = sum(`Assets ($ bn)`)) %>%
  ungroup() %>%
  group_by(Years) %>%
  slice(1:10) %>%
  arrange(Years, `Assets ($ bn)`) %>%
  unite("grp_ass", Years, Industry, sep = "_", remove=FALSE) %>%
  data.frame() %>%
  mutate(grp_ass = factor(grp_ass, levels = grp_ass)) %>%
  ungroup() %>%
  unite("Era", Years, Era, sep = ": ") %>%
  rename(`Assets ($ bn)`=Assets....bn.)

# Plots -------------------------------------------------------------------
(p1 <-
   top10_companies %>%
   # filter(Years == "1917")
   ggplot(aes(x=grp_ass, y=`Assets ($ bn)`))  +
   geom_col(fill=fig_colors[["main1"]]) +
   coord_flip() +
   facet_wrap(~Era, ncol=1, scales="free") +
   labs(x=NULL) +
   scale_x_discrete(breaks = top10_companies$grp_ass,
                    labels = top10_companies$Company)
)


(p2 <-
    top5_industries %>%
    # filter(Years == "1917")
    ggplot(aes(x=grp_ass, y=`Assets ($ bn)`))  +
    geom_col(fill=fig_colors[["main1"]]) +
    coord_flip() +
    facet_wrap(~Era, ncol=1, scales="free") +
    labs(x=NULL) +
    scale_x_discrete(breaks = top5_industries$grp_ass,
                     labels = top5_industries$Industry)
)


fig <-
  p1 + p2 +
  plot_annotation(
    caption="Data is not inflation adjusted. Sourced from: forbes.com/sites/jeffkauflin/2017/09/19/americas-top-50-companies-1917-2017"
  )
fig

# Save --------------------------------------------------------------------
ggsave('Figs/L05-most-valuable-firms.pdf', fig, width=7, height=6)
ragg::agg_png("Figs/L05-most-valuable-firms.png", width = 7, height = 6, units = "in", res = 144)
print(fig)
invisible(dev.off())
