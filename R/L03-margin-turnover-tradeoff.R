# Setup -------------------------------------------------------------------
library(tidyverse)
source("R/utils.R")



# Gen data ----------------------------------------------------------------

rnoa <- seq(0.05, 0.2, by=0.025)
turnover <- seq(0.5, 2, by=0.01)

margins <- lapply(rnoa, function(x) x / turnover)
d <- data.frame(margins)
colnames(d) <- paste("RNOA", rnoa, sep="_")
d$Turnover <- turnover

d2 <-
  d %>%
  pivot_longer(cols=RNOA_0.05:RNOA_0.2,
               names_to="RNOA_lab",
               values_to="Margin")
d2$RNOA <- as.ordered(rep(rnoa, times=length(turnover)))




# Plot --------------------------------------------------------------------

(
fig <-
  d2 %>%
  ggplot(aes(x=Turnover, y=Margin, group=RNOA, color=RNOA)) +
  geom_line() +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "top")
)

ggsave("Figs/L03-mt-tradeoff.pdf", fig, width = 6, height = 6 * 0.618, units = "in")
save_png("Figs/L03-mt-tradeoff.png", fig)
