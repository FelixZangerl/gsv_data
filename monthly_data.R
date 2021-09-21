#### MONTHLY DATA trendecon ######

library(readr)
library(dplyr)
library(xts)
library(tsbox)

pes <- read_csv("./raw/at/economic_sentiment_sa.csv") %>% select(time, value) %>% mutate(value = -value) %>% ts_xts()

pes_m <- apply.monthly(pes, mean)

ts_dygraphs(pes_m)
ts_dygraphs(pes)

ts