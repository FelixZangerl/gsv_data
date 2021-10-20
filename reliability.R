rm(list=ls())
library(tidyverse)
library(tsbox)
library(dygraphs)

### COMPARE MAIN INDEX AND CONSUMER CONFIDENCE AND GDP

### MAIN INDICATOR - monthly ####
pes <- read_csv("https://raw.githubusercontent.com/FelixZangerl/gsv_data/main/raw/at/economic_sentiment_sa.csv") %>%
  dplyr::select(time,value) %>%
  mutate(value = -value) %>%
  ts_xts()

pes <- ts_frequency(pes, to = "month")

cc <- read_csv("./real_data/consumer_confidence.csv")

cc <- cc %>%
  filter(`Data producer` == "Austria") %>%
  filter(str_detect(indicator, "Austria")) %>%
  mutate(time = paste0(year,"-", month,"-01")) %>%
  dplyr::select(time, value = values) %>%
  ts_xts()

cc$value <- cc$value/10

gdp <- read_csv("./real_data/gdp_aut_q_oecd.csv")

gdp <- gdp %>%
  rename(time = TIME, value = Value) %>%
  dplyr::select(time, value) %>%
  mutate(time = str_remove(time, "-"))%>%
  #mutate(time = as.yearqtr(time)) #%>%
  ts_xts()

gdp <- gdp %>% ts_frequency(to = "month")

ts_dygraphs(ts_c(
  `Perceived Economic Situation` = pes,
  `GDP growth` = gdp,
  `Consumer Confidence` = cc
))  %>%
  dySeries("Perceived Economic Situation", strokeWidth=3) %>%
  dySeries("GDP growth", strokePattern = "dashed") %>%
  dySeries("Consumer Confidence", strokePattern = "dotted") %>%
  dyAxis("x", drawGrid = FALSE) %>%
#  dyRangeSelector(dateWindow = c("2020-01-01", Sys.Date()))%>%
  dyOptions(useDataTimezone = TRUE)


save(cc, gdp, pes, file = "../gsv_data/r_data/reliability.RData")
