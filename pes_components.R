library(tidyverse)
library(tsbox)
library(dygraphs)

geo <- "at"
#geo <- "de"

### COMPARE MAIN INDEX AND CONSUMER CONFIDENCE AND GDP

### MAIN INDICATOR - monthly ####
pes_d <- read_csv(paste0("https://raw.githubusercontent.com/FelixZangerl/gsv_data/main/raw/",geo,"/economic_sentiment_sa.csv")) %>%
  select(time,value) %>%
  mutate(value = -value) %>%
  ts_xts() 

pes <- pes_d %>%
  ts_frequency(to = "month")

### MAIN INDICATOR components - monthly
Wirtschaftskrise_d <- read_csv(paste0("https://raw.githubusercontent.com/FelixZangerl/gsv_data/main/raw/",geo,"/Wirtschaftskrise_sa.csv")) %>%
  filter(id == "seas_adj") %>%
  select(time,value) %>%
  ts_xts() 

Wirtschaftskrise <- Wirtschaftskrise_d %>%
  ts_frequency(to = "month")

Kurzarbeit_d <- read_csv(paste0("https://raw.githubusercontent.com/FelixZangerl/gsv_data/main/raw/",geo,"/Kurzarbeit_sa.csv")) %>%
  filter(id == "seas_adj") %>%
  select(time,value) %>%
  mutate(value = -value) %>%
  ts_xts()

Kurzarbeit <- Kurzarbeit_d %>%
  ts_frequency(to = "month")

Insolvenz_d <- read_csv(paste0("https://raw.githubusercontent.com/FelixZangerl/gsv_data/main/raw/",geo,"/Insolvenz_sa.csv")) %>%
  filter(id == "seas_adj") %>%
  select(time,value) %>%
  mutate(value = -value) %>%
  ts_xts()

Insolvenz <- Insolvenz_d %>%
  ts_frequency(to = "month")

arbeitslos_d <- read_csv(paste0("https://raw.githubusercontent.com/FelixZangerl/gsv_data/main/raw/",geo,"/arbeitslos_sa.csv")) %>%
  filter(id == "seas_adj") %>%
  select(time,value) %>%
  mutate(value = -value) %>%
  ts_xts()

arbeitslos <- arbeitslos_d %>%
  ts_frequency(to = "month")

### REAL DATA #####

cc <- read_csv("./real_data/consumer_confidence.csv")

cc <- cc %>%
  filter(`Data producer` == "Austria") %>%
  filter(str_detect(indicator, "Austria")) %>%
  mutate(time = paste0(year,"-", month,"-01")) %>%
  select(time, value = values) %>%
  ts_xts()

cc$value <- cc$value/10

gdp <- read_csv("./real_data/gdp_aut_q_oecd.csv")

gdp <- gdp %>%
  rename(time = TIME, value = Value) %>%
  select(time, value) %>%
  mutate(time = str_remove(time, "-"))%>%
  #mutate(time = as.yearqtr(time)) #%>%
  ts_xts()

#### PLOTS #####

today <- as.character(Sys.Date())

##### DAILY 2020-21 ####
ts_dygraphs(ts_c(
  `Perceived Economic Situation` = pes_d,
  `Wirtschaftskrise` = Wirtschaftskrise_d, 
  `arbeitslos` = arbeitslos_d,
  `Insolvenz` = Insolvenz_d,
  `Kurzarbeit` = Kurzarbeit_d
#  `GDP growth` = gdp,
#  `Consumer Confidence` = cc
))  %>%
  dySeries("Perceived Economic Situation", strokeWidth=3) %>%
  dyEvent("2020-3-16", "Lockdown #1", labelLoc = "bottom")%>%
  dyEvent("2020-5-1", "End of Lockdown #1", labelLoc = "bottom")%>%
  dyEvent("2020-9-14", "Mandatory wearing of masks", labelLoc = "bottom")%>%
  dyEvent("2020-9-21", "Lockdown 'light'", labelLoc = "bottom")%>%
  dyEvent("2020-11-17", "Lockdown #2", labelLoc = "bottom")%>%
  dyEvent("2020-12-6", "End of Lockdown #2", labelLoc = "bottom")%>%
  dyEvent("2020-12-26", "Lockdown #3", labelLoc = "bottom")%>%
  dyEvent("2021-2-7", "End of Lockdown #3", labelLoc = "bottom")%>%
  dyEvent("2021-5-19", "Easing measures - gastronomy, tourism, sports", labelLoc = "bottom")%>%
  dyEvent("2021-6-10", "Easing measures - less social distancing", labelLoc = "bottom")%>%
  dyEvent("2021-7-01", "Easing measures - night gastronomy", labelLoc = "bottom")%>%
  dyEvent("2020-6-25", "Insolvency Wirecard AG", labelLoc = "bottom")%>%
#  dySeries("GDP growth", strokePattern = "dashed") %>%
#  dySeries("Consumer Confidence", strokePattern = "dotted") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyRangeSelector(dateWindow = c("2020-01-01", "2021-01-01")) %>%
  dyOptions(useDataTimezone = TRUE)

###### MONTHLY since 2007 #####

ts_dygraphs(ts_c(
  `Perceived Economic Situation` = pes,
  `Wirtschaftskrise` = Wirtschaftskrise, 
  `arbeitslos` = arbeitslos,
  `Insolvenz` = Insolvenz,
  `Kurzarbeit` = Kurzarbeit
  #  `GDP growth` = gdp,
  #  `Consumer Confidence` = cc
))  %>%
  dySeries("Perceived Economic Situation", strokeWidth=3) %>%
  #  dySeries("GDP growth", strokePattern = "dashed") %>%
  #  dySeries("Consumer Confidence", strokePattern = "dotted") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyRangeSelector(dateWindow = c("2020-01-01", today)) %>%
  dyOptions(useDataTimezone = TRUE)