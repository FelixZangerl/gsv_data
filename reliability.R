rm(list=ls())
library(tidyverse)
library(tsbox)
library(dygraphs)

### COMPARE MAIN INDEX AND CONSUMER CONFIDENCE AND GDP

### MAIN INDICATOR - monthly ####
pes <- read_csv("https://raw.githubusercontent.com/FelixZangerl/gsv_data/main/raw/at/trendecon_sa.csv") %>%
  dplyr::select(time,value) %>%
  #mutate(value = -value) %>%
  ts_xts()

pes_m <- ts_frequency(pes, to = "month")

pes_w <- ts_frequency(pes, to = "week") %>% ts_xts()

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

#### weekly indicators #####

## OECD

library(dplyr)
library(ggplot2)
library(readxl)
#devtools::loadall("/home/mohr/FMAr")

###### OECD Weekly tracker ######
#temp <- tempfile()
#download.file("https://github.com/NicolasWoloszko/OECD-Weekly-Tracker/raw/main/Data/weekly_tracker.xlsx",
#              destfile = temp)

oecd <- read_xlsx("./real_data/weekly_tracker.xlsx", sheet = "Sheet1") %>%
  filter(region %in% c("Austria", "Germany","Italy", "France")) %>%
  #filter(region %in% c("Austria", "Germany", "Switzerland")) %>%
  group_by(region) %>%
  mutate(date = as.Date(date),
         Tracker = `Tracker (yoy)` / 100,
         Low = `Low (yoy)` / 100,
         High = `High (yoy)` / 100) %>%
  filter(date >= "2020-01-01")

oecd_w <- oecd %>%
  ungroup() %>%
  filter(region == "Austria") %>%
  rename(time = date, value = Tracker) %>%
  dplyr::select(time, value) %>%
  ts_xts()

#unlink(temp)


ggplot(oecd %>% dplyr::filter(region=="Austria"), aes(x = date)) +
  geom_ribbon(aes(ymin = Low, ymax = High, fill = "a"), alpha = .3, show.legend = FALSE) +
  geom_line(aes(y = Tracker, colour = "a"), show.legend = FALSE) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "OECD Weekly GDP Tracker (Österreich)",
       subtitle = "Annualized growth rate",
       caption = paste("Quelle: OECD. Update:",max(oecd$date)),
       x="", y="") +
  theme_minimal()

ggplot(oecd,
       aes(x = date, y=Tracker, color = region)) +
  #geom_ribbon(aes(ymin = Low, ymax = High, fill = "a"), alpha = .3, show.legend = FALSE) +
  geom_line(size = 0.8) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "OECD Weekly GDP Tracker",
       subtitle = "Annualized growth rate",
       caption = paste("Quelle: OECD. Update:",max(data$date)),
       x="", y="") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(legend.title = element_blank())

###### WWWI WIFO ######
wwwi <- read_xlsx(path = "./real_data/wwwi_wifo.xlsx", sheet = "WWWI",
                  skip = 2, col_names = c("DATE", "VALUE", "cng_yavg", "cng_q"),
                  col_types = c("date", "guess", "guess", "guess"), n_max = 778) %>%
  mutate(DATE = as.character(DATE)) %>%
  select(DATE, VALUE) %>%
  rename(time = DATE, value = VALUE) %>%
  ts_xts()

wwwi_wifo <- read_excel("real_data/wwwi_wifo.xlsx", sheet = "WWWI")


###### OENB WEEKLY ######
wbip_oenb <- read_xlsx(path = "/mnt/home/wallnerm/data_repository/BIP_indikator_oenb.xlsx",
                       sheet = "Wochen-BIP-Indikator", skip = 13,
                       col_names = TRUE) %>%
  rename(DATE = ...3) %>%
  select(-...1, -...2) %>%
  pivot_longer(cols = c("Privater Konsum":"Tourismusexporte")) %>% 
  rename(CAT1 = name, VALUE = value, Gesamt = `Reales BIP im Vorkrisenvergleich`) %>%
  filter(!is.na(Gesamt)) %>%
  rename()

wecon_oenb <- read_delim("real_data/oenb_weekly_GDP-indicator.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  drop_na(Calenderweek) %>%
  rename(time = Calenderweek, value = `Real GDP compared to pre-crisis levels`) %>%
  dplyr::select(time, value)

wecon_oenb <- ts(wecon_oenb, start = c(2020,10), end = c(2021,47), frequency = 52) %>% ts_xts()




### PLOT REL MONTHLY ####

ts_dygraphs(ts_c(
  `Perceived Economic Situation` = pes_m,
  `GDP growth` = gdp,
  `Consumer Confidence` = cc
))  %>%
  dySeries("Perceived Economic Situation", strokeWidth=3) %>%
  dySeries("GDP growth", strokePattern = "dashed") %>%
  dySeries("Consumer Confidence", strokePattern = "dotted") %>%
  dyAxis("x", drawGrid = FALSE) %>%
#  dyRangeSelector(dateWindow = c("2020-01-01", Sys.Date()))%>%
  dyOptions(useDataTimezone = TRUE)

### PLOT WEEKLY IND ####

ts_dygraphs(ts_c(
  `Perceived Economic Situation` = pes_w,
  `WIFO WWWI` = wwwi,
  `OECD Weekly Tracker` = oecd_w,
  `OENB Weekly Indicator` = wecon_oenb
))  %>%
  dySeries("Perceived Economic Situation", strokeWidth=3) %>%
  dySeries("WIFO WWWI", strokeWidth = 3) %>%
  dySeries("OECD Weekly Tracker", strokeWidth = 3) %>%
  dySeries("OENB Weekly Indicator", strokeWidth = 3) %>%
  dyAxis("x", drawGrid = FALSE) %>%
    dyRangeSelector(dateWindow = c(as.Date("2020-01-01"), Sys.Date()))%>%
  dyOptions(useDataTimezone = TRUE)

### SAVE ####

save(cc, gdp, pes, pes_w, wwwi, oecd_w, wecon_oenb, file = "../gsv_data/r_data/reliability.RData")
