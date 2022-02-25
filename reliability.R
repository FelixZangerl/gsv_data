rm(list=ls())
library(tidyverse)
library(tsbox)
library(dygraphs)
library(lubridate)
#library(dplyr)
#library(ggplot2)
library(readxl)

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


#devtools::loadall("/home/mohr/FMAr")

###### OECD Weekly tracker ######
temp <- tempfile()
download.file("https://github.com/NicolasWoloszko/OECD-Weekly-Tracker/raw/main/Data/weekly_tracker.xlsx",
              destfile = temp)
oecd <- read_xlsx(temp, sheet = "Sheet1") %>%
#oecd <- read_xlsx("./real_data/weekly_tracker.xlsx", sheet = "Sheet1") %>%
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

#ggplot(oecd %>% dplyr::filter(region=="Austria"), aes(x = date)) +
#  geom_ribbon(aes(ymin = Low, ymax = High, fill = "a"), alpha = .3, show.legend = FALSE) +
#  geom_line(aes(y = Tracker, colour = "a"), show.legend = FALSE) +
#  geom_hline(yintercept = 0) +
#  scale_y_continuous(labels = scales::percent) +
#  scale_color_brewer(palette = "Dark2") +
#  scale_fill_brewer(palette = "Dark2") +
#  labs(title = "OECD Weekly GDP Tracker (Österreich)",
#       subtitle = "Annualized growth rate",
#       caption = paste("Quelle: OECD. Update:",max(oecd$date)),
#       x="", y="") +
#  theme_minimal()

#ggplot(oecd,
#       aes(x = date, y=Tracker, color = region)) +
#  #geom_ribbon(aes(ymin = Low, ymax = High, fill = "a"), alpha = .3, show.legend = FALSE) +
#  geom_line(size = 0.8) +
#  geom_hline(yintercept = 0) +
#  scale_y_continuous(labels = scales::percent) +
#  labs(title = "OECD Weekly GDP Tracker",
#       subtitle = "Annualized growth rate",
#       caption = paste("Quelle: OECD. Update:",max(data$date)),
#       x="", y="") +
#  scale_color_brewer(palette = "Dark2") +
#  theme_minimal() +
#  theme(legend.title = element_blank())

###### WWWI WIFO ######

temp <- tempfile()
download.file("https://www.wifo.ac.at/wwadocs/konjunktur/W%C3%B6chentlicherWIFOWirtschaftsindex/WIFO-Konjunkturberichterstattung_W%C3%B6chentlicherWIFOWirtschaftsindex.xlsx",
              destfile = temp)

t0 <- as.numeric(as.Date("2007-01-01"))
t1 <- as.numeric(as.Date(Sys.Date()))
t <- round((t1 - t0) / 7) - 1  # weeks passed since index started

wwwi <- read_xlsx(temp, sheet = "WWWI",
#wwwi <- read_xlsx(path = "./real_data/wwwi_wifo.xlsx", sheet = "WWWI",
                  skip = 2, col_names = c("DATE", "VALUE", "cng_yavg", "cng_q"),
                  #col_types = c("date", "guess", "guess", "guess"), 
                  n_max = t) %>%
  mutate(DATE = as.character(DATE)) %>%
  dplyr::select(DATE, VALUE) %>%
  rename(time = DATE, value = VALUE) %>%
  ts_xts()

wwwi_entstehung <- read_xlsx(temp, sheet = "Beiträge_Entstehung",
                             skip = 3)

wwwi_e_names <- c("Month/Year", "KW", "WWWI",
                  "Produzierender Bereich (ÖNACE A_F)"	,"Handel (ÖNACE G)"	,"Verkehr (ÖNACE H)"	,"Beherbergung und Gastronomie (ÖNACE I)"	,"Restliche Dienstleistungen (ÖNACE J_T)",	"Saldo Gütersteuern / -subventionen",	"empty",	
                  "WWWI",	"Produzierender Bereich (ÖNACE A_F)", "Handel (ÖNACE G)",	"Verkehr (ÖNACE H)",	"Beherbergung und Gastronomie (ÖNACE I)",	"Restliche Dienstleistungen (ÖNACE J_T)",	"Saldo Gütersteuern / -subventionen")

handel_vs_vj <- na.omit(wwwi_entstehung[[5]]) #starts KW2 2020
handel_vs_vj <- handel_vs_vj[-1]
handel_vs_vj <- ts(handel_vs_vj, start = c(2020,2), frequency = 53) %>% ts_xts() #52.17857
handel_vs_vj <- handel_vs_vj %>% xts:::.drop.time()

handel_vs_vj <- handel_vs_vj %>% ts_data.frame()
handel_vs_vj$time <- handel_vs_vj$time %m+% days(1)
handel_vs_vj$time[handel_vs_vj$time >= "2022-01-01"] <- handel_vs_vj$time[handel_vs_vj$time >= "2022-01-01"] %m+% days(1)
handel_vs_vj <- handel_vs_vj %>% ts_xts()

gastro_vs_vj <- na.omit(wwwi_entstehung[[7]]) #starts KW2 2020
gastro_vs_vj <- gastro_vs_vj[-1]
gastro_vs_vj <- ts(gastro_vs_vj, start = c(2020,2), frequency = 53) %>% ts_xts() #52.17857
gastro_vs_vj <- gastro_vs_vj %>% xts:::.drop.time()

gastro_vs_vj <- gastro_vs_vj %>% ts_data.frame()
gastro_vs_vj$time <- gastro_vs_vj$time %m+% days(1)
gastro_vs_vj$time[gastro_vs_vj$time >= "2022-01-01"] <- gastro_vs_vj$time[gastro_vs_vj$time >= "2022-01-01"] %m+% days(1)
gastro_vs_vj <- gastro_vs_vj %>% ts_xts()

#wwwi_wifo <- read_excel("real_data/wwwi_wifo.xlsx", sheet = "WWWI")


###### OENB WEEKLY ######
temp <- tempfile()
download.file("https://www.oenb.at/dam/jcr:7c5ab44b-204d-4d45-a802-884d1019f7f5/data_on_the_weekly_GDP-indicator.csv",
              destfile = temp)

wecon_oenb <- read_delim(temp, delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
#wecon_oenb <- read_delim("real_data/oenb_weekly_GDP-indicator.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  drop_na(Calenderweek) %>%
  rename(time = Calenderweek, value = `Real GDP compared to pre-crisis levels`) %>%
  dplyr::select(time, value)

#wecon_oenb <- ts(wecon_oenb, start = c(2020,10), end = c(2021,47), frequency = 52) %>% ts_xts()
wecon_oenb <- ts(wecon_oenb, start = c(2020,10), frequency = 52) %>% ts_xts()

### REALIGN WEEKLY ####

pes_w <- pes_w[time(pes_w) >= "2020-01-01"] 

wwwi <- wwwi[time(wwwi) >= "2020-01-01"]  

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

##### standardise week format  #####

wecon_oenb <- wecon_oenb %>% xts:::.drop.time()

wwwi <- wwwi %>% ts_data.frame()
wwwi$time <- wwwi$time %m-% days(3)
wwwi <- wwwi %>% ts_xts()

oecd_w <- oecd_w %>% ts_data.frame()
oecd_w$time <- oecd_w$time %m-% days(2)
oecd_w <- oecd_w %>% ts_xts()

pes_w <- pes_w %>% ts_data.frame()
pes_w$time <- pes_w$time %m-% days(2)
pes_w <- pes_w %>% ts_xts()


ts_dygraphs(ts_c(
  `Perceived Economic Situation` = pes_w * 5,
  `WIFO WWWI` = wwwi,
  `OECD Weekly Tracker` = oecd_w * 100
 # `OENB Weekly Indicator` = wecon_oenb
))  %>%
  dySeries("Perceived Economic Situation", strokeWidth=3) %>%
  dySeries("WIFO WWWI", strokeWidth = 3) %>%
  dySeries("OECD Weekly Tracker", strokeWidth = 3) %>%
#  dySeries("OENB Weekly Indicator", strokeWidth = 3) %>%
  dyAxis("x", drawGrid = FALSE) %>%
    dyRangeSelector(dateWindow = c(as.Date("2020-01-01"), Sys.Date()))%>%
  dyOptions(useDataTimezone = TRUE)

### SAVE ####

save(cc, gdp, pes, pes_w, pes_m, 
     wwwi, oecd_w, wecon_oenb, 
     handel_vs_vj, gastro_vs_vj, file = "../gsv_data/r_data/reliability.RData")

cat("Finished")