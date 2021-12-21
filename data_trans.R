rm(list=ls())
library(tsbox)
library(readxl)
library(tidyverse)
#library(ggplot2)
#library(GGally)

geo <- "at"
#geo <- "de"

### COMPARE MAIN INDEX AND CONSUMER CONFIDENCE AND GDP

### MAIN INDICATOR - monthly ####
pes_d <- read_csv(paste0("https://raw.githubusercontent.com/FelixZangerl/gsv_data/main/raw/",geo,"/economic_sentiment_sa.csv")) %>%
  dplyr::select(time,value) %>%
  mutate(value = -value) %>%
  ts_xts() 

pes <- pes_d %>%
  ts_frequency(to = "month")

### REAL DATA #####

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

int <- read_excel("./real_data/oenb_interest_rates.xls",skip=1)
int <- int[int$` `=="Österreich",]
int <- t(int)[-1]
int <- ts(int, start=c(2007,1), end=c(2021,8), frequency = 12)
int <- tsbox::ts_xts(int)

une <- read_excel("real_data/ergebnisse_im_ueberblick_arbeitslose_und_arbeitslosenquoten_internationale-1.xlsx", skip = 5)
une <- une[,c(1:3)]
colnames(une) <- c("time","roman","value")
une <- une[!is.na(une$value),]

une$roman <- gsub("\\.","", une$roman)
une$roman <- utils:::.roman2numeric(une$roman)

une <- une %>% fill(time)
une$time <- paste0(une$time, "-", une$roman, "-01")
une$time <- as.Date(une$time)
une <- une %>% dplyr::select(time, value) %>% ts_xts()
write_csv(data.frame(index = index(une), value = une), file = "~/Desktop/tmp/une_data0.csv")

p1 <- read_excel("real_data/vpi_inflationsraten_und_indizes_1999_bis_2020.xlsx", skip = 2) #vpi 2005
p1 <- p1[,c(1,5)]
colnames(p1) <- c("time", "value")
p1$time[p1$time == "Dez.07"] <- 39390
p1 <- p1[c(1:195),] # 181: keep until 2007-01, 183: keep until 2006-12, 195: keep until 2006-01 (für lag = 12, vorjahresmonatsvergleich)
p1$time <- as.numeric(p1$time) # full years to NA
p1 <- p1[!is.na(p1$time),] # throw full year values away
# p1 is 2007-01 - 2020-12
p1 <- p1[order(p1$time),]
p1 <- ts(p1$value, start = c(2006,12), end = c(2020,12), frequency = 12)
#p$
# TODO: merge 2021 into p 

p2 <- read_excel("real_data/vpi_aktuelle_werte.xlsx", skip = 12) # spalte 6 = vpi 2005
p2 <- p2[c(1:8),c(1,6)] # 8 rows until august 2021
colnames(p2) <- c("time", "value")
p2 <- ts(p2$value, start = c(2021,1), end = c(2021,8), frequency = 12)

p <- ts(c(p1,p2),
        start = start(p1),
        frequency = frequency(p1))

pdat <- data.frame(Y = as.numeric(p), date = time(p))
pdat$x <- (pdat$Y - lag(pdat$Y,12)) / lag(pdat$Y,12) * 100 # inflation zum vorjahresmonat 
#pdat$x <- (pdat$Y - lag(pdat$Y)) / lag(pdat$Y) *100
pdat <- pdat[!is.na(pdat$x),] # remove NAs

p <- ts(pdat$x, start = c(2007,1), end = c(2021,8), frequency = 12)
p <- tsbox::ts_xts(p)
rm(pdat, p1, p2)

save(list=ls(),file="r_data/data.RData")
