library(readr)
library(dplyr)
library(tsbox)
library(xts)
library(lubridate)
library(forecast)
library(ggplot2)
library(dygraphs)

### MAIN INDICATOR - monthly ####
pes <- read_csv("https://raw.githubusercontent.com/FelixZangerl/gsv_data/main/raw/at/economic_sentiment_sa.csv") %>%
  select(time,value) %>%
  mutate(value = -value)

pes <- ts_frequency(pes, to = "month")

### REAL DATA ####
### GDP ####
gdp <- read_csv("./real_data/gdp_aut_m.csv") %>% rename(time = DATE, value = AUTLORSGPNOSTSAM) %>%
  filter(time > "2006-12-31") %>% #%>% mutate(gdp_m = gdp_m / 100) 
  ts_xts()

### TRANSFORM DATA ####
pes <- ts_data.frame(pes)
gdp <- ts_data.frame(gdp)

lastgdp <- max(gdp$time)

pes <- pes %>% filter(time <= lastgdp) #throw away data where pes is more recent than gdp

#### REGRESSIONS ####

pes2 <- ts(pes$value, start = 2007, frequency = 12)

pes_train <- window(pes2,start=c(2007),end=c(2018))
pesfit1 <- meanf(pes2,h=10)
pesfit2 <- rwf(pes2,h=10)
pesfit3 <- snaive(pes2,h=10)
autoplot(window(pes2, start=2007)) +
  autolayer(pesfit1, series="Mean", PI=FALSE) +
  autolayer(pesfit2, series="Naïve", PI=FALSE) +
  autolayer(pesfit3, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Forecasts for quarterly pes production") +
  guides(colour=guide_legend(title="Forecast"))# Fit the models

