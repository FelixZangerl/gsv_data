rm(list=ls())
library(dygraphs)
library(readr)
library(tsbox)
library(xts)
library(dplyr)
library(forecast)
library(ggplot2)
library(GGally)
library(fpp) 
library(fpp2)
library(prophet)

### MAIN INDICATOR ####
pes <- read_csv("https://raw.githubusercontent.com/FelixZangerl/gsv_data/main/raw/at/economic_sentiment_sa.csv") %>%
#  select(time,value) %>%
  mutate(value = -value) #%>%
  #ts_ts()

### MAIN INDICATOR COMPONENTS ####

keywords <- c("Wirtschaftskrise","Kurzarbeit","arbeitslos","Insolvenz")

Wirtschaftskrise_m <- read_csv("./raw/at/Wirtschaftskrise_m.csv") %>% select(time,value) %>% ts_xts()
Kurzarbeit_m <- read_csv("./raw/at/Kurzarbeit_m.csv") %>% select(time,value) %>%  ts_xts()
arbeitslos_m <- read_csv("./raw/at/arbeitslos_m.csv") %>% select(time,value) %>% ts_xts()
Insolvenz_m <- read_csv("./raw/at/Insolvenz_m.csv") %>% select(time,value) %>% ts_xts()

### GDP ####
#gdp_q <- read_csv("./real_data/gdp_aut_q.csv") 
gdp_m <- read_csv("./real_data/gdp_aut_m.csv") %>% rename(time = DATE, gdp_m = AUTLORSGPNOSTSAM) %>%
  filter(time > "2006-12-31") %>% #%>% mutate(gdp_m = gdp_m / 100) 
  ts_xts()


### FORECAST ####
m <- pes %>% rename(ds = time, y = value)
max <- max(m$ds)

m <- prophet(m, growth = "linear",
             daily.seasonality = FALSE,
             yearly.seasonality = FALSE,
             weekly.seasonality = FALSE)

future <- make_future_dataframe(m, periods = 365)
tail(future)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast) 
#add_changepoints_to_plot(m)
abline(h = max)

prophet_plot_components(m, forecast)

### COMPOSITE ####
composite_m <- cbind(Wirtschaftskrise_m, Kurzarbeit_m, Insolvenz_m, arbeitslos_m, gdp_m)
colnames(composite_m)[colnames(composite_m)=="value"] <- "Wirtschaftskrise_m"

### Correlation - PAIRS ####
composite_m %>%
  as.data.frame() %>%
  GGally::ggpairs()

### TS-REG (tslm) ####
fit <- tslm(gdp_m ~ Wirtschaftskrise_m+Kurzarbeit_m+Insolvenz_m+arbeitslos_m, data=as.ts(composite_m))
summary(fit)

checkresiduals(fit)

ts0 <- ts(composite_m$gdp_m, start=c(2007,1), end = c(2021,5), frequency = 12)

fit_trend <- tslm(ts0 ~ trend + season, data=ts0)
summary(fit_trend)


## check
df <- as.data.frame(composite_m)
df[,"Residuals"]  <- as.numeric(residuals(fit))
p1 <- ggplot(df, aes(x=Wirtschaftskrise_m, y=Residuals)) +
  geom_point()
p2 <- ggplot(df, aes(x=Kurzarbeit_m, y=Residuals)) +
  geom_point()
p3 <- ggplot(df, aes(x=Insolvenz_m, y=Residuals)) +
  geom_point()
p4 <- ggplot(df, aes(x=arbeitslos_m, y=Residuals)) +
  geom_point()
gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)


#### TRYOUT ####

autoplot(composite_m) +
  ylab("% change") + xlab("Year")

beer2 <- window(ausbeer, start=1992)

fit.beer <- tslm(beer2 ~ trend + season)
summary(fit.beer)


plot(meanf(pes, 100))
plot(naive(pes, 100))
plot(snaive(pes, 100))

# Set training data from 1992 to 2007
pes2 <- window(pes,start=2018,end=c(2019))
# Plot some forecasts
autoplot(pes2) +
  autolayer(meanf(pes2, h=30),
            series="Mean", PI=FALSE) +
  autolayer(naive(pes2, h=30),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(pes2, h=30),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for perceived economic situation") +
  xlab("Year") + ylab("Score") +
  guides(colour=guide_legend(title="Forecast"))

pes_mon <- read_csv()

X10_1_d_out <- read_excel("real_data/10.1_d_out.xlsx", skip = 3, n_max = 22)

forecast::ggseasonplot(pes, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: pes")
