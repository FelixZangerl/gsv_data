rm(list=ls())
library(tsbox)
library(dygraphs)
library(readxl)
library(tidyverse)
library(xtable)
#library(ggplot2)
#library(GGally)

### LOAD DATA ####
load(file="./r_data/data.RData")

# une is in levels
# p is in changes (month on month)

begin <- as.Date("2007-01-01")

#### create dummies
start <- start(pes)
end <- lubridate::floor_date(Sys.Date(), unit="month")
dates <- seq(start, end, by="months") 
data <- rep(0, length(dates))
crisis <- xts::xts(x=data, order.by = dates)

#crisis["2020-02-01"] <- 1
crisis["2020-03-01"] <- 1
#crisis["2020-04-01"] <- 1
#crisis["2020-05-01"] <- 1
#crisis["2020-06-01"] <- 1


### une in changerates (month on month)
une <- diff(une, lag = 12)
#int <- as.numeric(int)

### BRING ALL TO SAME TIME HORIZON ####
frequency <- frequency(pes)
start <- start(pes)
end <- end(une)
#end <- as.Date(0, origin = "2020-01-01")

#### STATIONARITY #####
#Dint <- diff(int) # int not stationary
Dcc <- diff(cc) # cc stationary
Dune <- diff(une) %>% ts_ts() # une not stationary
Dune <- window(Dune, start=c(2007,1))
Dpes <- diff(pes) # pes stationary
Dcpi <- diff(p) # cpi stationary
Dint <- diff(int)

int <- window(int, start=start, end=end) %>% ts_ts()
cc <- window(cc, start=start, end=end) %>% ts_ts()
une <- window(une, start=start, end=end) %>% ts_ts()
pes <- window(pes, start=start, end=end) %>% ts_ts()
cpi <- window(p, start=start, end=end) %>% ts_ts()
crisis <- window(crisis, start=start, end=end) %>% ts_ts()

#### CORRELATIONS #####

composite_m <- cbind(pes, cc, une, cpi)

composite_m %>%
  as.data.frame() %>%
  GGally::ggpairs() #var_pairs.png

pesdat <- cbind(pes, Dune, cpi)
#pesdat <- cbind(pes, cpi, Dint)
#pesdat <- window(pesdat, start=as.Date("2007-01-01",origin = "1970-01-01")) ## TODO: include 2016-12
pesdat <- window(pesdat, start=c(2007,1))
#pesdat <- window(pesdat, end=as.Date("2019-01-12"))
#pesdat <- window(pesdat, end=c(2019,12)) # without crisis period
ccdat <- cbind(cc, Dune, cpi)
ccdat <- window(ccdat, start=start(Dune)) ## TODO: include 2016-12
crisis <- window(crisis, start=start(Dune))

library(tseries)
adf.test(int)
adf.test(cc)
adf.test(une)
adf.test(pes)
adf.test(cpi)

adf.test(Dint)
adf.test(Dcc)
adf.test(Dune)
adf.test(Dpes)
adf.test(Dcpi)

### VAR ####
library(fpp2)
library(vars)

#### include constant yes or no ######

vardat <- pesdat

var.AIC <- VAR(vardat,  type="none" , lag.max = 5, ic = c("AIC"))
var.SC <- VAR(vardat,  type="none" , lag.max = 5, ic = c("SC"))
var.AIC.c <- VAR(vardat,  type="const" , lag.max = 5, ic = c("AIC")) ## lag 2
var.SC.c <- VAR(vardat,  type="const" , lag.max = 5, ic = c("SC")) ## lag 1

AIC.woc <- cbind(AIC(var.AIC), BIC(var.AIC)) 
AIC.wc<- cbind(AIC(var.AIC.c), BIC(var.AIC.c)) 
SC.woc <- cbind(AIC(var.SC), BIC(var.SC)) 
SC.wc <- cbind(AIC(var.SC.c), BIC(var.SC.c)) 
MC.table <- rbind(AIC.woc, AIC.wc, SC.woc, SC.wc)
MC.table.names <- rbind("using AIC without constant", "using AIC with constant",
                        "using SC without constant","using SC with constant")  
rownames(MC.table)<- MC.table.names
colnames(MC.table) <- cbind("AIC", "BIC")
#MC.table
print(xtable::xtable(MC.table,caption="Model Choice Results",label="tab:mc"),
      sanitize.text.function=function(UR.table){UR.table},comment=FALSE)

rm(var.AIC, var.SC, var.AIC.c, var.SC.c, AIC.woc, AIC.wc, SC.woc, SC.wc)

#VARselect(uschange[,1:2], lag.max=8,
#          type="const")[["selection"]]

VARselect(pesdat, lag.max=2*12,
          type="const")[["selection"]]

#crisis <- NULL

var1 <- vars::VAR(pesdat, p=1, type="const", exogen = crisis)
serial.test(var1, lags.pt=10, type="PT.asymptotic")

var2 <- vars::VAR(pesdat, p=2, type="const", exogen = crisis)
serial.test(var2, lags.pt=10, type="PT.asymptotic")

var3 <- vars::VAR(pesdat, p=3, type="const", exogen = crisis)
serial.test(var3, lags.pt=10, type="PT.asymptotic")

var4 <- vars::VAR(pesdat, p=4, type="const", exogen = crisis)
serial.test(var4, lags.pt=10, type="PT.asymptotic")

var5 <- vars::VAR(pesdat, p=5, type="const", exogen = crisis)
serial.test(var5, lags.pt=10, type="PT.asymptotic")

var6d <- VAR(pesdat, p=18, type="const", exogen = crisis)
serial.test(var6d, lags.pt=30, type="PT.asymptotic") # no serial correlation

lag <- 12

var6 <- VAR(pesdat, p=lag, type="const")
serial.test(var6, lags.pt=lag*2, type="PT.asymptotic") # no serial correlation


var6cc <- VAR(ccdat, p=6, type="const")
serial.test(var6cc, lags.pt = 10, type="PT.asymptotic")

rm(var1, var2, var3, var4, var5)


#### MODEL TABLE ####

stargazer(var6$varresult, type = "text", header = F, title = "VAR(6)",  
          column.labels = c("$pes$","$cc$","$\\Delta{une}$","$cpi$"), 
          dep.var.caption = "dependent variables: $pes, \\Delta{une}, cpi$", 
          dep.var.labels = c(""), 
          covariate.labels = c("$pes_{t-1}$","$\\Delta{une_{t-1}}$","$int_{t-1}$",
                               "$pes_{t-2}$","$\\Delta{une_{t-2}}$","$int_{t-2}$",
                               "$pes_{t-3}$","$\\Delta{une_{t-3}}$","$int_{t-3}$",
                               "$pes_{t-4}$","$\\Delta{une_{t-4}}$","$int_{t-4}$",
                               "$pes_{t-5}$","$\\Delta{une_{t-5}}$","$int_{t-5}$",
                               "$pes_{t-6}$","$\\Delta{une_{t-6}}$","$int_{t-6}$",
                               "constant"))

#### MODEL DIAGNOSTICS ####

normtest <- normality.test(var6)
print(normtest)
plot(normtest)

arch <- arch.test(var6, lags.multi = 6, multivariate.only = T)
print(arch)

stability <- stability(var6, type = "OLS-CUSUM")
plot(stability)

ctest_e <- ca.jo(pesdat, type = "eigen", ecdet = "const", K = 15)
summary(ctest_e)

ctest_t <- ca.jo(pesdat, type = "trace", ecdet = "const", K = 15)
summary(ctest_t)

### VECM #####
library(tsDyn)

vecm16 <- VECM(pesdat,  lag = lag, r = 2)
summary(vecm16)

vecm16var <- vec2var(ctest_t, r=2)
serial.test(vecm16var, lags.pt = lag*2)

arch1 <- arch.test(vecm16var, lags.multi = lag, multivariate.only = T)
print(arch1)

norm1 <- normality.test(vecm16var)
print(norm1)

#### FORECAST ####
library(ggfortify)
forecast <- predict(vecm16var, n.ahead = 3)
forecast::autoplot(forecast)

roll <- 3

predict_roll <- predict_rolling(vecm16, n.ahead = 3, nroll=roll)
forecast_roll <- accuracy_stat(predict_roll)

forecast_roll

plot(window(pesdat[,"pes"]))
preds_roll_ts <- ts(predict_roll$pred, start=time(pesdat)[nrow(pesdat)-roll], freq=12)
lines(preds_roll_ts[,"pes"], col=2, lty=2)
legend("bottomright", lty=c(1,2), col=1:2, leg=c("True", "Fitted"))
title("Comparison of true and rolling 1-ahead forecasts\n")

plot(window(pesdat[,"Dune"]))
preds_roll_ts <- ts(predict_roll$pred, start=time(pesdat)[nrow(pesdat)-roll], freq=12)
lines(preds_roll_ts[,"Dune"], col=2, lty=2)
legend("bottomright", lty=c(1,2), col=1:2, leg=c("True", "Fitted"))
title("Comparison of true and rolling 1-ahead forecasts\n")

plot(window(pesdat[,"cpi"]))
preds_roll_ts <- ts(predict_roll$pred, start=time(pesdat)[nrow(pesdat)-roll], freq=12)
lines(preds_roll_ts[,"cpi"], col=2, lty=2)
legend("bottomright", lty=c(1,2), col=1:2, leg=c("True", "Fitted"))
title("Comparison of true and rolling 1-ahead forecasts\n")

##### BENCHMARK ####

###### CC ########

vecm16cc <- tsDyn::VECM(ccdat,  lag = lag, r = 2)
ctest_t_cc <- ca.jo(ccdat, type = "trace", ecdet = "const", K = 15)
vecm16var_cc <- vec2var(ctest_t, r=2)
predict_roll_cc <- predict_rolling(vecm16cc, n.ahead = 6, nroll=roll)
forecast_roll_cc <- accuracy_stat(predict_roll_cc)

###### CRISIS ########

pesdat_cov <- window(pesdat, end = c(2020,3))
vecm16cov <- tsDyn::VECM(pesdat_cov,  lag = lag, r = 2)
ctest_t_cov <- ca.jo(pesdat_cov, type = "trace", ecdet = "const", K = 15)
vecm16var_cov <- vec2var(ctest_t, r=2)
predict_roll_cov <- predict_rolling(vecm16cov, n.ahead = 6, nroll=roll)
forecast_roll_cov <- accuracy_stat(predict_roll_cov)

###### ACCS FIGURE AND TABLE ##########

forecast_roll$model <- "PES"
forecast_roll_cc$model <- "CC"
forecast_roll_cov$model <- "Crisis"

forecast_roll_cc$var[forecast_roll_cc$var=="cc"] <- "pes"

accs <- rbind(forecast_roll, forecast_roll_cc, forecast_roll_cov)

accs$model <- factor(accs$model, levels = c("PES","CC","Crisis"))


library(ggsci)
ggplot(accs, aes(x=var, y=RMSE, fill=model)) +
  geom_col(position = "dodge")+
  scale_fill_startrek(palette = c("uniform"), alpha = 1)+
  theme(axis.title.x = element_blank())+
  theme_minimal()

# SAVE ####
save(pesdat, ccdat, var6, vecm16var, vecm16, forecast_roll, forecast_roll_cc, forecast_roll_cov,
     accs, file="../gsv_data/r_data/var.RData")

#load("./r_data/var.RData")

# ###### 
# ######
# ##############
