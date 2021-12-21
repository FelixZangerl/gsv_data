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
#une <- diff(une, lag = 12)
#write_csv(data.frame(index = index(une), value = une), file = "~/Desktop/tmp/une_data1.csv")

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
#write_csv(data.frame(index = index(Dune), value = Dune), file = "~/Desktop/tmp/une_data2.csv")

Dpes <- diff(pes) # pes stationary
Dcpi <- diff(p) # cpi stationary
Dint <- diff(int)

int <- window(int, start=start, end=end) %>% ts_ts()
cc <- window(cc, start=start, end=end) %>% ts_ts()
une <- window(une, start=start, end=end) %>% ts_ts()
#une_test <- window(une, start=c(2007,1), end = c(2019,12))
une_test <- window(une, start=as.Date("2010-01-01"), end = as.Date("2019-01-12"))
pes <- window(pes, start=start, end=end) %>% ts_ts()
cpi <- window(p, start=start, end=end) %>% ts_ts()
crisis <- window(crisis, start=start, end=end) %>% ts_ts()

#### CORRELATIONS #####

composite_m <- cbind(pes, cc, une, cpi)

composite_m %>%
  as.data.frame() %>%
  GGally::ggpairs(axisLabels="show") #var_pairs.png

pesdat <- cbind(pes, une, cpi)
#pesdat <- cbind(pes, cpi, Dint)
#pesdat <- window(pesdat, start=as.Date("2007-01-01",origin = "1970-01-01")) ## TODO: include 2016-12
pesdat <- window(pesdat, start=c(2007,1))
#pesdat <- window(pesdat, end=as.Date("2019-01-12"))
#pesdat <- window(pesdat, end=c(2019,12)) # without crisis period
ccdat <- cbind(cc, une, cpi)
ccdat <- window(ccdat, start=start(une)) ## TODO: include 2016-12
crisis <- window(crisis, start=start(une))

library(tseries)
adf.test(int)
adf.test(cc)
adf.test(une)
adf.test(une_test)
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

lag <- lags <- 12

var6 <- VAR(pesdat, p=lag, type="const")
serial.test(var6, lags.pt=lag*2, type="PT.asymptotic") # no serial correlation
serial.test(var6, lags.pt = lag*2, type = "BG")
serial.test(var6, lags.pt = lag*2, type = "ES")

var6cc <- VAR(ccdat, p=lag, type="const")
serial.test(var6cc, lags.pt = lag*2, type="PT.asymptotic")

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
#plot(normtest)

arch <- arch.test(var6, lags.multi = 6, multivariate.only = T)
print(arch)

stability <- stability(var6, type = "OLS-CUSUM")
plot(stability)

#ctest_e <- ca.jo(pesdat, type = "eigen", ecdet = "const", K = 15)
#summary(ctest_e)

#ctest_t <- ca.jo(pesdat, type = "trace", ecdet = "const", K = 15)
#summary(ctest_t)

### BENCHMARK ####



#### GARCH ####

library(rugarch)
library(rmgarch)
library(quantmod)

#### ARIMA ####

fit <- auto.arima(pesdat[,"pes"])
plot(forecast(fit,h=3))
accuracy(fit)

pes2 <- window(pesdat[,"pes"], end=c(2021,4))
pesar2 <- auto.arima(pes2)
pesfit2 <- forecast(pesar2, h=3)
pes3 <- window(pesdat[,"pes"], start=c(2021,5))
accuracy(pesfit2, pes3)

### FORECAST ####

#### IN SAMPLE #### horizon = 3
# https://stackoverflow.com/questions/18244506/measuring-var-accuracy-using-accuracy-from-forecast Rob Hyndman
#lags <- 16
horizon <- 3
#trainingdata <- window(ccdat, end=c(2021,6))
#testdata <- window(ccdat, start=c(2021,7))
trainingdata <- window(pesdat, end=c(2021,4))
testdata <- window(pesdat, start=c(2021,5), end=c(2021,7))
v <- VAR(trainingdata, type="const", p=lags)
#v <- vecm16var
#v <- vecm16
p <- predict(v, n.ahead=horizon)
res <- residuals(v)
fits <- fitted(v)

for(i in 1:3)
{
  fc <- structure(list(mean=p$fcst[[i]][,"fcst"], x=trainingdata[,i],
                       fitted=c(rep(NA,lags),fits[,i])),class="forecast")
  acc_pes <- accuracy(fc,testdata[,1])
  acc_une <- accuracy(fc,testdata[,2])
  acc_cpi <- accuracy(fc,testdata[,3])
}

##### FCST CC ######
horizon <- 3
trainingdata <- window(ccdat, end=c(2021,4))
testdata <- window(ccdat, start=c(2021,5), end=c(2021,7))
v <- VAR(trainingdata, type="const", p=lags)
p <- predict(v, n.ahead=horizon)
res <- residuals(v)
fits <- fitted(v)

for(i in 1:3)
{
  fc <- structure(list(mean=p$fcst[[i]][,"fcst"], x=trainingdata[,i],
                       fitted=c(rep(NA,lags),fits[,i])),class="forecast")
  acc_pes_cc <- accuracy(fc,testdata[,1])
  acc_une_cc <- accuracy(fc,testdata[,2])
  acc_cpi_cc <- accuracy(fc,testdata[,3])
}

#### FCST PRECOV ######
horizon <- 3
trainingdata <- window(pesdat, end=c(2019,1))
testdata <- window(pesdat, start=c(2019,2), end=c(2019,4))
v <- VAR(trainingdata, type="const", p=lags)
p <- predict(v, n.ahead=horizon)
res <- residuals(v)
fits <- fitted(v)

for(i in 1:3)
{
  fc <- structure(list(mean=p$fcst[[i]][,"fcst"], x=trainingdata[,i],
                       fitted=c(rep(NA,lags),fits[,i])),class="forecast")
  acc_pes_precov <- accuracy(fc,testdata[,1])
  acc_une_precov <- accuracy(fc,testdata[,2])
  acc_cpi_precov <- accuracy(fc,testdata[,3])
}

#### FCST COV ######
horizon <- 3
trainingdata <- window(pesdat, end=c(2020,3))
testdata <- window(pesdat, start=c(2020,4), end=c(2020,6))
v <- VAR(trainingdata, type="const", p=lags)
p <- predict(v, n.ahead=horizon)
res <- residuals(v)
fits <- fitted(v)

for(i in 1:3)
{
  fc <- structure(list(mean=p$fcst[[i]][,"fcst"], x=trainingdata[,i],
                       fitted=c(rep(NA,lags),fits[,i])),class="forecast")
  acc_pes_cov <- accuracy(fc,testdata[,1])
  acc_une_cov <- accuracy(fc,testdata[,2])
  acc_cpi_cov <- accuracy(fc,testdata[,3])
}

library(ggfortify)
predplot <- forecast::autoplot(stats::predict(v, n.ahead=horizon)) #forecast::autoplot

plotly::ggplotly(predplot)

#autoplot(pesdat) +
#  autolayer(predict(v, n.ahead=3))


#### OUT OF SAMPLE ####

#fcst <- forecast(var6, h=12, dumvar = crisis)
pred_os <- predict(var6, n.ahead = 3)
pred_os_d <- predict(var6d, n.ahead = 3, dumvar = matrix(c(0,0,0,0,0,0,0,0,0), nrow=3, ncol=1))
plot(pred_os)
fanchart(pred_os)
forecast::autoplot(pred_os)

plot(pesdat)
#dygraph(pred)

#### ARIMA and RANDOM WALK FCST ####

fit <- auto.arima(pesdat[,"pes"])
plot(forecast(fit,h=3))
accuracy(fit)

# PES
pes2 <- window(pesdat[,"pes"], end=c(2021,4))
pes2_bef <- window(pesdat[,"pes"], end=c(2019,1))
pesar2 <- auto.arima(pes2)
pesar2_bef <- auto.arima(pes2_bef)
pesfit2 <- forecast(pesar2, h=3)
pesfit2_bef <- forecast(pesar2_bef, h=3)
pes3 <- window(pesdat[,"pes"], start=c(2021,5))
pes3_bef <- window(pesdat[,"pes"], start=c(2019,2))
(accar1 <- accuracy(pesfit2, pes3))
(accar1_bef <- accuracy(pesfit2_bef, pes3_bef))

pes2 <- pes2
pes_bef <- window(pes2, end = c(2019,1))
pesrwfit_bef <- rwf(pes_bef, h=3, drift = TRUE)
pesrwfit <- rwf(pes2, h=3, drift = TRUE)
pes3 <- pes3
pes3_bef <- window(pes, start = c(2019,2))
(accrw <- accuracy(pesrwfit, pes3))
(accrw_bef <- accuracy(pesrwfit_bef, pes3_bef))
#plot(pesrwfit)

# UNE
pes2 <- window(pesdat[,"une"], end=c(2021,4))
pes2_bef <- window(pesdat[,"une"], end=c(2019,1))
pesar2 <- auto.arima(pes2)
pesar2_bef <- auto.arima(pes2_bef)
pesfit2 <- forecast(pesar2, h=3)
pesfit2_bef <- forecast(pesar2_bef, h=3)
pes3 <- window(pesdat[,"une"], start=c(2021,5))
pes3_bef <- window(pesdat[,"une"], start=c(2019,2))
(accar1_une <- accuracy(pesfit2, pes3))
(accar1_bef_une <- accuracy(pesfit2_bef, pes3_bef))

pes2 <- pes2
pes_bef <- window(pes2, end = c(2019,1))
pesrwfit_bef <- rwf(pes_bef, h=3, drift = TRUE)
pesrwfit <- rwf(pes2, h=3, drift = TRUE)
pes3 <- pes3
pes3_bef <- window(pes, start = c(2019,2))
(accrw_une <- accuracy(pesrwfit, pes3))
(accrw_bef_une <- accuracy(pesrwfit_bef, pes3_bef))
#plot(pesrwfit)

# CPI
pes2 <- window(pesdat[,"cpi"], end=c(2021,4))
pes2_bef <- window(pesdat[,"cpi"], end=c(2019,1))
pesar2 <- auto.arima(pes2)
pesar2_bef <- auto.arima(pes2_bef)
pesfit2 <- forecast(pesar2, h=3)
pesfit2_bef <- forecast(pesar2_bef, h=3)
pes3 <- window(pesdat[,"cpi"], start=c(2021,5))
pes3_bef <- window(pesdat[,"cpi"], start=c(2019,2))
(accar1_cpi <- accuracy(pesfit2, pes3))
(accar1_bef_cpi <- accuracy(pesfit2_bef, pes3_bef))

pes2 <- pes2
pes_bef <- window(pes2, end = c(2019,1))
pesrwfit_bef <- rwf(pes_bef, h=3, drift = TRUE)
pesrwfit <- rwf(pes2, h=3, drift = TRUE)
pes3 <- pes3
pes3_bef <- window(pes, start = c(2019,2))
(accrw_cpi <- accuracy(pesrwfit, pes3))
(accrw_bef_cpi <- accuracy(pesrwfit_bef, pes3_bef))
#plot(pesrwfit)

### ACCURACIES ##########

forecast_roll$model <- "PES (current)"
forecast_roll$type <- "current"
forecast_roll_cc$model <- "CC"
forecast_roll_cc$type <- "current"
forecast_roll_cov$model <- "COV19 Crisis"
forecast_roll_cov$type <- "past"
forecast_roll_bef$model <- "Before COV19"
forecast_roll_bef$type <- "past"

###### MAIN MODEL ######
fc_pes <- data.frame(var = "pes", model = "PES", type = "current",
                     ME = acc_pes[2,"ME"], RMSE = acc_pes[2,"RMSE"], MAE = acc_pes[2,"MAE"], MPE = acc_pes[2,"MPE"], MAPE = acc_pes[2, "MAPE"])
fc_une <- data.frame(var = "une", model = "PES", type = "current",
                     ME = acc_une[2,"ME"], RMSE = acc_une[2,"RMSE"], MAE = acc_une[2,"MAE"], MPE = acc_une[2,"MPE"], MAPE = acc_une[2, "MAPE"])
fc_cpi <- data.frame(var = "cpi", model = "PES", type = "current",
                     ME = acc_cpi[2,"ME"], RMSE = acc_cpi[2,"RMSE"], MAE = acc_cpi[2,"MAE"], MPE = acc_cpi[2,"MPE"], MAPE = acc_cpi[2, "MAPE"])
######  CC MODEL ####### 
fc_pes_cc <- data.frame(var = "pes", model = "CC", type = "current",
                     ME = acc_pes_cc[2,"ME"], RMSE = acc_pes_cc[2,"RMSE"], MAE = acc_pes_cc[2,"MAE"], MPE = acc_pes_cc[2,"MPE"], MAPE = acc_pes_cc[2, "MAPE"])
fc_une_cc <- data.frame(var = "une", model = "CC", type = "current",
                        ME = acc_une_cc[2,"ME"], RMSE = acc_une_cc[2,"RMSE"], MAE = acc_une_cc[2,"MAE"], MPE = acc_une_cc[2,"MPE"], MAPE = acc_une_cc[2, "MAPE"])
fc_cpi_cc <- data.frame(var = "cpi", model = "CC", type = "current",
                        ME = acc_cpi_cc[2,"ME"], RMSE = acc_cpi_cc[2,"RMSE"], MAE = acc_cpi_cc[2,"MAE"], MPE = acc_cpi_cc[2,"MPE"], MAPE = acc_cpi_cc[2, "MAPE"])
######  PRECOV MODEL ####### 
fc_pes_precov <- data.frame(var = "pes", model = "Before COV19", type = "past",
                        ME = acc_pes_precov[2,"ME"], RMSE = acc_pes_precov[2,"RMSE"], MAE = acc_pes_precov[2,"MAE"], MPE = acc_pes_precov[2,"MPE"], MAPE = acc_pes_precov[2, "MAPE"])
fc_une_precov <- data.frame(var = "une", model = "Before COV19", type = "past",
                            ME = acc_une_precov[2,"ME"], RMSE = acc_une_precov[2,"RMSE"], MAE = acc_une_precov[2,"MAE"], MPE = acc_une_precov[2,"MPE"], MAPE = acc_une_precov[2, "MAPE"])
fc_cpi_precov <- data.frame(var = "cpi", model = "Before COV19", type = "past",
                            ME = acc_cpi_precov[2,"ME"], RMSE = acc_cpi_precov[2,"RMSE"], MAE = acc_cpi_precov[2,"MAE"], MPE = acc_cpi_precov[2,"MPE"], MAPE = acc_cpi_precov[2, "MAPE"])
######  COV MODEL ####### 
fc_pes_cov <- data.frame(var = "pes", model = "COV19 Crisis", type = "past",
                            ME = acc_pes_cov[2,"ME"], RMSE = acc_pes_cov[2,"RMSE"], MAE = acc_pes_cov[2,"MAE"], MPE = acc_pes_cov[2,"MPE"], MAPE = acc_pes_cov[2, "MAPE"])
fc_une_cov <- data.frame(var = "une", model = "COV19 Crisis", type = "past",
                            ME = acc_une_cov[2,"ME"], RMSE = acc_une_cov[2,"RMSE"], MAE = acc_une_cov[2,"MAE"], MPE = acc_une_cov[2,"MPE"], MAPE = acc_une_cov[2, "MAPE"])
fc_cpi_cov <- data.frame(var = "cpi", model = "COV19 Crisis", type = "past",
                            ME = acc_cpi_cov[2,"ME"], RMSE = acc_cpi_cov[2,"RMSE"], MAE = acc_cpi_cov[2,"MAE"], MPE = acc_cpi_cov[2,"MPE"], MAPE = acc_cpi_cov[2, "MAPE"])




###### UNIVARIATE BENCHMARK MODELS #####
#PES
forecast_ar1 <- data.frame(var = "pes", model = "AR(1)", type = "current",
                           ME = accar1[2,"ME"], RMSE = accar1[2,"RMSE"], MAE = accar1[2,"MAE"], MPE = accar1[2,"MPE"], MAPE = accar1[2,"MAPE"])
forecast_rw <- data.frame(var = "pes", model = "Random Walk", type = "current",
                          ME = accrw[2,"ME"], RMSE = accrw[2,"RMSE"], MAE = accrw[2,"MAE"], MPE = accrw[2,"MPE"], MAPE = accrw[2,"MAPE"])

forecast_ar1_bef <- data.frame(var = "pes", model = "AR(1)", type = "past",
                           ME = accar1_bef[2,"ME"], RMSE = accar1_bef[2,"RMSE"], MAE = accar1_bef[2,"MAE"], MPE = accar1_bef[2,"MPE"], MAPE = accar1_bef[2,"MAPE"])

forecast_rw_bef <- data.frame(var = "pes", model = "Random Walk", type = "past",
                          ME = accrw_bef[2,"ME"], RMSE = accrw_bef[2,"RMSE"], MAE = accrw_bef[2,"MAE"], MPE = accrw_bef[2,"MPE"], MAPE = accrw_bef[2,"MAPE"])
#UNE
forecast_ar1_une <- data.frame(var = "une", model = "AR(1)", type = "current",
                          ME = accar1_une[2,"ME"], RMSE = accar1_une[2,"RMSE"], MAE = accar1_une[2,"MAE"], MPE = accar1_une[2,"MPE"], MAPE = accar1_une[2,"MAPE"])
  
forecast_rw_une <- data.frame(var = "une", model = "Random Walk", type = "current",
                              ME = accrw_une[2,"ME"], RMSE = accrw_une[2,"RMSE"], MAE = accrw_une[2,"MAE"], MPE = accrw_une[2,"MPE"], MAPE = accrw_une[2,"MAPE"])

forecast_ar1_bef_une <- data.frame(var = "une", model = "AR(1)", type = "past",
                                   ME = accar1_bef_une[2,"ME"], RMSE = accar1_bef_une[2,"RMSE"], MAE = accar1_bef_une[2,"MAE"], MPE = accar1_bef_une[2,"MPE"], MAPE = accar1_bef_une[2,"MAPE"])

forecast_rw_bef_une <- data.frame(var = "une", model = "Random Walk", type = "past",
                                 ME = accrw_bef_une[2,"ME"], RMSE = accrw_bef_une[2,"RMSE"], MAE = accrw_bef_une[2,"MAE"], MPE = accrw_bef_une[2,"MPE"], MAPE = accrw_bef_une[2,"MAPE"])
#CPI
forecast_ar1_cpi <- data.frame(var = "cpi", model = "AR(1)", type = "current",
                               ME = accar1_cpi[2,"ME"], RMSE = accar1_cpi[2,"RMSE"], MAE = accar1_cpi[2,"MAE"], MPE = accar1_cpi[2,"MPE"], MAPE = accar1_cpi[2,"MAPE"])

forecast_rw_cpi <- data.frame(var = "cpi", model = "Random Walk", type = "current",
                              ME = accrw_cpi[2,"ME"], RMSE = accrw_cpi[2,"RMSE"], MAE = accrw_cpi[2,"MAE"], MPE = accrw_cpi[2,"MPE"], MAPE = accrw_cpi[2,"MAPE"])

forecast_ar1_bef_cpi <- data.frame(var = "cpi", model = "AR(1)", type = "past",
                                   ME = accar1_bef_cpi[2,"ME"], RMSE = accar1_bef_cpi[2,"RMSE"], MAE = accar1_bef_cpi[2,"MAE"], MPE = accar1_bef_cpi[2,"MPE"], MAPE = accar1_bef_cpi[2,"MAPE"])

forecast_rw_bef_cpi <- data.frame(var = "cpi", model = "Random Walk", type = "past",
                                  ME = accrw_bef_cpi[2,"ME"], RMSE = accrw_bef_cpi[2,"RMSE"], MAE = accrw_bef_cpi[2,"MAE"], MPE = accrw_bef_cpi[2,"MPE"], MAPE = accrw_bef_cpi[2,"MAPE"])



#forecast_roll_cc$var[forecast_roll_cc$var=="cc"] <- "pes"

#sds_pes <- data.frame(var = "pes", RMSE = sd(pesdat[,"pes"]), model = "sd")
#sds_Dune <- data.frame(var = "Dune", RMSE = sd(pesdat[,"Dune"]), model = "sd")
#sds_cpi <- data.frame(var = "cpi", RMSE = sd(pesdat[,"cpi"]), model = "sd")
#sds_global <- data.frame(var = "global", RMSE =0, model = "sd")

#sds <- rbind(sds_pes, sds_Dune, sds_cpi, sds_global)

accs <- rbind(#forecast_roll, forecast_roll_cc, forecast_roll_bef, forecast_roll_cov, 
  fc_pes, fc_une, fc_cpi, fc_pes_cc,  fc_une_cc, fc_cpi_cc,
  fc_pes_cov, fc_une_cov, fc_cpi_cov, fc_pes_precov, fc_une_precov, fc_cpi_precov,
  forecast_rw, forecast_ar1, forecast_rw_bef, forecast_ar1_bef,
  forecast_ar1_une, forecast_rw_une, forecast_rw_bef_une, forecast_ar1_bef_une,
  forecast_rw_cpi, forecast_ar1_cpi, forecast_rw_bef_cpi, forecast_ar1_bef_cpi)

accs_main <- accs %>% filter(model == "PES")
accs <- accs %>% dplyr::select(var, model, RMSE, type)
#accs <- rbind(accs, sds)

accs$model <- factor(accs$model, levels = c("PES","CC","Before COV19","COV19 Crisis", "Random Walk", "AR(1)"))
#accs$var[accs$var == "Dune"] <- "une"
accs$var <- factor(accs$var, levels = c("pes","cpi","une"))

#### TABLE #####
accs_main <- accs_main %>% dplyr::select(-type)
print(xtable::xtable(accs_main, label = "tab:acc", caption = "3-month ahead rolling in-sample forecast"), size="footnotesize", include.rownames = FALSE)

facet_names <- c(
  `current` = "(A) current period",
  `past` = "(B) past period"
)

library(ggsci)
g1 <- ggplot(accs, aes(x=var, y=RMSE, fill=model)) +
  geom_col(position = "dodge")+
  geom_text(aes(label = round(RMSE,2)),
            position = position_dodge(0.9),
            vjust = -.3,
            size = 3)+
  scale_fill_startrek(palette = c("uniform"), alpha = 1)+
  theme_minimal()+
  labs(y = "Root Mean Squared Error")+
  theme(axis.title.x = element_blank(),
        legend.position = "bottom")
g1

g2 <- g1 + facet_grid(~type, labeller = as_labeller(facet_names))
g2

#ggplot(sds, aes(x=var, y=RMSE, fill=model)) +
#  geom_point(group=1)+
#  scale_fill_startrek(palette = c("uniform"), alpha = 1)+
#  theme(axis.title.x = element_blank())+
#  theme_minimal()

# SAVE ####
save(pesdat, ccdat, var6, accs_main,
     #vecm16var, vecm16var2, vecm16, forecast_roll, forecast_roll_cc, forecast_roll_cov, forecast_roll_bef, forecast_rw, forecast_ar1,
     accs, g2, file="../gsv_data/r_data/var.RData")

#load("./r_data/var.RData")

# ###### 
# ######
# ##############
