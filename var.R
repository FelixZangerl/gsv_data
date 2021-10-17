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

#
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

int <- window(int, start=start, end=end) %>% ts_ts()
cc <- window(cc, start=start, end=end) %>% ts_ts()
une <- window(une, start=start, end=end) %>% ts_ts()
pes <- window(pes, start=start, end=end) %>% ts_ts()
cpi <- window(p, start=start, end=end) %>% ts_ts()
crisis <- window(crisis, start=start, end=end) %>% ts_ts()

#### CORRELATIONS #####

composite_m <- cbind(pes, cc, une, cpi)

#composite_m %>%
#  as.data.frame() %>%
#  GGally::ggpairs() #var_pairs.png

pesdat <- cbind(pes, Dune, cpi)
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

VARselect(pesdat, lag.max=8,
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

var6d <- VAR(pesdat, p=6, type="const", exogen = crisis)
serial.test(var6d, lags.pt=10, type="PT.asymptotic") # no serial correlation

var6 <- VAR(pesdat, p=6, type="const")
serial.test(var6, lags.pt=10, type="PT.asymptotic") # no serial correlation

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

serial.test(var6, lags.pt = 10, type="PT.asymptotic")

normtest <- normality.test(var6)
print(normtest)
plot(normtest)

arch <- arch.test(var6, lags.multi = 6, multivariate.only = T)
print(arch)

stability <- stability(var6, type = "OLS-CUSUM")
plot(stability)

ctest <- ca.jo(pesdat, type = "eigen", ecdet = "const", K = 6)
summary(ctest)

#### GRANGER CAUSALITY ####

pes_granger <- causality(var6, cause = "pes")
pes_granger

Dune_granger <- causality(var6, cause= "Dune")
Dune_granger

cpi_granger <- causality(var6, cause = "cpi")
cpi_granger

#### IRF #####

pes_irf <- irf(var6, impulse = "pes", response = "pes", n.ahead = 6)
plot(pes_irf)

une_irf <- irf(var6, impulse = "pes", response = "Dune", n.ahead = 6)
plot(une_irf)

cpi_irf <- irf(var6, impulse = "pes", response = "cpi", n.ahead = 6)
plot(cpi_irf)

fevd <- fevd(var6, n.ahead = 12)
plot(fevd)

### FORECAST ####

#### IN SAMPLE #### horizon = 3
# https://stackoverflow.com/questions/18244506/measuring-var-accuracy-using-accuracy-from-forecast Rob Hyndman
lags <- 6
horizon <- 1
trainingdata <- window(ccdat, end=c(2021,6))
testdata <- window(ccdat, start=c(2021,7))
trainingdata <- window(pesdat, end=c(2020,2))
testdata <- window(pesdat, start=c(2020,3), end=c(2020,3))
v <- VAR(trainingdata, type="const", p=lags)
p <- predict(v, n.ahead=horizon)
res <- residuals(v)
fits <- fitted(v)

for(i in 1:3)
{
  fc <- structure(list(mean=p$fcst[[i]][,"fcst"], x=trainingdata[,i],
                       fitted=c(rep(NA,lags),fits[,i])),class="forecast")
  acc_pes <- accuracy(fc,testdata[,1])
  acc_Dune <- accuracy(fc,testdata[,2])
  acc_cpi <- accuracy(fc,testdata[,3])
}
library(ggfortify)
predplot <- forecast::autoplot(stats::predict(v, n.ahead=3)) #forecast::autoplot

plotly::ggplotly(predplot)

autoplot(pesdat) +
  autolayer(predict(v, n.ahead=3))

reltable_pes <- data.frame(horizon = as.factor(c(1,1,1,3,3,3,6,6,6)), 
                       model = c("pes","cc","crisis","pes", "cc", "crisis","pes", "cc", "crisis"), 
                       rmse = c(0.3962482,0.08773341,6.8372466,0.3595160,0.1585272,4.3886801,0.5579713,0.8850608,3.1768921))

reltable_Dune <- data.frame(horizon = as.factor(c(1,1,1,3,3,3,6,6,6)), 
                           model = c("pes","cc","crisis","pes", "cc", "crisis","pes", "cc", "crisis"), 
                           rmse = c(0.5639840,0.7383306,0.3647817,0.9507732,0.9285637,0.8163448,1.0177657,0.9945709,1.0345714))

reltable_cpi <- data.frame(horizon = as.factor(c(1,1,1,3,3,3,6,6,6)), 
                           model = c("pes","cc","crisis","pes", "cc", "crisis","pes", "cc", "crisis"), 
                           rmse = c(0.0350968,0.2094434,0.1322960,0.5054660,0.5315527,0.1206312,0.6195726,0.6201764,0.1163663))

reltable <- data.frame(
  horizon = as.factor(c(1,1,1,3,3,3,6,6,6,1,1,1,3,3,3,6,6,6,1,1,1,3,3,3,6,6,6)), 
  model = c("pes","cc","crisis","pes", "cc", "crisis","pes", "cc", "crisis", 
            "pes","cc","crisis","pes", "cc", "crisis","pes", "cc", "crisis", 
            "pes","cc","crisis","pes", "cc", "crisis","pes", "cc", "crisis"),
  var = c(rep("pes",9),rep("Dune",9), rep("cpi",9)),
  rmse = c(0.3962482,0.08773341,6.8372466,0.3595160,0.1585272,4.3886801,0.5579713,0.8850608,3.1768921,
           0.5639840,0.7383306,0.3647817,0.9507732,0.9285637,0.8163448,1.0177657,0.9945709,1.0345714,
           0.0350968,0.2094434,0.1322960,0.5054660,0.5315527,0.1206312,0.6195726,0.6201764,0.1163663))


rel_mean <- data.frame(var = c("pes","Dune","cpi"), mean_val = c(0.6226604, 0.4588791, 0.3884595))

ggplot2::ggplot(reltable, aes(x=horizon, y = rmse, color=model, group=model))+
  facet_wrap(~var)+
  geom_point()+
  geom_line()+
  geom_hline(data=rel_mean, aes(yintercept = mean_val))

#### OUT OF SAMPLE ####

#fcst <- forecast(var6, h=12, dumvar = crisis)
pred_os <- predict(var6, n.ahead = 3)
pred_os_d <- predict(var6d, n.ahead = 3, dumvar = matrix(c(0,0,0,0,0,0,0,0,0), nrow=3, ncol=1))
plot(pred_os)
fanchart(pred_os)
forecast::autoplot(pred_os)

plot(pesdat)
dygraph(pred)

save(pesdat, ccdat, p, v, pred_os, var6, acc_pes, acc_Dune, acc_cpi, file="../gsv_data/r_data/var.RData")

load("./r_data/var.RData")
``