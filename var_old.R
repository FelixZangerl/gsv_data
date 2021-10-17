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


### BRING ALL TO SAME TIME HORIZON ####
start <- start(pes)
end <- end(une)

int <- window(int, start=start, end=end) %>% ts_ts()
cc <- window(cc, start=start, end=end) %>% ts_ts()
une <- window(une, start=start, end=end) %>% ts_ts()
pes <- window(pes, start=start, end=end) %>% ts_ts()
cpi <- window(p, start=start, end=end) %>% ts_ts()

#### CORRELATIONS #####

composite_m <- cbind(pes, cc, une, cpi)

composite_m %>%
  as.data.frame() %>%
  GGally::ggpairs() #var_pairs.png

Dint <- diff(int) # int not stationary
Dcc <- diff(cc) # cc stationary
Dune <- diff(une) # une not stationary
Dpes <- diff(pes) # pes stationary
Dcpi <- diff(cpi) # cpi stationary

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

### VAR ##########
## check length 2007-01 until 2021-02: 12*14+2 = 170
vardat <- matrix(data=c(pes,cc,Dune,cpi), nrow = length(pes), ncol=4, byrow=FALSE)
colnames(vardat) <- c("pes","cc","Dune","cpi")
# print(xtable::xtable(vardat))
library(vars)
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

library(stargazer)
library(latex2exp)
var.UR <- VAR(vardat,  type="none" , lag.max = 5, ic = c("SC"))
stargazer(var.UR$varresult, type = "latex", header = F, title = "unrestricted VAR(1)", label="tab:star1", 
          column.labels = c("$pes$","$cc$","$\\Delta{une}$","$cpi$"), 
          dep.var.caption = "dependent variables: $pes, cc, \\Delta{une}, cpi$", 
          dep.var.labels = c(""), 
          covariate.labels = c("$pes_{t-1}$","$cc_{t-1}$","$\\Delta{une_{t-1}}$","$int_{t-1}$","constant"))

### RESIDUAL DIAGNOSTICS ####

sum2 <- data.frame(
  var.UR$varresult$pes$residuals,
  var.UR$varresult$cc$residuals,
  var.UR$varresult$Dune$residuals,
  var.UR$varresult$cpi$residuals)

colnames(sum2) <- c("$pes$ residuals","$cc$ residuals","$\\Delta{une}$ residuals","$cpi$ residuals")

print(xtable::autoformat(xtable::xtable(summary(sum2),auto=T,floating=F,caption="Residual Summary",label="tab:res")),comment=F,include.rownames = F,sanitize.text.function=function(sum2){sum2})

adf.test(var.UR$varresult$pes$residuals)
adf.test(var.UR$varresult$cc$residuals)
adf.test(var.UR$varresult$Dune$residuals)
adf.test(var.UR$varresult$cpi$residuals)

Normtest <- normality.test(var.UR) # H0: normality
print(Normtest)
plot(Normtest)

ser.test <- serial.test(var.UR) # H0: no serial correlation
ser.test

### CHOLESKY DECOMP ####

colnames(sum2) <- c("pes residuals","cc residuals","une residuals","int residuals")
Matrix=matrix(c((var(sum2$`pes residuals`)),
                cov(sum2$`pes residuals`,sum2$`cc residuals`),
                cov(sum2$`pes residuals`,sum2$`une residuals`),
                cov(sum2$`pes residuals`,sum2$`int residuals`), 
                cov(sum2$`pes residuals`,sum2$`cc residuals`),
                var(sum2$`cc residuals`),
                cov(sum2$`cc residuals`,sum2$`une residuals`),
                cov(sum2$`cc residuals`,sum2$`int residuals`), 
                cov(sum2$`pes residuals`,sum2$`une residuals`), 
                cov(sum2$`cc residuals`,sum2$`une residuals`),
                var(sum2$`une residuals`),
                cov(sum2$`cc residuals`,sum2$`int residuals`),
                cov(sum2$`pes residuals`,sum2$`int residuals`),
                cov(sum2$`cc residuals`,sum2$`int residuals`),
                cov(sum2$`p`,sum2$`int residuals`),
                var(sum2$`int residuals`)), nrow=4, ncol=4)

colnames(Matrix) <- c("$\\Delta{y}$","$\\Delta{int}$","$\\Delta{p}$","$\\Delta{M1}$")
rownames(Matrix) <- c("$\\Delta{y}$","$\\Delta{int}$","$\\Delta{p}$","$\\Delta{M1}$") 

#Matrix
L <- t(chol(Matrix))
#print(xtable(L, digits = 5),comment=F)
print(xtable::autoformat(xtable::xtable(L,auto=T,floating=F,caption="Cholesky Decomposition Residuals",label="tab:chol")),comment=F,include.rownames = T,sanitize.text.function=function(L){L})

#### zero long run restrictions #####
BQm <- BQ(var.UR)
BQm

#### zero short run restrictions #####
resmat <- matrix(c(1,0,0,0,1,1,1,0,0,1,1,1,1,0,1,1,1,1,1,1), ncol=5, nrow= 4, byrow = TRUE) # with const
resmat <- matrix(c(1,0,0,0,1,1,1,0,0,1,1,1,1,0,1,1), ncol=4, nrow= 4, byrow = TRUE) # without const

var.SRR <- restrict(var.UR, method="manual", resmat= resmat)
var.SRR <- restrict(var.UR, method="ser")
summary(var.SRR)

### IRF ####
irf.pes <- irf(var.SRR, impulse = "pes", response = "pes", boot = TRUE)
irf.cc <- irf(var.SRR, impulse = "pes", response = "cc", boot = TRUE)
irf.Dune <- irf(var.SRR, impulse = "pes", response = "Dune", boot = TRUE)
irf.cpi <- irf(var.SRR, impulse = "pes", response = "cpi", boot = TRUE)

par(mfrow=c(2,2))
plot(irf.pes)
plot(irf.cc)
plot(irf.Dune)
plot(irf.cpi)

save(irf.pes, irf.cc, irf.Dune, irf.cpi, var.UR, file="./r_data/var_old.RData")


#### FORECAST ACCURACY #####
library(fpp3)
library(fable)

pdat <- pesdat %>% as_tsibble(pivot_longer = FALSE)

fit <- pdat %>%
  model(
    aicc = VAR(vars(pes, Dune, cpi)),
    bic = VAR(vars(pes, Dune, cpi), ic = "bic")
  )
fit

fit %>%
  augment() %>%
  ACF(.innov) %>%
  autoplot()

fit %>%
  dplyr::select(aicc) %>%
  forecast() %>%
  autoplot(pdat %>% filter(year(index) > 2010))

fit$aicc$res

breusch_godfrey(fit$aicc$model)

residuals <- residuals(fit)
vars::serial.test(resid = residuals)
breusch_godfrey(fit$aicc$resid)
lmtest::bgtest(residuals)
Box.test(residuals)
