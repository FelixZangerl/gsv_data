library(trendecon)
library(tsbox)
library(tempdisagg)
library(readr)
### NEW INDICATORS MOMENT #####

##### CORONA #####
#start <- "2014-01-01"
geo <- "AT"
today <- as.character(Sys.Date())
time <- "today 3-m" 

# constant 0: Krisenhilfen, Coronahilfen
#corona <- c("Krisenhilfen", "Cofag", "Coronahilfen", "Stundungen", "Kredite", "Inzidenz", "Coronafaelle")
corona <- c("Cofag", "Stundungen", "Kredite", "Inzidenz", "Coronafälle")

corona <- ts_gtrends(
  keyword = corona,
  geo     = "AT",
  time    = time
)

ts_plot(corona)
corona <- ts_pick(ts_prcomp(corona), "PC1")

write_csv(corona, "./tsgt/corona.csv")

dygraph(corona %>% ts_xts() ,group = "keywords")%>%
  dyAxis("x", drawGrid = FALSE)%>%
  dySeries("PC1", label = "Index")%>%
  dyEvent("2021-6-10", "Easing measures - less social distancing", labelLoc = "bottom")%>%
  dyEvent("2021-7-01", "Easing measures - night gastronomy", labelLoc = "bottom")%>%
  dyEvent("2021-11-22", "Lockdown #4", labelLoc = "bottom")%>%
  dyEvent("2021-12-12", "End of Lockdown #4", labelLoc = "bottom")%>%
  dyRangeSelector(dateWindow = c("2021-10-01", today))%>%
  dyOptions(useDataTimezone = TRUE)

##### SKI #####
ski <- c("Urlaub Tirol", "Skiurlaub Österreich", "Ischgl", "Winterurlaub", "Skifahren Österreich")

ski <- ts_gtrends(
  keyword = ski,
  geo     = "AT",
  time    = time
)

ts_plot(ski)
ski <- ts_pick(ts_prcomp(gt_data), "PC1")

write_csv(ski, "./tsgt/ski.csv")

##### HANDEL OFFLINE #####
handel_offline <- c("Shopping Center", "SCS", "SCN", "EKZ", "Geschäft")

handel_offline <- ts_gtrends(
  keyword = handel_offline,
  geo     = "AT",
)

ts_plot(handel_offline)
handel_offline <- (ts_pick(ts_prcomp(gt_data), "PC1"))

write_csv(handel_offline, "./tsgt/handel_offline.csv")

##### GASTRONOMIE OFFLINE #####
gastro <- c("Oeffnungszeiten", "Bar", "Restaurant", "Mittagsmenu", "Speisekarte")

gastro <- ts_gtrends(
  keyword = gastro,
  geo     = "AT",
  time    = time
)

ts_plot(gastro)
gastro <- (ts_pick(ts_prcomp(gastro), "PC1"))

write_csv(gastro, "./tsgt/gastro.csv")

##### ELEKTRO OFFLINE #####
elektro <- c("Geizhals", "Mediamarkt", "e-tec", "willhaben", "Elektronik")

##### DIENSTLEISTUNG OFFLINE #####
dienstleistung <- c("friseur", "massage")

dienstleistung <- ts_gtrends(
  keyword = dienstleistung,
  geo     = "AT",
  time    = time
)

ts_plot(dienstleistung)
dienstleistung <- (ts_pick(ts_prcomp(gt_data), "PC1"))

write_csv(dienstleistung, "./tsgt/dienstleistung.csv")

##### GASTRONOMIE OFFLINE #####
##### GASTRONOMIE OFFLINE #####
##### GASTRONOMIE OFFLINE #####
##### GASTRONOMIE OFFLINE #####
