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

##### SKI #####
ski <- c("Urlaub Tirol", "Skiurlaub Österreich", "Ischgl", "Winterurlaub", "Skifahren Österreich")

ski <- ts_gtrends(
  keyword = ski,
  geo     = "AT",
  time    = time
)

ts_plot(ski)
ski <- ts_pick(ts_prcomp(ski), "PC1")

write_csv(ski, "./tsgt/ski.csv")

##### HANDEL OFFLINE #####
handel_offline <- c("Shopping Center", "SCS", "SCN", "EKZ", "Geschäft")

handel_offline <- ts_gtrends(
  keyword = handel_offline,
  geo     = "AT",
)

ts_plot(handel_offline)
handel_offline <- (ts_pick(ts_prcomp(handel_offline), "PC1"))

write_csv(handel_offline, "./tsgt/handel_offline.csv")

##### BAUMARKT UND GARTEN OFFLINE #####
baumarkt <- c("dehner","b&b", "kika", "leiner", "bellaflora", "xxxlutz", "ikea")

baumarkt <- ts_gtrends(
  keyword = baumarkt,
  geo     = "AT",
)

ts_plot(baumarkt)
baumarkt <- (ts_pick(ts_prcomp(baumarkt), "PC1"))

write_csv(baumarkt, "./tsgt/baumarkt.csv")

##### ELEKTRO OFFLINE #####
elektro <- c("Geizhals", "Mediamarkt", "e-tec", "willhaben", "Elektronik")

elektro <- ts_gtrends(
  keyword = elektro,
  geo     = "AT",
  time    = time
)

ts_plot(elektro)
elektro <- (ts_pick(ts_prcomp(elektro), "PC1"))

write_csv(elektro, "./tsgt/elektro.csv")


##### KÖRERPERNAHE DIENSTLEISTUNG #####
dienstleistung <- c("friseur", "massage", "nagelstudio")

dienstleistung <- ts_gtrends(
  keyword = dienstleistung,
  geo     = "AT",
  time    = time
)

ts_plot(dienstleistung)
dienstleistung <- (ts_pick(ts_prcomp(dienstleistung), "PC1"))

write_csv(dienstleistung, "./tsgt/dienstleistung.csv")

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

##### EINKAUFSZENTREN #####
ekz <- c("Mariahilferstraße", "Einkaufszentrum", "Kärtner Straße", "Herrengasse", "Getreidegasse")

ekz <- ts_gtrends(
  keyword = ekz,
  geo     = "AT",
  time    = time
)

ts_plot(ekz)
ekz <- (ts_pick(ts_prcomp(ekz), "PC1"))

write_csv(ekz, "./tsgt/ekz.csv")

##### FITNESSCENTER #####
fitness <- c("fitinn", "mcfit", "crossfit", "fitnesscenter", "john harris")

fitness <- ts_gtrends(
  keyword = fitness,
  geo     = "AT",
  time    = time
)

ts_plot(fitness)
fitness <- (ts_pick(ts_prcomp(fitness), "PC1"))

write_csv(fitness, "./tsgt/fitness.csv")

##### BIBLIOTHEKEN #####
bibliotheken <- c("bibliothek", "bib", "unibib", "städtische bücherei")

bibliotheken <- ts_gtrends(
  keyword = bibliotheken,
  geo     = "AT",
  time    = time
)

ts_plot(bibliotheken)
bibliotheken <- (ts_pick(ts_prcomp(bibliotheken), "PC1"))

write_csv(bibliotheken, "./tsgt/bibliotheken.csv")

##### MOBILITÄT AUTO #####
mobilität_auto <- c("Tankpreisrechner", "kfz werkstatt", "Benzinpreis", "Autobahnvignette", "kfz Versicherung")

mobilität_auto <- ts_gtrends(
  keyword = mobilität_auto,
  geo     = "AT",
  time    = time
)

##### SEMANTISCHE CHECKS #####



ts_plot(mobilität_auto)
mobilität_auto <- (ts_pick(ts_prcomp(mobilität_auto), "PC1"))

write_csv(mobilität_auto, "./tsgt/mobilität_auto.csv")