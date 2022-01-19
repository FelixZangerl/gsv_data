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

# constant 0: 
# corona: Krisenhilfen, Coronahilfen
# ekz: "Kärtner Straße"
#corona <- c("Krisenhilfen", "Cofag", "Coronahilfen", "Stundungen", "Kredite", "Inzidenz", "Coronafaelle")
corona <- c("Cofag", "Stundungen", "Kredite", "Inzidenz", "Coronafälle")

corona <- ts_gtrends(
  keyword = corona,
  geo     = "AT",
  time    = time
)

write_csv(corona, "./tsgt/corona_comp.csv")

#ts_plot(corona)
corona <- ts_pick(ts_prcomp(corona), "PC1")

write_csv(corona, "./tsgt/corona.csv")

##### SKI #####
ski <- c("Urlaub Tirol", "Skiurlaub Österreich", "Ischgl", "Winterurlaub", "Skifahren Österreich")

ski <- ts_gtrends(
  keyword = ski,
  geo     = geo,
  time    = time
)

#ts_plot(ski)
ski <- ts_pick(ts_prcomp(ski), "PC1")

write_csv(ski, "./tsgt/ski.csv")

##### HANDEL OFFLINE #####
handel_offline <- c("Shopping Center", "SCS", "SCN", "EKZ", "Geschäft")

handel_offline <- ts_gtrends(
  keyword = handel_offline,
  geo     = geo,
  time = time
)

#ts_plot(handel_offline)
handel_offline <- (ts_pick(ts_prcomp(handel_offline), "PC1"))

write_csv(handel_offline, "./tsgt/handel_offline.csv")

##### BAUMARKT UND GARTEN OFFLINE #####
baumarkt <- c("dehner","b&b", "kika", "leiner", "bellaflora", "xxxlutz", "ikea")

baumarkt <- ts_gtrends(
  keyword = baumarkt,
  geo     = geo,
  time = time
)

#ts_plot(baumarkt)
baumarkt <- (ts_pick(ts_prcomp(baumarkt), "PC1"))

write_csv(baumarkt, "./tsgt/baumarkt.csv")

##### ELEKTRO OFFLINE #####
elektro <- c("Geizhals", "Mediamarkt", "e-tec", "willhaben", "Elektronik")

elektro <- ts_gtrends(
  keyword = elektro,
  geo     = geo,
  time    = time
)

#ts_plot(elektro)
elektro <- (ts_pick(ts_prcomp(elektro), "PC1"))

write_csv(elektro, "./tsgt/elektro.csv")


##### KÖRERPERNAHE DIENSTLEISTUNG #####
dienstleistung <- c("friseur", "massage", "nagelstudio")

dienstleistung <- ts_gtrends(
  keyword = dienstleistung,
  geo     = geo,
  time    = time
)

#ts_plot(dienstleistung)
dienstleistung <- (ts_pick(ts_prcomp(dienstleistung), "PC1"))

write_csv(dienstleistung, "./tsgt/dienstleistung.csv")

##### GASTRONOMIE OFFLINE #####
gastro <- c("Öffnungszeiten", "Bar", "Restaurant", "Mittagsmenu", "Speisekarte")

gastro <- ts_gtrends(
  keyword = gastro,
  geo     = geo,
  time    = time
)

#ts_plot(gastro)
gastro <- (ts_pick(ts_prcomp(gastro), "PC1"))

write_csv(gastro, "./tsgt/gastro.csv")

##### EINKAUFSZENTREN #####
ekz <- c("Mariahilferstraße", "Einkaufszentrum", "Herrengasse", "Getreidegasse")

ekz <- ts_gtrends(
  keyword = ekz,
  geo     = geo,
  time    = time
)

#ts_plot(ekz)
ekz <- (ts_pick(ts_prcomp(ekz), "PC1"))

write_csv(ekz, "./tsgt/ekz.csv")

##### FITNESSCENTER #####
fitness <- c("fitinn", "mcfit", "crossfit", "fitnesscenter", "john harris")

fitness <- ts_gtrends(
  keyword = fitness,
  geo     = geo,
  time    = time
)

#ts_plot(fitness)
fitness <- (ts_pick(ts_prcomp(fitness), "PC1"))

write_csv(fitness, "./tsgt/fitness.csv")

##### BIBLIOTHEKEN #####
bibliotheken <- c("bibliothek", "bib", "unibib", "städtische bücherei")

bibliotheken <- ts_gtrends(
  keyword = bibliotheken,
  geo     = geo,
  time    = time
)

#ts_plot(bibliotheken)
bibliotheken <- (ts_pick(ts_prcomp(bibliotheken), "PC1"))

write_csv(bibliotheken, "./tsgt/bibliotheken.csv")

##### MOBILITÄT AUTO #####
mobilitaet_auto <- c("Spritpreisrechner", "kfz werkstatt", "Benzinpreis", "Autobahnvignette", "kfz Versicherung")

mobilitaet_auto <- ts_gtrends(
  keyword = mobilitaet_auto,
  geo     = geo,
  time    = time
)

#ts_plot(mobilitaet_auto)
mobilitaet_auto <- (ts_pick(ts_prcomp(mobilitaet_auto), "PC1"))

write_csv(mobilitaet_auto, "./tsgt/mobilitaet_auto.csv")