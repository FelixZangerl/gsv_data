rm(list=ls())
load("./r_data/reliability.RData")
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
#  category = 265 # no results
)


ski <- ts_pick(ts_prcomp(ski), "PC1")
#ts_plot(ski)

write_csv(ski, "./tsgt/ski.csv")

##### HANDEL OFFLINE #####
handel_offline <- c("Shopping Center", "SCS", "SCN", "EKZ", "Geschäft")

handel_offline <- ts_gtrends(
  keyword = handel_offline,
  geo = geo,
  time = time,
  category = 18 # Shopping
)

ts_plot(handel_offline)
handel_offline <- (ts_pick(ts_prcomp(handel_offline), "PC1"))
handel_offline_w <- handel_offline %>% ts_frequency("week")
write_csv(handel_offline, "./tsgt/handel_offline.csv")

start <- min(handel_offline_w$time)
end <- max(time(handel_vs_vj))

handel_vs_vj <- handel_vs_vj %>% ts_data.frame()
handel_vs_vj$value <- as.numeric(handel_vs_vj$value)
handel_vs_vj$id <- "WWWI_Handel"
handel_vs_vj <- handel_vs_vj %>% filter(time >= start)
#a <- a %>% ts_xts()

handel_offline_w <- handel_offline_w %>% ts_data.frame()
handel_offline_w <- handel_offline_w %>% filter(time <= end)
#handel_offline_w <- handel_offline_w %>% ts_xts()

b <- rbind(handel_vs_vj, handel_offline_w)
b <- b %>% ts_xts()

dygraph(b)

##### BAUMARKT UND GARTEN OFFLINE #####
baumarkt <- c("dehner", "kika", "bellaflora", "xxxlutz", "ikea")

baumarkt <- ts_gtrends(
  keyword = baumarkt,
  geo     = geo,
  time = time,
  category = 650
)

#ts_plot(baumarkt)
baumarkt <- (ts_pick(ts_prcomp(baumarkt), "PC1"))

write_csv(baumarkt, "./tsgt/baumarkt.csv")

##### ELEKTRO OFFLINE #####
elektro <- c("Geizhals", "Mediamarkt", "willhaben", "Elektronik")

elektro <- ts_gtrends(
  keyword = elektro,
  geo     = geo,
  time    = time,
  category = 78 # Consumer Electronics
)

elektro <- (ts_pick(ts_prcomp(elektro), "PC1"))
#ts_plot(elektro)

write_csv(elektro, "./tsgt/elektro.csv")


##### KÖRERPERNAHE DIENSTLEISTUNG #####
dienstleistung <- c("Friseur", "Massage", "Nagelstudio")

dienstleistung <- ts_gtrends(
  keyword = dienstleistung,
  geo     = geo,
  time    = time
#  category = 145 # Spas and Beauty Services
)

dienstleistung <- (ts_pick(ts_prcomp(dienstleistung), "PC1"))
#ts_plot(dienstleistung)

write_csv(dienstleistung, "./tsgt/dienstleistung.csv")

##### GASTRONOMIE OFFLINE #####
gastro <- c("Öffnungszeiten", "Bar", "Restaurant", "Mittagsmenu", "Speisekarte")

gastro <- ts_gtrends(
  keyword = gastro,
  geo     = geo,
  time    = time,
  category = 71 # Food & Drink
)

gastro <- (ts_pick(ts_prcomp(gastro), "PC1"))
#ts_plot(gastro)

write_csv(gastro, "./tsgt/gastro.csv")
gastro_w <- gastro %>% ts_frequency("week")

start <- min(gastro_w$time)
end <- max(time(gastro_vs_vj))

gastro_vs_vj <- gastro_vs_vj %>% ts_data.frame()
gastro_vs_vj$value <- as.numeric(gastro_vs_vj$value)
gastro_vs_vj$id <- "WWWI_Gastro"
gastro_vs_vj <- gastro_vs_vj %>% filter(time >= start)
#a <- a %>% ts_xts()

gastro_w <- gastro_w %>% ts_data.frame()
gastro_w <- gastro_w %>% filter(time <= end)
#gastro_w <- gastro_w %>% ts_xts()

b <- rbind(gastro_vs_vj, gastro_w)
b <- b %>% ts_xts()

dygraph(b)


##### EINKAUFSZENTREN #####
ekz <- c("Mariahilferstraße", "Einkaufszentrum", "Herrengasse", "Getreidegasse")

ekz <- ts_gtrends(
  keyword = ekz,
  geo     = geo,
  time    = time,
  category = 18 # Shopping
)

#ts_plot(ekz)
ekz <- (ts_pick(ts_prcomp(ekz), "PC1"))

write_csv(ekz, "./tsgt/ekz.csv")

##### FITNESSCENTER #####
fitness <- c("FITINN", "McFit", "CrossFit", "Fitnesscenter", "John Harris")

fitness <- ts_gtrends(
  keyword = fitness,
  geo     = geo,
  time    = time,
  category = 94 # Fitness
)

fitness <- (ts_pick(ts_prcomp(fitness), "PC1"))
#ts_plot(fitness)

write_csv(fitness, "./tsgt/fitness.csv")

##### BIBLIOTHEKEN #####
bibliotheken <- c("bibliothek", "bib", "unibib", "städtische bücherei")

bibliotheken <- ts_gtrends(
  keyword = bibliotheken,
  geo     = geo,
  time    = time,
  category = 375
)

bibliotheken <- (ts_pick(ts_prcomp(bibliotheken), "PC1"))
#ts_plot(bibliotheken)

write_csv(bibliotheken, "./tsgt/bibliotheken.csv")

##### MOBILITÄT AUTO #####
mobilitaet_auto <- c("Spritpreisrechner", "Kfz Werkstatt", "Benzinpreis", "Kfz Versicherung", "ÖAMTC")

mobilitaet_auto <- ts_gtrends(
  keyword = mobilitaet_auto,
  geo     = geo,
  time    = time
)

mobilitaet_auto <- (ts_pick(ts_prcomp(mobilitaet_auto), "PC1"))
#ts_plot(mobilitaet_auto)

write_csv(mobilitaet_auto, "./tsgt/mobilitaet_auto.csv")