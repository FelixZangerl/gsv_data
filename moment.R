rm(list=ls())
load("~/Documents/proj/ma_thesis/gsv_data/r_data/reliability.RData")
library(trendecon)
library(tsbox)
library(tempdisagg)
library(dplyr)
library(readr)
### NEW INDICATORS MOMENT #####

#start <- "2014-01-01"
geo <- "AT"
today <- as.character(Sys.Date())
time <- "today 3-m" 

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

tmp <- read_csv("./tsgt/handel_offline.csv")
handel_offline <- full_join(handel_offline, tmp)
handel_offline <- handel_offline[!duplicated(handel_offline$time),]
write_csv(handel_offline, "./tsgt/handel_offline.csv")

###### WEEKLY DATA COMPARISION ######
handel_offline_w <- handel_offline %>% ts_frequency("week")

start <- min(handel_offline_w$time)
tmp <- handel_vs_vj %>% ts_data.frame()
end <- max(tmp$time)
#cat("/n!!!!!!!!",end,"/n")

handel_vs_vj <- handel_vs_vj %>% ts_data.frame()
handel_vs_vj$value <- as.numeric(handel_vs_vj$value)
handel_vs_vj$id <- "WWWI_Handel"
handel_vs_vj <- handel_vs_vj %>% filter(time >= start)
#a <- a %>% ts_xts()

handel_offline_w <- handel_offline_w %>% ts_data.frame()
handel_offline_w <- handel_offline_w %>% dplyr::filter(time <= end) # problem here 0 rows
#handel_offline_w <- handel_offline_w %>% ts_xts()

a <- rbind(handel_vs_vj, handel_offline_w)
#b <- b %>% ts_xts()

cat("Write a with", nrow(a), "\n")
cat("handel_vs_vj with" ,nrow(handel_vs_vj), "rows")
cat("handel_offline_w with" ,nrow(handel_offline_w), "rows")
write_csv(a, file = "./tsgt/handel_offline_vgr.csv")

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

tmp <- read_csv("./tsgt/gastro.csv")
gastro <- full_join(gastro, tmp)
gastro <- gastro[!duplicated(gastro$time),]
write_csv(gastro, "./tsgt/gastro.csv")

###### WEEKLY DATA COMPARISION ######
gastro_w <- gastro %>% ts_frequency("week")

start <- min(gastro_w$time)
end <- max(time(gastro_vs_vj))
#cat("/n!!!!!!!!",end,"/n")

gastro_vs_vj <- gastro_vs_vj %>% ts_data.frame()
gastro_vs_vj$value <- as.numeric(gastro_vs_vj$value)
gastro_vs_vj$id <- "WWWI_Gastro"
gastro_vs_vj <- gastro_vs_vj %>% filter(time >= start)
#a <- a %>% ts_xts()

gastro_w <- gastro_w %>% ts_data.frame()
gastro_w <- gastro_w %>% dplyr::filter(time <= end)
#gastro_w <- gastro_w %>% ts_xts()

b <- rbind(gastro_vs_vj, gastro_w)
#b <- b %>% ts_xts()

cat("Write b with", nrow(b), " \n")
write_csv(b, file = "./tsgt/gastro_vgr.csv")

#dygraph(b)

##### CORONA #####
# constant 0: 
# corona: Krisenhilfen, Coronahilfen
# ekz: "Kärtner Straße"
#corona <- c("Krisenhilfen", "Cofag", "Coronahilfen", "Stundungen", "Kredite", "Inzidenz", "Coronafaelle")
corona <- c("Cofag", "Kredite", "Inzidenz", "Coronafälle")

corona <- ts_gtrends(
  keyword = corona,
  geo     = "AT",
  time    = time
)

write_csv(corona, "./tsgt/corona_comp.csv")

#ts_plot(corona)
corona <- ts_pick(ts_prcomp(corona), "PC1")

tmp <- read_csv("./tsgt/corona.csv")
corona <- full_join(corona, tmp)
corona <- corona[!duplicated(corona$time),]
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

tmp <- read_csv("./tsgt/ski.csv")
ski <- full_join(tmp, ski)
ski <- ski[!duplicated(ski$time),]

write_csv(ski, "./tsgt/ski.csv")


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

tmp <- read_csv("./tsgt/baumarkt.csv")
baumarkt <- full_join(baumarkt, tmp)
baumarkt <- baumarkt[!duplicated(baumarkt$time),]
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

tmp <- read_csv("./tsgt/elektro.csv")
elektro <- full_join(elektro, tmp)
elektro <- elektro[!duplicated(elektro$time),]
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

tmp <- read_csv("./tsgt/dienstleistung.csv")
dienstleistung <- full_join(dienstleistung, tmp)
dienstleistung <- dienstleistung[!duplicated(dienstleistung$time),]
write_csv(dienstleistung, "./tsgt/dienstleistung.csv")


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

tmp <- read_csv("./tsgt/ekz.csv")
ekz <- full_join(ekz, tmp)
ekz <- ekz[!duplicated(ekz$time),]
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

tmp <- read_csv("./tsgt/fitness.csv")
fitness <- full_join(fitness, tmp)
fitness <- fitness[!duplicated(fitness$time),]
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

tmp <- read_csv("./tsgt/bibliotheken.csv")
bibliotheken <- full_join(bibliotheken, tmp)
bibliotheken <- bibliotheken[!duplicated(bibliotheken$time),]
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

tmp <- read_csv("./tsgt/mobilitaet_auto.csv")
mobilitaet_auto <- full_join(mobilitaet_auto, tmp)
mobilitaet_auto <- mobilitaet_auto[!duplicated(mobilitaet_auto$time),]
write_csv(mobilitaet_auto, "./tsgt/mobilitaet_auto.csv")

cat("Wrote a/b with", nrow(a), nrow(b), "rows")