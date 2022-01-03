### NEW INDICATORS MOMENT #####

##### CORONA #####
start <- "2014-01-01"

corona <- c("Krisenhilfen", "Cofag", "Coronahilfen", "Stundungen", "Kredite", "Inzidenz", "Coronafaelle")
#corona <- c("Coronahilfen", "Stundungen", "Kredite", "Inzidenz", "Coronafälle")

proc_keyword_init(corona[1], geo, from = start)
proc_keyword_init(corona[2], geo, from = start)
proc_keyword_init(corona[3], geo, from = start)
proc_keyword_init(corona[4], geo, from = start)
proc_keyword_init(corona[5], geo, from = start)
proc_keyword_init(corona[6], geo, from = start)
proc_keyword_init(corona[7], geo, from = start)

proc_index(corona, geo, "corona")

##### SKI #####
ski <- c("Urlaub Tirol", "Skiurlaub Österreich", "Ischgl", "Winterurlaub", "Skifahren Österreich")

proc_keyword_init(ski[1], geo, from = start)
proc_keyword_init(ski[2], geo, from = start)
proc_keyword_init(ski[3], geo, from = start)
proc_keyword_init(ski[4], geo, from = start)
proc_keyword_init(ski[5], geo, from = start)
proc_keyword_init(ski[6], geo, from = start)

proc_index(ski, geo, "ski")

##### HANDEL OFFLINE #####
handel_offline <- c("Shopping Center", "SCS", "SCN", "EKZ", "Geschäft")

proc_keyword_init(handel_offline[1], geo, from = start)
proc_keyword_init(handel_offline[2], geo, from = start)
proc_keyword_init(handel_offline[3], geo, from = start)
proc_keyword_init(handel_offline[4], geo, from = start)
proc_keyword_init(handel_offline[5], geo, from = start)

proc_index(handel_offline, geo, "handel_offline")