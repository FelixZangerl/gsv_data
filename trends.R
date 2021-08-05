library(trendecon)
library(tsbox)

geo <- "AT"
#
#### TRENDECON ####
keywords <- c("Wirtschaftskrise","Kurzarbeit","arbeitslos","Insolvenz")
#keywords <- "wirtschaftskrise"
#keywords <- c("arbeitslos","insolvenz")

proc_keyword_init(keywords[1], geo)
proc_keyword_init(keywords[2], geo)
proc_keyword_init(keywords[3], geo)
proc_keyword_init(keywords[4], geo)

proc_index(c("Wirtschaftskrise","Kurzarbeit","arbeitslos","Insolvenz"), 
           geo, "economic_sentiment")

proc_trendecon_at()

#### CLOTHING #####
clothing <- c("mango","zara","H&M","blue tomato","schuhe kaufen", "deichmann")

#proc_keyword_init(clothing[1], geo)
#proc_keyword_init(clothing[2], geo)
#proc_keyword_init(clothing[3], geo)
#proc_keyword_init(clothing[4], geo)
#proc_keyword_init(clothing[5], geo)
#proc_keyword_init(clothing[6], geo)

proc_index(clothing, geo, "clothing")

#### FOOD DELIVERY #####
food_delivery <- c("take away", "takeaway", "pizza bestellen")

#proc_keyword_init(food_delivery[1], geo)
#proc_keyword_init(food_delivery[2], geo)
#proc_keyword_init(food_delivery[3], geo)

proc_index(food_delivery, geo, "food_delivery")

#### HOME OFFICE #####
home_office <- c("headset", "monitor","maus","hdmi") 

#proc_keyword_init(home_office[1], geo)
#proc_keyword_init(home_office[2], geo)
#proc_keyword_init(home_office[3], geo)
#proc_keyword_init(home_office[4], geo)

proc_index(home_office, geo, "home_office")


#### GARDENING #####
gardening <- c("Heim+Hobby","Bau+Hobby","Bauhaus","hornbach","obi")

#proc_keyword_init(gardening[1], geo)
#proc_keyword_init(gardening[2], geo)
#proc_keyword_init(gardening[3], geo)
#proc_keyword_init(gardening[4], geo)
#proc_keyword_init(gardening[5], geo)

proc_index(gardening, geo, "gardening")

#### CULTURAL #####
cultural <- c("kino","theater","cinema","cineplexx","oper","konzert","oeticket")

#proc_keyword_init(cultural[1], geo)
#proc_keyword_init(cultural[2], geo)
#proc_keyword_init(cultural[3], geo)
#proc_keyword_init(cultural[4], geo)
#proc_keyword_init(cultural[5], geo)
#proc_keyword_init(cultural[6], geo)
#proc_keyword_init(cultural[7], geo)

proc_index(cultural, geo, "cultural")

#### TRAVEL #####
travel <- c("städtetrip","flug buchen","günstige flüge")

#proc_keyword_init(travel[1], geo)
#proc_keyword_init(travel[2], geo)
#proc_keyword_init(travel[3], geo)

proc_index(travel, geo, "travel")

#### MOBILITY #####
mobility <- c("Fahrplan","taxi","sixt","google maps")

#proc_keyword_init(mobility[1], geo)
#proc_keyword_init(mobility[2], geo)
#proc_keyword_init(mobility[3], geo)
#proc_keyword_init(mobility[4], geo)

proc_index(mobility, geo, "mobility")

#### LUXURY #####
luxury <- c("juwelier","swarovski","uhr","uhren","christ","feichtinger")

#proc_keyword_init(luxury[1], geo)
#proc_keyword_init(luxury[2], geo)
#proc_keyword_init(luxury[3], geo)
#proc_keyword_init(luxury[4], geo)
#proc_keyword_init(luxury[5], geo)
#proc_keyword_init(luxury[6], geo)

proc_index(luxury, geo, "luxury")

### TESTS
#x <- ts_gtrends_mwd("schlange", geo = "AT")
#ts_plot(x)
#
#ts_plot(economic_sentiment_sa)

krise2 <- c("Krise", "Abschwung", "arbeitslos", "Rezession")

proc_keyword_init(krise2[1], geo)
proc_keyword_init(krise2[2], geo)
proc_keyword_init(krise2[3], geo)
proc_keyword_init(krise2[4], geo)

proc_index(krise2, geo, "krise2")

