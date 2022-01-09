### SEMANTISCHE CHECKS #####

#### UMLAUTE ######
umlaute <- c("Mobilität", "Mobilitaet")

temp <- ts_gtrends(
  keyword = umlaute,
  geo     = geo,
  time    = time
)

#ts_plot(temp)

#### GROSSKLEIN ######
großklein <- c("SCS", "scs")
großklein <- c("KFZ", "kfz")
großklein <- c("Skifahren", "skifahren")
großklein <- c("Kurzarbeit", "kurzarbeit")

temp <- ts_gtrends(
  keyword = großklein,
  geo     = geo,
  time    = time
)

#ts_plot(temp)

#### ABSTÄNDE ######
abstaende <- c("Ski fahren", "Skifahren")

temp <- ts_gtrends(
  keyword = abstaende,
  geo     = geo,
  time    = time
)

#ts_plot(temp)

#### SCHREIBWEISE ######
schreibweise <- c("Schifahren", "Skifahren")

temp <- ts_gtrends(
  keyword = schreibweise,
  geo     = geo,
  time    = time
)

#ts_plot(temp)

keywords <- c("Wirtschaftskrise","Kurzarbeit","arbeitslos","Insolvenz")

temp <- ts_gtrends(
  keyword = t,
  geo     = geo,
  time    = time
)

#ts_plot(temp)
