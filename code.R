library(trendecon)
library(tsbox)

x <- ts_gtrends("ken jebsen", geo = "AT")
#> Downloading data for today+5-y
tsbox::ts_plot(x)
x <- ts_xts(x)

dygraph(x,group = "keywords",
        main = "Ken Jebsen - GTrends")%>%
  dyAxis("x", drawGrid = FALSE)%>%
  dySeries("value", label = "Index")


keywords <- c("wirtschaftskrise","kurzarbeit","arbeitslos","insolvenz")

x <- ts_gtrends(keywords, geo="AT")
tsbox::ts_plot(x)

ts_plot(ts_pick(ts_prcomp(x), "PC1"))
