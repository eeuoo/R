library(ggiraph)
library(ggplot2)
library(ggiraphExtra)
library(dplyr)
library(tibble)
library(kormaps2014)
library(plotly)
library(dygraphs)
library(xts)
library(gridExtra)

library(psych)

library(sqldf)
library(RMySQL)

library(devtools)
library(KoNLP)

library(tm)
library(RColorBrewer)
library(wordcloud)

library(ggplot2)
theme_set(theme_gray(base_family="AppleGothic"))
par(family = "AppleGothic")

library(twitteR); library(RCurl); library(RJSONIO); library(stringr)

library(streamR); library(ROAuth)

library(arules); library(igraph); library(combinat)

library(arulesViz); library(visNetwork)

library(rvest); library(httr); library(stringr); library(dplyr)

data = load('data/score.csv')
kdata = load('data/kdata.rda')
