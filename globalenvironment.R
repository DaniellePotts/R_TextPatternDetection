#Global environment
#libraries, loaded in data, parsed messages

install.packages(c("arulesViz","arules","jsonlite","caret","tm","wordcloud","tidytext","ggplot2","sentimentr","quanteda","randomForest",
                   "dplyr","stringr","catTools","SnowballC","proxy","dbscan","rpart","rpart.plot","RColorBrewer","hunspell"))

library(arulesViz)
library(arules)
library(jsonlite)
library(caret)
library(tm)
library(wordcloud)
library(tidytext)
library(ggplot2)
library(sentimentr)
library(quanteda)
library(randomForest)
library(dplyr)
library(stringr)
library(caTools)
library(SnowballC)
library(proxy)
library(dbscan)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(hunspell)

games <- fromJSON("games_formatted.json")
messages <- formatMessages(games,FALSE,FALSE,FALSE)
