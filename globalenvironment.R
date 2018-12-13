#Global environment
#libraries, loaded in data, parsed messages
library(arulesViz)
library(arules)
library(jsonlite)
library(caret)
library(tm)

games <- fromJSON("games_formatted.json")
messages <- formatMessages(games,FALSE,FALSE,FALSE)
