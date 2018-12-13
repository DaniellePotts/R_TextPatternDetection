sentiments <- tidy_messages %>% inner_join(get_sentiments("nrc"),by=(term="word"))

#remove data
sentiments$word <- NULL
head(sentiments)

head(sentiments)
#view the most frequent items
frequentItems <- eclat (sentiments, parameter = list(supp = 0.06, maxlen = 15)) 
#inspect most frequent
arules::inspect(frequentItems)

#run aprirori algorithm
rules <- apriori (sentiments, parameter = list(supp = 0.06, conf = 0.06))
#sort the outputs
rules_conf <- sort (rules, by="confidence", decreasing=TRUE)
rules_lift <- sort (rules, by="lift", decreasing=TRUE) 
rules
#inspect
arules::inspect(rules_lift)

#plot
plot(rules)
plot(rules, method="graph", control=list(type="items"))

plot(rules, method="graph", control=list(type="items"))