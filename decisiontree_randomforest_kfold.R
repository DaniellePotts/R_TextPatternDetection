messages <- formatMessages(games,FALSE,FALSE,FALSE)

messages$from <- NULL
messages$date <- NULL
messages$predictedAs <- NULL

messages$correct <- factor(messages$correct)

#pre-process
corpus <- Corpus(VectorSource(messages$text))
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeWords,c(stopwords("english")))
corpus <- tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus,stemDocument)

#optional
corpus <- tm_map(corpus,removeWords,c("robot","human"))

frequencies <- DocumentTermMatrix(corpus)

findFreqTerms(frequencies,lowfreq = 150)

sparseterms <- removeSparseTerms(frequencies,.99)
sparseterms <- as.data.frame(as.matrix(sparseterms))

colnames(sparseterms) <- make.names(colnames(sparseterms))

str(sparseterms)

sparseterms$correct <- messages$correct

split <- sample.split(sparseterms$correct,0.65)
train <- subset(sparseterms,split==T)
test <- subset(sparseterms,split=F)

index <- base::sample(1:nrow(sparseterms),0.7*nrow(sparseterms))
train <- sparseterms[index,]
test <- sparseterms[-index,]

#decision tree - train the model
tree.model <- rpart(correct~.,data=train,method = "class",minbucket=3)

#test the accuracy
tree.predict <- predict(tree.model,test,type="class")
#build a dataframe on the results
rpart.accuracy.table <- as.data.frame(table(test$correct, tree.predict))
print(paste("Deicision tree accuracy:",
            100*round(((rpart.accuracy.table$Freq[1]+rpart.accuracy.table$Freq[4])/nrow(test)), 4),
            "%"))

#random forest - train the model
rf.model <- randomForest(correct~.,data=train,ntree=400,mtry=15,importance=T)
head(importance(rf.model))

#the prediction model
rf.predict <- predict(rf.model,test,type="class")

#convert accuracy rating of each correct type to a table
accuracyTable <- as.data.frame(table(test$correct,rf.predict))

print(paste("Random forest accuracy:",
            100*round(((accuracyTable$Freq[1]+accuracyTable$Freq[4])/nrow(test)), 4),
            "%"))

rf.predict <- predict(rf.model,test,type="prob")
table(test$correct,rf.predict[,2]>0.7)

#using sentiments instead

#tokenize the data
tidy_messages <- messages %>% unnest_tokens(word, text)

#get sentiments from the bing/nrc datasets, both provide better accuracy than afinn dataset -- 
sentiments <- tidy_messages %>% inner_join(get_sentiments("bing"),by=(term="word"))
#remove term, only focusing on the sentiment itself
sentiments$word <- NULL
sentiments$correct <- as.factor(sentiments$correct)
sentiments$sentiment <- as.factor(sentiments$sentiment)
head(sentiments)

index <- base::sample(1:nrow(sentiments),0.7*nrow(sentiments))
train <- sentiments[index,]
test <- sentiments[-index,]

tree.model <- rpart(correct~.,data=train,method = "class",minbucket=13)

#test the accuracy
tree.predict <- predict(tree.model,test,type="class")
#build a dataframe on the results
rpart.accuracy.table <- as.data.frame(table(test$correct, tree.predict))
print(paste("Deicision tree accuracy:",
            100*round(((rpart.accuracy.table$Freq[1]+rpart.accuracy.table$Freq[4])/nrow(test)), 4),
            "%"))

#random forest - train the model
rf.model <- randomForest(correct~.,data=train,ntree=300,mtry=14,importance=T)
head(importance(rf.model))

#the prediction model
rf.predict <- predict(rf.model,test,type="class")

#convert accuracy rating of each correct type to a table
accuracyTable <- as.data.frame(table(test$correct,rf.predict))

print(paste("Random forest accuracy:",
            100*round(((accuracyTable$Freq[1]+accuracyTable$Freq[4])/nrow(test)), 4),
            "%"))

rf.predict <- predict(rf.model,test,type="prob")
table(test$correct,rf.predict[,2]>0.7)

#kfold sample
folds <- 10
sentiments <- sentiments[sample(nrow(sentiments)),]
folds <- cut(seq(1,nrow(sentiments)),breaks=folds,labels=FALSE)
for(i in 1:folds){
 
  index <- which(folds==i,arr.ind=TRUE)
  test <- sentiments[index, ]
  train <- sentiments[-index, ]

}
