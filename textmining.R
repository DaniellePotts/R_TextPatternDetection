messages <- formatMessages(games,FALSE,FALSE,FALSE)
messages <- removeData(messages,TRUE,TRUE,TRUE,TRUE)

#convert text to a vector, convert to a corpus and remove text elements such as punctionation, stop words and white spaces
text_vector <- VectorSource(messages)
#clean the text
corpus <- Corpus(text_vector)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus,removeWords,stopwords('english'))
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeWords,c("robot","human","hello"))

#convert to dtm then to a matrix
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)

#get the most frequent values by decreasing value
frequency <- colSums(dtm2)
frequency <- sort(frequency,decreasing = TRUE)

#display the world cloud
words <- names(frequency)
wordcloud(words[1:100],frequency[1:100])