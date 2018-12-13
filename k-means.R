messages <- formatMessages(games,FALSE,FALSE,FALSE)
messages$date <- NULL
messages$from <- NULL
messages$predictedAs <- NULL

messages <- head(messages,2000)
corpus = Corpus(VectorSource(messages$text))

corpus.cleaned <- tm_map(corpus,removeWords,stopwords('english'))
corpus.cleaned <- tm_map(corpus,stemDocument,language="english")
corpus.cleaned <- tm_map(corpus.cleaned,stripWhitespace)

tdm <- DocumentTermMatrix(corpus.cleaned)
tdm.tfidf <- weightTfIdf(tdm)

tdm.tfidf <- removeSparseTerms(tdm.tfidf,0.999)
tfidf.matrix <- as.matrix(tdm.tfidf)

#create a distance matrix based on the term frequency and inverse frequency, use method cosine
dist.matrix <- dist(tfidf.matrix,method = "cosine")
k_value <- 10 #set the number of k's
clustering.kmeans <- kmeans(tfidf.matrix,k_value) #run kmeans
clustering.heirarchical <- hclust(dist.matrix,method="ward.D2") #run heirarchical

#set the clusters
kmeans_cluster <- clustering.kmeans$cluster 
hierarchical_cluster <- cutree(clustering.heirarchical,k=k_value)

#set the points
points <- cmdscale(dist.matrix, k = k_value) 
palette <- colorspace::diverge_hcl(k_value) # Creating a color palette 
previous.par <- par(mfrow=c(2,2), mar = rep(1.5, 4)) 

#create the plots
plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 

plot(points, main = 'Hierarchical clustering', col = as.factor(hierarchical), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
