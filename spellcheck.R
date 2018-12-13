messages <- formatMessages(games,FALSE,FALSE,FALSE)
messages <- removeData(messages,TRUE,TRUE,FALSE,FALSE)

#set correct spelling to a default value of 0
messages$correctSpelling <- 0

#loop through and set the correct spelling to true or false based on if there is a misspelling in a setence
for(i in 1:nrow(messages)){
  
  txt <- unlist(strsplit(messages$text[i], " ")) #break the setences into words

  #append the 1/0 value based on correctnesss
  for(j in 1:length(txt)){
    if(hunspell_check(toString(txt[j])))
      messages$correctSpelling[i] <- 1
    else
      messages$correctSpelling[i] <- 0
  }
}

messages$correctSpelling <- as.factor(messages$correctSpelling)

#compare the relationship between correct values and if a message had spelling mistakes
barplot(table(messages$predictedAs,messages$correctSpelling))

messages %>%
  count(predictedAs,correct) %>%
  filter(n >= 15) %>%
  ggplot(aes(correct, n, fill = predictedAs)) +
  geom_bar(stat = "identity") +
  ylab("Spelling Count")
