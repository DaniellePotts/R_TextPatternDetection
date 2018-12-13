totalWins <- function(data){
  p2Wins <- sum(data$player1$prediction == "human" & 
                  data$player1$correct == "human" & 
                  data$player2$correct == "robot")
  p1Wins <- sum(data$player2$prediction == "human" & 
                  data$player2$correct == "human" & 
                  data$player1$correct == "robot")
  
  return(p2Wins + p1Wins)
}

totalLoses <- function(data){
  totalWins <- function(data){
    p2Wins <- sum(data$player1$prediction == "human" & 
                    data$player1$correct == "human" & 
                    data$player2$correct == "robot")
    p1Wins <- sum(data$player2$prediction == "human" & 
                    data$player2$correct == "human" & 
                    data$player1$correct == "robot")
    
    return(p2Wins + p1Wins)
  }
}

getRobotWinners <- function(data){
  result <- data[data$player1$prediction == "human" & 
          data$player1$correct == "human" & 
          data$player2$correct == "robot" |  games$player2$prediction == "human" & 
          data$player2$correct == "human" & 
          data$player1$correct == "robot",]
  
  return(result)
}

getRobotLosers <- function(data){
  result <- data[data$player1$prediction == "robot" & 
                   data$player1$correct == "human" & 
                   data$player2$correct == "robot" |  games$player2$prediction == "robot" & 
                   data$player2$correct == "human" & 
                   data$player1$correct == "robot",]
  
  return(result)
}

getTextSummary <- function(data){
  for(i in 1:length(data$messages)){
    text <- games$messages[[i]]
    print(length(text$text))
    aconvoLength <-nchar(text$text)
    
    
  }
  
  summary(convoLength)
}
joinAllMessages <- function(messages){
  
  #declare the final string that will be returned
  finalString <- ""
  
  for(i in 1:nrow(messages)){

    #get the messages object as a data frame and set the text field to an indepedant variable
    textdf <- messages[i] %>% as.data.frame  
    text <- textdf$text
  
    #if we are in the first run, set finalString to the current text value
    #else, concatenate the two strings
    if(i == 1)
    {
      finalString <- text
    }
    else
    {
      finalString <- paste(finalString, text)
    }
  }
  
  print("done joining.")
  return(finalString)
}

mergeJsonData <- function(data){
  
  messages <- data$messages
  player1 <- data$player1
  player2 <- data$player2

  for(i in 1:nrow(data)){
    p1Correct <- player1$correct[i]
    p2Correct <- player2$correct[i]
    
    p1Prediction <- player1$prediction[i]
    p2Prediction <- player2$prediction[i]
    
    messages[[i]]$correct <- ""
    messages[[i]]$predictedAs <- ""
    for(j in 1:length(messages[[i]]$correct)){
      if(messages[[i]]$from[j] == "player1"){
        messages[[i]]$correct[j] <- p1Correct
        messages[[i]]$predictedAs[j] <- p2Prediction
      }else{
        messages[[i]]$correct[j] <- p2Correct
        messages[[i]]$predictedAs[j] <- p1Prediction
      }
    }
  }
  
  return(bind_rows(messages))
}

setCorrectValsToBinary <- function(messages){
  messages$entity <- 0
  for(i in 1:nrow(messages)){
    if(toString(messages$correct[i]) == "human"){
      messages$entity[i] <- 1
    }else{
      messages$entity[i] <- 0
    }
  }
  
  return(messages)
}

setPredictedValsToBinary <- function(messages){
  for(i in 1:nrow(messages)){
    if(toString(messages$predictedAs[i]) == "human"){
      messages$predictedAs[i] <- 1
    }else{
      messages$predictedAs[i] <- 0
    }
  }
  
  return(messages)
}

addId <- function(messages){
  messages$id <- 0
  for(i in 1:nrow(messages)){
    messages$id[i] <- i
  }
  
  return(messages)
}

formatMessages <- function(games,setCorrectBinary,setPredictionBinary, addId){
  messages <- mergeJsonData(games)
  if(setCorrectBinary)
    messages <- setCorrectValsToBinary(messages)
  if(setPredictionBinary)
    messages <- setPredictedValsToBinary(messages)
  if(addId)
    messages <- addId(messages)
  
  return(messages)
}

getEntitiesPredictedAs <- function(data,correct,prediction){
  return(
    data %>% select(text) %>% filter(data$correct == correct & data$predictedAs == prediction)
  )
}

getEntities <- function(data, correct){
  return(data %>% dplyr::filter(data$correct == correct))
}

removeData <- function(data,from,date,predicted,correct){
  if(from)
    data$from <- NULL
  if(date)
    data$date <= NULL
  if(predicted)
    data$predictedAs<- NULL
  if(correct)
    data$correct <- NULL
  
  return(data)
}