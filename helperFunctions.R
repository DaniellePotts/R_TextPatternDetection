mergeJsonData <- function(data){
  #set the three nested objects to individual objecs
  messages <- data$messages
  player1 <- data$player1
  player2 <- data$player2

  for(i in 1:nrow(data)){
    p1Correct <- player1$correct[i] #get the correct values for both players
    p2Correct <- player2$correct[i]
    
    p1Prediction <- player1$prediction[i] #get the predictions for both players
    p2Prediction <- player2$prediction[i]
    
    #set default values on our new variables 
    messages[[i]]$correct <- "" 
    messages[[i]]$predictedAs <- ""
    
    #loop through the messages 
    for(j in 1:length(messages[[i]]$correct)){
      #check at a given time if the from value is player1 or 2, and set the correct and predicted values respectively
      if(messages[[i]]$from[j] == "player1"){
        messages[[i]]$correct[j] <- p1Correct
        messages[[i]]$predictedAs[j] <- p2Prediction
      }else{
        messages[[i]]$correct[j] <- p2Correct
        messages[[i]]$predictedAs[j] <- p1Prediction
      }
    }
  }
  
   #bind all the messages into one dataframe, rather than a array of dataframes
  return(bind_rows(messages))
}

#loop through the data, check if the correct value is human or robot and reset the value to 1/0 respectively
setCorrectValsToBinary <- function(messages){
  for(i in 1:nrow(messages)){
    if(toString(messages$correct[i]) == "human"){
      messages$correct[i] <- 1
    }else{
      messages$correct[i] <- 0
    }
  }
  
  return(messages)
}

#loop through the data, check if the predicted value is human or robot and reset the value to 1/0 respectively
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

#loop through and set id[i] to the value of i
addId <- function(messages){
  messages$id <- 0
  for(i in 1:nrow(messages)){
    messages$id[i] <- i
  }
  
  return(messages)
}

#take in bools and format the data as necessary
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

#take in bools and remove data based on the true/false values
removeData <- function(data,from,date,predicted,correct){
  if(from){
    data$from <- NULL
  }
  if(date)
  {
    data$date <- NULL
  }
  if(predicted)
  {
    data$predictedAs<- NULL
  }
  if(correct)
  {
    data$correct <- NULL
  }
  
  return(data)
}