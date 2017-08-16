rankhospital <- function(state, outcome, num){
  OC <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  testS <- match(state, OC$State)
  testO <- match(outcome, c("heart attack", "heart failure", "pneumonia"))
  if(is.na(testS) == TRUE){
    stop("invalid state")
  }
  else if(is.na(testO) == TRUE){
    stop("invalid outcome")
  }
  newdata <- OC[OC$State == state, ]
  rank <- 0
  if(num == "best"){
    num <- 1
  }
  DF <- data.frame(Rank = numeric(0))
  DF <- do.call(cbind, DF)
  if(outcome == "heart attack"){
    newdata <- newdata[order(newdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, newdata$Hospital.Name, na.last = NA), ]
    newdata <- subset(newdata, select= c(Provider.Number, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    if(num == "worst"){
      num <- nrow(newdata)
    }
    for(i in 1:nrow(newdata)){
      rank[i] <- i
      DF_temp <- data.frame(Rank = rank[i])
      DF_temp <- do.call(cbind, DF_temp)
      DF <- rbind(DF,DF_temp)
      
    }
    newdata <- cbind(newdata, DF)
    as.character(newdata[num, 2])
  }
  
  else if(outcome == "heart failure"){
    newdata <- newdata[order(newdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, newdata$Hospital.Name, na.last = NA), ]
    newdata <- subset(newdata, select= c(Provider.Number, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    if(num == "worst"){
      num <- nrow(newdata)
    }
    for(i in 1:nrow(newdata)){
      rank[i] <- i
      DF_temp <- data.frame(Rank = rank[i])
      DF_temp <- do.call(cbind, DF_temp)
      DF <- rbind(DF,DF_temp)
      
    }
    newdata <- cbind(newdata, DF)
    as.character(newdata[num, 2])
  }
  else if(outcome == "pneumonia"){
    newdata <- newdata[order(newdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, newdata$Hospital.Name, na.last = NA), ]
    newdata <- subset(newdata, select= c(Provider.Number, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    if(num == "worst"){
      num <- nrow(newdata)
    }
    for(i in 1:nrow(newdata)){
      rank[i] <- i
      DF_temp <- data.frame(Rank = rank[i])
      DF_temp <- do.call(cbind, DF_temp)
      DF <- rbind(DF,DF_temp)
      
    }
    newdata <- cbind(newdata, DF)
    as.character(newdata[num, 2])
  }
}