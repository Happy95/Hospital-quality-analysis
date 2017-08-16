best <- function(state, outcome){
   OC <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
   testS <- match(state, OC$State)
   testO <- match(outcome, c("heart attack", "heart failure", "pneumonia"))
   if(is.na(testS) == TRUE){
     stop("invalid state")
   }
   if(is.na(testO) == TRUE){
     stop("invalid outcome")
   }
   newdata <- OC[OC$State == state, ]
   newdata <- newdata[order(newdata$Hospital.Name, na.last = NA), ]
   if(outcome == "heart attack"){
     x <- which.min(newdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
     newdata$Hospital.Name[x]
   }
   else if(outcome == "heart failure"){
     x <- which.min(newdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
     newdata$Hospital.Name[x]
   }
   else if(outcome == "pneumonia"){
     x <- which.min(newdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
     newdata$Hospital.Name[x]
   }
}