best <- function(state, outcome) {
        ## Read outcome data
        
        hData <- read.csv("Data/outcome-of-care-measures.csv", colClasses = "character")
        outcome <- tolower(outcome)
        state <- toupper(state)
        
        # Check that state is valid
        if (!state %in% unique(hData[["State"]])) {
                stop('invalid state')
        }
        
        # Check that outcome is valid
        if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop('invalid outcome')
        }
        
        # convert state to exactly column name
        
        if(outcome =="heart attack"){
                outcome="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if(outcome =="heart failure"){
                outcome="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else{
                outcome="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }
        
        # select rows related to input state
        
        hData <- hData[which(hData$State == state),]
        
        # select the hospital name column and the desird outcome column
        
        hData <- subset(hData, select = c("Hospital.Name",outcome ))
        #print(hData)
        # REomev rows that don't contain numiric data
        hData <- hData[which(hData[outcome] != "Not Available"),]
        
        
        # convert outcome column to numeric
        hData[,outcome] <- as.numeric(hData[, eval(outcome)])
        
        
        # get the minumm value
        minOutcome <- min(hData[,outcome], na.rm = TRUE)
        #print((minOutcome))
        
        # select rows of minumm value
        hData <- hData$Hospital.Name[which(hData[outcome] == minOutcome)] 
        # sort result
        hData <- hData[order("Hospital.Name")]
        
        # select firt value
        return(hData[1])
        
}