rankhospital<- function(state, outcome, num = "best")
{

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
        hData1 <- hData[which(hData[outcome] != "Not Available"),]
       #print(hData1)
        
        # convert outcome column to numeric
        hData1[,outcome] <- as.numeric(hData1[, eval(outcome)])
       #print(hData1)
        #print("****************************************************")
        hData2 <- hData1[order(hData1[ ,outcome],hData1[,"Hospital.Name"]), ]
        #print(hData2)

        if(num == "best"){
                num <- 1
        }            
        else if (num == "worst"){
                num <- nrow(hData2)
        }      
       print(hData2[num,"Hospital.Name"])
}

