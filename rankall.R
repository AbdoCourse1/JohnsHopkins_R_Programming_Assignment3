
rankall <- function(outcome, num = "best") {
        
        rankhospital<- function(hData1,num)
        {
                hData1 <- hData1[order(hData1[ ,outcome],hData1[,"Hospital.Name"]), ]
                if(num == "best"){
                        num <- 1
                }            
                else if (num == "worst"){
                        num <- nrow(hData1)
                }    
                return (hData1$Hospital.Name[num])
        }
        
        ## Read outcome data
        hDataa <- read.csv("Data/outcome-of-care-measures.csv", colClasses = "character")
        outcome <- tolower(outcome)
        
        # Check that outcome is valid
        if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop('invalid outcome')
        }
        
        # convert outcome to exactly column name
        if(outcome =="heart attack"){
                outcome="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if(outcome =="heart failure"){
                outcome="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else{
                outcome="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }
        
        hDataa <- subset(hDataa, select = c("State","Hospital.Name",outcome ))
        # REomev rows that don't contain numiric data
        hDataa <- hDataa[which(hData[outcome] != "Not Available"),]
        hDataa[,outcome] <- as.numeric(hDataa[, eval(outcome)])
        splited = split(hDataa, hDataa$State)
        
        r2<-lapply(splited, rankhospital,num)
        return ( data.frame(hospital=unlist(r2), state=names(r2)) )
        
        
}

