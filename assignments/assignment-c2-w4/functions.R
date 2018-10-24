best <- function(state, outcome) {
    ## Read outcome data
    
    outcome.data <- read.csv("outcome-of-care-measures.csv",
                             colClasses = "character")
    
    ## Check that state(col7) and outcome are valid

    if(state %in% unique(outcome.data[,7])){
        print(state)
    } else {
        stop("invalid state")
    }

    if (outcome %in% list("heart failure","pneumonia","heart attack")){
    } else {
        stop("invalid outcome")
    }

    ## check which outcome col number is important and remove NA

    if (outcome=="heart attack") colnum <- 11
    if (outcome=="heart failure") colnum  <- 17
    if (outcome=="pneumonia") colnum <- 23
    print (colnum)
    
    outcome.data[,colnum] <- as.numeric(outcome.data[,colnum])
    good <- complete.cases(outcome.data[,colnum])
    outcome.data <- outcome.data[good,]

    print("data cleaned")
                     
    ## Return hospital name(col2) in that state with lowest 30-day death

    a <- split(outcome.data,outcome.data$State)
    b <- a[[state]]
    i <- min(b[,colnum])
    df <- b[which(b[,colnum]==i),]
    df <- df[order(df$Hospital.Name),]
    print(df$Hospital.Name)

    ## rate
}

## max state outcome
best("AK","heart failure")
## invalid outcome
## best("AK","pandian")

## invalid state
## best("pandian", "pneumonia")
