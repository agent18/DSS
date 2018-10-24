rankhospital <- function(state, outcome,num) {
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

    ## arrange and add ranks

    a <- split(outcome.data,outcome.data$State)
    b <- a[[state]] # df with outcome.data for that particular state
    
    ## arrange using order
    df <- b
    df$rank[order(df[,colnum])] <- 1:nrow(df)
    df <- df[order(df[,colnum],df$Hospital.Name),]
    
    ## return
    if (num=="best") num <- 1
    if (num=="worst") num <- nrow(df)

    print(df[num,2])
}

## ## max state outcome

rankhospital("TX", "heart failure", 4)

rankhospital("MD", "heart attack", "worst")

rankhospital("MN", "pneumonia", 5000)
