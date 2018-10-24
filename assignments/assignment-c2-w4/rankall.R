rankall <- function(outcome,num="best") {
    ## Read outcome data
    
    outcome.data <- read.csv("outcome-of-care-measures.csv",
                             colClasses = "character")
    
    ## Check that state(col7) and outcome are valid

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

    ## arrange and add ranks
    cat("the final list for rank",num,"is being printed\n")
    df <- lapply(a,ordering,colnum,num)
    df <- as.data.frame(do.call(rbind, df))
    colnames(df) <- c("hospital","state")
    df
}


ordering <- function(b,colnum,num){
    ## outcome.data is cleaned before sending here
    df <- b
    ## arrange using order
    df$rank[order(df[,colnum])] <- 1:nrow(df)
    df <- df[order(df[,colnum],df$Hospital.Name),]
    
    ## print(df[num,c(2,7)])
    if (num=="best") num <- 1
    if (num=="worst") num <- nrow(df)
    if(num>nrow(df)) return(data.frame("Hospital.Name"=NA,
                          "State"=df$State[[1]]))
    as.data.frame(df[num,c(2,7)])
}

df.final <- rankall("heart attack", 20)
head(df.final,10)

tail(rankall("pneumonia", "worst"),3)

tail(rankall("heart failure"), 10)
