 
## data  <- read.csv("hw1_data.csv")


pollutantmean  <- function(directory, pollutant, id=1:332) {

    x_mean <- vector("integer",length(id))
    print(x)
    
    for (i in seq_along(id)) {

        filename_short <- formatC(id[i], width = 3, format = "d",
                                  flag = "0")
        filename = paste(directory, "/", as.character(filename_short),
                         ".csv", sep="")
        data <- read.csv(filename)

        x_mean[i] <- mean(data[,pollutant], na.rm=TRUE)
        
    }

    newlist <- list(x_mean,mean(x_mean, na.rm=TRUE))
}

newlist <- pollutantmean("specdata","nitrate", 70:72)
    
print(newlist[[1]])
print(newlist[[2]])

    ## loop

     ## read data[i]

     ## obtain mean[i]

     ## Store mean[i]

    ## Compute mean of all


    
