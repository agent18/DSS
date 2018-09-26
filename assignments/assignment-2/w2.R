 
## data  <- read.csv("hw1_data.csv")


pollutantmean  <- function(directory, pollutant, id=1:332) {

    x <- vector(mode="numeric", length=0)

    for (i in seq_along(id)) {

        filename_short <- formatC(id[i], width = 3, format = "d",
                                  flag = "0")
        filename = paste(directory, "/", as.character(filename_short),
                         ".csv", sep="")
        data <- read.csv(filename)

        d <- data[,pollutant]
        x <- c(x,d[!is.na(d)])
        
    }

    mean(x, na.rm=TRUE)
}

pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 1:10)    
pollutantmean("specdata", "nitrate", 23) ## works

    
complete  <- function(directory, id=1:332) {

    edf <- data.frame("id"=numeric(0),"nobs"=numeric(0))
    for(i in seq_along(id)) {
        
        filename_short <- formatC(id[i], width = 3, format = "d",
                                  flag = "0")
        filename = paste(directory, "/", as.character(filename_short),
                         ".csv", sep="")
        data <- read.csv(filename)

        com_cases <- dim(data[!is.na(data$sulfate) &
                              !is.na(data$nitrate),])[
            1]
        ## edf <- rbind(edf,c(id[i],com_cases))
        edf[i,] <- c(id[i],com_cases)
    }
    edf
}
complete("specdata", 1)

complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

corr <- function(directory, threshold=0) {
    x <- vector("numeric",length=0)
    count <- 1
    df.nobs <- complete(directory)
    id <- df.nobs$id
    com.cases <- df.nobs$nobs
    
    for (i in seq_along(id)){

        if (com.cases[i]>threshold){
            
            filename_short <- formatC(id[i], width = 3, format = "d",
                                  flag = "0")
            filename = paste(directory, "/", as.character(filename_short),
                         ".csv", sep="")
            data <- read.csv(filename)

            ref.data <- data[!is.na(data$sulfate) & !is.na(data$nitrate),]

            x[count] <- cor(ref.data$sulfate,ref.data$nitrate)
            count <- count + 1
        }

    }

    x
}
       
cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)


cr <- corr("specdata", 50000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)
