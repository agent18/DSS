data <- read.csv("specdata/002.csv")
ref.data <- data[!is.na(data$sulfate) & !is.na(data$nitrate),]

cor(ref.data$sulfate,ref.data$nitrate)
