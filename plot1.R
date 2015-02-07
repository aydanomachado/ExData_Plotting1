## Function to get user confirmation
## Only quit with "y" or "n"
readconfirm <- function() { 
    n <- readline(prompt="Do you want to continue? (y/n): ")
    if(n=="y" || n=="n") {
        return(n)
    } else {
        return(readconfirm())
    }
}

readData <- function() {
    file <- "./data/household_power_consumption.txt" 
    library(data.table)
    ## Estimating table size
    top.size <- object.size(fread(file, nrow=1000))
    lines <- as.numeric(gsub("[^0-9]", "", system(paste("wc -l", file, sep = " "), intern=T)))
    size.estimate <- lines / 1000 * top.size
    ## Asking user if he want to continue 
    print(paste("You will manipulate a table of size: ", size.estimate, " bytes"))
    confirm <- readconfirm()
    if (confirm=="n")
        return()
    ## Loading data of days "1/2/2007", "2/2/2007"
    findRows<-fread(file, header = TRUE, select = 1)
    all<-(which(findRows$Date %in% c("1/2/2007", "2/2/2007")) )
    skipLines<- min(all)
    keepRows<- length(all)
    
    data <- fread(file, skip = (skipLines) , nrows = keepRows, header = FALSE)
    rm(findRows)
    dataNames<- names(fread(file, nrow = 1))
    setnames(data, dataNames)  
    
    return(data)
}

plot1 <- function(){
    data <- readData()
    if (!is.null(data)){
        png("plot1.png")
        hist(data$Global_active_power, col = "red",
             main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
        dev.off()
        print("plot1.pgn is done!")
    } else {
        print("See you!")
    }
}