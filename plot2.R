## Exploratory Data Analysis
## Course project: Plotting2
plot2 <- function() {
    # name of the plot
    plotName <- "plot2.png"
    
    # names of the data and zipped file
    fileNameZip <- "exdata-data-household_power_consumption.zip"
    fileNameTxt <- "household_power_consumption.txt"
    
    dateFrom <- as.POSIXct("2007-02-01")
    dateTo   <- as.POSIXct("2007-02-02")
    
    # check if data file exists. (in zip or txt format)
    if (!file.exists(fileNameTxt)) {
        if (!file.exists(fileNameZip)) {
            stop(paste("No data file present! (",fileNameZip,"or",fileNameTxt,")"))
        }
        unzip(fileNameZip, fileNameTxt)
    }
    
    # read the data from the text data file
    # first read the first row to get the first date in the data file
    exdata <- read.table(fileNameTxt, 
                         sep = ";",
                         header = TRUE, 
                         na.strings = "?", 
                         nrows = 1,
                         row.names = NULL,
                         colClasses = c("character","character","numeric","numeric","numeric",
                                        "numeric","numeric","numeric","numeric"))
    
    # get the columnnames
    exdataColNames <- colnames(exdata)
    
    # convert date and time column to one POSIXct Date column
    exdata$Date <- as.POSIXct(strptime(paste(exdata$Date, exdata$Time), "%d/%m/%Y %H:%M:%S"))
    
    # calculate the number of rows to skip and to get from the data file
    dateTo <- dateTo + 24*60*60
    dataSkip <- as.integer(difftime(dateFrom, exdata$Date[1], units = "mins")) + 1
    dataRows <- as.integer(difftime(dateTo, dateFrom, units = "mins"))
    
    # read the data from dateFrom to dateTo
    exdata <- read.table(fileNameTxt, 
                         sep = ";",
                         header = FALSE, 
                         na.strings = "?", 
                         skip = dataSkip,
                         nrows = dataRows,
                         col.names = exdataColNames,
                         row.names = NULL,
                         colClasses = c("character","character","numeric","numeric","numeric",
                                        "numeric","numeric","numeric","numeric"))

    # convert date and time column to one POSIXct Date column
    exdata$Date <- as.POSIXct(strptime(paste(exdata$Date, exdata$Time), "%d/%m/%Y %H:%M:%S"))
    exdata$Time <- NULL
    
    # make plot
    png(filename = plotName, width = 480, height = 480)
    plot(exdata$Date, 
         exdata$Global_active_power, 
         type = "l",
         xlab = "",
         ylab = "Global active power (kilowatts)" )
    dev.off()
    message(paste("Plotted", plotName))
}