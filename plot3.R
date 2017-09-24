## plot3.R contains a function drawSubMetering which will draw a sub metering line diagram 
## using data from 1st and 2nd of Feb, 2007 from the dataset in this assignment.
## Result will be available in a file called plot3.png
drawSubMetering <- function() {
    ## First read in the data, which is positioned one folder up from working directory
    ## Also make it a data table for easier proseccing later on
    datafile <- "../household_power_consumption.txt"
    dt<- data.table(read.table(file=datafile, sep=";", na.strings = "?", header = TRUE))
    ## Now subset the data as we focus only on the 1st and 2nd of February 2007
    dtt<-dt[which(dt$Date == "1/2/2007" | dt$Date == "2/2/2007"), ]
    ## Set the margin and number of figures
    par(mfrow = c(1,1), mar = c(4,4,2,1))
    ## Draw the figure on screen to see it looks ok / like the reference picture
    ## Use base plotting "Artist Palette" ie. draw the canvas first using plot and type ="n"
    ## then plot the actual graph using lines. Note that we need to combine the date and time
    ## fields to be of correct datetime format so that correct graph can be drawn.
    ## Change the Date column from factor to Date
    dtt[,1]<-lapply(dtt[,1], as.Date, format="%d/%m/%Y")
    x<-as.POSIXct(strptime(paste(dtt$Date, dtt$Time, sep = " "), format = "%Y-%m-%d %H:%M:%S"))
    ## Extract values for the 3 lines
    s1 <- dtt$Sub_metering_1
    s2 <- dtt$Sub_metering_2
    s3 <- dtt$Sub_metering_3
    ## Calculate the range for the values
    subRange <- range(min(c(s1,s2,s3)), max(c(s1,s2,s3)))
    ## Plot canvas using ranges
    plot(range(x), subRange, type="n", xlab="", ylab="Energy sub metering")
    ## Draw the actual lines
    lines(x,s1)
    lines(x, s2, col="red")
    lines(x, s3, col = "blue")
    ## Finally add the legend
    legend("topright", col=c("black", "red", "blue"), lty = "solid", 
        legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
        text.col=c("black", "red", "blue"))
    ## Draw the file directy in png file. 
    ## Default is 480px x 480px so no need to resize the graph
    png(file="plot3.png")
    ## Draw the plot
    plot(range(x), subRange, type="n", xlab="", ylab="Energy sub metering")
    lines(x,s1)
    lines(x, s2, col="red")
    lines(x, s3, col = "blue")
    ## Finally add the legend
    legend("topright", col=c("black", "red", "blue"), lty = "solid", 
        legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
        text.col=c("black", "red", "blue"))
    ## Close the device
    dev.off()
    return("Success")
}
