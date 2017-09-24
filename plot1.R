## plot1.R contains a function drawHistogram which will draw a histogram 
## using data from 1st and 2nd of Feb, 2007 from the dataset in this assignment.
## Now generalizations have been included even though it would be possible to
## make the function more general allowing for example to define the dates and/or
## variable that the historgam is drawn from.
## Result will be available in a file called plot1.png
drawHistogram <- function() {
    ## First read in the data, which is positioned one folder up from working directory
    ## Also make it a data table for easier proseccing later on
    dt<- data.table(read.table(file="../household_power_consumption.txt", sep=";", na.strings = "?", header=TRUE))
    ## Now subset the data as we focus only on the 1st and 2nd of February 2007
    dtt<-dt[which(dt$Date == "1/2/2007" | dt$Date == "2/2/2007"),]
    ## Set the margin and number of figures
    par(mfrow=c(1,1), mar=c(4,4,2,1))
    ## Draw the figure on screen to see it looks ok
    ## Use red color, x and y labels and title like in the reference picture
    ## Do all the drawing in single call even though base plotting "Artist Palette"
    ## would enable setting labels etc with different calls.
    hist(dtt$Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)", ylab="Frequency")
    ## Draw the file directy in png file. 
    ## Default is 480px x 480px so no need to resize the graph
    png(file="plot1.png")
    ## Draw the plot
    hist(dtt$Global_active_power, col="red", main="Global Active Power", 
        xlab="Global Active Power (kilowatts)", ylab="Frequency")
    ## Close the device
    dev.off()
    return("Success")
}
