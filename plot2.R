## plot2.R contains a function drawLineGraph which will draw a line diagram 
## using data from 1st and 2nd of Feb, 2007 from the dataset in this assignment.
## Now generalizations have been included even though it would be possible to
## make the function more general allowing for example to define the dates and/or
## variable that the historgam is drawn from. Also decided to use base plotting
## even though ggplot would allow for more straight forward approach.
## Result will be available in a file called plot2.png
drawLineGraph <- function() {
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
    y<-dtt$Global_active_power
    plot(x, y, type="n", xlab="", ylab="Global Active Power (kilowatts)")
    lines(x,y)
    ## Draw the file directy in png file. 
    ## Default is 480px x 480px so no need to resize the graph
    png(file="plot2.png")
    ## Draw the plot
    plot(x, y, type="n", xlab="", ylab="Global Active Power (kilowatts)")
    lines(x, y)
    ## Close the device
    dev.off()
    return("Success")
}
