plot4 <- function() {
  ## read the data into a data frame
  data <- read.table("household_power_consumption.txt", sep=";", header=TRUE)
  
  ## convert the Date to Date format
  data <- transform(data, Date = as.Date(data$Date, "%d/%m/%Y"))
  
  ## limit the data by the two dates
  data <- data[data$Date==as.Date("2007-02-01") | data$Date==as.Date("2007-02-02"),]
  
  ## convert ? to NA
  data$Global_active_power[data$Global_active_power == "?" ] <- NA
  data$Sub_metering_1[data$Sub_metering_1 == "?" ] <- NA
  data$Sub_metering_2[data$Sub_metering_2 == "?" ] <- NA
  data$Sub_metering_3[data$Sub_metering_3 == "?" ] <- NA
  
  ## set up for 4 charts
  par(mfcol = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 0, 0))
  
  ## -------------------------------------------------
  ## chart 1
  ## copy data into data for chart 1
  data1 <- data
  ## remove the NAs from the Global_active_power column
  data1 <- data1[complete.cases(data1[,3]),]
  
  ## convert Global_active_power to numeric
  data1$Global_active_power <- as.numeric( as.character(data1$Global_active_power))
  
  ## add new column as concatenation of Date and Time fields
  data1$newCol = paste(data1$Date, data1$Time, sep=" ")
  
  ## convert the new column into date/time format
  data1 <- transform(data1, newCol = strptime(data1$newCol, "%Y-%m-%d %H:%M:%S"))
  
  ## create the line chart
  plot(x=data1$newCol, y=data1$Global_active_power, type="l", ylab="Global Active Power", xlab="",  cex.lab=0.75, cex=0.75, cex.axis=0.75)
  ## -------------------------------------------------
  
  ## -------------------------------------------------
  ## chart 2
  ## copy data into data for chart 2
  data2 <- data
  ## remove the NAs from the columns
  data2 <- data2[complete.cases(data2[,7]),]
  data2 <- data2[complete.cases(data2[,8]),]
  data2 <- data2[complete.cases(data2[,9]),]
  
  ## convert columns to numeric
  data2$Sub_metering_1 <- as.numeric( as.character(data2$Sub_metering_1))
  data2$Sub_metering_2 <- as.numeric( as.character(data2$Sub_metering_2))
  data2$Sub_metering_3 <- as.numeric( as.character(data2$Sub_metering_3))
  
  ## add new column as concatenation of Date and Time fields
  data2$newCol = paste(data2$Date, data2$Time, sep=" ")
  
  ## convert the new column into date/time format
  data2 <- transform(data2, newCol = strptime(data2$newCol, "%Y-%m-%d %H:%M:%S"))
  
  ## create the line chart for Sub_metering_1
  plot(x=data2$newCol, y=data2$Sub_metering_1, type="l", ylab="Energy sub metering", xlab="", cex.lab=0.75, cex=0.75, cex.axis=0.75)
  ## add a line for Sub_metering_2
  lines(x=data2$newCol, y=data2$Sub_metering_2, col="red") 
  ## add a line for Sub_metering_3
  lines(x=data2$newCol, y=data2$Sub_metering_3, col="blue") 
  ## add a legend
  legend("topright", lty=c(1,1,1), col=c("black","red","blue"), inset = c(-0.1, -0.05), bty="n", xjust=1, yjust=.5, cex=0.6, x.intersp=1, y.intersp=0.5, legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  ## -------------------------------------------------
  
  ## -------------------------------------------------
  ## chart 3
  ## copy data into data for chart 3
  data3 <- data
  ## remove the NAs from the Voltage column
  data3 <- data3[complete.cases(data3[,5]),]
  
  ## convert Voltage to numeric
  data3$Voltage <- as.numeric( as.character(data3$Voltage))
  
  ## add new column as concatenation of Date and Time fields
  data3$newCol = paste(data3$Date, data3$Time, sep=" ")
  
  ## convert the new column into date/time format
  data3 <- transform(data3, newCol = strptime(data3$newCol, "%Y-%m-%d %H:%M:%S"))
  
  ## create the line chart
  plot(x=data3$newCol, y=data3$Voltage, type="l", ylab="Voltage", xlab="datetime",  cex.lab=0.75, cex=0.75, cex.axis=0.75)
  ## -------------------------------------------------

  ## -------------------------------------------------
  ## chart 4
  ## copy data into data for chart 4
  data4 <- data
  ## remove the NAs from the Global_reactive_power column
  data4 <- data4[complete.cases(data4[,4]),]
  
  ## convert Global_reactive_power to numeric
  data4$Global_reactive_power <- as.numeric( as.character(data4$Global_reactive_power))
  
  ## add new column as concatenation of Date and Time fields
  data4$newCol = paste(data4$Date, data4$Time, sep=" ")
  
  ## convert the new column into date/time format
  data4 <- transform(data4, newCol = strptime(data4$newCol, "%Y-%m-%d %H:%M:%S"))
  
  ## create the line chart
  plot(x=data4$newCol, y=data4$Global_reactive_power, type="l", ylab="Global_reactive_power", xlab="datetime",  cex.lab=0.75, cex=0.75, cex.axis=0.75)
  ## -------------------------------------------------
  
  ## copy to PNG file format with size 480x480 pixels  
  dev.copy(png, file = "plot4.png", height=480, width=480) 
  ## close the PNG device
  dev.off()   
}