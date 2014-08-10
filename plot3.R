plot3 <- function() {
  ## read the data into a data frame
  data <- read.table("household_power_consumption.txt", sep=";", header=TRUE)
  
  ## convert the Date to Date format
  data <- transform(data, Date = as.Date(data$Date, "%d/%m/%Y"))
  
  ## limit the data by the two dates
  data <- data[data$Date==as.Date("2007-02-01") | data$Date==as.Date("2007-02-02"),]
  
  ## convert ? to NA
  data$Sub_metering_1[data$Sub_metering_1 == "?" ] <- NA
  data$Sub_metering_2[data$Sub_metering_2 == "?" ] <- NA
  data$Sub_metering_3[data$Sub_metering_3 == "?" ] <- NA
  
  ## remove the NAs from the columns
  data <- data[complete.cases(data[,7]),]
  data <- data[complete.cases(data[,8]),]
  data <- data[complete.cases(data[,9]),]
  
  ## convert columns to numeric
  data$Sub_metering_1 <- as.numeric( as.character(data$Sub_metering_1))
  data$Sub_metering_2 <- as.numeric( as.character(data$Sub_metering_2))
  data$Sub_metering_3 <- as.numeric( as.character(data$Sub_metering_3))
  
  ## add new column as concatenation of Date and Time fields
  data$newCol = paste(data$Date, data$Time, sep=" ")
  
  ## convert the new column into date/time format
  data <- transform(data, newCol = strptime(data$newCol, "%Y-%m-%d %H:%M:%S"))
  
  ## create the line chart for Sub_metering_1
  plot(x=data$newCol, y=data$Sub_metering_1, type="l", ylab="Energy sub metering", xlab="")
  ## add a line for Sub_metering_2
  lines(x=data$newCol, y=data$Sub_metering_2, col="red", lwd=2.5) 
  ## add a line for Sub_metering_3
  lines(x=data$newCol, y=data$Sub_metering_3, col="blue", lwd=2.5) 
  ## add a legend
  legend("topright", lty=c(1,1,1), col=c("black","red","blue"), x.intersp=1, y.intersp=0.5, legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  
  ## copy to PNG file format with size 480x480 pixels  
  dev.copy(png, file = "plot3.png", height=480, width=480) 
  ## close the PNG device
  dev.off() 
}