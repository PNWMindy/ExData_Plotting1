plot2 <- function() {
  ## read the data into a data frame
  data <- read.table("household_power_consumption.txt", sep=";", header=TRUE)
  
  ## convert the Date to Date format
  data <- transform(data, Date = as.Date(data$Date, "%d/%m/%Y"))
  
  ## limit the data by the two dates
  data <- data[data$Date==as.Date("2007-02-01") | data$Date==as.Date("2007-02-02"),]

  ## convert ? to NA
  data$Global_active_power[data$Global_active_power == "?" ] <- NA

  ## remove the NAs from the Global_active_power column
  data <- data[complete.cases(data[,3]),]
  
  ## convert Global_active_power to numeric
  data$Global_active_power <- as.numeric( as.character(data$Global_active_power))
  
  ## add new column as concatenation of Date and Time fields
  data$newCol = paste(data$Date, data$Time, sep=" ")
  
  ## convert the new column into date/time format
  data <- transform(data, newCol = strptime(data$newCol, "%Y-%m-%d %H:%M:%S"))
 
  ## create the line chart
  plot(x=data$newCol, y=data$Global_active_power, type="l", ylab="Global Active Power (kilowatts)", xlab="")
  
  ## copy to PNG file format with size 480x480 pixels  
  dev.copy(png, file = "plot2.png", height=480, width=480) 
  ## close the PNG device
  dev.off() 
}