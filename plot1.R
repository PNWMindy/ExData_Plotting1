plot1 <- function() {
  ## read the data into a data frame
  data <- read.table("household_power_consumption.txt", sep=";", header=TRUE)
    
  ## convert the Date to Date format
  data <- transform(data, Date = as.Date(data$Date, "%d/%m/%Y"))

  ##limit the data by date
  data <- data[data$Date==as.Date("2007-02-01") | data$Date==as.Date("2007-02-02"),]
  
  ## convert ? to NA
  data$Global_active_power[data$Global_active_power == "?" ] <- NA
  
  ## convert column to numeric
  data$Global_active_power <- as.numeric( as.character(data$Global_active_power))
  
  ## create the bar chart for Global_active_power
  hist(data$Global_active_power, col = "red", xlab="Global Active Power (kilowatts)", ylab="Frequency", main="Global Active Power")

  ## copy to PNG file format with size 480x480 pixels  
  dev.copy(png, file = "plot1.png", height=480, width=480)
  ## close the PNG device
  dev.off() 
}