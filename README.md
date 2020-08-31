# Exploratory_Data_Plotting
Assigment of the week1 of Exploratory Data Course.

## Code book
# Getting and cleaning the data

House <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?", colClasses = c('character', 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))

#Choose the time's observation between 'Feb. 1, 2007 to Feb. 2, 2007'
House1 <- subset(House, Date %in% c("1/2/2007","2/2/2007"))

#Convert the Date col to Type Date
House1$Date <- as.Date(House1$Date, format="%d/%m/%Y")

# PLOT 1 
#Creating the first histogram
hist(House1$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")

# Next step is to Save the file and close the device
dev.copy(png, "plot1.png", width= 480, height=480)
dev.off()


# PLOT 2
library(ggplot2)
## Remove incomplete observation
House1 <- House1[complete.cases(House1),]
dateTime <- setNames(dateTime, "DateTime")
House1 <- House1[ ,!(names(House1) %in% c("Date","Time"))]

## Add DateTime column
House1 <- cbind(dateTime, House1)

## Format dateTime Column
House1$dateTime <- as.POSIXct(dateTime)
 
#Mix Data and time col
dateTime <- paste(House1$Date, House1$Time)

plot(House1$Global_active_power~House1$dateTime, ylab = "Global Active Power (kilowatts)", xlab = "", pch= ".", type="l")

# Next step is to Save the file and close the device
dev.copy(png, "plot2.png", width= 480, height=480)
dev.off()


# PLOT 3
library(ggplot2)
with(House1, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

# Next step is to Save the file and close the device
dev.copy(png, "plot3.png", width= 480, height=480)
dev.off()



# PLOT 4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(House1, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", ylab="Global Rective Power (kilowatts)",xlab="")
})

## Saving to file
dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()
