df <- read.csv("household_power_consumption.txt", sep=";")
naFilter <- !is.na(df$Sub_metering_3)
df <- df[naFilter,]

##Translate the Date from Factor to Date
df$Date <- as.Date(as.character(df$Date), "%d/%m/%Y")
##We will be using the days "2007-02-01" and "2007-02-02"
dateFilter <- df$Date == as.Date("2007-02-01") | df$Date == as.Date("2007-02-02")
df <- df[dateFilter,]

#Translate the Global_active_power from Factor to Numeric
df$Global_active_power <- as.numeric(as.character(df$Global_active_power))

##Function to be used on sapply
concat <- function(index, arr1, arr2, ...){
  paste(arr1[index], arr2[index], ...)
}

##Constructing the X Axis of the plot
xAxis <- sapply(1:length(df$Date), concat, as.character(df$Date), as.character(df$Time), collapse=" ")
xAxis <- as.POSIXct(xAxis, format="%Y-%m-%d %H:%M:%S")

png(filename = "plot2.png")
plot(xAxis, df$Global_active_power, type="l", axes=FALSE, xlab = "", ylab = "Global Active Power (kilowatts)")
axis(1, at=c(as.numeric(xAxis[1]), as.numeric(xAxis[length(xAxis)/2+1]), as.numeric(xAxis[length(xAxis)])), labels=c("Thu", "Fri", "Sat"))
axis(2, at=seq(0, 6, by=2), labels=seq(0,6,by=2))
dev.off()
