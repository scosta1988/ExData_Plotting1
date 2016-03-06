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

png(filename = "plot1.png")
hist(df$Global_active_power, col="red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
dev.off()

