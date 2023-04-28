# PRACTICAL 01 

classes = c('character', 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 
            'numeric', 'numeric')

data = read.table('household_power_consumption.txt', colClasses = classes, na.strings = '?', header = TRUE, sep  = ';')

data

data$Date = as.Date(data$Date, '%d/%m/%Y')
head(data)
consumption = subset(data, Date>= ('2007-2-1') & Date<= as.Date('2007-2-2'))
consumption = consumption[complete.cases(consumption),]
head(consumption)

DateTime = as.POSIXct(paste(consumption$Date, consumption$Time))
consumption = consumption[, !names(consumption) %in% c('Date', 'Time')]
consumption = cbind(consumption, DateTime)
head(consumption)

# Plotting Histogram

hist(consumption$Global_active_power, main = 'Global Active Power', xlab = 'Global Active Power(kw)' ,col = 'red')
plot(consumption$Global_active_power ~ consumption$DateTime, type = 'l', ylab = 'Global Active Power(kWs)', xlab = '')
lines(consumption$Sub_metering_2~consumption$DateTime, col = 'Red')
lines(consumption$Global_active_power ~ consumption$DateTime)
with(consumption, {
  plot(Sub_metering_1~DateTime, type = 'l', ylab = 'Global Actib=ve Power(kWs)', xlab = '')
  lines(Sub_metering_2~DateTime, col = 'Red')
  lines(Sub_metering_3~DateTime, col = 'Blue')
  
})

legend('topright', col = c('red', 'black', 'blue'), lwd = c(1,1,1), legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'))

par(mfrow = c(2,2), mar = c(4,4,2,1), oma = c(0,0,2,0))
with(consumption,{
  plot(Global_active_power ~ DateTime, type = 'l', ylab = 'Global Active Power(kWs)', xlab = '')
  plot(Voltage ~ DateTime, type = 'l', ylab = 'Voltage', xlab ='')
  plot(Sub_metering_1 ~ DateTime, type = 'l', ylab = 'Global Active Power(kWs)', xlab = '')
  lines(Sub_metering_2 ~ DateTime, col = 'Red')
  lines(Sub_metering_3 ~ DateTime, col = 'Blue')
  legend('topright', col = c('black', 'red', 'blue'), lwd = c(1,1,1), legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'))
  plot(Global_active_power ~ DateTime, type = 'l', ylab = 'Global Reactive Power(kWs)', xlab ='')
  
})

