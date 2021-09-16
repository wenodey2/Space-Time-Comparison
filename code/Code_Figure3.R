library(DSWE)

##################...........Edit in this section..............#############################
path = ''
data1 = read.csv(paste0(path,'Dataset2_1.csv'))
data2 = read.csv(paste0(path,'Dataset2_2.csv'))



#######################.....Do not edit below this...............###########################
pdf(file = paste0(path,'Figure3.pdf'), # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 6) # The height of the plot in inches

# Arguments preparation
dataList = list(data1, data2)
xCol = c(4, 1, 6)
thrs = c(0.1, 0.1, 0.05)

# Executing matching
matched = CovMatch(data = dataList, xCol = xCol, thrs = thrs)
matched = matched$matchedData
#plot the before and after matched data
par(mfrow = c(3,2))

plot(density(data1$wind_speed), ylim = c(0, 0.20), xlim = c(0, 18), xlab = 'W (m/s)', main = ' ')
lines(density(data2$wind_speed), col = 'red', lty = 2)

plot(density(matched[[1]][, 'wind_speed']), ylim = c(0, 0.20), xlim = c(0, 18), xlab = 'W (m/s)', main = ' ')
lines(density(matched[[2]][, 'wind_speed']), col = 'red', lty = 2)

plot(density(data1$ambient_temp), xlim = c(15, 35), ylim = c(0, 0.08), 
     xlab = expression(paste('T ('*degree*'C)')), main = ' ')
lines(density(data2$ambient_temp), col = 'red', lty = 2)

plot(density(matched[[1]][, 'ambient_temp']), xlim = c(15, 35), ylim = c(0, 0.08), 
     xlab = expression(paste('T ('*degree*'C)')), main = ' ')
lines(density(matched[[2]][, 'ambient_temp']), col = 'red', lty = 2)

plot(density(data1$turbulence_intensity), ylim = c(0, 8), xlim = c(0, 0.4), xlab = 'TI ', main = ' ')
lines(density(data2$turbulence_intensity), col = 'red', lty = 2)

plot(density(matched[[1]][, 'turbulence_intensity']), ylim = c(0, 8), xlim = c(0, 0.4), xlab = 'TI', main = ' ')
lines(density(matched[[2]][, 'turbulence_intensity']), col = 'red', lty = 2)

# Run dev.off() to create the file!
dev.off()
