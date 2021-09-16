# Set a working directoy
# Create a folder named 'YearData' in working directory.
# Then run the script below.
# All the results will be written in working directory
library(DSWE)
library(ggplot2)

####################### Split datasets into 4 years each##############################
for (i in 1:66) {
  data = read.csv(paste0('Dataset4_',i,'.csv'))
  colnames(data) = c('time', 'wind_speed', 'wind_direction', 
                     'ambient_temp', 'terrain', 'active_power', 
                     'stdeviation', 'turbulence_intensity')
  data$time=as.POSIXct(data$time, format = '%Y-%m-%d%H:%M:%S')
  
  
  data1 = data[data$time > '2014-12-31' & data$time <= '2015-12-31', ]
  data2 = data[data$time > '2015-12-31' & data$time <= '2016-12-31', ]
  data3 = data[data$time > '2016-12-31' & data$time <= '2017-12-31', ]
  data4 = data[data$time > '2017-12-31' & data$time <= '2018-12-31', ]
  
  data_x = list(data1, data2, data3, data4)
  year = c(2015, 2016, 2017, 2018)
  
  for (j in 1:length(year)){
    write.csv(data_x[[j]], paste0('YearData/','Dataset4_',i,'_',year[j], '.csv'), row.names = FALSE)
  }
}


#######################Creating a metric and location file##########################
location_data = read.csv('Dataset_location.csv')
figure7_metric = location_data

for(i in c(2015, 2016, 2017, 2018)){
  data_ref = read.csv(paste0('YearData/', 'Dataset4_',12,'_',i, '.csv'))
  xCol = c(4, 2, 8)
  yCol = 6
  testCol = c(2, 4)
  str1 = paste0('Group', i)
  str2 = paste0('Metric', i)
  for(j in 1:66){
    if(j == 12) next
    data_num = read.csv(paste0('YearData/', 'Dataset4_',j,'_',i, '.csv'))
    datalist = list(data_num, data_ref)
    ComparePCurveOutput = ComparePCurve(datalist, xCol = xCol, yCol = yCol, 
                                       xCol.circ = NULL, testCol = c(2,4))$scaledDiff
    figure7_metric[j, str2] = ComparePCurveOutput
  }
  
  figure7_metric[is.na(figure7_metric[ , str2]), str2] = 0
  
  figure7_metric[figure7_metric[, str2] > 0, str1] = 1
  figure7_metric[figure7_metric[, str2] < 0, str1] = 2
  
  figure7_metric[figure7_metric$Turbine == 12, str2] = 20
  figure7_metric[figure7_metric$Turbine == 12, str1] = 3
  
}

##################Visualizing the results############################################
####plot 2015 layout
pdf(file = 'Figure7a.pdf',   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 3) # The height of the plot in inches

g1 = ggplot(figure7_metric, aes(Longitude, Latitude, color = as.factor(Group2015), 
                     shape = as.factor(Group2015), label = Turbine, alpha = abs(Metric2015)))
g1 + geom_point(size = 6, show.legend = FALSE) + 
  geom_text(size = 4, vjust = 2, color = 'black', fontface = 'bold', show.legend = FALSE) + 
  ylim(c(40.04, 40.135)) + 
  scale_color_manual(breaks = c("2", "3"), values=c("red", "blue"))+
  scale_shape_manual(values = c(19, 15))+
  xlab('Longitude') + 
  ylab('Latitude') + 
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))
  
dev.off()
  
####plot 2016 layout 
pdf(file = 'Figure7b.pdf',   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 3) # The height of the plot in inches

  g2 = ggplot(figure7_metric, aes(Longitude, Latitude, color = as.factor(Group2016), 
                                  shape = as.factor(Group2016), label = Turbine, alpha = abs(Metric2016)))
  g2 + geom_point(size = 6, show.legend = FALSE) + 
    geom_text(size = 4, vjust = 2, color = 'black', fontface = 'bold', show.legend = FALSE) + 
    ylim(c(40.04, 40.135)) + 
    scale_color_manual(breaks = c("1","2", "3"), values=c("green","red", "blue"))+
  scale_shape_manual(values = c(17, 19, 15))+
    xlab('Longitude') + 
    ylab('Latitude') +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size=14, face="bold"),
      axis.title.y = element_text(size=14, face="bold"))
dev.off()

#####plot 2017 layout  
pdf(file = 'Figure7c.pdf',   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 3) # The height of the plot in inches

  g3 = ggplot(figure7_metric, aes(Longitude, Latitude, color = as.factor(Group2017), 
                                  shape = as.factor(Group2017), label = Turbine, alpha = abs(Metric2017)))
  g3 + geom_point(size = 6, show.legend = FALSE) + 
    geom_text(size = 4, vjust = 2, color = 'black', fontface = 'bold', show.legend = FALSE) + 
    ylim(c(40.04, 40.135)) + 
    scale_color_manual(breaks = c("1","2", "3"), values=c("green","red", "blue"))+
  scale_shape_manual(values = c(17, 19, 15))+
    xlab('Longitude') + 
    ylab('Latitude') +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size=14, face="bold"),
      axis.title.y = element_text(size=14, face="bold"))
 
dev.off()
  
###plot 2018 layout 
pdf(file = 'Figure7d.pdf',   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 3) # The height of the plot in inches

  g4 = ggplot(figure7_metric, aes(Longitude, Latitude, color = as.factor(Group2018), 
                                  shape = as.factor(Group2018), label = Turbine, alpha = abs(Metric2018)))
  g4 + geom_point(size = 6, show.legend = FALSE) + 
    geom_text(size = 4, vjust = 2, color = 'black', fontface = 'bold', show.legend = FALSE) + 
    ylim(c(40.04, 40.135)) + 
    scale_color_manual(breaks = c("1","2", "3"), values=c("green","red", "blue"))+
  scale_shape_manual(values = c(17, 19, 15))+
    xlab('Longitude') + 
    ylab('Latitude') +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size=14, face="bold"),
      axis.title.y = element_text(size=14, face="bold"))

dev.off()
