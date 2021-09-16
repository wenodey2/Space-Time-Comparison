# Data stored in 'YearData' folder will be used.
library(DSWE)
library(ggplot2)

#######################Creating a metric and location file##########################
location_data = read.csv('Dataset_location.csv')
figure8_metric = location_data

for(i in c(2015, 2016, 2017, 2018)){
  
  data_ref = read.csv(paste0('YearData/', 'Dataset4_',12,'_',2015, '.csv'))
  xCol = c(4, 2, 8)
  yCol = 6
  testCol = c(2, 4)
  str1 = paste0('Group', i)
  str2 = paste0('Metric', i)
  for(j in 1:66){
    if(j == 12 & i == 2015) next
    data_num = read.csv(paste0('YearData/', 'Dataset4_',j,'_',i, '.csv'))
    datalist = list(data_num, data_ref)
    ComparePCurveOutput = ComparePCurve(datalist, xCol = xCol, yCol = yCol, 
                                        xCol.circ = NULL, testCol = c(2,4))$scaledDiff
    figure8_metric[j, str2] = ComparePCurveOutput
  }
  
  figure8_metric[is.na(figure8_metric[ , str2]), str2] = 0
  
  figure8_metric[figure8_metric[, str2] > 0, str1] = 1
  figure8_metric[figure8_metric[, str2] < 0, str1] = 2
  
  if( i == 2015){
    figure8_metric[figure8_metric$Turbine == 12, str2] = 20
    figure8_metric[figure8_metric$Turbine == 12, str1] = 3
  }
  
}

##################Visualizing the results############################################
####plot 2015 layout
pdf(file = 'Figure8a.pdf',   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 3) # The height of the plot in inches

g1 = ggplot(figure8_metric, aes(Longitude, Latitude, color = as.factor(Group2015), 
                                shape = as.factor(Group2015), label = Turbine, alpha = abs(Metric2015)))
g1 + geom_point(size = 6, show.legend = FALSE) + 
  geom_text(size = 4, vjust = 2, color = 'black', fontface = 'bold', show.legend = FALSE) + 
  ylim(c(40.04, 40.135)) + 
  scale_color_manual(breaks = c("2", "3"), values=c("red", "blue"))+
  scale_shape_manual(values = c(19, 15))+
  xlab('Longitude') + 
  ylab('Latitude') +
  theme_bw()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))
dev.off()

####plot 2016 layout 
pdf(file = 'Figure8b.pdf',   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 3) # The height of the plot in inches

g2 = ggplot(figure8_metric, aes(Longitude, Latitude, color = as.factor(Group2016), 
                                shape = as.factor(Group2016), label = Turbine, alpha = abs(Metric2016)))
g2 + geom_point(size = 6, show.legend = FALSE) + 
  geom_text(size = 4, vjust = 2, color = 'black', fontface = 'bold', show.legend = FALSE) + 
  ylim(c(40.04, 40.135)) + 
  scale_color_manual(breaks = c("1","2"), values=c("green","red"))+
  scale_shape_manual(values = c(17, 19))+
  xlab('Longitude') + 
  ylab('Latitude') +
  theme_bw()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))
dev.off()

#####plot 2017 layout  
pdf(file = 'Figure8c.pdf',   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 3) # The height of the plot in inches

g3 = ggplot(figure8_metric, aes(Longitude, Latitude, color = as.factor(Group2017), 
                                shape = as.factor(Group2017), label = Turbine, alpha = abs(Metric2017)))
g3 + geom_point(size = 6, show.legend = FALSE) + 
  geom_text(size = 4, vjust = 2, color = 'black', fontface = 'bold', show.legend = FALSE) + 
  ylim(c(40.04, 40.135)) + 
  scale_color_manual(breaks = c("1","2"), values=c("green","red"))+
  scale_shape_manual(values = c(17, 19))+
  xlab('Longitude') + 
  ylab('Latitude') +
  theme_bw()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))
dev.off()

###plot 2018 layout 
pdf(file = 'Figure8d.pdf',   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 3) # The height of the plot in inches

g4 = ggplot(figure8_metric, aes(Longitude, Latitude, color = as.factor(Group2018), 
                                shape = as.factor(Group2018), label = Turbine, alpha = abs(Metric2018)))
g4 + geom_point(size = 6, show.legend = FALSE) + 
  geom_text(size = 4, vjust = 2, color = 'black', fontface = 'bold', show.legend = FALSE) + 
  ylim(c(40.04, 40.135)) + 
  scale_color_manual(breaks = c("1","2"), values=c("green","red"))+
  scale_shape_manual(values = c(17, 19))+
  xlab('Longitude') + 
  ylab('Latitude') +
  theme_bw()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))

dev.off()