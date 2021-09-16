# Data stored in 'PeriodData' folder will be used.
library(DSWE)
library(ggplot2)

#######################Creating a metric and location file##########################
location_data = read.csv('Dataset_location.csv')
figure9_metric = location_data

for(i in c(1, 2, 3, 4)){
  
  data_ref = read.csv(paste0('PeriodData/', 'Dataset4_',12,'_',1, '.csv'))
  xCol = c(4, 2, 8)
  yCol = 6
  testCol = c(2, 4)
  str1 = paste0('Period', i)
  str2 = paste0('Metric', i)
  for(j in 1:66){
    if(j == 12 & i == 1) next
    data_num = read.csv(paste0('PeriodData/', 'Dataset4_',j,'_',i, '.csv'))
    datalist = list(data_num, data_ref)
    ComparePCurveOutput = ComparePCurve(datalist, xCol = xCol, yCol = yCol, 
                                        xCol.circ = NULL, testCol = c(2,4))$scaledDiff
    figure9_metric[j, str2] = ComparePCurveOutput
  }
  
  figure9_metric[is.na(figure9_metric[ , str2]), str2] = 0
  
  figure9_metric[figure9_metric[, str2] > 0, str1] = 1
  figure9_metric[figure9_metric[, str2] < 0, str1] = 2
  
  if( i == 1){
    figure9_metric[figure9_metric$Turbine == 12, str2] = 20
    figure9_metric[figure9_metric$Turbine == 12, str1] = 3
  }
  
}

##################Visualizing the results############################################
####plot 2015 layout
pdf(file = 'Figure9a.pdf',   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 3) # The height of the plot in inches

g1 = ggplot(figure9_metric, aes(Longitude, Latitude, color = as.factor(Period1), 
                                shape = as.factor(Period1), label = Turbine, alpha = abs(Metric1)))
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
pdf(file = 'Figure9b.pdf',   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 3) # The height of the plot in inches

g2 = ggplot(figure9_metric, aes(Longitude, Latitude, color = as.factor(Period2), 
                                shape = as.factor(Period2), label = Turbine, alpha = abs(Metric2)))
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
pdf(file = 'Figure9c.pdf',   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 3) # The height of the plot in inches

g3 = ggplot(figure9_metric, aes(Longitude, Latitude, color = as.factor(Period3), 
                                shape = as.factor(Period3), label = Turbine, alpha = abs(Metric3)))
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
pdf(file = 'Figure9d.pdf',   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 3) # The height of the plot in inches

g4 = ggplot(figure9_metric, aes(Longitude, Latitude, color = as.factor(Period4), 
                                shape = as.factor(Period4), label = Turbine, alpha = abs(Metric4)))
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