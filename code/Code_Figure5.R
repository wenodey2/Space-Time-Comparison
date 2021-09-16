# Create a folder named 'PeriodData' in working directory.
# Then run the script below.


### Split datasets into 4 periods each
for (i in 1:66) {
  data = read.csv(paste0('Dataset4_',i,'.csv'))
  colnames(data) = c('time', 'wind_speed', 'wind_direction', 
                     'ambient_temp', 'terrain', 'active_power', 
                     'stdeviation', 'turbulence_intensity')
  data$time=as.POSIXct(data$time, format = '%Y-%m-%d%H:%M:%S')
  
  data1 = data[data$time < '2016-07-01',]
  data2 = data[data$time > '2016-07-31' & data$time < '2017-11-01',]
  data3 = data[data$time > '2017-11-30' & data$time < '2018-06-01',]
  data4 = data[data$time > '2018-06-30',]
  data_x = list(data1, data2, data3, data4)
  
  for (j in 1:4) {
    write.csv(data_x[[j]], paste0('PeriodData/','Dataset4_',i,'_',j, '.csv'), 
              row.names = FALSE)
  }
}


### Compare periods for all datasets
library(DSWE)

for (p in 1:3) {
  # Periods data frame
  periods_df = data.frame(
    Dataset=double(), weightedDiff=double(), weightedStatDiff=double(),
    scaledDiff=double(), scaledStatDiff=double(), unweightedDiff=double(),
    unweightedStatDiff=double()
  )
  
  # Compare periods
  for (j in 1:66) {
    dat1 = read.csv(paste0('PeriodData/Dataset4_', j, '_', p, '.csv'))
    dat2 = read.csv(paste0('PeriodData/Dataset4_', j, '_', p + 1, '.csv'))
    datalist = list(dat1, dat2)
    xCol = c(2, 4, 8)
    yCol = 6
    testCol = c(2, 4)
    ComparePCurveOutput = ComparePCurve(datalist, xCol = xCol, yCol = yCol, 
                                        xCol.circ = NULL, testCol = c(2,4))
    periods_df[j,] = c(j, ComparePCurveOutput$weightedDiff, 
                       ComparePCurveOutput$weightedStatDiff, 
                       ComparePCurveOutput$scaledDiff,
                       ComparePCurveOutput$scaledStatDiff, 
                       ComparePCurveOutput$unweightedDiff, 
                       ComparePCurveOutput$unweightedStatDiff)
  }
  write.csv(periods_df, 
            paste0('PeriodData/Period_', p, '_', p + 1, '_results.csv'), 
            row.names = FALSE)
}


### Plots
library(ggplot2)

dat = list()
for (i in 1:3) {
  dat[[i]] = read.csv(paste0('PeriodData/Period_', i,'_', i+1, '_results.csv'))
}

regular_diff = c(dat[[1]]$weightedDiff, dat[[2]]$weightedDiff, 
                 dat[[3]]$weightedDiff, dat[[1]]$scaledDiff,  
                 dat[[2]]$scaledDiff, dat[[3]]$scaledDiff)
stat_diff = c(dat[[1]]$weightedStatDiff, dat[[2]]$weightedStatDiff,
              dat[[3]]$weightedStatDiff, dat[[1]]$scaledStatDiff,
              dat[[2]]$scaledStatDiff, dat[[3]]$scaledStatDiff)

status <- c(rep('Weighted', 198), rep('Scaled', 198))
comp <- c(rep('[2-1]', 66), rep('[3-2]', 66), rep('[4-3]', 66), 
          rep('[2-1]', 66), rep('[3-2]', 66), rep('[4-3]', 66))

perdf = data.frame(regular_diff, status, comp)
actdf = data.frame(stat_diff, status, comp)

# Figure 5
fig5 <- ggplot(perdf, aes(x = comp, y = regular_diff, fill = comp)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  facet_wrap(~status) +
  labs(x="Time Period Comparisons", y = "Power Difference (%)") +
  theme_bw() +
  ylim(-13, 22) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 14),
        legend.position = "none") + 
  scale_fill_grey(start = 0.6, end = 0.99)
fig5

ggsave("Figure5.pdf", width = 7.35, height = 5.6)