# Run Code_Figure5.R to generate necessary results before running this file.

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


# Figure 6
fig6 <- ggplot(perdf, aes(x = comp, y = stat_diff, fill = comp)) +
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
fig6

ggsave("Figure6.pdf", width = 7.35, height = 5.6)