library(ggplot2)

difSamples <- read.csv("variedSamples.csv")

p1 <- ggplot(difSamples, aes(y=pvalue, x=experiment, col=test)) + 
  geom_point()

p1 + geom_hline(yintercept = c(0.025,0.075), linetype='dashed')+
  scale_x_continuous(breaks = seq(1,9, by=1))

################ graph with same sample sizes, diff SD#######
sameSamples <- read.csv("sameSampleSize.csv")

p2 <- ggplot(sameSamples, aes(y=pvalue, x=experiment, col=test)) + 
  geom_point()

p2 + geom_hline(yintercept = c(0.025,0.075), linetype='dashed')+
  scale_x_continuous(breaks = seq(1,9, by=1))
############## graph with diff sample sizes and SD #########
comboSizeSD <- read.csv("difSampleSD.csv")

p3 <- ggplot(comboSizeSD, aes(y=pvalue, x=experiment, col=test)) + 
  geom_point()

p3 + geom_hline(yintercept = c(0.025,0.075), linetype='dashed') +
  scale_x_continuous(breaks = seq(1,12, by=1))

