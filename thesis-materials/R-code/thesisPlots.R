library(ggplot2)

difSamples <- read.csv("variedSamples.csv")

p1 <- ggplot(difSamples, aes(y=pvalue, x=experiment, col=test)) + 
  geom_point()

p1 + geom_hline(yintercept = c(0.025,0.075), linetype='dashed')

sameSamples <- read.csv("sameSampleSize.csv")

p2 <- ggplot(sameSamples, aes(y=pvalue, x=expirement, col=test)) + 
  geom_point()

p2 + geom_hline(yintercept = c(0.025,0.075), linetype='dashed')