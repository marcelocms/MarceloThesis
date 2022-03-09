library(ggplot2)

difSamples <- read.csv("tab4.csv")

p1 <- ggplot(difSamples, aes(y=pvalue, x=experiment, col=test)) + 
  geom_point()

p1 + geom_hline(yintercept = c(0.025,0.075), linetype='dashed')+
  scale_x_continuous(breaks = seq(1,12, by=1))+
  scale_color_manual(labels = c("ANOVA", "Two-Stage"), values = c("red", "blue"))

################ graph with same sample sizes, diff SD#######
sameSamples <- read.csv("sameSampleSize.csv")

p2 <- ggplot(sameSamples, aes(y=pvalue, x=experiment, col=test)) + 
  geom_point()

p2 + geom_hline(yintercept = c(0.025,0.075), linetype='dashed')+
  scale_x_continuous(breaks = seq(1,9, by=1))+
  scale_color_manual(labels = c("ANOVA", "Two-Stage"), values = c("red", "blue"))
############## graph with diff sample sizes and SD #########
comboSizeSD <- read.csv("difSampleSD.csv")
  

p3 <- ggplot(comboSizeSD, aes(y=pvalue, x=experiment, col=test)) + 
  geom_point()

p3 + geom_hline(yintercept = c(0.025,0.075), linetype='dashed') +
  scale_x_continuous(breaks = seq(1,12, by=1))+
  scale_color_manual(labels = c("ANOVA", "Two-Stage"), values = c("red", "blue"))

# Bootstrapping graphs

# diff sample size, same SD 
difBootSamples <- read.csv("DiffSampleSizeBoot.csv")

p1 <- ggplot(difBootSamples, aes(y=pvalue, x=experiment, col=test)) + 
  geom_point()

p1 + geom_hline(yintercept = c(0.025,0.075), linetype='dashed')+
  scale_x_continuous(breaks = seq(1,12, by=1))+
  scale_color_manual(labels = c("ANOVA", "Boot"), values = c("red", "blue"))

################ BOOT graph with same sample sizes, diff SD#######
sameSamples <- read.csv("DiffSDBoot.csv")

p2 <- ggplot(sameSamples, aes(y=pvalue, x=experiment, col=test)) + 
  geom_point()

p2 + geom_hline(yintercept = c(0.025,0.075), linetype='dashed')+
  scale_x_continuous(breaks = seq(1,9, by=1))+
  scale_color_manual(labels = c("ANOVA", "Boot"), values = c("red", "blue"))

############## BOOT graph with diff sample sizes and SD #########

comboSizeBootSD <- read.csv("tableEight.csv")


p3 <- ggplot(comboSizeBootSD, aes(y=pvalue, x=experiment, col=test)) + 
  geom_point()

p3 + geom_hline(yintercept = c(0.025,0.075), linetype='dashed') +
  scale_x_continuous(breaks = seq(1,12, by=1))+
  scale_color_manual(labels = c("ANOVA", "Boot"), values = c("red", "blue"))
