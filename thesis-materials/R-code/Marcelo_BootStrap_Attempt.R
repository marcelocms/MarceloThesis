library(LaplacesDemon)
n = 1000 #number of bootstrap resamples
set.seed(42) # for reproducibility
n1=20
n2=40
n3=60

seq_a = rep("a",n1)
seq_b = rep("b",n2)
seq_c = rep("c", n3)
seq_total = c(seq_a, seq_b, seq_c)
sequence <- factor(seq_total)
totalTrials <- 0
error <- 0

for(i in 1:10000) {
  #s1 = runif(n3, -sqrt(3), sqrt(3))
  s1 = runif(n3, -sqrt(3), sqrt(3))
  s2 = runif(n2, -sqrt(12), sqrt(12))
  s3 = runif(n1, -sqrt(48), sqrt(48))
  #s2 = rnorm(n1,0,4)
  #s3 = rnormm(n2,p,mu,sigma2)
  nums= c(s1, s2, s3)
  

  pvalue = # mean( Boot.test.stat >= F.stat)
  if(pvalue <= 0.05){
    error = error +1
  }
  totalTrials = totalTrials +1
}

error/totalTrials

# Code to use as a foundation/integrate into function



anova_test <- function(d, i){ #first argument is passing the data set
  # second argument could be an index of the observations within the data set
}

#Loop over n

for (i in seq_len(n)) {
  bootanova <- aov(x ~ y)
}