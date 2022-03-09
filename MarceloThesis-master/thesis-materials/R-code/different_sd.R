# normal sigma = 2
x = rnorm(10000, 0, 2)
sd(x)


# uniform sigma = 2
x2 = runif(100000, -sqrt(12), sqrt(12))
mean(x2)
sd(x2)
n=20
seq_a = rep("a",n)
seq_b = rep("b",n)
seq_c = rep("c", n)
seq_total = c(seq_a, seq_b, seq_c)
sequence <- factor(seq_total)
totalTrials <- 0
error <- 0


# Aug 18 Uniform Dist analysis

for(i in 1:10000) {
  s1 = runif(n, -sqrt(3), sqrt(3))
  s2 = runif(n, -sqrt(12), sqrt(12))
  s3 = runif(n, -sqrt(24), sqrt(24))
  nums= c(s1, s2, s3)
  
  results = oneway.test(nums ~ sequence, var.equal = TRUE)
  pvalue = results[[3]]
  if(pvalue <= 0.05){
    error = error +1
  }
  totalTrials = totalTrials +1
}

error/totalTrials




# Mix-normal distribution again
# -> has outliers
sigma <- c(sqrt(0.75), sqrt(13.25))
p <- c(0.25, 0.75)
mu <- c(0,10)

n = 1000
x1 = rnormm(n, p, mu, sigma)
hist(x1, breaks = 25)

sd(x1)
mean(x1)
# mixed normal sigma = 2
library(LaplacesDemon)
sigma <- c(sqrt(3), sqrt(53))
p <- c(0.98, 0.02)
mu <- c(0,0)
s2 = rnormm(100000, p, mu, sigma)
mean(s2)
sd(s2)




##########
# normal sigma = 4
x = rnorm(10000, 0, 4)
sd(x)


# uniform sigma = 4
x2 = runif(100000, -sqrt(48), sqrt(48))
mean(x2)
sd(x2)


# mixed normal sigma = 4
library(LaplacesDemon)
sigma <- c(sqrt(12), sqrt(212))
p <- c(0.98, 0.02)
mu <- c(0,0)
s2 = rnormm(100000, p, mu, sigma)
mean(s2)
sd(s2)




