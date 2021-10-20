library(LaplacesDemon)

sigma <- c(sqrt(0.75), sqrt(13.25))
p <- c(0.98, 0.02)
mu <- c(0,0)

sigma2 <- c(sqrt(3), sqrt(53))
p <- c(0.98, 0.02)
mu <- c(0,0)
s2 = rnormm(100000, p, mu, sigma)
mean(s2)
sd(s2)

sigma4 <- c(sqrt(12), sqrt(212))
p <- c(0.98, 0.02)
mu <- c(0,0)
s2 = rnormm(100000, p, mu, sigma)
mean(s2)
sd(s2)

n1=10
n2=20
n3=30

seq_a = rep("a",n1)
seq_b = rep("b",n2)
seq_c = rep("c", n3)
seq_total = c(seq_a, seq_b, seq_c)
sequence <- factor(seq_total)
totalTrials <- 0
error <- 0

for(i in 1:10000) {
  s1 = runif(n3, -sqrt(3), sqrt(3))
  s2 = rnorm(n1,0,4)
  s3 = rnormm(n2,p,mu,sigma2)
  nums= c(s1, s2, s3)
  
  results = oneway.test(nums ~ sequence, var.equal = TRUE)
  pvalue = results[[3]]
  if(pvalue <= 0.05){
    error = error +1
  }
  totalTrials = totalTrials +1
}

error/totalTrials
