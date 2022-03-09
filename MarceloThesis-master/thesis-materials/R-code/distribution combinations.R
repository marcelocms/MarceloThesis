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

n = 10000
x1 = rnormm(n, p, mu, sigma)
sd(x1)
mean(x1)


n1 = 20
n2 = 40
n3 = 60
seq_a = rep("a",n1)
seq_b = rep("b",n2)
seq_c = rep("c", n3)
seq_total = c(seq_a, seq_b, seq_c)
sequence <- factor(seq_total)
totalTrials <- 0
error <- 0

# Sep 29 normality testing from mix, norm, and unif dist combinations
for(i in 1:10000) {
  
  #s1 = rnormm(n1, p, mu, sigma)
  #s1= rnorm(n1,0,1)
  
  s1= rnorm(n3,0,1)
  s2= rnorm(n2,0,2)
  s3= rnorm(n1,0,4)
  #s2 = rnormm(n1, p, mu, sigma)
  #s3 = rnormm(n3, p, mu, sigma)
  #s3 = runif(n2,-sqrt(3),sqrt(3))
  nums= c(s1, s2, s3)
  norm_results1 = shapiro.test(s1)
  norm_results2 = shapiro.test(s2)
  norm_results3 = shapiro.test(s3)
  
  pvalue_s1 = norm_results1[[2]]
  pvalue_s2 = norm_results2[[2]]
  pvalue_s3 = norm_results3[[2]]
  p_min = min(pvalue_s1, pvalue_s2, pvalue_s3)
  if(p_min <= 0.05){
    results = kruskal.test(nums ~ sequence)
    pvalue = results[[3]]
  }else{
    results = oneway.test(nums ~ sequence, var.equal = TRUE)
    pvalue = results[[3]]
  }
  
  if(pvalue <= 0.05){
    error = error +1
  }
  totalTrials = totalTrials +1
}

error/totalTrials

